module CoinFlipGameTest exposing (clockSuite, extraTimeSuite, formattingSuite, gambleSuite, gameOverSuite, hiddenBiasSuite, shopSuite, winCapSuite)

{-| Tests for the rigged-coin game engine shared by level 1
(even-with-an-edge-you-lose.html) and level 2 (hidden-rewards.html).
They drive the real `update` with the real level configs; the coin landing
is a plain message (`CoinLanded`), so outcomes are testable without
running the random command.
-}

import CoinFlipGame
    exposing
        ( Autoclicker(..)
        , BiasState(..)
        , CoinSide(..)
        , GamePhase(..)
        , LevelConfig
        , LogTone(..)
        , Model
        , Msg(..)
        , TrackerState(..)
        , WinCapTier(..)
        , WinCapUpsell(..)
        , formatCents
        , formatClock
        , initialModel
        , landedPercent
        , PendingPurchase(..)
        , ShopItemKind(..)
        , parseBetCents
        , quickBetCents
        , trueEndingMessage
        , update
        , view
        , winCapUpsell
        )
import CoinFlipLevel1
import CoinFlipLevel2
import Expect
import Test exposing (Test, describe, test)
import ShopDialog
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, tag, text)


apply : LevelConfig -> List Msg -> Model -> Model
apply config msgs model =
    List.foldl (\msg current -> Tuple.first (update config msg current)) model msgs


level1Start : Model
level1Start =
    initialModel CoinFlipLevel1.levelConfig


level2Start : Model
level2Start =
    initialModel CoinFlipLevel2.levelConfig


landedFlip : CoinSide -> Int -> CoinSide -> Msg
landedFlip playerChoice betCents landed =
    CoinLanded { playerChoice = playerChoice, betCents = betCents, landed = landed }


latestLogText : Model -> String
latestLogText model =
    case List.head model.log of
        Nothing ->
            "<empty log>"

        Just line ->
            line.text


gambleSuite : Test
gambleSuite =
    describe "CoinFlipGame betting and settling"
        [ test "a winning flip pays out the bet" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig [ landedFlip Heads 1000 Heads ] level1Start
                    |> .balanceCents
                    |> Expect.equal 3500
        , test "a losing flip deducts the bet" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig [ landedFlip Heads 1000 Tails ] level1Start
                    |> .balanceCents
                    |> Expect.equal 1500
        , test "reaching the target ends the game as won" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ landedFlip Heads 1000 Heads ]
                    { level1Start | balanceCents = 99000 }
                    |> .phase
                    |> Expect.equal WonGame
        , test "losing the whole balance goes bust at exactly $0.00" <|
            \_ ->
                let
                    busted =
                        apply CoinFlipLevel1.levelConfig
                            [ landedFlip Heads 1000 Tails ]
                            { level1Start | balanceCents = 1000 }
                in
                ( busted.phase, busted.balanceCents )
                    |> Expect.equal ( WentBust, 0 )
        , test "flip log lines carry their flip number" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ landedFlip Heads 1000 Heads, landedFlip Heads 500 Tails ]
                    level1Start
                    |> latestLogText
                    |> Expect.equal "2: Landed Tails! You lost $5.00"
        , test "every settled flip adds one log divider" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ landedFlip Heads 100 Heads, landedFlip Heads 100 Tails ]
                    level1Start
                    |> .log
                    |> List.filter (\line -> line.tone == DividerTone)
                    |> List.length
                    |> Expect.equal 2
        , test "flips count both what was bet and what landed" <|
            \_ ->
                let
                    played =
                        apply CoinFlipLevel1.levelConfig
                            [ landedFlip Heads 100 Tails
                            , landedFlip Tails 100 Tails
                            , landedFlip Heads 100 Heads
                            ]
                            level1Start
                in
                { flips = played.flipCount
                , headsBets = played.headsBetCount
                , tailsBets = played.tailsBetCount
                , headsLanded = played.headsLandedCount
                , tailsLanded = played.tailsLandedCount
                }
                    |> Expect.equal
                        { flips = 3, headsBets = 2, tailsBets = 1, headsLanded = 1, tailsLanded = 2 }
        , test "a bet above the balance is refused" <|
            \_ ->
                let
                    refused =
                        apply CoinFlipLevel1.levelConfig
                            [ BetInputChanged "26.00", BetPlaced Heads ]
                            level1Start
                in
                ( refused.balanceCents, latestLogText refused )
                    |> Expect.equal ( 2500, "You cannot bet more than your current balance!" )
        , test "an unparseable bet is refused" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ BetInputChanged "much", BetPlaced Heads ]
                    level1Start
                    |> latestLogText
                    |> Expect.equal "Enter a bet amount above $0.00."
        , test "the bet input is clamped down to the new balance after a loss" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ BetInputChanged "25.00", landedFlip Heads 2000 Tails ]
                    level1Start
                    |> .betInput
                    |> Expect.equal "5.00"
        , test "a quick-bet button fills in the fraction of the balance" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig [ QuickBetPicked 0.1 ] level1Start
                    |> .betInput
                    |> Expect.equal "2.50"
        , test "quickBetCents floors to whole cents but never to zero" <|
            \_ ->
                ( quickBetCents 0.1 3, quickBetCents 1.0 2500 )
                    |> Expect.equal ( 1, 2500 )
        ]


hiddenBiasSuite : Test
hiddenBiasSuite =
    describe "CoinFlipGame hidden bias (level 2)"
        [ test "the hidden bias starts undrawn" <|
            \_ ->
                level2Start.biasState
                    |> Expect.equal BiasUndrawn
        , test "betting before the bias is drawn is refused" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ BetInputChanged "1.00", BetPlaced Tails ]
                    level2Start
                    |> latestLogText
                    |> Expect.equal "The coin is still being rigged, try again."
        , test "the drawn bias is stored and unblocks betting" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ BiasDrawn { favored = Tails, favoredPercent = 61 } ]
                    level2Start
                    |> .biasState
                    |> Expect.equal (BiasReady { favored = Tails, favoredPercent = 61 })
        , test "landedPercent reports whole percents of all flips" <|
            \_ ->
                ( landedPercent 1 3, landedPercent 0 0 )
                    |> Expect.equal ( 33, 0 )
        ]


clickAtOrigin : ShopDialog.ClickPoint
clickAtOrigin =
    { x = 0, y = 0 }


shopSuite : Test
shopSuite =
    describe "CoinFlipGame shop (level 2)"
        [ test "the shop starts collapsed" <|
            \_ ->
                view CoinFlipLevel2.levelConfig level2Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "shop-item" ]
        , test "toggling the shop opens its items" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig [ ShopToggled ] level2Start
                    |> view CoinFlipLevel2.levelConfig
                    |> Query.fromHtml
                    |> Query.has [ class "shop-item" ]
        , test "toggling twice collapses the shop again" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig [ ShopToggled, ShopToggled ] level2Start
                    |> view CoinFlipLevel2.levelConfig
                    |> Query.fromHtml
                    |> Query.hasNot [ class "shop-item" ]
        , test "considering the tracker charges nothing" <|
            \_ ->
                let
                    considering =
                        apply CoinFlipLevel2.levelConfig
                            [ PurchaseConsidered TrackerItem clickAtOrigin ]
                            level2Start
                in
                ( considering.balanceCents, considering.tracker )
                    |> Expect.equal ( 2500, TrackerNotBought )
        , test "confirming the tracker buys it and closes the dialog" <|
            \_ ->
                let
                    bought =
                        apply CoinFlipLevel2.levelConfig
                            [ PurchaseConsidered TrackerItem clickAtOrigin, PurchaseConfirmed ]
                            level2Start
                in
                ( bought.balanceCents, bought.tracker, bought.pendingPurchase )
                    |> Expect.equal ( 1000, TrackerBought, NoPendingPurchase )
        , test "confirming uncle keeps the dialog open for repeat customers" <|
            \_ ->
                let
                    advised =
                        apply CoinFlipLevel2.levelConfig
                            [ PurchaseConsidered UncleAdviceItem clickAtOrigin
                            , PurchaseConfirmed
                            , PurchaseConfirmed
                            ]
                            level2Start
                in
                ( advised.balanceCents, advised.uncleAdviceCount, advised.pendingPurchase )
                    |> Expect.equal ( 1500, 2, Considering UncleAdviceItem clickAtOrigin )
        , test "cancelling a considered purchase keeps the money" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ PurchaseConsidered UncleAdviceItem clickAtOrigin, PurchaseCancelled ]
                    level2Start
                    |> .uncleAdviceCount
                    |> Expect.equal 0
        , test "buying the ratio tracker costs $15.00" <|
            \_ ->
                let
                    bought =
                        apply CoinFlipLevel2.levelConfig [ TrackerPurchased ] level2Start
                in
                ( bought.balanceCents, bought.tracker )
                    |> Expect.equal ( 1000, TrackerBought )
        , test "the tracker cannot be bought twice" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ TrackerPurchased, TrackerPurchased ]
                    level2Start
                    |> .balanceCents
                    |> Expect.equal 1000
        , test "the tracker is refused when it would wipe the balance" <|
            \_ ->
                let
                    refused =
                        apply CoinFlipLevel2.levelConfig
                            [ TrackerPurchased ]
                            { level2Start | balanceCents = 1500 }
                in
                ( refused.balanceCents, refused.tracker )
                    |> Expect.equal ( 1500, TrackerNotBought )
        , test "uncle charges $5.00 per piece of advice" <|
            \_ ->
                let
                    advised =
                        apply CoinFlipLevel2.levelConfig
                            [ UncleAdviceRequested, UncleAdviceRequested ]
                            level2Start
                in
                ( advised.balanceCents, advised.uncleAdviceCount )
                    |> Expect.equal ( 1500, 2 )
        , test "spending the last dollars on uncle goes bust" <|
            \_ ->
                let
                    busted =
                        apply CoinFlipLevel2.levelConfig
                            [ UncleAdviceRequested ]
                            { level2Start | balanceCents = 500 }
                in
                ( busted.balanceCents, busted.phase, busted.uncleAdviceCount )
                    |> Expect.equal ( 0, WentBust, 1 )
        , test "uncle is refused below his price" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ UncleAdviceRequested ]
                    { level2Start | balanceCents = 499 }
                    |> .uncleAdviceCount
                    |> Expect.equal 0
        , test "busting on uncle's advice gloats after the advice lands, once" <|
            \_ ->
                let
                    gloatLine =
                        "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"

                    busted =
                        apply CoinFlipLevel2.levelConfig
                            [ UncleAdviceRequested, UncleAdviceGiven "Bet big." ]
                            { level2Start | balanceCents = 500 }

                    gloatCount =
                        List.length (List.filter (\line -> line.text == gloatLine) busted.log)
                in
                ( latestLogText busted, gloatCount )
                    |> Expect.equal ( gloatLine, 1 )
        , test "a betting bust leaves uncle silent" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ landedFlip Heads 1000 Tails ]
                    { level2Start | balanceCents = 1000 }
                    |> latestLogText
                    |> Expect.notEqual "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
        , test "a betting bust after buying advice still leaves uncle silent" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ landedFlip Heads 1000 Tails ]
                    { level2Start | balanceCents = 1000, uncleAdviceCount = 1 }
                    |> latestLogText
                    |> Expect.notEqual "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
        , test "level 1 has no uncle to gloat on a bust" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ landedFlip Heads 1000 Tails ]
                    { level1Start | balanceCents = 1000 }
                    |> latestLogText
                    |> Expect.notEqual "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
        , test "uncle's advice ends up in the log" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ UncleAdviceGiven "Winners don't do math, son." ]
                    level2Start
                    |> latestLogText
                    |> Expect.equal "\u{1F9D3} Uncle: \u{201C}Winners don't do math, son.\u{201D}"
        , test "level 1 sells no tracker or uncle, stray purchases change nothing" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ TrackerPurchased, UncleAdviceRequested ]
                    level1Start
                    |> .balanceCents
                    |> Expect.equal 2500
        , test "buying the autoclicker costs $10.00" <|
            \_ ->
                let
                    bought =
                        apply CoinFlipLevel2.levelConfig [ AutoclickerPurchased ] level2Start
                in
                ( bought.balanceCents, bought.autoclicker )
                    |> Expect.equal ( 1500, ClickerBought )
        , test "level 1 sells the autoclicker too" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig [ AutoclickerPurchased ] level1Start
                    |> .balanceCents
                    |> Expect.equal 1500
        , test "the autoclicker cannot be bought twice" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ AutoclickerPurchased, AutoclickerPurchased ]
                    level2Start
                    |> .balanceCents
                    |> Expect.equal 1500
        , test "the autoclicker is refused when it would wipe the balance" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ AutoclickerPurchased ]
                    { level2Start | balanceCents = 1000 }
                    |> .autoclicker
                    |> Expect.equal ClickerNotBought
        , test "holding and releasing a bet button tracks the held side" <|
            \_ ->
                let
                    held =
                        apply CoinFlipLevel2.levelConfig [ BetHoldStarted Heads ] level2Start

                    released =
                        apply CoinFlipLevel2.levelConfig [ BetHoldEnded ] held
                in
                ( held.betHold, released.betHold )
                    |> Expect.equal ( CoinFlipGame.BetHeld Heads, CoinFlipGame.NoBetHeld )
        , test "an autoclicker tick validates the bet exactly like a manual press" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ AutoclickerPurchased, BetHoldStarted Heads, AutoclickerTicked ]
                    level2Start
                    |> latestLogText
                    |> Expect.equal "Enter a bet amount above $0.00."
        , test "a tick with no button held bets nothing" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ AutoclickerPurchased, AutoclickerTicked ]
                    level2Start
                    |> latestLogText
                    |> Expect.equal "Bought the autoclicker for $10.00. Hold a bet button down to use it."
        ]


clockSuite : Test
clockSuite =
    describe "CoinFlipGame clock"
        [ test "a tick removes one second" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig [ ClockTicked ] level1Start
                    |> .secondsLeft
                    |> Expect.equal (30 * 60 - 1)
        , test "the last second ends the game as out of time" <|
            \_ ->
                let
                    expired =
                        apply CoinFlipLevel1.levelConfig
                            [ ClockTicked ]
                            { level1Start | secondsLeft = 1 }
                in
                ( expired.phase, expired.secondsLeft )
                    |> Expect.equal ( RanOutOfTime, 0 )
        , test "flips landing after the game ended no longer settle" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ landedFlip Heads 1000 Heads ]
                    { level1Start | phase = RanOutOfTime }
                    |> .balanceCents
                    |> Expect.equal 2500
        ]


{-| The end screen renders through the real `view`; these tests assert
what is gated on the game phase (next-level link, bust quote, uncle
verdict) by element, never by wording.
-}
gameOverSuite : Test
gameOverSuite =
    describe "CoinFlipGame game-over gating"
        [ test "level 1 links the next level on a win" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.has [ tag "a" ]
        , test "level 1 shows no next-level link on a bust" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ tag "a" ]
        , test "level 2 links the next level on a win" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.has [ tag "a" ]
        , test "level 2 shows no next-level link on a bust" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ tag "a" ]
        , test "level 2 shows the bust ending when uncle got paid" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WentBust, uncleAdviceCount = 1 }
                    |> Query.fromHtml
                    |> Query.has [ class "bust-ending" ]
        , test "no bust ending when uncle never got a cent" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "bust-ending" ]
        , test "the bust ending stays hidden on a win" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "bust-ending" ]
        , test "level 1 has no bust ending configured" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "bust-ending" ]
        , test "bought uncle advice draws a verdict on a bust" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start | phase = WentBust, uncleAdviceCount = 1 }
                    |> Query.fromHtml
                    |> Query.has [ class "uncle-verdict" ]
        , test "bought uncle advice draws a verdict on a win" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start | phase = WonGame, uncleAdviceCount = 1 }
                    |> Query.fromHtml
                    |> Query.has [ class "uncle-verdict" ]
        , test "without uncle advice there is no verdict at all" <|
            \_ ->
                view CoinFlipLevel2.levelConfig { level2Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "uncle-verdict" ]
        , test "a bust points at the explanation below the game" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.has [ class "explanation-hint" ]
        , test "running out of time points at the explanation too" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = RanOutOfTime }
                    |> Query.fromHtml
                    |> Query.has [ class "explanation-hint" ]
        , test "a win shows no explanation hint" <|
            \_ ->
                view CoinFlipLevel1.levelConfig { level1Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "explanation-hint" ]
        ]


oneMinutePack : CoinFlipGame.ExtraTimePackage
oneMinutePack =
    { priceCents = 2000, extraSeconds = 60 }


thirtyMinutePack : CoinFlipGame.ExtraTimePackage
thirtyMinutePack =
    { priceCents = 75000, extraSeconds = 30 * 60 }


extraTimeSuite : Test
extraTimeSuite =
    describe "CoinFlipGame extra time (level 2)"
        [ test "buying 1 more minute costs $20 and extends the clock" <|
            \_ ->
                let
                    extended =
                        apply CoinFlipLevel2.levelConfig
                            [ ExtraTimePurchased oneMinutePack ]
                            level2Start
                in
                ( extended.balanceCents, extended.secondsLeft )
                    |> Expect.equal ( 500, 30 * 60 + 60 )
        , test "buying 30 more minutes costs $750 and extends the clock" <|
            \_ ->
                let
                    extended =
                        apply CoinFlipLevel2.levelConfig
                            [ ExtraTimePurchased thirtyMinutePack ]
                            { level2Start | balanceCents = 100000 }
                in
                ( extended.balanceCents, extended.secondsLeft )
                    |> Expect.equal ( 25000, 2 * 30 * 60 )
        , test "buying time is repeatable" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ ExtraTimePurchased oneMinutePack, ExtraTimePurchased oneMinutePack ]
                    { level2Start | balanceCents = 100000 }
                    |> .secondsLeft
                    |> Expect.equal (30 * 60 + 120)
        , test "more time is refused when it would wipe the balance" <|
            \_ ->
                let
                    refused =
                        apply CoinFlipLevel2.levelConfig
                            [ ExtraTimePurchased oneMinutePack ]
                            { level2Start | balanceCents = 2000 }
                in
                ( refused.balanceCents, refused.secondsLeft )
                    |> Expect.equal ( 2000, 30 * 60 )
        , test "a package the shop never offered is ignored" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ ExtraTimePurchased { priceCents = 1, extraSeconds = 9999 } ]
                    level2Start
                    |> .secondsLeft
                    |> Expect.equal (30 * 60)
        , test "level 1 sells no time" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ ExtraTimePurchased oneMinutePack ]
                    level1Start
                    |> .secondsLeft
                    |> Expect.equal (30 * 60)
        , test "confirming a time purchase keeps the dialog open for repeat buys" <|
            \_ ->
                let
                    bought =
                        apply CoinFlipLevel2.levelConfig
                            [ PurchaseConsidered (ExtraTimeItem oneMinutePack) clickAtOrigin
                            , PurchaseConfirmed
                            ]
                            { level2Start | balanceCents = 100000 }
                in
                ( bought.secondsLeft
                , bought.pendingPurchase
                )
                    |> Expect.equal
                        ( 30 * 60 + 60
                        , Considering (ExtraTimeItem oneMinutePack) clickAtOrigin
                        )
        ]


{-| The button text an upsell would render, so view tests can compare
against what `winCapUpsell` actually picked without pinning wording.
-}
upsellLabel : WinCapUpsell -> String
upsellLabel upsell =
    case upsell of
        NoFurtherUpsell ->
            "<no upsell>"

        WinCapUpsellFor sale ->
            sale.label


winCapSuite : Test
winCapSuite =
    describe "CoinFlipGame win cap upsell"
        [ test "raising the cap costs $500 and resumes play" <|
            \_ ->
                let
                    raised =
                        apply CoinFlipLevel2.levelConfig
                            [ WinCapRaised ]
                            { level2Start | phase = WonGame, balanceCents = 99900 }
                in
                ( raised.phase, raised.balanceCents, raised.winCapTier )
                    |> Expect.equal ( Playing, 49900, SecondCap )
        , test "the old target no longer wins after the raise" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ WinCapRaised, landedFlip Heads 50000 Heads ]
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 99900
                        , biasState = BiasReady { favored = Heads, favoredPercent = 60 }
                    }
                    |> .phase
                    |> Expect.equal Playing
        , test "reaching $9,999 wins the raised game" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ WinCapRaised, landedFlip Heads 950000 Heads ]
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 99900
                        , biasState = BiasReady { favored = Heads, favoredPercent = 60 }
                    }
                    |> .phase
                    |> Expect.equal WonGame
        , test "the second raise costs $8,500 and targets $99,999" <|
            \_ ->
                let
                    raised =
                        apply CoinFlipLevel2.levelConfig
                            [ WinCapRaised ]
                            { level2Start
                                | phase = WonGame
                                , balanceCents = 999900
                                , winCapTier = SecondCap
                            }
                in
                ( raised.phase, raised.balanceCents, raised.winCapTier )
                    |> Expect.equal ( Playing, 149900, DegenerateCap )
        , test "raising the cap releases a bet button still held from before the win" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ WinCapRaised ]
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 99900
                        , autoclicker = ClickerBought
                        , betHold = CoinFlipGame.BetHeld Heads
                    }
                    |> .betHold
                    |> Expect.equal CoinFlipGame.NoBetHeld
        , test "an uncle-less level gets its own degenerate label, not the uncle one" <|
            \_ ->
                upsellLabel (winCapUpsell CoinFlipLevel1.levelConfig.uncleOffer SecondCap)
                    |> Expect.notEqual
                        (upsellLabel (winCapUpsell CoinFlipLevel2.levelConfig.uncleOffer SecondCap))
        , test "level 1's win screen renders the label its own config picks" <|
            \_ ->
                view CoinFlipLevel1.levelConfig
                    { level1Start
                        | phase = WonGame
                        , balanceCents = 999900
                        , winCapTier = SecondCap
                    }
                    |> Query.fromHtml
                    |> Query.has
                        [ text (upsellLabel (winCapUpsell CoinFlipLevel1.levelConfig.uncleOffer SecondCap)) ]
        , test "level 2's win screen renders the label its own config picks" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 999900
                        , winCapTier = SecondCap
                    }
                    |> Query.fromHtml
                    |> Query.has
                        [ text (upsellLabel (winCapUpsell CoinFlipLevel2.levelConfig.uncleOffer SecondCap)) ]
        , test "a raise already past the new target wins again on the spot" <|
            \_ ->
                let
                    raised =
                        apply CoinFlipLevel2.levelConfig
                            [ WinCapRaised ]
                            { level2Start | phase = WonGame, balanceCents = 3000000 }
                in
                ( raised.phase, raised.winCapTier, raised.balanceCents )
                    |> Expect.equal ( WonGame, SecondCap, 2950000 )
        , test "there is no raise beyond the degenerate cap" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ WinCapRaised ]
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> .balanceCents
                    |> Expect.equal 9999900
        , test "raising the cap does nothing while still playing" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig [ WinCapRaised ] level2Start
                    |> .balanceCents
                    |> Expect.equal 2500
        , test "the win screen renders the upsell button" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start | phase = WonGame, balanceCents = 99900 }
                    |> Query.fromHtml
                    |> Query.has [ class "win-cap-upsell" ]
        , test "the bust screen renders no upsell button" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "win-cap-upsell" ]
        , test "the degenerate win renders no upsell button" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "win-cap-upsell" ]
        , test "the degenerate win draws the true ending" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> Query.fromHtml
                    |> Query.has [ text trueEndingMessage ]
        , test "lower-cap wins do not draw the true ending" <|
            \_ ->
                view CoinFlipLevel2.levelConfig
                    { level2Start | phase = WonGame, balanceCents = 99900 }
                    |> Query.fromHtml
                    |> Query.hasNot [ text trueEndingMessage ]
        ]


formattingSuite : Test
formattingSuite =
    describe "CoinFlipGame formatting"
        [ test "formatCents pads cents to two digits" <|
            \_ ->
                ( formatCents 999, formatCents 25, formatCents 2500 )
                    |> Expect.equal ( "9.99", "0.25", "25.00" )
        , test "formatClock matches the original m:ss display" <|
            \_ ->
                ( formatClock (30 * 60), formatClock 599 )
                    |> Expect.equal ( "30:00", "9:59" )
        , test "parseBetCents accepts dollars and rejects junk" <|
            \_ ->
                ( parseBetCents "10.00", parseBetCents "0.01", parseBetCents "much" )
                    |> Expect.equal ( Just 1000, Just 1, Nothing )
        , test "parseBetCents rejects zero and negative amounts" <|
            \_ ->
                ( parseBetCents "0", parseBetCents "-5" )
                    |> Expect.equal ( Nothing, Nothing )
        ]
