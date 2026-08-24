module MultiCoinGameTest exposing (correlationSuite, correlationUpgradeSuite, extraTimeSuite, extraTurnsSuite, flipSuite, refundSuite, gatingSuite, payoutSuite, shopSuite, shuffleSuite, stakeSuite, winCapSuite)

{-| Tests for the multi-coin engine behind level 3 (black-swan.html).
They drive the real `update` with the real level config; the coins
landing is a plain message (`CoinsLanded`), so outcomes are testable
without running the random command. Level 3's coins: Swan wins on rolls
1-5 paying 30x, Magpie on 1-45 paying 1x, Sparrow on 1-60 paying 0.5x.
-}

import CoinFlipGame
    exposing
        ( Autoclicker(..)
        , GamePhase(..)
        , TrackerState(..)
        , UncleOffer(..)
        , WinCapTier(..)
        , trueEndingMessage
        )
import CoinFlipLevel3
import CoinFlipLevel4
import Expect
import MultiCoinGame
    exposing
        ( AllocationMode(..)
        , RefundState(..)
        , BustCause(..)
        , CoinOdds(..)
        , CorrelationBook(..)
        , FlipHold(..)
        , GoldenGlasses(..)
        , Model
        , PendingPurchase(..)
        , ShopItemKind(..)
        , Msg(..)
        , StakeInput(..)
        , formatPayout
        , initialModel
        , parseStake
        , payoutCents
        , shuffledCoinsGenerator
        , update
        , view
        )
import Random
import ShopDialog
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)


apply : List Msg -> Model -> Model
apply msgs model =
    List.foldl
        (\msg current -> Tuple.first (update CoinFlipLevel3.levelConfig msg current))
        model
        msgs


level3Start : Model
level3Start =
    initialModel CoinFlipLevel3.levelConfig


landedRound : List Int -> List Int -> Msg
landedRound stakes rolls =
    CoinsLanded { stakes = stakes, weatherRoll = 1, rolls = rolls }


clickAtOrigin : ShopDialog.ClickPoint
clickAtOrigin =
    { x = 0, y = 0 }


latestLogText : Model -> String
latestLogText model =
    case List.head model.log of
        Nothing ->
            "<empty log>"

        Just line ->
            line.text


flipSuite : Test
flipSuite =
    describe "MultiCoinGame settling a round"
        [ test "a swan win pays thirty times the stake" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 5, 100, 100 ] ] level3Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 3000)
        , test "a swan loss costs only the stake" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 6, 100, 100 ] ] level3Start
                    |> .balanceCents
                    |> Expect.equal 2400
        , test "all staked coins settle together from one balance" <|
            \_ ->
                -- swan roll 5 wins +3000, magpie roll 50 loses -200,
                -- sparrow roll 50 wins +150 (half of 300)
                apply [ landedRound [ 100, 200, 300 ] [ 5, 50, 50 ] ] level3Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 3000 - 200 + 150)
        , test "unstaked coins do not touch the balance or tallies" <|
            \_ ->
                let
                    played =
                        apply [ landedRound [ 0, 200, 0 ] [ 5, 50, 50 ] ] level3Start
                in
                ( played.balanceCents, List.map .flipCount played.tallies )
                    |> Expect.equal ( 2300, [ 0, 1, 0 ] )
        , test "every settled round adds one log divider" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 5, 100, 100 ], landedRound [ 100, 0, 0 ] [ 5, 100, 100 ] ]
                    level3Start
                    |> .log
                    |> List.filter (\line -> line.tone == CoinFlipGame.DividerTone)
                    |> List.length
                    |> Expect.equal 2
        , test "tallies count heads per staked coin" <|
            \_ ->
                apply
                    [ landedRound [ 100, 100, 100 ] [ 5, 50, 50 ]
                    , landedRound [ 100, 100, 0 ] [ 90, 40, 50 ]
                    ]
                    level3Start
                    |> .tallies
                    |> Expect.equal
                        [ { headsCount = 1, flipCount = 2 }
                        , { headsCount = 1, flipCount = 2 }
                        , { headsCount = 1, flipCount = 1 }
                        ]
        , test "losing the whole balance goes bust at exactly $0.00" <|
            \_ ->
                let
                    busted =
                        apply [ landedRound [ 100, 100, 100 ] [ 100, 100, 100 ] ]
                            { level3Start | balanceCents = 300 }
                in
                ( busted.phase, busted.balanceCents )
                    |> Expect.equal ( WentBust, 0 )
        , test "reaching the target ends the game as won" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 1, 100, 100 ] ]
                    { level3Start | balanceCents = 97000 }
                    |> .phase
                    |> Expect.equal WonGame
        , test "rounds landing after the game ended no longer settle" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 1, 100, 100 ] ]
                    { level3Start | phase = RanOutOfTime }
                    |> .balanceCents
                    |> Expect.equal 2500
        ]


stakeSuite : Test
stakeSuite =
    describe "MultiCoinGame stake validation on flip"
        [ test "no stakes at all is refused" <|
            \_ ->
                apply [ FlipPressed ] level3Start
                    |> latestLogText
                    |> Expect.equal "Place a bet on at least one coin first."
        , test "staking more than the balance is refused" <|
            \_ ->
                apply
                    [ StakeInputChanged 0 "20.00"
                    , StakeInputChanged 1 "10.00"
                    , FlipPressed
                    ]
                    level3Start
                    |> latestLogText
                    |> Expect.equal "You cannot bet more than your current balance!"
        , test "an unreadable stake is an error naming the coin, never a silent no-bet" <|
            \_ ->
                apply [ StakeInputChanged 1 "much", FlipPressed ] level3Start
                    |> latestLogText
                    |> Expect.equal "Cannot read your bet on Magpie."
        , test "parseStake classifies blank, junk, and amounts" <|
            \_ ->
                ( parseStake "", parseStake "much", parseStake "1.50" )
                    |> Expect.equal ( NoStake, UnreadableStake, Stake 150 )
        , test "parseStake treats zero as no stake" <|
            \_ ->
                ( parseStake "0", parseStake "0.00" )
                    |> Expect.equal ( NoStake, NoStake )
        ]


payoutSuite : Test
payoutSuite =
    describe "MultiCoinGame payouts"
        [ test "payouts floor to whole cents" <|
            \_ ->
                payoutCents 50 3
                    |> Expect.equal 1
        , test "a win never pays zero cents" <|
            \_ ->
                payoutCents 50 1
                    |> Expect.equal 1
        , test "formatPayout renders whole and fractional multipliers" <|
            \_ ->
                ( formatPayout 3000, formatPayout 100, formatPayout 50 )
                    |> Expect.equal ( "30\u{00D7}", "1\u{00D7}", "0.5\u{00D7}" )
        ]


shopSuite : Test
shopSuite =
    describe "MultiCoinGame shop"
        [ test "buying the ratio tracker costs $15.00" <|
            \_ ->
                let
                    bought =
                        apply [ TrackerPurchased ] level3Start
                in
                ( bought.balanceCents, bought.tracker )
                    |> Expect.equal ( 1000, TrackerBought )
        , test "the tracker cannot be bought twice" <|
            \_ ->
                apply [ TrackerPurchased, TrackerPurchased ] level3Start
                    |> .balanceCents
                    |> Expect.equal 1000
        , test "the tracker is refused when it would wipe the balance" <|
            \_ ->
                apply [ TrackerPurchased ] { level3Start | balanceCents = 1500 }
                    |> .tracker
                    |> Expect.equal TrackerNotBought
        , test "uncle charges $5.00 per piece of advice" <|
            \_ ->
                let
                    advised =
                        apply [ UncleAdviceRequested, UncleAdviceRequested ] level3Start
                in
                ( advised.balanceCents, advised.uncleAdviceCount )
                    |> Expect.equal ( 1500, 2 )
        , test "spending the last dollars on uncle goes bust, attributed to uncle" <|
            \_ ->
                let
                    busted =
                        apply [ UncleAdviceRequested ] { level3Start | balanceCents = 500 }
                in
                ( busted.balanceCents, busted.phase, busted.bustCause )
                    |> Expect.equal ( 0, WentBust, BustByUncleAdvice )
        , test "busting on uncle's advice gloats after the advice lands, once" <|
            \_ ->
                let
                    gloatLine =
                        "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"

                    busted =
                        apply [ UncleAdviceRequested, UncleAdviceGiven "Bet big." ]
                            { level3Start | balanceCents = 500 }

                    gloatCount =
                        List.length (List.filter (\line -> line.text == gloatLine) busted.log)
                in
                ( latestLogText busted, gloatCount )
                    |> Expect.equal ( gloatLine, 1 )
        , test "a betting bust leaves uncle silent" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 100, 100, 100 ] ]
                    { level3Start | balanceCents = 100 }
                    |> latestLogText
                    |> Expect.notEqual "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
        , test "a betting bust after buying advice still leaves uncle silent" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 100, 100, 100 ] ]
                    { level3Start | balanceCents = 100, uncleAdviceCount = 1 }
                    |> latestLogText
                    |> Expect.notEqual "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
        , test "busting on a flip is attributed to betting" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 100, 100, 100 ] ]
                    { level3Start | balanceCents = 100 }
                    |> .bustCause
                    |> Expect.equal BustByBetting
        , test "uncle is refused below his price" <|
            \_ ->
                apply [ UncleAdviceRequested ] { level3Start | balanceCents = 499 }
                    |> .uncleAdviceCount
                    |> Expect.equal 0
        , test "buying the autoclicker costs $10.00" <|
            \_ ->
                let
                    bought =
                        apply [ AutoclickerPurchased ] level3Start
                in
                ( bought.balanceCents, bought.autoclicker )
                    |> Expect.equal ( 1500, ClickerBought )
        , test "the autoclicker cannot be bought twice" <|
            \_ ->
                apply [ AutoclickerPurchased, AutoclickerPurchased ] level3Start
                    |> .balanceCents
                    |> Expect.equal 1500
        , test "the autoclicker is refused when it would wipe the balance" <|
            \_ ->
                apply [ AutoclickerPurchased ] { level3Start | balanceCents = 1000 }
                    |> .autoclicker
                    |> Expect.equal ClickerNotBought
        , test "holding and releasing the flip button tracks the hold state" <|
            \_ ->
                ( apply [ FlipHoldStarted ] level3Start |> .flipHold
                , apply [ FlipHoldStarted, FlipHoldEnded ] level3Start |> .flipHold
                )
                    |> Expect.equal ( FlipHeld, FlipReleased )
        , test "an autoclicker tick validates stakes exactly like a manual flip" <|
            \_ ->
                apply [ AutoclickerTicked ] level3Start
                    |> latestLogText
                    |> Expect.equal "Place a bet on at least one coin first."
        , test "considering a purchase charges nothing" <|
            \_ ->
                let
                    considering =
                        apply [ PurchaseConsidered GlassesItem clickAtOrigin ] level3Start
                in
                ( considering.balanceCents, considering.glasses, considering.pendingPurchase )
                    |> Expect.equal ( 2500, GlassesNotBought, Considering GlassesItem clickAtOrigin )
        , test "confirming the considered purchase buys it and closes the dialog" <|
            \_ ->
                let
                    bought =
                        apply [ PurchaseConsidered GlassesItem clickAtOrigin, PurchaseConfirmed ] level3Start
                in
                ( bought.balanceCents, bought.glasses, bought.pendingPurchase )
                    |> Expect.equal ( 500, GlassesBought, NoPendingPurchase )
        , test "cancelling the considered purchase keeps the money" <|
            \_ ->
                let
                    cancelled =
                        apply [ PurchaseConsidered TrackerItem clickAtOrigin, PurchaseCancelled ] level3Start
                in
                ( cancelled.balanceCents, cancelled.tracker, cancelled.pendingPurchase )
                    |> Expect.equal ( 2500, TrackerNotBought, NoPendingPurchase )
        , test "uncle's advice goes through the dialog, which stays open" <|
            \_ ->
                let
                    advised =
                        apply [ PurchaseConsidered UncleAdviceItem clickAtOrigin, PurchaseConfirmed ] level3Start
                in
                ( advised.balanceCents, advised.uncleAdviceCount, advised.pendingPurchase )
                    |> Expect.equal ( 2000, 1, Considering UncleAdviceItem clickAtOrigin )
        , test "a swallowed click inside the dialog keeps it open" <|
            \_ ->
                apply [ PurchaseConsidered FlipHelperItem clickAtOrigin, DialogClicked ] level3Start
                    |> .pendingPurchase
                    |> Expect.equal (Considering FlipHelperItem clickAtOrigin)
        , test "cancelling uncle's advice charges nothing" <|
            \_ ->
                apply [ PurchaseConsidered UncleAdviceItem clickAtOrigin, PurchaseCancelled ] level3Start
                    |> .uncleAdviceCount
                    |> Expect.equal 0
        , test "confirming a helper hire goes through the compounding price" <|
            \_ ->
                apply [ PurchaseConsidered FlipHelperItem clickAtOrigin, PurchaseConfirmed ] level3Start
                    |> .nextHelperPriceCents
                    |> Expect.equal 110
        , test "mashing confirm hires a fleet at compounding prices" <|
            \_ ->
                let
                    fleet =
                        apply
                            [ PurchaseConsidered FlipHelperItem clickAtOrigin
                            , PurchaseConfirmed
                            , PurchaseConfirmed
                            , PurchaseConfirmed
                            ]
                            level3Start
                in
                ( fleet.flipHelperCount, fleet.balanceCents, fleet.pendingPurchase )
                    |> Expect.equal ( 3, 2500 - 100 - 110 - 121, Considering FlipHelperItem clickAtOrigin )
        , test "hiring a flip helper costs the base price and raises the next one by 10%" <|
            \_ ->
                let
                    hired =
                        apply [ FlipHelperHired ] level3Start
                in
                ( hired.balanceCents, hired.flipHelperCount, hired.nextHelperPriceCents )
                    |> Expect.equal ( 2400, 1, 110 )
        , test "helper prices compound per hire" <|
            \_ ->
                let
                    hired =
                        apply [ FlipHelperHired, FlipHelperHired, FlipHelperHired ] level3Start
                in
                ( hired.balanceCents, hired.flipHelperCount, hired.nextHelperPriceCents )
                    |> Expect.equal ( 2500 - 100 - 110 - 121, 3, 133 )
        , test "a helper hire is refused when it would wipe the balance" <|
            \_ ->
                apply [ FlipHelperHired ] { level3Start | balanceCents = 100 }
                    |> .flipHelperCount
                    |> Expect.equal 0
        , test "a helper tick validates stakes exactly like a manual flip" <|
            \_ ->
                apply [ FlipHelpersTicked ] level3Start
                    |> latestLogText
                    |> Expect.equal "Place a bet on at least one coin first."
        , test "helper ticks count up, keying the bird's backflip" <|
            \_ ->
                apply [ FlipHelpersTicked, FlipHelpersTicked, FlipHelpersTicked ] level3Start
                    |> .helperFlipCount
                    |> Expect.equal 3
        , test "buying the golden glasses costs $20.00" <|
            \_ ->
                let
                    bought =
                        apply [ GlassesPurchased ] level3Start
                in
                ( bought.balanceCents, bought.glasses )
                    |> Expect.equal ( 500, GlassesBought )
        , test "the glasses cannot be bought twice" <|
            \_ ->
                apply [ GlassesPurchased, GlassesPurchased ] level3Start
                    |> .balanceCents
                    |> Expect.equal 500
        , test "the glasses are refused when they would wipe the balance" <|
            \_ ->
                apply [ GlassesPurchased ] { level3Start | balanceCents = 2000 }
                    |> .glasses
                    |> Expect.equal GlassesNotBought
        ]


gatingSuite : Test
gatingSuite =
    describe "MultiCoinGame view gating"
        [ test "a bust points at the explanation below the game" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | phase = WentBust }
                    |> Query.fromHtml
                    |> Query.has [ class "explanation-hint" ]
        , test "running out of time points at the explanation too" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | phase = RanOutOfTime }
                    |> Query.fromHtml
                    |> Query.has [ class "explanation-hint" ]
        , test "a win shows no explanation hint" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "explanation-hint" ]
        , test "the tally only renders once the tracker is bought" <|
            \_ ->
                view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "tally" ]
        , test "the bought tracker renders the tally" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | tracker = TrackerBought }
                    |> Query.fromHtml
                    |> Query.has [ class "tally" ]
        , test "the shop starts collapsed" <|
            \_ ->
                view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "shop-item" ]
        , test "toggling the shop opens the section headings and their items" <|
            \_ ->
                apply [ ShopToggled ] level3Start
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ class "shop-group-heading" ]
                        , Query.has [ class "shop-item" ]
                        ]
        , test "toggling twice collapses the shop again" <|
            \_ ->
                apply [ ShopToggled, ShopToggled ] level3Start
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.hasNot [ class "shop-item" ]
        , test "the helper count only renders once helpers are hired" <|
            \_ ->
                ( view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "helpers" ]
                , apply [ FlipHelperHired ] level3Start
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.has [ class "helpers" ]
                )
                    |> (\( noHelpers, oneHelper ) -> Expect.all [ \_ -> noHelpers, \_ -> oneHelper ] ())
        , test "hired helpers render a pause button" <|
            \_ ->
                apply [ FlipHelperHired ] level3Start
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.has [ class "helper-pause" ]
        , test "a paused helper fleet ignores its ticks" <|
            \_ ->
                apply [ FlipHelperHired, HelperPauseToggled, FlipHelpersTicked ] level3Start
                    |> .helperFlipCount
                    |> Expect.equal 0
        , test "resuming the fleet makes ticks flip again" <|
            \_ ->
                apply
                    [ FlipHelperHired
                    , HelperPauseToggled
                    , HelperPauseToggled
                    , FlipHelpersTicked
                    ]
                    level3Start
                    |> .helperFlipCount
                    |> Expect.equal 1
        , test "the payout line only renders once the glasses are bought" <|
            \_ ->
                view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "glasses" ]
        , test "the bought glasses render the payout line" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | glasses = GlassesBought }
                    |> Query.fromHtml
                    |> Query.has [ class "glasses" ]
        , test "bought uncle advice draws a verdict on a bust" <|
            \_ ->
                view CoinFlipLevel3.levelConfig
                    { level3Start | phase = WentBust, uncleAdviceCount = 1 }
                    |> Query.fromHtml
                    |> Query.has [ class "uncle-verdict" ]
        , test "busting on uncle's advice draws the sad callout" <|
            \_ ->
                apply [ UncleAdviceRequested ] { level3Start | balanceCents = 500 }
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.has [ class "uncle-bust" ]
        , test "busting on a flip draws no uncle-bust callout" <|
            \_ ->
                apply [ landedRound [ 100, 0, 0 ] [ 100, 100, 100 ] ]
                    { level3Start | balanceCents = 100 }
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.hasNot [ class "uncle-bust" ]
        , test "the ended game keeps every coin's odds and payout hidden" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "bias-reveal" ]
        ]


shuffleSuite : Test
shuffleSuite =
    describe "MultiCoinGame profile shuffling"
        [ test "the shuffled draw replaces the active coins" <|
            \_ ->
                let
                    reassigned =
                        [ { coinName = "Swan", odds = IndependentPercent 60, payoutPercent = 50 }
                        , { coinName = "Magpie", odds = IndependentPercent 5, payoutPercent = 3000 }
                        , { coinName = "Sparrow", odds = IndependentPercent 45, payoutPercent = 100 }
                        ]
                in
                apply [ ProfilesShuffled reassigned ] level3Start
                    |> .coins
                    |> Expect.equal reassigned
        , test "settling uses the shuffled profiles, not the written ones" <|
            \_ ->
                -- After the reassignment above, Magpie carries the 30x
                -- swan profile: a winning roll of 5 on Magpie pays 30x.
                apply
                    [ ProfilesShuffled
                        [ { coinName = "Swan", odds = IndependentPercent 60, payoutPercent = 50 }
                        , { coinName = "Magpie", odds = IndependentPercent 5, payoutPercent = 3000 }
                        , { coinName = "Sparrow", odds = IndependentPercent 45, payoutPercent = 100 }
                        ]
                    , landedRound [ 0, 100, 0 ] [ 50, 5, 50 ]
                    ]
                    level3Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 3000)
        , test "the glasses report the shuffled deal, not the written one" <|
            \_ ->
                apply
                    [ ProfilesShuffled
                        [ { coinName = "Swan", odds = IndependentPercent 60, payoutPercent = 50 }
                        , { coinName = "Magpie", odds = IndependentPercent 5, payoutPercent = 3000 }
                        , { coinName = "Sparrow", odds = IndependentPercent 45, payoutPercent = 100 }
                        ]
                    ]
                    { level3Start | glasses = GlassesBought }
                    |> view CoinFlipLevel3.levelConfig
                    |> Query.fromHtml
                    |> Query.find [ class "glasses" ]
                    |> Query.has [ text "Magpie 30\u{00D7}" ]
        , test "shuffling keeps the names in place and deals every profile once" <|
            \_ ->
                let
                    shuffled =
                        Tuple.first
                            (Random.step
                                (shuffledCoinsGenerator CoinFlipLevel3.levelConfig.coins)
                                (Random.initialSeed 42)
                            )
                in
                ( List.map .coinName shuffled, sortedProfiles shuffled )
                    |> Expect.equal
                        ( [ "Swan", "Magpie", "Sparrow" ]
                        , sortedProfiles CoinFlipLevel3.levelConfig.coins
                        )
        ]


{-| The (odds, payout) profiles of a coin list, order-insensitive, for
comparing a shuffle against the original deal.
-}
sortedProfiles : List MultiCoinGame.CoinConfig -> List { odds : CoinOdds, payoutPercent : Int }
sortedProfiles coins =
    List.sortBy .payoutPercent
        (List.map (\coin -> { odds = coin.odds, payoutPercent = coin.payoutPercent }) coins)


{-| Level 4 (birds of a feather): weather-driven anti-correlated coins,
a flip budget instead of a clock, and the Dutch-book portfolio.
Sun percent is 60, so weather rolls 1-60 are sunny and 61-100 rainy.
-}
apply4 : List Msg -> Model -> Model
apply4 msgs model =
    List.foldl
        (\msg current -> Tuple.first (update CoinFlipLevel4.levelConfig msg current))
        model
        msgs


level4Start : Model
level4Start =
    initialModel CoinFlipLevel4.levelConfig


sunnyRound : List Int -> Msg
sunnyRound stakes =
    CoinsLanded { stakes = stakes, weatherRoll = 1, rolls = [ 100, 100, 100 ] }


rainyRound : List Int -> Msg
rainyRound stakes =
    CoinsLanded { stakes = stakes, weatherRoll = 100, rolls = [ 100, 100, 100 ] }


correlationSuite : Test
correlationSuite =
    describe "MultiCoinGame weather correlation (level 4)"
        [ test "on a sunny round the sunbird wins and the rainbird loses" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 100, 0 ] ] level4Start
                    |> .tallies
                    |> Expect.equal
                        [ { headsCount = 1, flipCount = 1 }
                        , { headsCount = 0, flipCount = 1 }
                        , { headsCount = 0, flipCount = 0 }
                        ]
        , test "on a rainy round the rainbird wins and the sunbird loses" <|
            \_ ->
                apply4 [ rainyRound [ 100, 100, 0 ] ] level4Start
                    |> .tallies
                    |> Expect.equal
                        [ { headsCount = 0, flipCount = 1 }
                        , { headsCount = 1, flipCount = 1 }
                        , { headsCount = 0, flipCount = 0 }
                        ]
        , test "the Dutch-book split profits on a sunny round" <|
            \_ ->
                -- $6.10 on Sunbird (pays 0.8x) and $3.90 on Rainbird:
                -- sunny nets 610*0.8 - 390 = +98 cents.
                apply4 [ sunnyRound [ 610, 390, 0 ] ] level4Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 488 - 390)
        , test "the Dutch-book split profits on a rainy round too" <|
            \_ ->
                -- rainy nets 390*1.8 - 610 = +92 cents.
                apply4 [ rainyRound [ 610, 390, 0 ] ] level4Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 702 - 610)
        , test "the cuckoo rolls independently of the weather" <|
            \_ ->
                -- rainy round, but the cuckoo's own roll of 1 (<= 2) wins:
                -- 40x on a $0.10 stake pays $4.00.
                apply4 [ CoinsLanded { stakes = [ 0, 0, 10 ], weatherRoll = 100, rolls = [ 100, 100, 1 ] } ]
                    level4Start
                    |> .balanceCents
                    |> Expect.equal (2500 + 400)
        , test "the game ends after the flip budget is spent" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ] ]
                    { level4Start | roundCount = 199 }
                    |> .phase
                    |> Expect.equal RanOutOfTime
        , test "winning on the final flip beats running out of flips" <|
            \_ ->
                apply4 [ rainyRound [ 0, 55000, 0 ] ]
                    { level4Start | roundCount = 199, balanceCents = 55000 }
                    |> .phase
                    |> Expect.equal WonGame
        , test "flips before the budget runs out keep the game going" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ] ]
                    { level4Start | roundCount = 198 }
                    |> .phase
                    |> Expect.equal Playing
        ]


{-| The correlation upgrades: therefore-clauses in the log, the
percentage allocator, and The Elegant Universe's pair tallies.
-}
correlationUpgradeSuite : Test
correlationUpgradeSuite =
    describe "MultiCoinGame correlation upgrades (level 4)"
        [ test "weather coin log lines stay plain, revealing nothing" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ] ] level4Start
                    |> latestLogText
                    |> Expect.equal
                        "1: Starling landed heads! You win $1.80, of which $1.00 is your stake."
        , test "a losing weather coin's line is equally silent about its partner" <|
            \_ ->
                apply4 [ rainyRound [ 100, 0, 0 ] ] level4Start
                    |> latestLogText
                    |> Expect.equal "1: Starling landed tails. You lost your $1.00 stake."
        , test "buying the allocator costs $10 and switches to percent mode" <|
            \_ ->
                let
                    bought =
                        apply4 [ AllocatorPurchased ] level4Start
                in
                ( bought.balanceCents, bought.allocationMode )
                    |> Expect.equal ( 1500, PercentAllocation )
        , test "percent stakes are taken from the live balance" <|
            \_ ->
                -- After the $10 allocator: balance $15.00, staking 10%
                -- of it on the sunny coin is $1.50, winning 0.8x pays
                -- $1.20: balance 1500 + 120.
                apply4
                    [ AllocatorPurchased
                    , StakeInputChanged 0 "10"
                    , FlipPressed
                    ]
                    level4Start
                    |> (\beforeLanding ->
                            apply4 [ sunnyRound [ 150, 0, 0 ] ] beforeLanding
                       )
                    |> .balanceCents
                    |> Expect.equal (1500 + 120)
        , test "percent totals above 100 are refused like overdrawn dollars" <|
            \_ ->
                apply4
                    [ AllocatorPurchased
                    , StakeInputChanged 0 "60"
                    , StakeInputChanged 1 "50"
                    , FlipPressed
                    ]
                    level4Start
                    |> latestLogText
                    |> Expect.equal "You cannot bet more than your current balance!"
        , test "buying the book costs $20 and starts tracking pairs" <|
            \_ ->
                let
                    read =
                        apply4
                            [ BookPurchased
                            , sunnyRound [ 100, 100, 10 ]
                            , rainyRound [ 100, 100, 0 ]
                            ]
                            level4Start
                in
                ( read.balanceCents |> (\_ -> read.book)
                , read.pairTallies
                )
                    |> Expect.equal
                        ( BookBought
                        , [ { firstIndex = 0, secondIndex = 1, sameCount = 0, bothCount = 2 }
                          , { firstIndex = 0, secondIndex = 2, sameCount = 0, bothCount = 1 }
                          , { firstIndex = 1, secondIndex = 2, sameCount = 1, bothCount = 1 }
                          ]
                        )
        , test "pairs only count rounds where both coins were staked" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ] ] level4Start
                    |> .pairTallies
                    |> Expect.equal
                        [ { firstIndex = 0, secondIndex = 1, sameCount = 0, bothCount = 0 }
                        , { firstIndex = 0, secondIndex = 2, sameCount = 0, bothCount = 0 }
                        , { firstIndex = 1, secondIndex = 2, sameCount = 0, bothCount = 0 }
                        ]
        ]


{-| Uncle's complete works, read straight from the level-4 config so
this list can never drift from it. Hearing all of them unlocks asking
for the money back.
-}
allUnclePhrases : List String
allUnclePhrases =
    case CoinFlipLevel4.levelConfig.uncleOffer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            uncle.firstPhrase :: uncle.morePhrases


heardEverything : Model
heardEverything =
    apply4 (List.map UncleAdviceGiven allUnclePhrases)
        { level4Start | balanceCents = 10000 }


refundSuite : Test
refundSuite =
    describe "MultiCoinGame refund request (level 4)"
        [ test "hearing a phrase twice counts once" <|
            \_ ->
                apply4
                    (List.map UncleAdviceGiven
                        [ "A real gambler doesn't hedge.", "A real gambler doesn't hedge." ]
                    )
                    level4Start
                    |> .adviceHeard
                    |> List.length
                    |> Expect.equal 1
        , test "the refund is not for sale before the complete works are heard" <|
            \_ ->
                view CoinFlipLevel4.levelConfig level4Start
                    |> Query.fromHtml
                    |> Query.hasNot [ text "Ask your money back" ]
        , test "hearing every phrase puts the refund up for sale" <|
            \_ ->
                apply4 [ ShopToggled ] heardEverything
                    |> view CoinFlipLevel4.levelConfig
                    |> Query.fromHtml
                    |> Query.has [ text "Ask your money back" ]
        , test "asking costs $10 and uncle replies with his farewell" <|
            \_ ->
                let
                    asked =
                        apply4 [ RefundRequested ] heardEverything
                in
                ( asked.balanceCents, asked.refund, String.contains "that ship has sailed" (latestLogText asked) )
                    |> Expect.equal ( 9000, RefundAsked, True )
        , test "uncle does not do repeat refund conversations" <|
            \_ ->
                apply4 [ RefundRequested, RefundRequested ] heardEverything
                    |> .balanceCents
                    |> Expect.equal 9000
        , test "asking with your last ten dollars goes bust, gloat and all" <|
            \_ ->
                let
                    busted =
                        apply4 [ RefundRequested ] { heardEverything | balanceCents = 1000 }
                in
                ( busted.balanceCents, busted.phase, latestLogText busted )
                    |> Expect.equal
                        ( 0
                        , WentBust
                        , "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
                        )
        ]


{-| The three extra-flip packages sold in level 4's shop.
-}
oneFlipPack : MultiCoinGame.ExtraTurnsPackage
oneFlipPack =
    { priceCents = 600, extraFlips = 1 }


tenFlipPack : MultiCoinGame.ExtraTurnsPackage
tenFlipPack =
    { priceCents = 5500, extraFlips = 10 }


fiftyFlipPack : MultiCoinGame.ExtraTurnsPackage
fiftyFlipPack =
    { priceCents = 50000, extraFlips = 50 }


{-| Buying more turns: $6 for 1 flip, $55 for 10, $500 for 50,
repeatable, satire; plus the same single flip at $8 on the
out-of-flips screen, the desperation premium.
-}
extraTurnsSuite : Test
extraTurnsSuite =
    describe "MultiCoinGame extra turns (level 4)"
        [ test "buying 50 more flips costs $500 and extends the budget" <|
            \_ ->
                let
                    extended =
                        apply4 [ ExtraTurnsPurchased fiftyFlipPack, sunnyRound [ 100, 0, 0 ] ]
                            { level4Start | balanceCents = 60000, roundCount = 199 }
                in
                ( extended.balanceCents, extended.phase )
                    |> Expect.equal ( 60000 - 50000 + 80, Playing )
        , test "buying 1 more flip from the shop costs $6 and lasts one round" <|
            \_ ->
                let
                    extended =
                        apply4
                            [ ExtraTurnsPurchased oneFlipPack
                            , sunnyRound [ 100, 0, 0 ]
                            , sunnyRound [ 100, 0, 0 ]
                            ]
                            { level4Start | balanceCents = 60000, roundCount = 199 }
                in
                ( extended.balanceCents, extended.phase )
                    |> Expect.equal ( 60000 - 600 + 80 + 80, RanOutOfTime )
        , test "buying 10 more flips costs $55 and extends the budget" <|
            \_ ->
                let
                    extended =
                        apply4 [ ExtraTurnsPurchased tenFlipPack, sunnyRound [ 100, 0, 0 ] ]
                            { level4Start | balanceCents = 60000, roundCount = 199 }
                in
                ( extended.balanceCents, extended.phase )
                    |> Expect.equal ( 60000 - 5500 + 80, Playing )
        , test "without the purchase the same flip ends the game" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ] ]
                    { level4Start | balanceCents = 60000, roundCount = 199 }
                    |> .phase
                    |> Expect.equal RanOutOfTime
        , test "buying flips is repeatable and packages mix" <|
            \_ ->
                apply4 [ ExtraTurnsPurchased fiftyFlipPack, ExtraTurnsPurchased tenFlipPack ]
                    { level4Start | balanceCents = 110000 }
                    |> .extraFlipsBought
                    |> Expect.equal 60
        , test "more flips are refused below the price" <|
            \_ ->
                apply4 [ ExtraTurnsPurchased fiftyFlipPack ] { level4Start | balanceCents = 50000 }
                    |> .extraFlipsBought
                    |> Expect.equal 0
        , test "a package the shop never offered is ignored" <|
            \_ ->
                apply4 [ ExtraTurnsPurchased { priceCents = 1, extraFlips = 1000 } ]
                    { level4Start | balanceCents = 60000 }
                    |> .extraFlipsBought
                    |> Expect.equal 0
        , test "the $8 rescue revives an out-of-flips game for one flip" <|
            \_ ->
                let
                    revived =
                        apply4
                            [ sunnyRound [ 100, 0, 0 ]
                            , LastChanceTurnPurchased
                            , sunnyRound [ 100, 0, 0 ]
                            ]
                            { level4Start | balanceCents = 60000, roundCount = 199 }
                in
                ( revived.balanceCents, revived.extraFlipsBought, revived.phase )
                    |> Expect.equal ( 60000 + 80 - 800 + 80, 1, RanOutOfTime )
        , test "the rescue does nothing while still playing" <|
            \_ ->
                apply4 [ LastChanceTurnPurchased ] { level4Start | balanceCents = 60000 }
                    |> .extraFlipsBought
                    |> Expect.equal 0
        , test "the rescue is refused when the balance cannot pay it" <|
            \_ ->
                apply4 [ sunnyRound [ 100, 0, 0 ], LastChanceTurnPurchased ]
                    { level4Start | balanceCents = 500, roundCount = 199 }
                    |> .phase
                    |> Expect.equal RanOutOfTime
        ]


oneMinutePack : CoinFlipGame.ExtraTimePackage
oneMinutePack =
    { priceCents = 2000, extraSeconds = 60 }


thirtyMinutePack : CoinFlipGame.ExtraTimePackage
thirtyMinutePack =
    { priceCents = 75000, extraSeconds = 30 * 60 }


extraTimeSuite : Test
extraTimeSuite =
    describe "MultiCoinGame extra time and level-3 upgrades"
        [ test "buying 1 more minute costs $20 and extends the clock" <|
            \_ ->
                let
                    extended =
                        apply [ ExtraTimePurchased oneMinutePack ] level3Start
                in
                ( extended.balanceCents, extended.secondsLeft )
                    |> Expect.equal ( 500, 30 * 60 + 60 )
        , test "buying 30 more minutes costs $750 and extends the clock" <|
            \_ ->
                let
                    extended =
                        apply [ ExtraTimePurchased thirtyMinutePack ]
                            { level3Start | balanceCents = 100000 }
                in
                ( extended.balanceCents, extended.secondsLeft )
                    |> Expect.equal ( 25000, 2 * 30 * 60 )
        , test "more time is refused when it would wipe the balance" <|
            \_ ->
                let
                    refused =
                        apply [ ExtraTimePurchased oneMinutePack ]
                            { level3Start | balanceCents = 2000 }
                in
                ( refused.balanceCents, refused.secondsLeft )
                    |> Expect.equal ( 2000, 30 * 60 )
        , test "a package the shop never offered is ignored" <|
            \_ ->
                apply [ ExtraTimePurchased { priceCents = 1, extraSeconds = 9999 } ] level3Start
                    |> .secondsLeft
                    |> Expect.equal (30 * 60)
        , test "the flip-limited level 4 sells no time" <|
            \_ ->
                apply4 [ ExtraTimePurchased oneMinutePack ]
                    { level4Start | balanceCents = 60000 }
                    |> .balanceCents
                    |> Expect.equal 60000
        , test "level 3 sells the percentage allocator for $10" <|
            \_ ->
                let
                    bought =
                        apply [ AllocatorPurchased ] level3Start
                in
                ( bought.balanceCents, bought.allocationMode )
                    |> Expect.equal ( 1500, PercentAllocation )
        ]


winCapSuite : Test
winCapSuite =
    describe "MultiCoinGame win cap upsell"
        [ test "raising the cap costs $500 and resumes play" <|
            \_ ->
                let
                    raised =
                        apply [ WinCapRaised ]
                            { level3Start | phase = WonGame, balanceCents = 99900 }
                in
                ( raised.phase, raised.balanceCents, raised.winCapTier )
                    |> Expect.equal ( Playing, 49900, SecondCap )
        , test "the old target no longer wins after the raise" <|
            \_ ->
                apply
                    [ WinCapRaised, landedRound [ 1700, 0, 0 ] [ 5, 100, 100 ] ]
                    { level3Start | phase = WonGame, balanceCents = 99900 }
                    |> .phase
                    |> Expect.equal Playing
        , test "reaching $9,999 wins the raised game" <|
            \_ ->
                apply
                    [ WinCapRaised, landedRound [ 31667, 0, 0 ] [ 5, 100, 100 ] ]
                    { level3Start | phase = WonGame, balanceCents = 99900 }
                    |> .phase
                    |> Expect.equal WonGame
        , test "the second raise costs $8,500 and targets $99,999" <|
            \_ ->
                let
                    raised =
                        apply [ WinCapRaised ]
                            { level3Start
                                | phase = WonGame
                                , balanceCents = 999900
                                , winCapTier = SecondCap
                            }
                in
                ( raised.phase, raised.balanceCents, raised.winCapTier )
                    |> Expect.equal ( Playing, 149900, DegenerateCap )
        , test "raising the cap releases a flip button still held from before the win" <|
            \_ ->
                apply [ WinCapRaised ]
                    { level3Start
                        | phase = WonGame
                        , balanceCents = 99900
                        , autoclicker = ClickerBought
                        , flipHold = FlipHeld
                    }
                    |> .flipHold
                    |> Expect.equal FlipReleased
        , test "the last-chance rescue releases a stale flip hold too" <|
            \_ ->
                let
                    rescued =
                        apply4 [ LastChanceTurnPurchased ]
                            { level4Start
                                | phase = RanOutOfTime
                                , balanceCents = 60000
                                , autoclicker = ClickerBought
                                , flipHold = FlipHeld
                            }
                in
                ( rescued.phase, rescued.flipHold )
                    |> Expect.equal ( Playing, FlipReleased )
        , test "a raise already past the new target wins again on the spot" <|
            \_ ->
                let
                    raised =
                        apply [ WinCapRaised ]
                            { level3Start | phase = WonGame, balanceCents = 3000000 }
                in
                ( raised.phase, raised.winCapTier, raised.balanceCents )
                    |> Expect.equal ( WonGame, SecondCap, 2950000 )
        , test "there is no raise beyond the degenerate cap" <|
            \_ ->
                apply [ WinCapRaised ]
                    { level3Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> .balanceCents
                    |> Expect.equal 9999900
        , test "raising the cap does nothing while still playing" <|
            \_ ->
                apply [ WinCapRaised ] level3Start
                    |> .balanceCents
                    |> Expect.equal 2500
        , test "the win screen renders the upsell button" <|
            \_ ->
                view CoinFlipLevel3.levelConfig
                    { level3Start | phase = WonGame, balanceCents = 99900 }
                    |> Query.fromHtml
                    |> Query.has [ class "win-cap-upsell" ]
        , test "the degenerate win renders no upsell button" <|
            \_ ->
                view CoinFlipLevel3.levelConfig
                    { level3Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> Query.fromHtml
                    |> Query.hasNot [ class "win-cap-upsell" ]
        , test "the degenerate win draws the true ending" <|
            \_ ->
                view CoinFlipLevel3.levelConfig
                    { level3Start
                        | phase = WonGame
                        , balanceCents = 9999900
                        , winCapTier = DegenerateCap
                    }
                    |> Query.fromHtml
                    |> Query.has [ text trueEndingMessage ]
        , test "lower-cap wins do not draw the true ending" <|
            \_ ->
                view CoinFlipLevel3.levelConfig
                    { level3Start | phase = WonGame, balanceCents = 99900 }
                    |> Query.fromHtml
                    |> Query.hasNot [ text trueEndingMessage ]
        ]
