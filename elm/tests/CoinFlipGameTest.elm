module CoinFlipGameTest exposing (clockSuite, formattingSuite, gambleSuite, hiddenBiasSuite, shopSuite)

{-| Tests for the rigged-coin game engine shared by level 1
(even-with-an-edge-you-lose.html) and level 2 (hidden-rewards.html).
They drive the real `update` with the real level configs; the coin landing
is a plain message (`CoinLanded`), so outcomes are testable without
running the random command.
-}

import CoinFlipGame
    exposing
        ( BiasState(..)
        , CoinSide(..)
        , GamePhase(..)
        , LevelConfig
        , Model
        , Msg(..)
        , TrackerState(..)
        , formatCents
        , formatClock
        , initialModel
        , landedPercent
        , parseBetCents
        , quickBetCents
        , update
        )
import CoinFlipLevel1
import CoinFlipLevel2
import Expect
import Test exposing (Test, describe, test)


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


shopSuite : Test
shopSuite =
    describe "CoinFlipGame shop (level 2)"
        [ test "buying the ratio tracker costs $15.00" <|
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
        , test "uncle is refused when it would wipe the balance" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ UncleAdviceRequested ]
                    { level2Start | balanceCents = 500 }
                    |> .uncleAdviceCount
                    |> Expect.equal 0
        , test "uncle's advice ends up in the log" <|
            \_ ->
                apply CoinFlipLevel2.levelConfig
                    [ UncleAdviceGiven "Winners don't do math, son." ]
                    level2Start
                    |> latestLogText
                    |> Expect.equal "\u{1F9D3} Uncle: \u{201C}Winners don't do math, son.\u{201D}"
        , test "level 1 has no shop, so a stray purchase changes nothing" <|
            \_ ->
                apply CoinFlipLevel1.levelConfig
                    [ TrackerPurchased, UncleAdviceRequested ]
                    level1Start
                    |> .balanceCents
                    |> Expect.equal 2500
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
