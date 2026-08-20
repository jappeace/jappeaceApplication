module MultiCoinGameTest exposing (flipSuite, gatingSuite, payoutSuite, shopSuite, stakeSuite)

{-| Tests for the multi-coin engine behind level 3 (black-swan.html).
They drive the real `update` with the real level config; the coins
landing is a plain message (`CoinsLanded`), so outcomes are testable
without running the random command. Level 3's coins: Swan wins on rolls
1-5 paying 30x, Magpie on 1-45 paying 1x, Sparrow on 1-60 paying 0.5x.
-}

import CoinFlipGame
    exposing
        ( GamePhase(..)
        , TrackerState(..)
        )
import CoinFlipLevel3
import Expect
import MultiCoinGame
    exposing
        ( BustCause(..)
        , GoldenGlasses(..)
        , Model
        , Msg(..)
        , StakeInput(..)
        , formatPayout
        , initialModel
        , parseStake
        , payoutCents
        , update
        , view
        )
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class)


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
    CoinsLanded { stakes = stakes, rolls = rolls }


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
        [ test "the tally only renders once the tracker is bought" <|
            \_ ->
                view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "tally" ]
        , test "the bought tracker renders the tally" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | tracker = TrackerBought }
                    |> Query.fromHtml
                    |> Query.has [ class "tally" ]
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
        , test "the coin reveal only renders after the game ends" <|
            \_ ->
                view CoinFlipLevel3.levelConfig level3Start
                    |> Query.fromHtml
                    |> Query.hasNot [ class "bias-reveal" ]
        , test "the ended game reveals every coin" <|
            \_ ->
                view CoinFlipLevel3.levelConfig { level3Start | phase = WonGame }
                    |> Query.fromHtml
                    |> Query.findAll [ class "bias-reveal" ]
                    |> Query.count (Expect.equal 3)
        ]
