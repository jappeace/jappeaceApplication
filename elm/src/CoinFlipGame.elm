module CoinFlipGame exposing
    ( BiasState(..)
    , CoinBias(..)
    , CoinSide(..)
    , GamePhase(..)
    , LevelConfig
    , LogLine
    , LogTone(..)
    , Model
    , Msg(..)
    , BustEnding(..)
    , NextLevelLink(..)
    , TrackerOffer(..)
    , TrackerState(..)
    , UncleOffer(..)
    , formatCents
    , formatClock
    , gameProgram
    , initialModel
    , landedPercent
    , oppositeSide
    , parseBetCents
    , quickBetCents
    , startingBalanceCents
    , targetBalanceCents
    , timeLimitSeconds
    , update
    , view
    )

{-| Shared engine for the rigged-coin betting games on the blog.

Level 1 (`CoinFlipLevel1`, even-with-an-edge-you-lose.html) shows the bias
up front; level 2 (`CoinFlipLevel2`) hides it and sells a ratio tracker in
a little shop instead. Start at $25, reach $999 within 30 minutes.

All money is in cents (Int) so balances stay exact; the old JS version
rounded floats after every flip to paper over drift.

-}
-- Decision: both levels share this one engine, parameterized by
-- LevelConfig, and each level is a thin main module compiled to its own
-- bundle (coin-flip-level1.js / coin-flip-level2.js). Alternatives were
-- copying the game per level (the logic would drift apart) or one bundle
-- exposing both mains (each post would then redownload on changes to the
-- other level). The coin landing is a plain CoinLanded message rather
-- than resolved inside the random command, so the settle logic is pure
-- and the test suite can drive it deterministically.

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Time


type CoinSide
    = Heads
    | Tails


{-| How the coin is rigged for a level.

  - `KnownHeadsPercent`: the bias is fixed and printed on the bet buttons.
  - `HiddenFavoredPercentRange`: the favored side's win percent is drawn
    uniformly from the inclusive range at game start, and never shown.

-}
type CoinBias
    = KnownHeadsPercent Int
    | HiddenFavoredPercentRange { favored : CoinSide, minPercent : Int, maxPercent : Int }


type TrackerOffer
    = NoTrackerForSale
    | TrackerForSale Int


{-| Uncle sells terrible gambling advice, one random phrase per purchase.
The phrase list is split head/tail so an empty list cannot be configured.
-}
type UncleOffer
    = NoUncleAdvice
    | UncleAdviceForSale { priceCents : Int, firstPhrase : String, morePhrases : List String }


{-| Shown in the win text when this level has a sequel to link to.
-}
type NextLevelLink
    = NoNextLevelLink
    | NextLevelLinkTo { url : String, label : String }


{-| An extra parting line under the bust message, for levels that want to
rub it in.
-}
type BustEnding
    = PlainBustEnding
    | BustEndingQuote String


type alias LevelConfig =
    { title : String
    , bias : CoinBias
    , trackerOffer : TrackerOffer
    , uncleOffer : UncleOffer
    , nextLevelLink : NextLevelLink
    , bustEnding : BustEnding
    , introLogLine : String
    }


{-| The actual bias in play. Hidden biases start `BiasUndrawn` until the
random draw lands (one frame after init); bets are refused until then.
-}
type BiasState
    = BiasUndrawn
    | BiasReady { favored : CoinSide, favoredPercent : Int }


type GamePhase
    = Playing
    | WonGame
    | WentBust
    | RanOutOfTime


type ClockState
    = ClockIdle
    | ClockRunning


type TrackerState
    = TrackerNotBought
    | TrackerBought


type LogTone
    = WinTone
    | LoseTone
    | NeutralTone


type alias LogLine =
    { tone : LogTone, text : String }


type alias Model =
    { balanceCents : Int
    , betInput : String
    , biasState : BiasState
    , phase : GamePhase
    , clock : ClockState
    , secondsLeft : Int
    , flipCount : Int
    , headsBetCount : Int
    , tailsBetCount : Int
    , headsLandedCount : Int
    , tailsLandedCount : Int
    , tracker : TrackerState
    , uncleAdviceCount : Int
    , log : List LogLine
    }


type Msg
    = BiasDrawn { favored : CoinSide, favoredPercent : Int }
    | QuickBetPicked Float
    | BetInputChanged String
    | BetPlaced CoinSide
    | CoinLanded { playerChoice : CoinSide, betCents : Int, landed : CoinSide }
    | ClockTicked
    | TrackerPurchased
    | UncleAdviceRequested
    | UncleAdviceGiven String


startingBalanceCents : Int
startingBalanceCents = 2500


targetBalanceCents : Int
targetBalanceCents = 99900


timeLimitSeconds : Int
timeLimitSeconds = 30 * 60


gameProgram : LevelConfig -> Program () Model Msg
gameProgram config =
    Browser.element
        { init = init config
        , update = update config
        , subscriptions = subscriptions
        , view = view config
        }


initialModel : LevelConfig -> Model
initialModel config =
    { balanceCents = startingBalanceCents
    , betInput = "0.00"
    , biasState =
        case config.bias of
            KnownHeadsPercent percent ->
                BiasReady { favored = Heads, favoredPercent = percent }

            HiddenFavoredPercentRange _ ->
                BiasUndrawn
    , phase = Playing
    , clock = ClockIdle
    , secondsLeft = timeLimitSeconds
    , flipCount = 0
    , headsBetCount = 0
    , tailsBetCount = 0
    , headsLandedCount = 0
    , tailsLandedCount = 0
    , tracker = TrackerNotBought
    , uncleAdviceCount = 0
    , log = [ { tone = NeutralTone, text = config.introLogLine } ]
    }


init : LevelConfig -> () -> ( Model, Cmd Msg )
init config () =
    ( initialModel config
    , case config.bias of
        KnownHeadsPercent _ ->
            Cmd.none

        HiddenFavoredPercentRange range ->
            Random.generate
                (\percent -> BiasDrawn { favored = range.favored, favoredPercent = percent })
                (Random.int range.minPercent range.maxPercent)
    )


update : LevelConfig -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        BiasDrawn bias ->
            ( { model | biasState = BiasReady bias }, Cmd.none )

        QuickBetPicked fraction ->
            if model.phase == Playing then
                ( { model | betInput = formatCents (quickBetCents fraction model.balanceCents) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        BetInputChanged newInput ->
            ( { model | betInput = newInput }, Cmd.none )

        BetPlaced playerChoice ->
            placeBet playerChoice model

        CoinLanded flip ->
            ( settleFlip flip model, Cmd.none )

        ClockTicked ->
            ( tickClock model, Cmd.none )

        TrackerPurchased ->
            ( purchaseTracker config.trackerOffer model, Cmd.none )

        UncleAdviceRequested ->
            purchaseUncleAdvice config.uncleOffer model

        UncleAdviceGiven phrase ->
            ( logLine NeutralTone ("\u{1F9D3} Uncle: \u{201C}" ++ phrase ++ "\u{201D}") model
            , Cmd.none
            )


{-| Validate the bet input and, if it holds up, flip the coin. The clock
only starts on the first accepted bet, not on page load.
-}
placeBet : CoinSide -> Model -> ( Model, Cmd Msg )
placeBet playerChoice model =
    if model.phase /= Playing then
        ( model, Cmd.none )

    else
        case ( parseBetCents model.betInput, model.biasState ) of
            ( Nothing, _ ) ->
                ( logLine NeutralTone "Enter a bet amount above $0.00." model, Cmd.none )

            ( Just betCents, _ ) ->
                if betCents > model.balanceCents then
                    ( logLine NeutralTone "You cannot bet more than your current balance!" model, Cmd.none )

                else
                    case model.biasState of
                        BiasUndrawn ->
                            ( logLine NeutralTone "The coin is still being rigged, try again." model, Cmd.none )

                        BiasReady bias ->
                            ( { model | clock = ClockRunning }
                            , Random.generate
                                (\landed ->
                                    CoinLanded
                                        { playerChoice = playerChoice
                                        , betCents = betCents
                                        , landed = landed
                                        }
                                )
                                (coinFlipGenerator bias)
                            )


coinFlipGenerator : { favored : CoinSide, favoredPercent : Int } -> Random.Generator CoinSide
coinFlipGenerator bias =
    Random.map
        (\roll ->
            if roll <= bias.favoredPercent then
                bias.favored

            else
                oppositeSide bias.favored
        )
        (Random.int 1 100)


{-| Apply a landed coin flip: pay out or collect, count it, and check for
the win/bust end states.
-}
settleFlip : { playerChoice : CoinSide, betCents : Int, landed : CoinSide } -> Model -> Model
settleFlip flip model =
    if model.phase /= Playing then
        model

    else
        let
            won =
                flip.playerChoice == flip.landed

            newBalance =
                if won then
                    model.balanceCents + flip.betCents

                else
                    max 0 (model.balanceCents - flip.betCents)

            outcomeText =
                "Landed "
                    ++ sideName flip.landed
                    ++ "! You "
                    ++ (if won then
                            "won"

                        else
                            "lost"
                       )
                    ++ " $"
                    ++ formatCents flip.betCents

            counted =
                { model
                    | balanceCents = newBalance
                    , flipCount = model.flipCount + 1
                    , headsBetCount = countSide Heads flip.playerChoice model.headsBetCount
                    , tailsBetCount = countSide Tails flip.playerChoice model.tailsBetCount
                    , headsLandedCount = countSide Heads flip.landed model.headsLandedCount
                    , tailsLandedCount = countSide Tails flip.landed model.tailsLandedCount
                    , betInput = clampBetInput newBalance model.betInput
                }
        in
        checkEndState
            (logLine
                (if won then
                    WinTone

                 else
                    LoseTone
                )
                outcomeText
                counted
            )


countSide : CoinSide -> CoinSide -> Int -> Int
countSide side actual count =
    if side == actual then
        count + 1

    else
        count


{-| Keep the bet input within the new balance, like the JS version did
after every flip.
-}
clampBetInput : Int -> String -> String
clampBetInput balanceCents input =
    case parseBetCents input of
        Nothing ->
            input

        Just cents ->
            if cents > balanceCents then
                formatCents balanceCents

            else
                input


checkEndState : Model -> Model
checkEndState model =
    if model.balanceCents >= targetBalanceCents then
        { model | phase = WonGame }

    else if model.balanceCents <= 0 then
        { model | phase = WentBust }

    else
        model


tickClock : Model -> Model
tickClock model =
    if model.phase /= Playing then
        model

    else
        let
            newSecondsLeft =
                model.secondsLeft - 1
        in
        if newSecondsLeft <= 0 then
            { model | secondsLeft = 0, phase = RanOutOfTime }

        else
            { model | secondsLeft = newSecondsLeft }


{-| Buy the ratio tracker. Refused when it would wipe the balance to $0,
because paying yourself straight into bankruptcy is a bug, not a lesson.
-}
purchaseTracker : TrackerOffer -> Model -> Model
purchaseTracker offer model =
    case ( offer, model.tracker ) of
        ( NoTrackerForSale, TrackerNotBought ) ->
            model

        ( NoTrackerForSale, TrackerBought ) ->
            model

        ( TrackerForSale _, TrackerBought ) ->
            model

        ( TrackerForSale priceCents, TrackerNotBought ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= priceCents then
                logLine NeutralTone "You cannot afford the ratio tracker." model

            else
                logLine NeutralTone
                    ("Bought the ratio tracker for $" ++ formatCents priceCents ++ ".")
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , tracker = TrackerBought
                        , betInput = clampBetInput (model.balanceCents - priceCents) model.betInput
                    }


{-| Pay uncle and draw one of his pre-programmed pearls of wisdom. Like
the tracker, refused when it would wipe the balance to $0.
-}
purchaseUncleAdvice : UncleOffer -> Model -> ( Model, Cmd Msg )
purchaseUncleAdvice offer model =
    case offer of
        NoUncleAdvice ->
            ( model, Cmd.none )

        UncleAdviceForSale uncle ->
            if model.phase /= Playing then
                ( model, Cmd.none )

            else if model.balanceCents <= uncle.priceCents then
                ( logLine NeutralTone "You cannot afford uncle's advice." model, Cmd.none )

            else
                ( { model
                    | balanceCents = model.balanceCents - uncle.priceCents
                    , uncleAdviceCount = model.uncleAdviceCount + 1
                    , betInput = clampBetInput (model.balanceCents - uncle.priceCents) model.betInput
                  }
                , Random.generate UncleAdviceGiven
                    (Random.uniform uncle.firstPhrase uncle.morePhrases)
                )


logLine : LogTone -> String -> Model -> Model
logLine tone text model =
    { model | log = { tone = tone, text = text } :: model.log }


oppositeSide : CoinSide -> CoinSide
oppositeSide side =
    case side of
        Heads ->
            Tails

        Tails ->
            Heads


sideName : CoinSide -> String
sideName side =
    case side of
        Heads ->
            "Heads"

        Tails ->
            "Tails"


{-| Bet this fraction of the balance, floored to whole cents, but never
zero while there is still money to bet.
-}
quickBetCents : Float -> Int -> Int
quickBetCents fraction balanceCents =
    let
        flooredCents =
            floor (toFloat balanceCents * fraction)
    in
    if flooredCents <= 0 && balanceCents > 0 then
        1

    else
        flooredCents


{-| Parse a dollar amount ("12.34") into cents. Anything that is not a
positive amount of at least one cent is rejected.
-}
parseBetCents : String -> Maybe Int
parseBetCents input =
    case String.toFloat input of
        Nothing ->
            Nothing

        Just dollars ->
            let
                cents =
                    round (dollars * 100)
            in
            if cents <= 0 then
                Nothing

            else
                Just cents


{-| Cents as a dollar string without the sign: 999 -> "9.99".
-}
formatCents : Int -> String
formatCents cents =
    String.fromInt (cents // 100)
        ++ "."
        ++ String.padLeft 2 '0' (String.fromInt (modBy 100 cents))


{-| Remaining time as m:ss, matching the JS original ("30:00", "9:07").
-}
formatClock : Int -> String
formatClock totalSeconds =
    String.fromInt (totalSeconds // 60)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (modBy 60 totalSeconds))


{-| How often a side landed, as a whole percent of all flips so far.
-}
landedPercent : Int -> Int -> Int
landedPercent sideCount totalFlips =
    if totalFlips == 0 then
        0

    else
        round (100 * toFloat sideCount / toFloat totalFlips)


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.phase, model.clock ) of
        ( Playing, ClockRunning ) ->
            Time.every 1000 (\_ -> ClockTicked)

        ( Playing, ClockIdle ) ->
            Sub.none

        ( WonGame, _ ) ->
            Sub.none

        ( WentBust, _ ) ->
            Sub.none

        ( RanOutOfTime, _ ) ->
            Sub.none



-- VIEW


{-| The root carries the same id the markdown mount div has, because
Browser.element replaces the mount node and the posts' inline CSS is
scoped to #coin-flip-game.
-}
view : LevelConfig -> Model -> Html Msg
view config model =
    Html.div [ Html.Attributes.id "coin-flip-game" ]
        (List.concat
            [ [ Html.h3 [] [ Html.text config.title ]
              , viewStats model
              , viewProgressBar model
              , Html.div []
                    [ Html.text "Total flips: "
                    , Html.span [ Html.Attributes.class "flip-count" ]
                        [ Html.text (String.fromInt model.flipCount) ]
                    ]
              , Html.div [ Html.Attributes.class "balance" ]
                    [ Html.text ("$" ++ formatCents model.balanceCents) ]
              ]
            , if model.phase == Playing then
                [ viewControls config model ]

              else
                [ viewGameOver config model ]
            , [ viewLog model ]
            ]
        )


viewStats : Model -> Html Msg
viewStats model =
    Html.div [ Html.Attributes.class "stats" ]
        [ Html.div [] [ Html.text ("Target: $" ++ String.fromInt (targetBalanceCents // 100)) ]
        , Html.div []
            [ Html.text "Time left: "
            , Html.span [ Html.Attributes.class "timer" ]
                [ Html.text (formatClock model.secondsLeft) ]
            ]
        ]


viewProgressBar : Model -> Html Msg
viewProgressBar model =
    let
        progressPercent =
            min 100 (100 * toFloat model.balanceCents / toFloat targetBalanceCents)
    in
    Html.div [ Html.Attributes.class "progress-track" ]
        [ Html.div
            [ Html.Attributes.class "progress-fill"
            , Html.Attributes.style "width" (String.fromFloat progressPercent ++ "%")
            ]
            []
        ]


viewControls : LevelConfig -> Model -> Html Msg
viewControls config model =
    Html.div [ Html.Attributes.class "controls" ]
        (List.concat
            [ [ viewQuickBets
              , Html.div []
                    [ Html.label []
                        [ Html.text "Bet $: "
                        , Html.input
                            [ Html.Attributes.class "bet-amount"
                            , Html.Attributes.type_ "number"
                            , Html.Attributes.step "0.01"
                            , Html.Attributes.min "0.01"
                            , Html.Attributes.value model.betInput
                            , Html.Events.onInput BetInputChanged
                            ]
                            []
                        ]
                    ]
              , Html.div [ Html.Attributes.class "gamble-actions" ]
                    [ Html.button [ Html.Events.onClick (BetPlaced Heads) ]
                        [ Html.text (betButtonLabel config.bias Heads) ]
                    , Html.button [ Html.Events.onClick (BetPlaced Tails) ]
                        [ Html.text (betButtonLabel config.bias Tails) ]
                    ]
              ]
            , viewTally model
            , viewShop config model
            ]
        )


viewQuickBets : Html Msg
viewQuickBets =
    Html.div [ Html.Attributes.class "quick-bets" ]
        [ Html.text "Bet "
        , Html.button [ Html.Events.onClick (QuickBetPicked 0.1) ] [ Html.text "10%" ]
        , Html.button [ Html.Events.onClick (QuickBetPicked 0.2) ] [ Html.text "20%" ]
        , Html.button [ Html.Events.onClick (QuickBetPicked 0.5) ] [ Html.text "50%" ]
        , Html.button [ Html.Events.onClick (QuickBetPicked 1.0) ] [ Html.text "Max" ]
        , Html.text " of balance"
        ]


betButtonLabel : CoinBias -> CoinSide -> String
betButtonLabel bias side =
    case bias of
        KnownHeadsPercent headsPercent ->
            case side of
                Heads ->
                    "Bet HEADS (" ++ String.fromInt headsPercent ++ "%)"

                Tails ->
                    "Bet TAILS (" ++ String.fromInt (100 - headsPercent) ++ "%)"

        HiddenFavoredPercentRange _ ->
            case side of
                Heads ->
                    "Bet HEADS"

                Tails ->
                    "Bet TAILS"


{-| The tally the ratio tracker paints under the bet buttons. Without the
tracker you get nothing here: scroll the log and count by hand.
-}
viewTally : Model -> List (Html Msg)
viewTally model =
    case model.tracker of
        TrackerNotBought ->
            []

        TrackerBought ->
            [ Html.div [ Html.Attributes.class "tally" ]
                [ Html.text
                    ("\u{1F4CA} Heads won "
                        ++ String.fromInt model.headsLandedCount
                        ++ "\u{00D7} ("
                        ++ String.fromInt (landedPercent model.headsLandedCount model.flipCount)
                        ++ "%) \u{00B7} Tails won "
                        ++ String.fromInt model.tailsLandedCount
                        ++ "\u{00D7} ("
                        ++ String.fromInt (landedPercent model.tailsLandedCount model.flipCount)
                        ++ "%)"
                    )
                ]
            ]


viewShop : LevelConfig -> Model -> List (Html Msg)
viewShop config model =
    case viewTrackerShopItem config.trackerOffer model ++ viewUncleShopItem config.uncleOffer of
        [] ->
            []

        shopItems ->
            [ Html.div [ Html.Attributes.class "shop" ]
                (Html.div [ Html.Attributes.class "shop-header" ] [ Html.text "\u{1F6D2} Shop" ]
                    :: shopItems
                )
            ]


{-| One row in the shop: item name left, price right, so the prices line
up across items (the .shop-item CSS spreads the two spans apart).
-}
viewShopItem : Msg -> String -> Int -> Html Msg
viewShopItem onBuy itemName priceCents =
    Html.button
        [ Html.Attributes.class "shop-item", Html.Events.onClick onBuy ]
        [ Html.span [] [ Html.text itemName ]
        , Html.span [] [ Html.text ("$" ++ formatCents priceCents) ]
        ]


viewTrackerShopItem : TrackerOffer -> Model -> List (Html Msg)
viewTrackerShopItem offer model =
    case ( offer, model.tracker ) of
        ( NoTrackerForSale, TrackerNotBought ) ->
            []

        ( NoTrackerForSale, TrackerBought ) ->
            []

        ( TrackerForSale _, TrackerBought ) ->
            []

        ( TrackerForSale priceCents, TrackerNotBought ) ->
            [ viewShopItem TrackerPurchased "Buy ratio tracker" priceCents ]


viewUncleShopItem : UncleOffer -> List (Html Msg)
viewUncleShopItem offer =
    case offer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            [ viewShopItem UncleAdviceRequested "Ask uncle for advice" uncle.priceCents ]


viewGameOver : LevelConfig -> Model -> Html Msg
viewGameOver config model =
    let
        ( message, tone ) =
            gameOverMessage model
    in
    Html.div
        [ Html.Attributes.class ("game-over " ++ toneClass tone) ]
        (List.concat
            [ [ Html.text message ]
            , viewNextLevelLink config.nextLevelLink model
            , viewBustEnding config.bustEnding model
            , [ Html.div []
                    [ Html.text "It took you exactly "
                    , Html.strong [] [ Html.text (String.fromInt model.flipCount) ]
                    , Html.text " presses to get here."
                    ]
              ]
            , viewBiasReveal config.bias model
            , viewUncleSpend config.uncleOffer model
            ]
        )


viewNextLevelLink : NextLevelLink -> Model -> List (Html Msg)
viewNextLevelLink link model =
    case link of
        NoNextLevelLink ->
            []

        NextLevelLinkTo nextLevel ->
            if model.phase == WonGame then
                [ Html.text " "
                , Html.a [ Html.Attributes.href nextLevel.url ] [ Html.text nextLevel.label ]
                ]

            else
                []


viewBustEnding : BustEnding -> Model -> List (Html Msg)
viewBustEnding ending model =
    case ending of
        PlainBustEnding ->
            []

        BustEndingQuote quote ->
            if model.phase == WentBust then
                [ Html.div [ Html.Attributes.class "bust-ending" ]
                    [ Html.em [] [ Html.text quote ] ]
                ]

            else
                []


viewUncleSpend : UncleOffer -> Model -> List (Html Msg)
viewUncleSpend offer model =
    case offer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            if model.uncleAdviceCount == 0 then
                []

            else
                [ Html.div []
                    ([ Html.text "You paid uncle $"
                     , Html.strong []
                        [ Html.text (formatCents (model.uncleAdviceCount * uncle.priceCents)) ]
                     , Html.text " for his advice."
                     ]
                        ++ viewUncleVerdict model.phase
                    )
                ]


{-| The parting word on uncle's advice: snark on a loss, applause for
winning in spite of it. Its own element so the test suite can assert the
gating without pinning the wording.
-}
viewUncleVerdict : GamePhase -> List (Html Msg)
viewUncleVerdict phase =
    case phase of
        Playing ->
            []

        WonGame ->
            [ Html.span [ Html.Attributes.class "uncle-verdict" ]
                [ Html.text " Congratulations on ignoring every word of it." ]
            ]

        WentBust ->
            [ Html.span [ Html.Attributes.class "uncle-verdict" ]
                [ Html.text " It shows." ]
            ]

        RanOutOfTime ->
            [ Html.span [ Html.Attributes.class "uncle-verdict" ]
                [ Html.text " It shows." ]
            ]


gameOverMessage : Model -> ( String, LogTone )
gameOverMessage model =
    case model.phase of
        Playing ->
            ( "", NeutralTone )

        WonGame ->
            ( "\u{1F389} YOU WIN! You reached $" ++ formatCents model.balanceCents ++ "!", WinTone )

        WentBust ->
            ( "\u{1F480} REKT! You hit $0.00. Bankrupt.", LoseTone )

        RanOutOfTime ->
            ( "Time's up! You failed to reach the target.", LoseTone )


{-| Only revealed at the end, so the reader can incriminate themselves
first. Level 1 calls out tails bets; level 2 also reveals the drawn bias.
-}
viewBiasReveal : CoinBias -> Model -> List (Html Msg)
viewBiasReveal bias model =
    case bias of
        KnownHeadsPercent _ ->
            if model.tailsBetCount == 0 then
                []

            else
                [ Html.div []
                    [ Html.text "You bet on tails "
                    , Html.strong [] [ Html.text (String.fromInt model.tailsBetCount) ]
                    , Html.text
                        ((if model.tailsBetCount == 1 then
                            " time"

                          else
                            " times"
                         )
                            ++ ". The coin was rigged for heads, dear reader."
                        )
                    ]
                ]

        HiddenFavoredPercentRange range ->
            case model.biasState of
                BiasUndrawn ->
                    []

                BiasReady actualBias ->
                    List.concat
                        [ [ Html.div []
                                [ Html.text "The coin was rigged for "
                                , Html.strong [] [ Html.text (String.toLower (sideName actualBias.favored)) ]
                                , Html.text
                                    (" this time: it wins "
                                        ++ String.fromInt actualBias.favoredPercent
                                        ++ "% of the flips."
                                    )
                                ]
                          ]
                        , viewLosingSideBets (oppositeSide range.favored) model
                        ]


viewLosingSideBets : CoinSide -> Model -> List (Html Msg)
viewLosingSideBets losingSide model =
    let
        losingBetCount =
            case losingSide of
                Heads ->
                    model.headsBetCount

                Tails ->
                    model.tailsBetCount
    in
    if losingBetCount == 0 then
        []

    else
        [ Html.div []
            [ Html.text ("You bet on " ++ String.toLower (sideName losingSide) ++ " ")
            , Html.strong [] [ Html.text (String.fromInt losingBetCount) ]
            , Html.text
                (if losingBetCount == 1 then
                    " time."

                 else
                    " times."
                )
            ]
        ]


viewLog : Model -> Html Msg
viewLog model =
    Html.div [ Html.Attributes.class "log" ]
        (List.map viewLogLine model.log)


viewLogLine : LogLine -> Html Msg
viewLogLine line =
    case line.tone of
        NeutralTone ->
            Html.div [] [ Html.em [] [ Html.text line.text ] ]

        WinTone ->
            Html.div [ Html.Attributes.class (toneClass line.tone) ] [ Html.text line.text ]

        LoseTone ->
            Html.div [ Html.Attributes.class (toneClass line.tone) ] [ Html.text line.text ]


toneClass : LogTone -> String
toneClass tone =
    case tone of
        WinTone ->
            "win-text"

        LoseTone ->
            "lose-text"

        NeutralTone ->
            ""
