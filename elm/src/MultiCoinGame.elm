module MultiCoinGame exposing
    ( BustCause(..)
    , CoinConfig
    , CoinTally
    , GlassesOffer(..)
    , GoldenGlasses(..)
    , Model
    , Msg(..)
    , MultiCoinConfig
    , StakeInput(..)
    , formatPayout
    , gameProgram
    , initialModel
    , parseStake
    , payoutCents
    , update
    , view
    )

{-| Engine for the multi-coin betting game (level 3, black-swan.html).

Several named coins can be staked at once: the player fills in an amount
per coin (always betting heads), presses flip, and every staked coin
resolves with its own hidden win percent and hidden payout multiplier.
Finding out which coin is worth betting on IS the game, so nothing is
printed on the controls and the win logs state the exact payout.

Money and clock formatting, the game phases, and the shop offers are
shared with the single-coin engine (CoinFlipGame, levels 1 and 2); the
model, update, and view are separate because the interaction differs:
many simultaneous stakes and one flip button instead of a heads/tails
choice on a single coin.

-}
-- Decision: a separate engine module next to CoinFlipGame rather than
-- generalizing CoinFlipGame to N coins. The single-coin levels bet on a
-- side of one biased coin; this level stakes amounts on heads of several
-- coins at once. Forcing both interaction models through one
-- Model/Msg/view doubled the case analysis everywhere it was tried on
-- paper; sharing the pure pieces (phases, money, clock, shop types) keeps
-- the duplication to the small view fragments.

import Browser
import CoinFlipGame
    exposing
        ( ClockState(..)
        , CoinSide(..)
        , GamePhase(..)
        , LogLine
        , LogTone(..)
        , TrackerOffer(..)
        , TrackerState(..)
        , UncleOffer(..)
        , formatCents
        , formatClock
        , landedPercent
        , startingBalanceCents
        , targetBalanceCents
        , timeLimitSeconds
        )
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Time


{-| One coin on the table. The win percent and the payout (as a percent
of the stake: 100 = 1:1, 3000 = 30x) are never shown to the player.
-}
type alias CoinConfig =
    { coinName : String
    , winPercent : Int
    , payoutPercent : Int
    }


{-| The golden glasses reveal every coin's payout multiplier while
playing (the win percents stay hidden; that is what the ratio tracker
approximates). Price in cents.
-}
type GlassesOffer
    = NoGoldenGlasses
    | GoldenGlassesForSale Int


type GoldenGlasses
    = GlassesNotBought
    | GlassesBought


type alias MultiCoinConfig =
    { title : String
    , coins : List CoinConfig
    , trackerOffer : TrackerOffer
    , uncleOffer : UncleOffer
    , glassesOffer : GlassesOffer
    , introLogLine : String
    }


{-| Per-coin flip statistics, counting only flips the player staked.
-}
type alias CoinTally =
    { headsCount : Int
    , flipCount : Int
    }


type alias Model =
    { balanceCents : Int
    , stakeInputs : List String
    , tallies : List CoinTally
    , phase : GamePhase
    , clock : ClockState
    , secondsLeft : Int
    , roundCount : Int
    , tracker : TrackerState
    , glasses : GoldenGlasses
    , uncleAdviceCount : Int
    , bustCause : BustCause
    , log : List LogLine
    }


{-| How the bankruptcy happened. Busting on a coin flip is the game
working as intended; busting on uncle's consulting fee earns its own
callout.
-}
type BustCause
    = NotBusted
    | BustByBetting
    | BustByUncleAdvice


type Msg
    = StakeInputChanged Int String
    | FlipPressed
    | CoinsLanded { stakes : List Int, rolls : List Int }
    | ClockTicked
    | TrackerPurchased
    | GlassesPurchased
    | UncleAdviceRequested
    | UncleAdviceGiven String


gameProgram : MultiCoinConfig -> Program () Model Msg
gameProgram config =
    Browser.element
        { init = \() -> ( initialModel config, Cmd.none )
        , update = update config
        , subscriptions = subscriptions
        , view = view config
        }


initialModel : MultiCoinConfig -> Model
initialModel config =
    { balanceCents = startingBalanceCents
    , stakeInputs = List.map (\_ -> "0.00") config.coins
    , tallies = List.map (\_ -> { headsCount = 0, flipCount = 0 }) config.coins
    , phase = Playing
    , clock = ClockIdle
    , secondsLeft = timeLimitSeconds
    , roundCount = 0
    , tracker = TrackerNotBought
    , glasses = GlassesNotBought
    , uncleAdviceCount = 0
    , bustCause = NotBusted
    , log = [ { tone = NeutralTone, text = config.introLogLine } ]
    }


update : MultiCoinConfig -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        StakeInputChanged coinIndex newInput ->
            ( { model
                | stakeInputs =
                    List.indexedMap
                        (\index input ->
                            if index == coinIndex then
                                newInput

                            else
                                input
                        )
                        model.stakeInputs
              }
            , Cmd.none
            )

        FlipPressed ->
            pressFlip config model

        CoinsLanded landedRound ->
            ( settleRound config landedRound model, Cmd.none )

        ClockTicked ->
            ( tickClock model, Cmd.none )

        TrackerPurchased ->
            ( purchaseTracker config.trackerOffer model, Cmd.none )

        GlassesPurchased ->
            ( purchaseGlasses config.glassesOffer model, Cmd.none )

        UncleAdviceRequested ->
            purchaseUncleAdvice config.uncleOffer model

        UncleAdviceGiven phrase ->
            ( logLine NeutralTone ("\u{1F9D3} Uncle: \u{201C}" ++ phrase ++ "\u{201D}") model
            , Cmd.none
            )


{-| A stake input field, classified. Blank or zero means the coin simply
is not played this round; junk that fails to parse is an error the
player must see, never a silent no-bet.
-}
type StakeInput
    = NoStake
    | Stake Int
    | UnreadableStake


parseStake : String -> StakeInput
parseStake input =
    if String.isEmpty (String.trim input) then
        NoStake

    else
        case String.toFloat input of
            Nothing ->
                UnreadableStake

            Just dollars ->
                let
                    cents =
                        round (dollars * 100)
                in
                if cents <= 0 then
                    NoStake

                else
                    Stake cents


{-| Validate all stake inputs and, if they hold up, flip every staked
coin at once. The clock starts on the first accepted flip.
-}
pressFlip : MultiCoinConfig -> Model -> ( Model, Cmd Msg )
pressFlip config model =
    if model.phase /= Playing then
        ( model, Cmd.none )

    else
        case validateStakes config.coins model.stakeInputs of
            UnreadableCoin coinName ->
                ( logLine NeutralTone ("Cannot read your bet on " ++ coinName ++ ".") model
                , Cmd.none
                )

            ReadableStakes stakes ->
                let
                    totalStaked =
                        List.sum stakes
                in
                if totalStaked <= 0 then
                    ( logLine NeutralTone "Place a bet on at least one coin first." model
                    , Cmd.none
                    )

                else if totalStaked > model.balanceCents then
                    ( logLine NeutralTone "You cannot bet more than your current balance!" model
                    , Cmd.none
                    )

                else
                    ( { model | clock = ClockRunning }
                    , Random.generate
                        (\rolls -> CoinsLanded { stakes = stakes, rolls = rolls })
                        (Random.list (List.length config.coins) (Random.int 1 100))
                    )


{-| All stake inputs validated together: either every input reads as an
amount (unplayed coins as zero), or the first unreadable coin's name.
The unreadable case never turns into a number, so a typo can never be
silently played as a $0 bet.
-}
type StakesValidation
    = ReadableStakes (List Int)
    | UnreadableCoin String


validateStakes : List CoinConfig -> List String -> StakesValidation
validateStakes coins stakeInputs =
    validateStakePairs (List.map2 Tuple.pair coins stakeInputs)


validateStakePairs : List ( CoinConfig, String ) -> StakesValidation
validateStakePairs pairs =
    case pairs of
        [] ->
            ReadableStakes []

        ( coin, input ) :: rest ->
            case parseStake input of
                UnreadableStake ->
                    UnreadableCoin coin.coinName

                NoStake ->
                    prependStake 0 (validateStakePairs rest)

                Stake cents ->
                    prependStake cents (validateStakePairs rest)


prependStake : Int -> StakesValidation -> StakesValidation
prependStake cents validated =
    case validated of
        UnreadableCoin coinName ->
            UnreadableCoin coinName

        ReadableStakes stakes ->
            ReadableStakes (cents :: stakes)


{-| What a win on this stake pays out (the profit; the stake itself is
never taken on a win, matching levels 1 and 2). Floored to whole cents
but never zero: a "win" that pays nothing would be a silent lie.
-}
payoutCents : Int -> Int -> Int
payoutCents payoutPercent stake =
    max 1 (stake * payoutPercent // 100)


{-| The result of one coin in a settled round.
-}
type alias CoinOutcome =
    { tally : CoinTally
    , deltaCents : Int
    , logLines : List LogLine
    }


resolveCoin : CoinConfig -> CoinTally -> Int -> Int -> CoinOutcome
resolveCoin coin tally stake roll =
    if stake <= 0 then
        { tally = tally, deltaCents = 0, logLines = [] }

    else
        let
            landed =
                if roll <= coin.winPercent then
                    Heads

                else
                    Tails
        in
        case landed of
            Heads ->
                let
                    paidOut =
                        payoutCents coin.payoutPercent stake
                in
                -- The logged amount is the full return, stake plus
                -- winnings, because "paid out $0.10 on your $0.10" read
                -- as a break-even coin. The balance delta stays the
                -- winnings alone: the stake never left the balance.
                { tally = { headsCount = tally.headsCount + 1, flipCount = tally.flipCount + 1 }
                , deltaCents = paidOut
                , logLines =
                    [ { tone = WinTone
                      , text =
                            coin.coinName
                                ++ " landed heads! You win $"
                                ++ formatCents (stake + paidOut)
                                ++ ", of which $"
                                ++ formatCents stake
                                ++ " is your stake."
                      }
                    ]
                }

            Tails ->
                { tally = { headsCount = tally.headsCount, flipCount = tally.flipCount + 1 }
                , deltaCents = -stake
                , logLines =
                    [ { tone = LoseTone
                      , text = coin.coinName ++ " landed tails. You lost your $" ++ formatCents stake ++ " stake."
                      }
                    ]
                }


{-| Apply a landed round: pay out or collect per staked coin, update the
tallies, and check for the win/bust end states.
-}
settleRound : MultiCoinConfig -> { stakes : List Int, rolls : List Int } -> Model -> Model
settleRound config landedRound model =
    if model.phase /= Playing then
        model

    else
        let
            outcomes =
                List.map4 resolveCoin config.coins model.tallies landedRound.stakes landedRound.rolls

            newBalance =
                max 0 (model.balanceCents + List.sum (List.map .deltaCents outcomes))

            roundLogLines =
                List.concatMap .logLines outcomes
        in
        checkEndState config.uncleOffer
            BustByBetting
            { model
                | balanceCents = newBalance
                , tallies = List.map .tally outcomes
                , roundCount = model.roundCount + 1
                , log = List.reverse roundLogLines ++ model.log
            }


checkEndState : UncleOffer -> BustCause -> Model -> Model
checkEndState uncleOffer causeIfBusted model =
    if model.balanceCents >= targetBalanceCents then
        { model | phase = WonGame }

    else if model.balanceCents <= 0 then
        uncleGloatsOnBust uncleOffer { model | phase = WentBust, bustCause = causeIfBusted }

    else
        model


{-| The moment you go bankrupt, uncle pops into the log, if this level
has an uncle at all.
-}
uncleGloatsOnBust : UncleOffer -> Model -> Model
uncleGloatsOnBust offer model =
    case offer of
        NoUncleAdvice ->
            model

        UncleAdviceForSale _ ->
            logLine NeutralTone "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}" model


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
same rule as levels 1 and 2.
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
                    }


{-| Buy the golden glasses. Same wipe-out guard as the tracker.
-}
purchaseGlasses : GlassesOffer -> Model -> Model
purchaseGlasses offer model =
    case ( offer, model.glasses ) of
        ( NoGoldenGlasses, GlassesNotBought ) ->
            model

        ( NoGoldenGlasses, GlassesBought ) ->
            model

        ( GoldenGlassesForSale _, GlassesBought ) ->
            model

        ( GoldenGlassesForSale priceCents, GlassesNotBought ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= priceCents then
                logLine NeutralTone "You cannot afford the golden glasses." model

            else
                logLine NeutralTone
                    ("Bought the golden glasses for $" ++ formatCents priceCents ++ ".")
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , glasses = GlassesBought
                    }


{-| Pay uncle and draw one of his pre-programmed pearls of wisdom.
Unlike the tracker and glasses, uncle happily takes your last dollars:
spending yourself into bankruptcy on advice is a lesson the game wants
to allow.
-}
purchaseUncleAdvice : UncleOffer -> Model -> ( Model, Cmd Msg )
purchaseUncleAdvice offer model =
    case offer of
        NoUncleAdvice ->
            ( model, Cmd.none )

        UncleAdviceForSale uncle ->
            if model.phase /= Playing then
                ( model, Cmd.none )

            else if model.balanceCents < uncle.priceCents then
                ( logLine NeutralTone "You cannot afford uncle's advice." model, Cmd.none )

            else
                ( checkEndState offer
                    BustByUncleAdvice
                    { model
                        | balanceCents = model.balanceCents - uncle.priceCents
                        , uncleAdviceCount = model.uncleAdviceCount + 1
                    }
                , Random.generate UncleAdviceGiven
                    (Random.uniform uncle.firstPhrase uncle.morePhrases)
                )


logLine : LogTone -> String -> Model -> Model
logLine tone text model =
    { model | log = { tone = tone, text = text } :: model.log }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.phase of
        Playing ->
            case model.clock of
                ClockRunning ->
                    Time.every 1000 (\_ -> ClockTicked)

                ClockIdle ->
                    Sub.none

        WonGame ->
            Sub.none

        WentBust ->
            Sub.none

        RanOutOfTime ->
            Sub.none



-- VIEW


{-| The root carries the same id the markdown mount div has, because
Browser.element replaces the mount node and the posts' inline CSS is
scoped to #coin-flip-game.
-}
view : MultiCoinConfig -> Model -> Html Msg
view config model =
    Html.div [ Html.Attributes.id "coin-flip-game" ]
        (List.concat
            [ [ Html.h3 [] [ Html.text config.title ]
              , viewStats model
              , viewProgressBar model
              , Html.div []
                    [ Html.text "Total flips: "
                    , Html.span [ Html.Attributes.class "flip-count" ]
                        [ Html.text (String.fromInt model.roundCount) ]
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


viewControls : MultiCoinConfig -> Model -> Html Msg
viewControls config model =
    Html.div [ Html.Attributes.class "controls" ]
        (List.concat
            [ List.indexedMap viewCoinRow
                (List.map2 Tuple.pair config.coins model.stakeInputs)
            , [ Html.button
                    [ Html.Attributes.class "flip-button"
                    , Html.Events.onClick FlipPressed
                    ]
                    [ Html.text "FLIP" ]
              ]
            , viewGlassesPayouts config model
            , viewTally config model
            , viewShop config model
            ]
        )


{-| The payout multipliers the golden glasses reveal, under the flip
button. Only the payouts: the win percents stay hidden.
-}
viewGlassesPayouts : MultiCoinConfig -> Model -> List (Html Msg)
viewGlassesPayouts config model =
    case model.glasses of
        GlassesNotBought ->
            []

        GlassesBought ->
            [ Html.div [ Html.Attributes.class "glasses" ]
                [ Html.text
                    ("\u{1F453} Pays: "
                        ++ String.join " \u{00B7} "
                            (List.map
                                (\coin -> coin.coinName ++ " " ++ formatPayout coin.payoutPercent)
                                config.coins
                            )
                    )
                ]
            ]


viewCoinRow : Int -> ( CoinConfig, String ) -> Html Msg
viewCoinRow coinIndex ( coin, stakeInput ) =
    Html.div [ Html.Attributes.class "coin-bet" ]
        [ Html.label [] [ Html.text (coin.coinName ++ " $:") ]
        , Html.input
            [ Html.Attributes.class "bet-amount"
            , Html.Attributes.type_ "number"
            , Html.Attributes.step "0.01"
            , Html.Attributes.min "0.01"
            , Html.Attributes.value stakeInput
            , Html.Events.onInput (StakeInputChanged coinIndex)
            ]
            []
        ]


{-| The per-coin tally the ratio tracker paints under the flip button.
-}
viewTally : MultiCoinConfig -> Model -> List (Html Msg)
viewTally config model =
    case model.tracker of
        TrackerNotBought ->
            []

        TrackerBought ->
            [ Html.div [ Html.Attributes.class "tally" ]
                [ Html.text
                    ("\u{1F4CA} "
                        ++ String.join " \u{00B7} "
                            (List.map2 coinTallyText config.coins model.tallies)
                    )
                ]
            ]


coinTallyText : CoinConfig -> CoinTally -> String
coinTallyText coin tally =
    coin.coinName
        ++ " "
        ++ String.fromInt tally.headsCount
        ++ "/"
        ++ String.fromInt tally.flipCount
        ++ " ("
        ++ String.fromInt (landedPercent tally.headsCount tally.flipCount)
        ++ "%)"


viewShop : MultiCoinConfig -> Model -> List (Html Msg)
viewShop config model =
    case
        viewTrackerShopItem config.trackerOffer model
            ++ viewGlassesShopItem config.glassesOffer model
            ++ viewUncleShopItem config.uncleOffer
    of
        [] ->
            []

        shopItems ->
            [ Html.div [ Html.Attributes.class "shop" ]
                (Html.div [ Html.Attributes.class "shop-header" ] [ Html.text "\u{1F6D2} Shop" ]
                    :: shopItems
                )
            ]


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


viewGlassesShopItem : GlassesOffer -> Model -> List (Html Msg)
viewGlassesShopItem offer model =
    case ( offer, model.glasses ) of
        ( NoGoldenGlasses, GlassesNotBought ) ->
            []

        ( NoGoldenGlasses, GlassesBought ) ->
            []

        ( GoldenGlassesForSale _, GlassesBought ) ->
            []

        ( GoldenGlassesForSale priceCents, GlassesNotBought ) ->
            [ viewShopItem GlassesPurchased "Buy golden glasses" priceCents ]


viewUncleShopItem : UncleOffer -> List (Html Msg)
viewUncleShopItem offer =
    case offer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            [ viewShopItem UncleAdviceRequested "Ask uncle for advice" uncle.priceCents ]


viewGameOver : MultiCoinConfig -> Model -> Html Msg
viewGameOver config model =
    let
        ( message, tone ) =
            gameOverMessage model
    in
    Html.div
        [ Html.Attributes.class ("game-over " ++ CoinFlipGame.toneClass tone) ]
        (List.concat
            [ [ Html.text message ]
            , viewUncleBustCallout model
            , [ Html.div []
                    [ Html.text "It took you exactly "
                    , Html.strong [] [ Html.text (String.fromInt model.roundCount) ]
                    , Html.text " presses to get here."
                    ]
              ]
            , List.map viewCoinReveal config.coins
            , viewUncleSpend config.uncleOffer model
            ]
        )


viewUncleBustCallout : Model -> List (Html Msg)
viewUncleBustCallout model =
    case model.bustCause of
        NotBusted ->
            []

        BustByBetting ->
            []

        BustByUncleAdvice ->
            [ Html.div [ Html.Attributes.class "uncle-bust" ]
                [ Html.text "You didn't even lose it betting: you spent your last dollars on uncle's advice. That's just sad." ]
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


{-| Only revealed at the end: what each coin actually did.
-}
viewCoinReveal : CoinConfig -> Html Msg
viewCoinReveal coin =
    Html.div [ Html.Attributes.class "bias-reveal" ]
        [ Html.strong [] [ Html.text coin.coinName ]
        , Html.text
            (": heads "
                ++ String.fromInt coin.winPercent
                ++ "% of the time, paying "
                ++ formatPayout coin.payoutPercent
                ++ " the stake."
            )
        ]


{-| A payout percent as a multiplier: 3000 -> "30x", 50 -> "0.5x".
-}
formatPayout : Int -> String
formatPayout payoutPercent =
    if modBy 100 payoutPercent == 0 then
        String.fromInt (payoutPercent // 100) ++ "\u{00D7}"

    else
        String.fromInt (payoutPercent // 100)
            ++ "."
            ++ String.fromInt (modBy 100 payoutPercent // 10)
            ++ "\u{00D7}"


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
            Html.div [ Html.Attributes.class (CoinFlipGame.toneClass line.tone) ]
                [ Html.text line.text ]

        LoseTone ->
            Html.div [ Html.Attributes.class (CoinFlipGame.toneClass line.tone) ]
                [ Html.text line.text ]
