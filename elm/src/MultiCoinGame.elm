module MultiCoinGame exposing
    ( Autoclicker(..)
    , AutoclickerOffer(..)
    , BustCause(..)
    , CoinConfig
    , CoinTally
    , FlipHelperOffer(..)
    , FlipHold(..)
    , GlassesOffer(..)
    , GoldenGlasses(..)
    , Model
    , Msg(..)
    , MultiCoinConfig
    , ProfileAssignment(..)
    , ShopFold(..)
    , StakeInput(..)
    , formatPayout
    , gameProgram
    , initialModel
    , parseStake
    , payoutCents
    , shuffledCoinsGenerator
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
        , UncleGloat(..)
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
import Json.Decode as Decode
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


{-| The autoclicker spares the player's mouse finger: once bought,
holding the flip button down fires a flip every 100ms. Price in cents.
-}
type AutoclickerOffer
    = NoAutoclicker
    | AutoclickerForSale Int


type Autoclicker
    = ClickerNotBought
    | ClickerBought


{-| Whether the flip button is currently held down (mouse or touch).
Only meaningful with a bought autoclicker: the auto-flip subscription
runs while held.
-}
type FlipHold
    = FlipReleased
    | FlipHeld


{-| Flip helpers flip for the player: each one presses flip once per
five seconds, staggered, so owning N means a flip every 5000/N ms. The
price starts at the base and rises by the given percent on every hire,
compounding: automation gets progressively more expensive.
-}
type FlipHelperOffer
    = NoFlipHelpers
    | FlipHelpersForSale { basePriceCents : Int, priceIncreasePercent : Int }


{-| The shop folds away behind its header and starts collapsed, so the
game opens on the bets, not the catalogue.
-}
type ShopFold
    = ShopCollapsed
    | ShopExpanded


{-| Whether the (win percent, payout) profiles play on the coins as
written in the config, or get dealt across the coin names at random on
every game start, so a replay means rediscovering which coin is which.
-}
type ProfileAssignment
    = ProfilesAsWritten
    | ProfilesShuffledAcrossCoins


type alias MultiCoinConfig =
    { title : String
    , coins : List CoinConfig
    , profileAssignment : ProfileAssignment
    , trackerOffer : TrackerOffer
    , uncleOffer : UncleOffer
    , glassesOffer : GlassesOffer
    , autoclickerOffer : AutoclickerOffer
    , flipHelperOffer : FlipHelperOffer
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
    , coins : List CoinConfig
    , stakeInputs : List String
    , tallies : List CoinTally
    , phase : GamePhase
    , clock : ClockState
    , secondsLeft : Int
    , roundCount : Int
    , tracker : TrackerState
    , glasses : GoldenGlasses
    , autoclicker : Autoclicker
    , flipHold : FlipHold
    , flipHelperCount : Int
    , nextHelperPriceCents : Int
    , shopFold : ShopFold
    , uncleAdviceCount : Int
    , uncleGloat : UncleGloat
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
    = ProfilesShuffled (List CoinConfig)
    | StakeInputChanged Int String
    | FlipPressed
    | FlipHoldStarted
    | FlipHoldEnded
    | AutoclickerTicked
    | FlipHelperHired
    | FlipHelpersTicked
    | ShopToggled
    | CoinsLanded { stakes : List Int, rolls : List Int }
    | ClockTicked
    | TrackerPurchased
    | GlassesPurchased
    | AutoclickerPurchased
    | UncleAdviceRequested
    | UncleAdviceGiven String


gameProgram : MultiCoinConfig -> Program () Model Msg
gameProgram config =
    Browser.element
        { init = init config
        , update = update config
        , subscriptions = subscriptions
        , view = view config
        }


-- Decision: with shuffled profiles the model briefly holds the coins as
-- written until the ProfilesShuffled draw lands, one runtime tick after
-- init. No pending state guards this window (unlike level 2's
-- BiasUndrawn): the as-written assignment is itself a valid hidden
-- assignment, no human can act within the tick, and gating every coin
-- access on an assignment state doubled the case analysis for nothing.
init : MultiCoinConfig -> () -> ( Model, Cmd Msg )
init config () =
    ( initialModel config
    , case config.profileAssignment of
        ProfilesAsWritten ->
            Cmd.none

        ProfilesShuffledAcrossCoins ->
            Random.generate ProfilesShuffled (shuffledCoinsGenerator config.coins)
    )


{-| Deal the (win percent, payout) profiles across the coin names at
random: the names keep their display order, the profiles land on a
random coin each.
-}
shuffledCoinsGenerator : List CoinConfig -> Random.Generator (List CoinConfig)
shuffledCoinsGenerator coins =
    Random.map
        (\sortKeys ->
            List.map2
                (\coin profile ->
                    { coin | winPercent = profile.winPercent, payoutPercent = profile.payoutPercent }
                )
                coins
                (List.map Tuple.second
                    (List.sortBy Tuple.first (List.map2 Tuple.pair sortKeys coins))
                )
        )
        (Random.list (List.length coins) (Random.float 0 1))


initialModel : MultiCoinConfig -> Model
initialModel config =
    { balanceCents = startingBalanceCents
    , coins = config.coins
    , stakeInputs = List.map (\_ -> "0.00") config.coins
    , tallies = List.map (\_ -> { headsCount = 0, flipCount = 0 }) config.coins
    , phase = Playing
    , clock = ClockIdle
    , secondsLeft = timeLimitSeconds
    , roundCount = 0
    , tracker = TrackerNotBought
    , glasses = GlassesNotBought
    , autoclicker = ClickerNotBought
    , flipHold = FlipReleased
    , flipHelperCount = 0
    , nextHelperPriceCents = initialHelperPriceCents config.flipHelperOffer
    , shopFold = ShopCollapsed
    , uncleAdviceCount = 0
    , uncleGloat = UncleHasNotGloated
    , bustCause = NotBusted
    , log = [ { tone = NeutralTone, text = config.introLogLine } ]
    }


update : MultiCoinConfig -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        ProfilesShuffled shuffledCoins ->
            ( { model | coins = shuffledCoins }, Cmd.none )

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

        FlipHoldStarted ->
            ( { model | flipHold = FlipHeld }, Cmd.none )

        FlipHoldEnded ->
            ( { model | flipHold = FlipReleased }, Cmd.none )

        AutoclickerTicked ->
            pressFlip config model

        FlipHelperHired ->
            ( hireFlipHelper config.flipHelperOffer model, Cmd.none )

        FlipHelpersTicked ->
            pressFlip config model

        ShopToggled ->
            ( { model
                | shopFold =
                    case model.shopFold of
                        ShopCollapsed ->
                            ShopExpanded

                        ShopExpanded ->
                            ShopCollapsed
              }
            , Cmd.none
            )

        CoinsLanded landedRound ->
            ( settleRound config landedRound model, Cmd.none )

        ClockTicked ->
            ( tickClock model, Cmd.none )

        TrackerPurchased ->
            ( purchaseTracker config.trackerOffer model, Cmd.none )

        GlassesPurchased ->
            ( purchaseGlasses config.glassesOffer model, Cmd.none )

        AutoclickerPurchased ->
            ( purchaseAutoclicker config.autoclickerOffer model, Cmd.none )

        UncleAdviceRequested ->
            purchaseUncleAdvice config.uncleOffer model

        UncleAdviceGiven phrase ->
            ( gloatIfBusted config.uncleOffer
                (logLine NeutralTone ("\u{1F9D3} Uncle: \u{201C}" ++ phrase ++ "\u{201D}") model)
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
        case validateStakes model.coins model.stakeInputs of
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
                        (Random.list (List.length model.coins) (Random.int 1 100))
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
                List.map4 resolveCoin model.coins model.tallies landedRound.stakes landedRound.rolls

            newBalance =
                max 0 (model.balanceCents + List.sum (List.map .deltaCents outcomes))

            roundLogLines =
                List.concatMap .logLines outcomes
        in
        gloatIfBusted config.uncleOffer
            (checkEndState BustByBetting
                { model
                    | balanceCents = newBalance
                    , tallies = List.map .tally outcomes
                    , roundCount = model.roundCount + 1
                    , log = List.reverse roundLogLines ++ model.log
                }
            )


checkEndState : BustCause -> Model -> Model
checkEndState causeIfBusted model =
    if model.balanceCents >= targetBalanceCents then
        { model | phase = WonGame }

    else if model.balanceCents <= 0 then
        { model | phase = WentBust, bustCause = causeIfBusted }

    else
        model


{-| Once bankrupt, uncle pops into the log, if this level has an uncle
at all. Called after every event that could have busted the game (a
settled round, or uncle's own advice landing when the purchase took the
last dollars), so his gloat is always the newest log line, exactly
once.
-}
gloatIfBusted : UncleOffer -> Model -> Model
gloatIfBusted offer model =
    case offer of
        NoUncleAdvice ->
            model

        UncleAdviceForSale _ ->
            if model.phase == WentBust && model.uncleGloat == UncleHasNotGloated then
                logLine NeutralTone
                    "\u{1F9D3} Uncle: \u{201C}I'm proud of you kid\u{201D} \u{1F911}"
                    { model | uncleGloat = UncleHasGloated }

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


{-| What the very first flip helper costs. Zero without an offer, which
is never read: hiring is impossible when no helpers are for sale, the
purchase path matches on the offer before touching the price.
-}
initialHelperPriceCents : FlipHelperOffer -> Int
initialHelperPriceCents offer =
    case offer of
        NoFlipHelpers ->
            0

        FlipHelpersForSale sale ->
            sale.basePriceCents


{-| Hire one more flip helper at the current price, then raise the
price for the next one. Same wipe-out guard as the other equipment.
-}
hireFlipHelper : FlipHelperOffer -> Model -> Model
hireFlipHelper offer model =
    case offer of
        NoFlipHelpers ->
            model

        FlipHelpersForSale sale ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= model.nextHelperPriceCents then
                logLine NeutralTone "You cannot afford another flip helper." model

            else
                logLine NeutralTone
                    ("Hired flip helper number "
                        ++ String.fromInt (model.flipHelperCount + 1)
                        ++ " for $"
                        ++ formatCents model.nextHelperPriceCents
                        ++ ". Each helper flips once per five seconds."
                    )
                    { model
                        | balanceCents = model.balanceCents - model.nextHelperPriceCents
                        , flipHelperCount = model.flipHelperCount + 1
                        , nextHelperPriceCents =
                            raisePriceByPercent sale.priceIncreasePercent model.nextHelperPriceCents
                    }


{-| A price raised by the given percent, rounded to whole cents.
-}
raisePriceByPercent : Int -> Int -> Int
raisePriceByPercent increasePercent priceCents =
    (priceCents * (100 + increasePercent) + 50) // 100


{-| Buy the autoclicker. Same wipe-out guard as the tracker.
-}
purchaseAutoclicker : AutoclickerOffer -> Model -> Model
purchaseAutoclicker offer model =
    case ( offer, model.autoclicker ) of
        ( NoAutoclicker, ClickerNotBought ) ->
            model

        ( NoAutoclicker, ClickerBought ) ->
            model

        ( AutoclickerForSale _, ClickerBought ) ->
            model

        ( AutoclickerForSale priceCents, ClickerNotBought ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= priceCents then
                logLine NeutralTone "You cannot afford the autoclicker." model

            else
                logLine NeutralTone
                    ("Bought the autoclicker for $"
                        ++ formatCents priceCents
                        ++ ". Hold the flip button down to use it."
                    )
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , autoclicker = ClickerBought
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
                -- No gloat here even when this purchase busts the game:
                -- the bought advice is still in flight, and the gloat
                -- must land after it (see gloatIfBusted).
                ( checkEndState BustByUncleAdvice
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
    Sub.batch
        [ clockSubscription model
        , autoclickerSubscription model
        , flipHelperSubscription model
        ]


{-| Each hired helper flips once per five seconds, staggered: N helpers
share one timer at 5000/N ms, which is N evenly spread flips per five
seconds.
-}
flipHelperSubscription : Model -> Sub Msg
flipHelperSubscription model =
    case model.phase of
        Playing ->
            if model.flipHelperCount > 0 then
                Time.every (5000 / toFloat model.flipHelperCount) (\_ -> FlipHelpersTicked)

            else
                Sub.none

        WonGame ->
            Sub.none

        WentBust ->
            Sub.none

        RanOutOfTime ->
            Sub.none


clockSubscription : Model -> Sub Msg
clockSubscription model =
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


{-| While a bought autoclicker's flip button is held down, a flip fires
every 100ms.
-}
autoclickerSubscription : Model -> Sub Msg
autoclickerSubscription model =
    case model.phase of
        Playing ->
            case ( model.autoclicker, model.flipHold ) of
                ( ClickerBought, FlipHeld ) ->
                    Time.every 100 (\_ -> AutoclickerTicked)

                ( ClickerBought, FlipReleased ) ->
                    Sub.none

                ( ClickerNotBought, FlipHeld ) ->
                    Sub.none

                ( ClickerNotBought, FlipReleased ) ->
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
                (List.map2 Tuple.pair model.coins model.stakeInputs)
            , [ Html.button
                    (Html.Attributes.class "flip-button"
                        :: Html.Events.onClick FlipPressed
                        :: flipHoldEvents model.autoclicker
                    )
                    [ Html.text "FLIP" ]
              ]
            , viewGlassesPayouts model
            , viewTally config model
            , viewFlipHelpers model
            , viewShop config model
            ]
        )


{-| How many helpers are flipping for the player, shown once any are
hired.
-}
viewFlipHelpers : Model -> List (Html Msg)
viewFlipHelpers model =
    if model.flipHelperCount == 0 then
        []

    else
        [ Html.div [ Html.Attributes.class "helpers" ]
            [ Html.text ("\u{1F424} Flip helpers: " ++ String.fromInt model.flipHelperCount) ]
        ]


{-| The payout multipliers the golden glasses reveal, under the flip
button. Only the payouts: the win percents stay hidden. Read from the
model's coins, not the config: with shuffled profiles the glasses must
report the deal actually in play.
-}
viewGlassesPayouts : Model -> List (Html Msg)
viewGlassesPayouts model =
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
                                model.coins
                            )
                    )
                ]
            ]


{-| Hold events for the flip button, mouse and touch. Only attached once
the autoclicker is bought, so an unbought game never tracks hold state.
Mouse-leave and touch-cancel count as releasing: dragging off the button
must not leave the clicker running.
-}
flipHoldEvents : Autoclicker -> List (Html.Attribute Msg)
flipHoldEvents clicker =
    case clicker of
        ClickerNotBought ->
            []

        ClickerBought ->
            [ Html.Events.onMouseDown FlipHoldStarted
            , Html.Events.onMouseUp FlipHoldEnded
            , Html.Events.onMouseLeave FlipHoldEnded
            , Html.Events.on "touchstart" (Decode.succeed FlipHoldStarted)
            , Html.Events.on "touchend" (Decode.succeed FlipHoldEnded)
            , Html.Events.on "touchcancel" (Decode.succeed FlipHoldEnded)
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
                            (List.map2 coinTallyText model.coins model.tallies)
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
            ++ viewAutoclickerShopItem config.autoclickerOffer model
            ++ viewFlipHelperShopItem config.flipHelperOffer model
            ++ viewUncleShopItem config.uncleOffer
    of
        [] ->
            []

        shopItems ->
            [ Html.div [ Html.Attributes.class "shop" ]
                (viewShopToggle model.shopFold
                    :: (case model.shopFold of
                            ShopCollapsed ->
                                []

                            ShopExpanded ->
                                shopItems
                       )
                )
            ]


viewShopToggle : ShopFold -> Html Msg
viewShopToggle fold =
    Html.button
        [ Html.Attributes.class "shop-toggle", Html.Events.onClick ShopToggled ]
        [ Html.text
            (case fold of
                ShopCollapsed ->
                    "\u{1F6D2} Shop \u{25B8}"

                ShopExpanded ->
                    "\u{1F6D2} Shop \u{25BE}"
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


viewAutoclickerShopItem : AutoclickerOffer -> Model -> List (Html Msg)
viewAutoclickerShopItem offer model =
    case ( offer, model.autoclicker ) of
        ( NoAutoclicker, ClickerNotBought ) ->
            []

        ( NoAutoclicker, ClickerBought ) ->
            []

        ( AutoclickerForSale _, ClickerBought ) ->
            []

        ( AutoclickerForSale priceCents, ClickerNotBought ) ->
            [ viewShopItem AutoclickerPurchased "Buy autoclicker" priceCents ]


{-| Unlike the one-shot equipment, helpers stay for sale forever: the
price just keeps climbing.
-}
viewFlipHelperShopItem : FlipHelperOffer -> Model -> List (Html Msg)
viewFlipHelperShopItem offer model =
    case offer of
        NoFlipHelpers ->
            []

        FlipHelpersForSale _ ->
            [ viewShopItem FlipHelperHired "Hire flip helper" model.nextHelperPriceCents ]


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
