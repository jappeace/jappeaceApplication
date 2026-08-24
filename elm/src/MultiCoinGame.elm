module MultiCoinGame exposing
    ( AllocationMode(..)
    , AllocatorOffer(..)
    , RefundOffer(..)
    , RefundState(..)
    , BustCause(..)
    , CorrelationBook(..)
    , CorrelationBookOffer(..)
    , ExtraTurnsOffer(..)
    , ExtraTurnsPackage
    , LastChanceTurnOffer(..)
    , CoinConfig
    , CoinOdds(..)
    , CoinTally
    , FlipHelperOffer(..)
    , SkyState(..)
    , TurnBudget(..)
    , FlipHold(..)
    , GlassesOffer(..)
    , HelperPause(..)
    , GoldenGlasses(..)
    , Model
    , Msg(..)
    , MultiCoinConfig
    , PendingPurchase(..)
    , ProfileAssignment(..)
    , ShopItemKind(..)
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
import Browser.Events
import CoinFlipGame
    exposing
        ( Autoclicker(..)
        , AutoclickerOffer(..)
        , ClockState(..)
        , CoinSide(..)
        , ExtraTimeOffer(..)
        , ExtraTimePackage
        , GamePhase(..)
        , LogLine
        , LogTone(..)
        , NextLevelLink(..)
        , TrackerOffer(..)
        , TrackerState(..)
        , UncleGloat(..)
        , UncleOffer(..)
        , WinCapTier(..)
        , WinCapUpsell(..)
        , capTargetCents
        , extraTimeText
        , formatCents
        , formatClock
        , landedPercent
        , startingBalanceCents
        , timeLimitSeconds
        , trueEndingMessage
        , winCapUpsell
        )
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode
import Random
import ShopDialog exposing (ClickPoint, ShopFold(..))
import Time


{-| One coin on the table. The odds and the payout (as a percent of the
stake: 100 = 1:1, 3000 = 30x) are never shown to the player.
-}
type alias CoinConfig =
    { coinName : String
    , odds : CoinOdds
    , payoutPercent : Int
    }


{-| How a coin decides its landing. Independent coins roll their own
die; weather coins win exactly when the round's single hidden weather
roll goes their way, which makes a sunny and a rainy coin perfectly
anti-correlated: exactly one of them wins every round.
-}
type CoinOdds
    = IndependentPercent Int
    | WinsWhenSunny
    | WinsWhenRainy


{-| The hidden weather of one round, drawn once and shared by every
weather coin.
-}
type SkyState
    = Sunny
    | Rainy


{-| Whether the game ends by clock or by a fixed number of flips.
-}
type TurnBudget
    = TimeLimit Int
    | FlipLimit Int


{-| One purchasable batch of extra flips: how many, for how much.
-}
type alias ExtraTurnsPackage =
    { priceCents : Int
    , extraFlips : Int
    }


{-| Buying more turns: satire made purchasable. Only offered in
flip-limited games, repeatable. Several package sizes can be on offer
at once; the "bulk discounts" are part of the joke.
-}
type ExtraTurnsOffer
    = NoExtraTurns
    | ExtraTurnsForSale (List ExtraTurnsPackage)


{-| The out-of-flips rescue: once the flip budget runs dry, the game
over screen offers a single extra flip for a few dollars. Buying it
resumes play, which runs dry again one flip later, which offers again.
-}
type LastChanceTurnOffer
    = NoLastChanceTurn
    | LastChanceTurnForSale ExtraTurnsPackage


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


{-| The percentage allocator upgrades the stake inputs: amounts become
percentages of the live balance, re-sized automatically on every flip.
Price in cents.
-}
type AllocatorOffer
    = NoAllocator
    | AllocatorForSale Int


{-| How the stake inputs are read: dollar amounts, or (after buying the
allocator) percentages of the current balance.
-}
type AllocationMode
    = DollarAllocation
    | PercentAllocation


{-| The Elegant Universe, a book about strings: it reveals which coins
move together by tracking how often each pair lands the same way.
Price in cents.
-}
type CorrelationBookOffer
    = NoCorrelationBook
    | CorrelationBookForSale Int


type CorrelationBook
    = BookNotBought
    | BookBought


{-| The refund request: only for sale once the player has collected
every one of uncle's phrases, and asking naturally costs money too.
The reply is configurable because it is the whole joke.
-}
type RefundOffer
    = NoRefund
    | RefundForSale { priceCents : Int, reply : String }


type RefundState
    = RefundNotAsked
    | RefundAsked


{-| Same-outcome bookkeeping for one pair of coins, counting only
rounds where both were staked. Indices point into the coin list, whose
names never move (only profiles shuffle).
-}
type alias PairTally =
    { firstIndex : Int
    , secondIndex : Int
    , sameCount : Int
    , bothCount : Int
    }


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


{-| Whether the hired flip helpers are flipping or on a break. Pausing
stops their shared timer; the fleet stays hired and can resume.
-}
type HelperPause
    = HelpersFlipping
    | HelpersPaused


{-| The shop items a purchase dialog explains before any money moves.
-}
type ShopItemKind
    = TrackerItem
    | GlassesItem
    | AutoclickerItem
    | FlipHelperItem
    | AllocatorItem
    | BookItem
    | RefundItem
    | ExtraTurnsItem ExtraTurnsPackage
    | ExtraTimeItem ExtraTimePackage
    | UncleAdviceItem


{-| A purchase mid-consideration: clicking a shop item opens a dialog
explaining it, and only confirming actually buys. Repeatable items
(helpers, uncle) keep the dialog open after confirming, so a fleet can
be hired by mashing confirm; one-shot equipment closes it.
-}
type PendingPurchase
    = NoPendingPurchase
    | Considering ShopItemKind ClickPoint


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
    , weatherSunPercent : Int
    , turnBudget : TurnBudget
    , nextLevelLink : NextLevelLink
    , profileAssignment : ProfileAssignment
    , trackerOffer : TrackerOffer
    , uncleOffer : UncleOffer
    , glassesOffer : GlassesOffer
    , autoclickerOffer : AutoclickerOffer
    , flipHelperOffer : FlipHelperOffer
    , allocatorOffer : AllocatorOffer
    , bookOffer : CorrelationBookOffer
    , refundOffer : RefundOffer
    , extraTurnsOffer : ExtraTurnsOffer
    , extraTimeOffer : ExtraTimeOffer
    , lastChanceTurnOffer : LastChanceTurnOffer
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
    , winCapTier : WinCapTier
    , glasses : GoldenGlasses
    , allocationMode : AllocationMode
    , book : CorrelationBook
    , pairTallies : List PairTally
    , adviceHeard : List String
    , refund : RefundState
    , extraFlipsBought : Int
    , autoclicker : Autoclicker
    , flipHold : FlipHold
    , flipHelperCount : Int
    , helperPause : HelperPause
    , helperFlipCount : Int
    , nextHelperPriceCents : Int
    , shopFold : ShopFold
    , pendingPurchase : PendingPurchase
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
    | HelperPauseToggled
    | ShopToggled
    | RefundRequested
    | ExtraTurnsPurchased ExtraTurnsPackage
    | ExtraTimePurchased ExtraTimePackage
    | WinCapRaised
    | LastChanceTurnPurchased
    | PurchaseConsidered ShopItemKind ClickPoint
    | PurchaseConfirmed
    | PurchaseCancelled
    | DialogClicked
    | CoinsLanded { stakes : List Int, weatherRoll : Int, rolls : List Int }
    | ClockTicked
    | TrackerPurchased
    | GlassesPurchased
    | AllocatorPurchased
    | BookPurchased
    | AutoclickerPurchased
    | UncleAdviceRequested
    | UncleAdviceGiven String


gameProgram : MultiCoinConfig -> Program () Model Msg
gameProgram config =
    Browser.element
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
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


{-| Deal the (odds, payout) profiles across the coin names at random:
the names keep their display order, the profiles land on a random coin
each.
-}
shuffledCoinsGenerator : List CoinConfig -> Random.Generator (List CoinConfig)
shuffledCoinsGenerator coins =
    Random.map
        (\sortKeys ->
            List.map2
                (\coin profile ->
                    { coin | odds = profile.odds, payoutPercent = profile.payoutPercent }
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
    , secondsLeft =
        case config.turnBudget of
            TimeLimit seconds ->
                seconds

            FlipLimit _ ->
                0
    , roundCount = 0
    , tracker = TrackerNotBought
    , winCapTier = FirstCap
    , glasses = GlassesNotBought
    , allocationMode = DollarAllocation
    , book = BookNotBought
    , pairTallies = initialPairTallies (List.length config.coins)
    , adviceHeard = []
    , refund = RefundNotAsked
    , extraFlipsBought = 0
    , autoclicker = ClickerNotBought
    , flipHold = FlipReleased
    , flipHelperCount = 0
    , helperPause = HelpersFlipping
    , helperFlipCount = 0
    , nextHelperPriceCents = initialHelperPriceCents config.flipHelperOffer
    , shopFold = ShopCollapsed
    , pendingPurchase = NoPendingPurchase
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

        HelperPauseToggled ->
            ( { model | helperPause = toggleHelperPause model.helperPause }, Cmd.none )

        FlipHelpersTicked ->
            case model.helperPause of
                HelpersPaused ->
                    -- A tick that raced the pause: the fleet is on
                    -- break, nobody presses anything.
                    ( model, Cmd.none )

                HelpersFlipping ->
                    -- The counter keys the bird emoji's backflip
                    -- animation: a changed key recreates the span,
                    -- replaying the CSS animation once per helper
                    -- flip.
                    pressFlip config { model | helperFlipCount = model.helperFlipCount + 1 }

        ShopToggled ->
            ( { model | shopFold = toggleFold model.shopFold }, Cmd.none )

        RefundRequested ->
            ( requestRefund config.uncleOffer config.refundOffer model, Cmd.none )

        ExtraTurnsPurchased package ->
            ( purchaseExtraTurns config.extraTurnsOffer package model, Cmd.none )

        ExtraTimePurchased package ->
            ( purchaseExtraTime config.turnBudget config.extraTimeOffer package model, Cmd.none )

        WinCapRaised ->
            ( raiseWinCap config.uncleOffer model, Cmd.none )

        LastChanceTurnPurchased ->
            ( purchaseLastChanceTurn config.lastChanceTurnOffer model, Cmd.none )

        PurchaseConsidered itemKind clickPoint ->
            ( { model | pendingPurchase = Considering itemKind clickPoint }, Cmd.none )

        PurchaseConfirmed ->
            case model.pendingPurchase of
                NoPendingPurchase ->
                    ( model, Cmd.none )

                Considering TrackerItem _ ->
                    ( purchaseTracker config.trackerOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering GlassesItem _ ->
                    ( purchaseGlasses config.glassesOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering AutoclickerItem _ ->
                    ( purchaseAutoclicker config.autoclickerOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering FlipHelperItem _ ->
                    -- Stays open: fleets are hired by mashing confirm.
                    ( hireFlipHelper config.flipHelperOffer model, Cmd.none )

                Considering AllocatorItem _ ->
                    ( purchaseAllocator config.allocatorOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering BookItem _ ->
                    ( purchaseBook config.bookOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering (ExtraTurnsItem package) _ ->
                    -- Stays open: the house sells time by the armful.
                    ( purchaseExtraTurns config.extraTurnsOffer package model, Cmd.none )

                Considering (ExtraTimeItem package) _ ->
                    -- Stays open: the house sells time by the armful.
                    ( purchaseExtraTime config.turnBudget config.extraTimeOffer package model, Cmd.none )

                Considering RefundItem _ ->
                    ( requestRefund config.uncleOffer
                        config.refundOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering UncleAdviceItem _ ->
                    -- Stays open: uncle appreciates repeat customers.
                    purchaseUncleAdvice config.uncleOffer model

        PurchaseCancelled ->
            ( { model | pendingPurchase = NoPendingPurchase }, Cmd.none )

        DialogClicked ->
            -- A click inside the dialog, swallowed (stopPropagation) so
            -- the document-level dismiss listener never sees it.
            ( model, Cmd.none )

        CoinsLanded landedRound ->
            ( settleRound config landedRound model, Cmd.none )

        ClockTicked ->
            ( tickClock model, Cmd.none )

        TrackerPurchased ->
            ( purchaseTracker config.trackerOffer model, Cmd.none )

        GlassesPurchased ->
            ( purchaseGlasses config.glassesOffer model, Cmd.none )

        AllocatorPurchased ->
            ( purchaseAllocator config.allocatorOffer model, Cmd.none )

        BookPurchased ->
            ( purchaseBook config.bookOffer model, Cmd.none )

        AutoclickerPurchased ->
            ( purchaseAutoclicker config.autoclickerOffer model, Cmd.none )

        UncleAdviceRequested ->
            purchaseUncleAdvice config.uncleOffer model

        UncleAdviceGiven phrase ->
            ( gloatIfBusted config.uncleOffer
                (logLine NeutralTone
                    ("\u{1F9D3} Uncle: \u{201C}" ++ phrase ++ "\u{201D}")
                    (hearAdvice phrase model)
                )
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

            ReadableStakes parsedStakes ->
                let
                    stakes =
                        case model.allocationMode of
                            DollarAllocation ->
                                parsedStakes

                            PercentAllocation ->
                                List.map (percentStakeCents model.balanceCents) parsedStakes

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
                        (\( weatherRoll, rolls ) ->
                            CoinsLanded { stakes = stakes, weatherRoll = weatherRoll, rolls = rolls }
                        )
                        (Random.pair
                            (Random.int 1 100)
                            (Random.list (List.length model.coins) (Random.int 1 100))
                        )
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


{-| A percent-mode stake: the input parses to hundredths of a percent
("24.4" -> 2440), applied to the live balance, floored to whole cents
but never zero while a percentage was actually entered.
-}
percentStakeCents : Int -> Int -> Int
percentStakeCents balanceCents percentHundredths =
    if percentHundredths <= 0 then
        0

    else
        max 1 (balanceCents * percentHundredths // 10000)


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
    , landedSide : Maybe CoinSide
    , logLines : List LogLine
    }


resolveCoin : SkyState -> CoinConfig -> CoinTally -> Int -> Int -> CoinOutcome
resolveCoin sky coin tally stake roll =
    if stake <= 0 then
        { tally = tally, deltaCents = 0, landedSide = Nothing, logLines = [] }

    else
        let
            landed =
                case coin.odds of
                    IndependentPercent winPercent ->
                        if roll <= winPercent then
                            Heads

                        else
                            Tails

                    WinsWhenSunny ->
                        case sky of
                            Sunny ->
                                Heads

                            Rainy ->
                                Tails

                    WinsWhenRainy ->
                        case sky of
                            Sunny ->
                                Tails

                            Rainy ->
                                Heads
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
                , landedSide = Just Heads
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
                , landedSide = Just Tails
                , logLines =
                    [ { tone = LoseTone
                      , text =
                            coin.coinName
                                ++ " landed tails. You lost your $"
                                ++ formatCents stake
                                ++ " stake."
                      }
                    ]
                }


{-| Apply a landed round: pay out or collect per staked coin, update the
tallies, and check for the win/bust end states.
-}
settleRound : MultiCoinConfig -> { stakes : List Int, weatherRoll : Int, rolls : List Int } -> Model -> Model
settleRound config landedRound model =
    if model.phase /= Playing then
        model

    else
        let
            sky =
                if landedRound.weatherRoll <= config.weatherSunPercent then
                    Sunny

                else
                    Rainy

            outcomes =
                List.map4 (resolveCoin sky) model.coins model.tallies landedRound.stakes landedRound.rolls

            newBalance =
                max 0 (model.balanceCents + List.sum (List.map .deltaCents outcomes))

            -- Flip lines carry their round number, so lines from the
            -- same round can be told apart at a glance in the log.
            roundNumber =
                model.roundCount + 1

            -- The divider goes first so that after the List.reverse in
            -- the log update it ends up between this round's lines and
            -- the older entries.
            roundLogLines =
                { tone = DividerTone, text = "" }
                    :: List.map
                        (\line -> { line | text = String.fromInt roundNumber ++ ": " ++ line.text })
                        (List.concatMap .logLines outcomes)
        in
        gloatIfBusted config.uncleOffer
            (endWhenOutOfFlips config.turnBudget
                (checkEndState BustByBetting
                    { model
                        | balanceCents = newBalance
                        , tallies = List.map .tally outcomes
                        , pairTallies =
                            updatePairTallies (List.map .landedSide outcomes) model.pairTallies
                        , roundCount = model.roundCount + 1
                        , log = List.reverse roundLogLines ++ model.log
                    }
                )
            )


{-| Count same-outcome landings for every pair where both coins were
staked this round.
-}
updatePairTallies : List (Maybe CoinSide) -> List PairTally -> List PairTally
updatePairTallies landedSides pairTallies =
    List.map (updatePairTally landedSides) pairTallies


updatePairTally : List (Maybe CoinSide) -> PairTally -> PairTally
updatePairTally landedSides tally =
    case ( landedSideAt tally.firstIndex landedSides, landedSideAt tally.secondIndex landedSides ) of
        ( Just firstSide, Just secondSide ) ->
            { tally
                | bothCount = tally.bothCount + 1
                , sameCount =
                    if firstSide == secondSide then
                        tally.sameCount + 1

                    else
                        tally.sameCount
            }

        ( Just _, Nothing ) ->
            tally

        ( Nothing, Just _ ) ->
            tally

        ( Nothing, Nothing ) ->
            tally


landedSideAt : Int -> List (Maybe CoinSide) -> Maybe CoinSide
landedSideAt index landedSides =
    List.drop index landedSides
        |> List.head
        |> Maybe.andThen identity


{-| With a flip budget, the game ends once the last flip has settled.
Winning or busting on that final flip takes precedence: this only fires
while still playing.
-}
endWhenOutOfFlips : TurnBudget -> Model -> Model
endWhenOutOfFlips budget model =
    case budget of
        TimeLimit _ ->
            model

        FlipLimit flipLimit ->
            if model.phase == Playing && model.roundCount >= flipLimit + model.extraFlipsBought then
                { model | phase = RanOutOfTime }

            else
                model


checkEndState : BustCause -> Model -> Model
checkEndState causeIfBusted model =
    if model.balanceCents >= capTargetCents model.winCapTier then
        { model | phase = WonGame }

    else if model.balanceCents <= 0 then
        { model | phase = WentBust, bustCause = causeIfBusted }

    else
        model


{-| Once bankrupt, uncle pops into the log, if this level has an uncle
at all. Called after every event that could have busted the game (a
settled round, or uncle's own advice landing when the purchase took the
last dollars), so his gloat is always the newest log line, exactly
once. The bustCause guard makes the gloat the last-dollars easter
egg: only a bust caused by the advice purchase itself earns it, a
betting bust leaves uncle silent (no sane person buys themselves
bankrupt, hence the reward).
-}
gloatIfBusted : UncleOffer -> Model -> Model
gloatIfBusted offer model =
    case offer of
        NoUncleAdvice ->
            model

        UncleAdviceForSale _ ->
            if model.phase == WentBust && model.uncleGloat == UncleHasNotGloated && model.bustCause == BustByUncleAdvice then
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


{-| Every unordered pair of coin indices, tallies at zero.
-}
initialPairTallies : Int -> List PairTally
initialPairTallies coinCount =
    List.concatMap
        (\firstIndex ->
            List.map
                (\secondIndex ->
                    { firstIndex = firstIndex
                    , secondIndex = secondIndex
                    , sameCount = 0
                    , bothCount = 0
                    }
                )
                (List.range (firstIndex + 1) (coinCount - 1))
        )
        (List.range 0 (coinCount - 1))


{-| Buy the percentage allocator. Same wipe-out guard as the tracker.
-}
purchaseAllocator : AllocatorOffer -> Model -> Model
purchaseAllocator offer model =
    case ( offer, model.allocationMode ) of
        ( NoAllocator, DollarAllocation ) ->
            model

        ( NoAllocator, PercentAllocation ) ->
            model

        ( AllocatorForSale _, PercentAllocation ) ->
            model

        ( AllocatorForSale priceCents, DollarAllocation ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= priceCents then
                logLine NeutralTone "You cannot afford the percentage allocator." model

            else
                logLine NeutralTone
                    ("Bought the percentage allocator for $"
                        ++ formatCents priceCents
                        ++ ". Stakes are now percentages of your balance, re-sized every flip."
                    )
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , allocationMode = PercentAllocation
                    }


{-| Buy more flips. Repeatable: the house is always happy to extend
your opportunity to give it money. Only a package actually on offer is
honoured; a message carrying any other package does nothing.
-}
purchaseExtraTurns : ExtraTurnsOffer -> ExtraTurnsPackage -> Model -> Model
purchaseExtraTurns offer package model =
    case offer of
        NoExtraTurns ->
            model

        ExtraTurnsForSale packages ->
            if not (List.member package packages) then
                model

            else if model.phase /= Playing then
                model

            else if model.balanceCents <= package.priceCents then
                logLine NeutralTone "You cannot afford more flips." model

            else
                logLine NeutralTone
                    ("Bought "
                        ++ String.fromInt package.extraFlips
                        ++ " more flips for $"
                        ++ formatCents package.priceCents
                        ++ ". Money can buy time after all."
                    )
                    { model
                        | balanceCents = model.balanceCents - package.priceCents
                        , extraFlipsBought = model.extraFlipsBought + package.extraFlips
                    }


{-| Buy more time on the clock. The time-limited twin of the extra
flips: repeatable, only meaningful under a time budget (a flip-limited
game has no clock to extend, so the purchase is refused there), and
only a package actually on offer is honoured.
-}
purchaseExtraTime : TurnBudget -> ExtraTimeOffer -> ExtraTimePackage -> Model -> Model
purchaseExtraTime budget offer package model =
    case ( budget, offer ) of
        ( FlipLimit _, NoExtraTime ) ->
            model

        ( FlipLimit _, ExtraTimeForSale _ ) ->
            model

        ( TimeLimit _, NoExtraTime ) ->
            model

        ( TimeLimit _, ExtraTimeForSale packages ) ->
            if not (List.member package packages) then
                model

            else if model.phase /= Playing then
                model

            else if model.balanceCents <= package.priceCents then
                logLine NeutralTone "You cannot afford more time." model

            else
                logLine NeutralTone
                    ("Bought "
                        ++ extraTimeText package.extraSeconds
                        ++ " for $"
                        ++ formatCents package.priceCents
                        ++ ". Time is money, apparently in that order."
                    )
                    { model
                        | balanceCents = model.balanceCents - package.priceCents
                        , secondsLeft = model.secondsLeft + package.extraSeconds
                    }


{-| Pay to raise the winning target and play on. Only offered on the
win screen; the winning balance always covers the price, but the guard
stays so a stray message can never charge into bankruptcy. The end
state is re-checked right away: a win that overshot the raised target
too (a 30x swan can) wins again on the spot instead of leaving a
"playing" game already past its target. The bust cause is a formality,
the guarded balance can never be zero here. The flip hold is released:
the flip button vanished mid-hold when the win screen appeared, so no
mouse-up ever landed, and resuming must not auto-fire a stale hold.
-}
raiseWinCap : UncleOffer -> Model -> Model
raiseWinCap uncleOffer model =
    if model.phase /= WonGame then
        model

    else
        case winCapUpsell uncleOffer model.winCapTier of
            NoFurtherUpsell ->
                model

            WinCapUpsellFor upsell ->
                if model.balanceCents <= upsell.priceCents then
                    model

                else
                    checkEndState BustByBetting
                        (logLine NeutralTone
                            ("Paid $"
                                ++ formatCents upsell.priceCents
                                ++ " to raise the target to $"
                                ++ String.fromInt (capTargetCents upsell.raisedTier // 100)
                                ++ ". Winning wasn't enough."
                            )
                            { model
                                | balanceCents = model.balanceCents - upsell.priceCents
                                , winCapTier = upsell.raisedTier
                                , phase = Playing
                                , flipHold = FlipReleased
                            }
                        )


{-| The out-of-flips rescue purchase. Revives the game: the flip
budget grows by the package and play resumes, only to run dry again
shortly after, when the same offer reappears. The house calls this
customer retention. The flip hold is released like on a cap raise: the
budget usually runs dry mid-hold, and the rescued flip must be the
player's own press, not a stale hold's.
-}
purchaseLastChanceTurn : LastChanceTurnOffer -> Model -> Model
purchaseLastChanceTurn offer model =
    case offer of
        NoLastChanceTurn ->
            model

        LastChanceTurnForSale package ->
            if model.phase /= RanOutOfTime then
                model

            else if model.balanceCents <= package.priceCents then
                model

            else
                logLine NeutralTone
                    ("Bought "
                        ++ flipCountText package.extraFlips
                        ++ " for $"
                        ++ formatCents package.priceCents
                        ++ ". Just this one, then you'll stop, right?"
                    )
                    { model
                        | balanceCents = model.balanceCents - package.priceCents
                        , extraFlipsBought = model.extraFlipsBought + package.extraFlips
                        , phase = Playing
                        , flipHold = FlipReleased
                    }


{-| "1 more flip" versus "10 more flips".
-}
flipCountText : Int -> String
flipCountText flipCount =
    if flipCount == 1 then
        "1 more flip"

    else
        String.fromInt flipCount ++ " more flips"


{-| Buy The Elegant Universe. Same wipe-out guard as the tracker.
-}
purchaseBook : CorrelationBookOffer -> Model -> Model
purchaseBook offer model =
    case ( offer, model.book ) of
        ( NoCorrelationBook, BookNotBought ) ->
            model

        ( NoCorrelationBook, BookBought ) ->
            model

        ( CorrelationBookForSale _, BookBought ) ->
            model

        ( CorrelationBookForSale priceCents, BookNotBought ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents <= priceCents then
                logLine NeutralTone "You cannot afford The Elegant Universe." model

            else
                logLine NeutralTone
                    ("Bought The Elegant Universe for $"
                        ++ formatCents priceCents
                        ++ ". It tracks how often each pair of coins lands the same way."
                    )
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , book = BookBought
                    }


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


toggleFold : ShopFold -> ShopFold
toggleFold fold =
    case fold of
        ShopCollapsed ->
            ShopExpanded

        ShopExpanded ->
            ShopCollapsed


{-| Remember each distinct phrase, so the refund request can unlock
once uncle's complete works have been collected.
-}
hearAdvice : String -> Model -> Model
hearAdvice phrase model =
    if List.member phrase model.adviceHeard then
        model

    else
        { model | adviceHeard = phrase :: model.adviceHeard }


{-| Whether every one of uncle's phrases has been heard at least once.
-}
allAdviceHeard : UncleOffer -> Model -> Bool
allAdviceHeard offer model =
    case offer of
        NoUncleAdvice ->
            False

        UncleAdviceForSale uncle ->
            List.length model.adviceHeard >= 1 + List.length uncle.morePhrases


{-| Ask uncle for the money back. Asking costs money too, uncle can
take your last dollars doing it (he is family, after all), and the
configured reply is the only thing you get. No async advice is in
flight here, so the gloat lands right away when asking was the final
straw.
-}
requestRefund : UncleOffer -> RefundOffer -> Model -> Model
requestRefund uncleOffer offer model =
    case ( offer, model.refund ) of
        ( NoRefund, RefundNotAsked ) ->
            model

        ( NoRefund, RefundAsked ) ->
            model

        ( RefundForSale _, RefundAsked ) ->
            model

        ( RefundForSale refund, RefundNotAsked ) ->
            if model.phase /= Playing then
                model

            else if model.balanceCents < refund.priceCents then
                logLine NeutralTone "You cannot afford to ask uncle for your money back." model

            else
                gloatIfBusted uncleOffer
                    (logLine NeutralTone ("\u{1F9D3} Uncle: \u{201C}" ++ refund.reply ++ "\u{201D}")
                        (checkEndState BustByUncleAdvice
                            { model
                                | balanceCents = model.balanceCents - refund.priceCents
                                , refund = RefundAsked
                            }
                        )
                    )


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


subscriptions : MultiCoinConfig -> Model -> Sub Msg
subscriptions config model =
    Sub.batch
        [ clockSubscription config model
        , autoclickerSubscription model
        , flipHelperSubscription model
        , dialogDismissSubscription model
        ]


{-| While a purchase dialog is open, any document-level click closes it.
Clicks inside the dialog and on the shop items stop propagation, so
they never reach this listener: only genuinely-outside clicks dismiss.
-}
dialogDismissSubscription : Model -> Sub Msg
dialogDismissSubscription model =
    case model.pendingPurchase of
        NoPendingPurchase ->
            Sub.none

        Considering _ _ ->
            Browser.Events.onClick (Decode.succeed PurchaseCancelled)


{-| Each hired helper flips once per five seconds, staggered: N helpers
share one timer at 5000/N ms, which is N evenly spread flips per five
seconds. A paused fleet has no timer at all.
-}
flipHelperSubscription : Model -> Sub Msg
flipHelperSubscription model =
    case model.phase of
        Playing ->
            case model.helperPause of
                HelpersPaused ->
                    Sub.none

                HelpersFlipping ->
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


clockSubscription : MultiCoinConfig -> Model -> Sub Msg
clockSubscription config model =
    case config.turnBudget of
        FlipLimit _ ->
            Sub.none

        TimeLimit _ ->
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
              , viewStats config model
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


viewStats : MultiCoinConfig -> Model -> Html Msg
viewStats config model =
    Html.div [ Html.Attributes.class "stats" ]
        [ Html.div [] [ Html.text ("Target: $" ++ String.fromInt (capTargetCents model.winCapTier // 100)) ]
        , case config.turnBudget of
            TimeLimit _ ->
                Html.div []
                    [ Html.text "Time left: "
                    , Html.span [ Html.Attributes.class "timer" ]
                        [ Html.text (formatClock model.secondsLeft) ]
                    ]

            FlipLimit flipLimit ->
                Html.div []
                    [ Html.text "Flips left: "
                    , Html.span [ Html.Attributes.class "timer" ]
                        [ Html.text
                            (String.fromInt
                                (max 0 (flipLimit + model.extraFlipsBought - model.roundCount))
                            )
                        ]
                    ]
        ]


viewProgressBar : Model -> Html Msg
viewProgressBar model =
    let
        progressPercent =
            min 100 (100 * toFloat model.balanceCents / toFloat (capTargetCents model.winCapTier))
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
            [ List.indexedMap (viewCoinRow model.allocationMode)
                (List.map2 Tuple.pair model.coins model.stakeInputs)
            , [ Html.button
                    (Html.Attributes.class "flip-button"
                        :: Html.Events.onClick FlipPressed
                        :: flipHoldEvents model.autoclicker
                    )
                    [ Html.text "FLIP" ]
              ]
            , viewGlassesPayouts model
            , viewCorrelations model
            , viewTally config model
            , viewFlipHelpers model
            , viewShop config model
            ]
        )


toggleHelperPause : HelperPause -> HelperPause
toggleHelperPause pause =
    case pause of
        HelpersFlipping ->
            HelpersPaused

        HelpersPaused ->
            HelpersFlipping


{-| How many helpers are flipping for the player, shown once any are
hired, with a button to pause and resume the whole fleet.
-}
viewFlipHelpers : Model -> List (Html Msg)
viewFlipHelpers model =
    if model.flipHelperCount == 0 then
        []

    else
        [ Html.div [ Html.Attributes.class "helpers" ]
            [ Html.Keyed.node "span"
                []
                [ ( String.fromInt model.helperFlipCount
                  , Html.span [ Html.Attributes.class "helper-bird" ] [ Html.text "\u{1F424}" ]
                  )
                ]
            , Html.text (" Flip helpers: " ++ String.fromInt model.flipHelperCount ++ " ")
            , Html.button
                [ Html.Attributes.class "helper-pause"
                , Html.Events.onClick HelperPauseToggled
                ]
                [ Html.text
                    (case model.helperPause of
                        HelpersFlipping ->
                            "pause"

                        HelpersPaused ->
                            "resume"
                    )
                ]
            ]
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


viewCoinRow : AllocationMode -> Int -> ( CoinConfig, String ) -> Html Msg
viewCoinRow mode coinIndex ( coin, stakeInput ) =
    Html.div [ Html.Attributes.class "coin-bet" ]
        [ Html.label []
            [ Html.text
                (coin.coinName
                    ++ (case mode of
                            DollarAllocation ->
                                " $:"

                            PercentAllocation ->
                                " %:"
                       )
                )
            ]
        , Html.input
            [ Html.Attributes.class "bet-amount"
            , Html.Attributes.type_ "number"
            , Html.Attributes.step "0.01"

            -- Zero is a real value here, it means "not playing this
            -- coin", so the stepper must be able to reach it.
            , Html.Attributes.min "0"
            , Html.Attributes.value stakeInput
            , Html.Events.onInput (StakeInputChanged coinIndex)
            ]
            []
        ]


{-| The Elegant Universe's readings: how often each pair of coins
landed the same way, over the rounds both were staked. Only rendered
once the book is bought.
-}
viewCorrelations : Model -> List (Html Msg)
viewCorrelations model =
    case model.book of
        BookNotBought ->
            []

        BookBought ->
            [ Html.div [ Html.Attributes.class "correlations" ]
                [ Html.text
                    ("\u{1F4D6} Same landing: "
                        ++ String.join " \u{00B7} "
                            (List.map (pairTallyText model.coins) model.pairTallies)
                    )
                ]
            ]


pairTallyText : List CoinConfig -> PairTally -> String
pairTallyText coins tally =
    coinNameAt tally.firstIndex coins
        ++ "+"
        ++ coinNameAt tally.secondIndex coins
        ++ " "
        ++ String.fromInt tally.sameCount
        ++ "/"
        ++ String.fromInt tally.bothCount


{-| A coin's display name by position. The fallback is unreachable:
pair tallies are built from the same list's length at init and names
never move.
-}
coinNameAt : Int -> List CoinConfig -> String
coinNameAt index coins =
    List.drop index coins
        |> List.head
        |> Maybe.map .coinName
        |> Maybe.withDefault "?"


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
    let
        upgradeItems =
            viewAutoclickerShopItem config.autoclickerOffer model
                ++ viewFlipHelperShopItem config.flipHelperOffer model
                ++ viewAllocatorShopItem config.allocatorOffer model
                ++ viewExtraTurnsShopItem config
                ++ viewExtraTimeShopItem config

        intelItems =
            viewTrackerShopItem config.trackerOffer model
                ++ viewGlassesShopItem config.glassesOffer model
                ++ viewBookShopItem config.bookOffer model
                ++ viewUncleShopItem config.uncleOffer
                ++ viewRefundShopItem config model
    in
    case upgradeItems ++ intelItems of
        [] ->
            []

        _ ->
            [ Html.div [ Html.Attributes.class "shop" ]
                (viewShopToggle model.shopFold
                    :: (case model.shopFold of
                            ShopCollapsed ->
                                []

                            ShopExpanded ->
                                List.concat
                                    [ viewShopGroup "Upgrades" upgradeItems
                                    , viewShopGroup "Intel" intelItems
                                    , viewPurchaseDialog config model
                                    ]
                       )
                )
            ]


{-| A shop section: a plain heading with its items, hidden entirely
when the section has nothing for sale.
-}
viewShopGroup : String -> List (Html Msg) -> List (Html Msg)
viewShopGroup label groupItems =
    case groupItems of
        [] ->
            []

        _ ->
            ShopDialog.viewShopGroupHeading label :: groupItems


{-| The confirm/cancel dialog a considered purchase opens, explaining
what the item actually does before any money moves. The dialog is
fixed-positioned so the confirm button (a known 120x42 box, first in
the dialog, inside the 0.6em padding and 1px border) renders centered
under the click that opened it: buying again is a click without moving
the mouse.
-}
viewPurchaseDialog : MultiCoinConfig -> Model -> List (Html Msg)
viewPurchaseDialog config model =
    case model.pendingPurchase of
        NoPendingPurchase ->
            []

        Considering itemKind clickPoint ->
            [ ShopDialog.viewPurchaseDialog
                { clickPoint = clickPoint
                , question =
                    "Buy "
                        ++ itemDisplayName itemKind
                        ++ " for $"
                        ++ formatCents (itemPriceCents config model itemKind)
                        ++ "?"
                , explanation = itemExplanation itemKind
                , onConfirm = PurchaseConfirmed
                , onCancel = PurchaseCancelled
                , onInsideClick = DialogClicked
                }
            ]


{-| The item's name with its article, ready for "Buy ... for $x?".
-}
itemDisplayName : ShopItemKind -> String
itemDisplayName itemKind =
    case itemKind of
        TrackerItem ->
            "the ratio tracker"

        GlassesItem ->
            "the golden glasses"

        AutoclickerItem ->
            "the autoclicker"

        FlipHelperItem ->
            "a flip helper"

        AllocatorItem ->
            "the percentage allocator"

        BookItem ->
            "The Elegant Universe"

        RefundItem ->
            "your money back from uncle"

        ExtraTurnsItem package ->
            flipCountText package.extraFlips

        ExtraTimeItem package ->
            extraTimeText package.extraSeconds

        UncleAdviceItem ->
            "uncle's advice"


itemExplanation : ShopItemKind -> String
itemExplanation itemKind =
    case itemKind of
        TrackerItem ->
            "Counts how often each coin landed heads across your flips, so you don't have to tally the log by hand."

        GlassesItem ->
            "Reveal what each coin pays out on a win, right under the flip button. The win chances stay hidden."

        AutoclickerItem ->
            "Hold the flip button down and it presses it for you, ten times a second."

        FlipHelperItem ->
            "A tireless helper who presses flip for you once every five seconds. Every next hire asks 10% more."

        AllocatorItem ->
            "Your stakes become percentages of your balance instead of dollars, re-sized automatically on every flip."

        BookItem ->
            "A book about strings. It tracks how often each pair of coins lands the same way, revealing which ones are attached."

        RefundItem ->
            "After all that terrible advice, surely a refund is in order. Asking costs money, he is a busy man."

        ExtraTurnsItem _ ->
            "Out of flips? Nonsense. The house happily extends your opportunity to give it money."

        ExtraTimeItem _ ->
            "Clock running low? Nonsense. The house happily extends your opportunity to give it money."

        UncleAdviceItem ->
            "Uncle's advice speaks for itself. He's your uncle, surely he knows best."


{-| The price the dialog quotes. Zero for an absent offer, which is
never read: the dialog only opens from a shop button that an absent
offer never renders.
-}
itemPriceCents : MultiCoinConfig -> Model -> ShopItemKind -> Int
itemPriceCents config model itemKind =
    case itemKind of
        TrackerItem ->
            case config.trackerOffer of
                NoTrackerForSale ->
                    0

                TrackerForSale priceCents ->
                    priceCents

        GlassesItem ->
            case config.glassesOffer of
                NoGoldenGlasses ->
                    0

                GoldenGlassesForSale priceCents ->
                    priceCents

        AutoclickerItem ->
            case config.autoclickerOffer of
                NoAutoclicker ->
                    0

                AutoclickerForSale priceCents ->
                    priceCents

        FlipHelperItem ->
            model.nextHelperPriceCents

        AllocatorItem ->
            case config.allocatorOffer of
                NoAllocator ->
                    0

                AllocatorForSale priceCents ->
                    priceCents

        BookItem ->
            case config.bookOffer of
                NoCorrelationBook ->
                    0

                CorrelationBookForSale priceCents ->
                    priceCents

        RefundItem ->
            case config.refundOffer of
                NoRefund ->
                    0

                RefundForSale refund ->
                    refund.priceCents

        ExtraTurnsItem package ->
            package.priceCents

        ExtraTimeItem package ->
            package.priceCents

        UncleAdviceItem ->
            case config.uncleOffer of
                NoUncleAdvice ->
                    0

                UncleAdviceForSale uncle ->
                    uncle.priceCents


viewShopToggle : ShopFold -> Html Msg
viewShopToggle fold =
    ShopDialog.viewShopToggle ShopToggled fold


viewShopItem : (ClickPoint -> Msg) -> String -> Int -> Html Msg
viewShopItem onBuyAt itemName priceCents =
    ShopDialog.viewShopItem onBuyAt itemName ("$" ++ formatCents priceCents)


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
            [ viewShopItem (PurchaseConsidered TrackerItem) "Buy ratio tracker" priceCents ]


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
            [ viewShopItem (PurchaseConsidered GlassesItem) "Buy golden glasses" priceCents ]


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
            [ viewShopItem (PurchaseConsidered AutoclickerItem) "Buy autoclicker" priceCents ]


{-| Unlike the one-shot equipment, helpers stay for sale forever: the
price just keeps climbing.
-}
viewFlipHelperShopItem : FlipHelperOffer -> Model -> List (Html Msg)
viewFlipHelperShopItem offer model =
    case offer of
        NoFlipHelpers ->
            []

        FlipHelpersForSale _ ->
            [ viewShopItem (PurchaseConsidered FlipHelperItem) "Hire flip helper" model.nextHelperPriceCents ]


viewAllocatorShopItem : AllocatorOffer -> Model -> List (Html Msg)
viewAllocatorShopItem offer model =
    case ( offer, model.allocationMode ) of
        ( NoAllocator, DollarAllocation ) ->
            []

        ( NoAllocator, PercentAllocation ) ->
            []

        ( AllocatorForSale _, PercentAllocation ) ->
            []

        ( AllocatorForSale priceCents, DollarAllocation ) ->
            [ viewShopItem (PurchaseConsidered AllocatorItem) "Buy percentage allocator" priceCents ]


viewBookShopItem : CorrelationBookOffer -> Model -> List (Html Msg)
viewBookShopItem offer model =
    case ( offer, model.book ) of
        ( NoCorrelationBook, BookNotBought ) ->
            []

        ( NoCorrelationBook, BookBought ) ->
            []

        ( CorrelationBookForSale _, BookBought ) ->
            []

        ( CorrelationBookForSale priceCents, BookNotBought ) ->
            [ viewShopItem (PurchaseConsidered BookItem) "Buy The Elegant Universe" priceCents ]


viewUncleShopItem : UncleOffer -> List (Html Msg)
viewUncleShopItem offer =
    case offer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            [ viewShopItem (PurchaseConsidered UncleAdviceItem) "Ask uncle for advice" uncle.priceCents ]


{-| More flips, for sale forever, but only where a flip limit exists to
extend: under a time limit the item would be nonsense.
-}
viewExtraTurnsShopItem : MultiCoinConfig -> List (Html Msg)
viewExtraTurnsShopItem config =
    case ( config.extraTurnsOffer, config.turnBudget ) of
        ( NoExtraTurns, TimeLimit _ ) ->
            []

        ( NoExtraTurns, FlipLimit _ ) ->
            []

        ( ExtraTurnsForSale _, TimeLimit _ ) ->
            []

        ( ExtraTurnsForSale packages, FlipLimit _ ) ->
            List.map
                (\package ->
                    viewShopItem (PurchaseConsidered (ExtraTurnsItem package))
                        ("Buy " ++ flipCountText package.extraFlips)
                        package.priceCents
                )
                packages


{-| More clock time, for sale forever, but only where a clock exists to
extend: under a flip limit the item would be nonsense.
-}
viewExtraTimeShopItem : MultiCoinConfig -> List (Html Msg)
viewExtraTimeShopItem config =
    case ( config.extraTimeOffer, config.turnBudget ) of
        ( NoExtraTime, TimeLimit _ ) ->
            []

        ( NoExtraTime, FlipLimit _ ) ->
            []

        ( ExtraTimeForSale _, FlipLimit _ ) ->
            []

        ( ExtraTimeForSale packages, TimeLimit _ ) ->
            List.map
                (\package ->
                    viewShopItem (PurchaseConsidered (ExtraTimeItem package))
                        ("Buy " ++ extraTimeText package.extraSeconds)
                        package.priceCents
                )
                packages


{-| Only for sale once uncle's complete works have been heard, and only
once: uncle does not do repeat refund conversations.
-}
viewRefundShopItem : MultiCoinConfig -> Model -> List (Html Msg)
viewRefundShopItem config model =
    case ( config.refundOffer, model.refund ) of
        ( NoRefund, RefundNotAsked ) ->
            []

        ( NoRefund, RefundAsked ) ->
            []

        ( RefundForSale _, RefundAsked ) ->
            []

        ( RefundForSale refund, RefundNotAsked ) ->
            if allAdviceHeard config.uncleOffer model then
                [ viewShopItem (PurchaseConsidered RefundItem) "Ask your money back" refund.priceCents ]

            else
                []


viewGameOver : MultiCoinConfig -> Model -> Html Msg
viewGameOver config model =
    let
        ( message, tone ) =
            gameOverMessage config model
    in
    Html.div
        [ Html.Attributes.class ("game-over " ++ CoinFlipGame.toneClass tone) ]
        (List.concat
            [ [ Html.text message ]
            , viewNextLevelLink config.nextLevelLink model
            , viewWinCapUpsellButton config.uncleOffer model
            , viewLastChanceTurnButton config model
            , viewUncleBustCallout model
            , CoinFlipGame.viewExplanationHint model.phase
            , [ Html.div []
                    [ Html.text "It took you exactly "
                    , Html.strong [] [ Html.text (String.fromInt model.roundCount) ]
                    , Html.text " presses to get here."
                    ]
              ]
            , viewUncleSpend config.uncleOffer model
            ]
        )


{-| The win screen's "keep going" upsell: pay to raise the target
tenfold and resume playing. Gone once the degenerate cap has been won,
because there is genuinely nothing left to sell.
-}
viewWinCapUpsellButton : UncleOffer -> Model -> List (Html Msg)
viewWinCapUpsellButton uncleOffer model =
    if model.phase /= WonGame then
        []

    else
        case winCapUpsell uncleOffer model.winCapTier of
            NoFurtherUpsell ->
                []

            WinCapUpsellFor upsell ->
                [ Html.div []
                    [ Html.button
                        [ Html.Attributes.class "win-cap-upsell"
                        , Html.Events.onClick WinCapRaised
                        ]
                        [ Html.text (upsell.label ++ " ($" ++ formatCents upsell.priceCents ++ ")") ]
                    ]
                ]


{-| The out-of-flips rescue button. Only on the out-of-flips screen of
a flip-limited game, and only while the price can actually be paid:
the offer quietly disappears once the player cannot afford it, like
all the best predatory upsells.
-}
viewLastChanceTurnButton : MultiCoinConfig -> Model -> List (Html Msg)
viewLastChanceTurnButton config model =
    case ( config.lastChanceTurnOffer, config.turnBudget ) of
        ( NoLastChanceTurn, TimeLimit _ ) ->
            []

        ( NoLastChanceTurn, FlipLimit _ ) ->
            []

        ( LastChanceTurnForSale _, TimeLimit _ ) ->
            []

        ( LastChanceTurnForSale package, FlipLimit _ ) ->
            if model.phase == RanOutOfTime && model.balanceCents > package.priceCents then
                [ Html.div []
                    [ Html.button
                        [ Html.Attributes.class "last-chance-turn"
                        , Html.Events.onClick LastChanceTurnPurchased
                        ]
                        [ Html.text
                            ("Buy "
                                ++ flipCountText package.extraFlips
                                ++ " ($"
                                ++ formatCents package.priceCents
                                ++ ")"
                            )
                        ]
                    ]
                ]

            else
                []


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


gameOverMessage : MultiCoinConfig -> Model -> ( String, LogTone )
gameOverMessage config model =
    case model.phase of
        Playing ->
            ( "", NeutralTone )

        WonGame ->
            case model.winCapTier of
                FirstCap ->
                    ( "\u{1F389} YOU WIN! You reached $" ++ formatCents model.balanceCents ++ "!", WinTone )

                SecondCap ->
                    ( "\u{1F389} YOU WIN! You reached $" ++ formatCents model.balanceCents ++ "!", WinTone )

                DegenerateCap ->
                    ( trueEndingMessage, WinTone )

        WentBust ->
            ( "\u{1F480} REKT! You hit $0.00. Bankrupt.", LoseTone )

        RanOutOfTime ->
            case config.turnBudget of
                TimeLimit _ ->
                    ( "Time's up! You failed to reach the target.", LoseTone )

                FlipLimit _ ->
                    ( "Out of flips! You failed to reach the target.", LoseTone )


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

        DividerTone ->
            Html.div [ Html.Attributes.class "log-divider" ] []
