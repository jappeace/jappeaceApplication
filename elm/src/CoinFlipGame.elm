module CoinFlipGame exposing
    ( Autoclicker(..)
    , AutoclickerOffer(..)
    , BetHold(..)
    , BiasState(..)
    , ClockState(..)
    , CoinBias(..)
    , CoinSide(..)
    , ExtraTimeOffer(..)
    , ExtraTimePackage
    , GamePhase(..)
    , LevelConfig
    , LogLine
    , LogTone(..)
    , Model
    , Msg(..)
    , BustEnding(..)
    , NextLevelLink(..)
    , PendingPurchase(..)
    , ShopItemKind(..)
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
    , gameProgram
    , initialModel
    , landedPercent
    , oppositeSide
    , parseBetCents
    , quickBetCents
    , startingBalanceCents
    , targetBalanceCents
    , timeLimitSeconds
    , toneClass
    , trueEndingMessage
    , update
    , view
    , viewExplanationHint
    , winCapUpsell
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
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Random
import ShopDialog exposing (ClickPoint, ShopFold(..))
import Time


type CoinSide
    = Heads
    | Tails


{-| How the coin is rigged for a level.

  - `KnownHeadsPercent`: the bias is fixed and printed on the bet buttons.
  - `HiddenRandomBias`: the favored side is drawn fifty-fifty and its win
    percent uniformly from the inclusive range, both at game start, and
    neither is ever shown, not even on the end screen.

-}
type CoinBias
    = KnownHeadsPercent Int
    | HiddenRandomBias { minPercent : Int, maxPercent : Int }


type TrackerOffer
    = NoTrackerForSale
    | TrackerForSale Int


{-| The autoclicker spares the player's mouse finger: once bought,
holding a bet or flip button down fires a press every 100ms. Price in
cents. Shared with the multi-coin engine.
-}
type AutoclickerOffer
    = NoAutoclicker
    | AutoclickerForSale Int


type Autoclicker
    = ClickerNotBought
    | ClickerBought


{-| Which bet button is currently held down (mouse or touch), if any.
Only meaningful with a bought autoclicker: the auto-bet subscription
runs while a side is held.
-}
type BetHold
    = NoBetHeld
    | BetHeld CoinSide


{-| One purchasable batch of extra clock time: how many seconds, for how
much. The time-limited twin of the flip-limited games' extra flips.
-}
type alias ExtraTimePackage =
    { priceCents : Int
    , extraSeconds : Int
    }


{-| Buying more time on the clock: satire made purchasable, repeatable,
and deliberately overpriced. Several package sizes can be on offer at
once.
-}
type ExtraTimeOffer
    = NoExtraTime
    | ExtraTimeForSale (List ExtraTimePackage)


{-| How high the winning target currently sits. Every game starts at
$999; the win screen sells raising it to $9,999 and, after winning
that, once more to $99,999. There is nothing beyond the degenerate cap:
winning it is the true ending.
-}
type WinCapTier
    = FirstCap
    | SecondCap
    | DegenerateCap


{-| What the win screen offers a freshly minted winner: pay up and play
on toward a ten-times-higher target, or nothing once the final cap has
been won.
-}
type WinCapUpsell
    = NoFurtherUpsell
    | WinCapUpsellFor { priceCents : Int, label : String, raisedTier : WinCapTier }


{-| Uncle sells terrible gambling advice, one random phrase per purchase.
The phrase list is split head/tail so an empty list cannot be configured.
-}
type UncleOffer
    = NoUncleAdvice
    | UncleAdviceForSale { priceCents : Int, firstPhrase : String, morePhrases : List String }


{-| The shop items the purchase dialog explains before any money moves.
-}
type ShopItemKind
    = TrackerItem
    | AutoclickerItem
    | ExtraTimeItem ExtraTimePackage
    | UncleAdviceItem


{-| A purchase mid-consideration: clicking a shop item opens a dialog
explaining it, and only confirming actually buys. Uncle's advice keeps
the dialog open after confirming (repeat customers welcome); the
tracker closes it.
-}
type PendingPurchase
    = NoPendingPurchase
    | Considering ShopItemKind ClickPoint


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
    , autoclickerOffer : AutoclickerOffer
    , extraTimeOffer : ExtraTimeOffer
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
    | DividerTone


{-| Whether uncle already dropped his "proud of you" line. He gloats
exactly once, and only after his final piece of advice has landed in
the log, so the gloat stays the newest line.
-}
type UncleGloat
    = UncleHasNotGloated
    | UncleHasGloated


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
    , winCapTier : WinCapTier
    , autoclicker : Autoclicker
    , betHold : BetHold
    , shopFold : ShopFold
    , pendingPurchase : PendingPurchase
    , uncleAdviceCount : Int
    , uncleGloat : UncleGloat
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
    | AutoclickerPurchased
    | BetHoldStarted CoinSide
    | BetHoldEnded
    | AutoclickerTicked
    | ExtraTimePurchased ExtraTimePackage
    | WinCapRaised
    | ShopToggled
    | PurchaseConsidered ShopItemKind ClickPoint
    | PurchaseConfirmed
    | PurchaseCancelled
    | DialogClicked
    | UncleAdviceRequested
    | UncleAdviceGiven String


startingBalanceCents : Int
startingBalanceCents = 2500


{-| The first cap's target, the one every game starts chasing. The
higher caps only exist through `capTargetCents`.
-}
targetBalanceCents : Int
targetBalanceCents = 99900


{-| The balance a game at this cap must reach to win: $999, $9,999, or
$99,999.
-}
capTargetCents : WinCapTier -> Int
capTargetCents tier =
    case tier of
        FirstCap ->
            targetBalanceCents

        SecondCap ->
            999900

        DegenerateCap ->
            9999900


-- Decision: the win-cap upsell is engine-behaviour, not a per-level
-- config offer like the shop items: every level's win screen sells the
-- same two escalations at the same prices, so a config knob would only
-- be one more field every level must fill in identically. The ladder
-- lives here (shared) because both engines render it.
{-| What raising the cap from here costs, and what the button says.
$500 out of a $999 win, then $8,500 out of a $9,999 win: winning never
buys satisfaction, only a bigger number to chase. The final button
name-drops uncle only on levels that actually have one; level 1 has
never met him, so its degenerate shows off to the world instead.
-}
winCapUpsell : UncleOffer -> WinCapTier -> WinCapUpsell
winCapUpsell uncleOffer tier =
    case tier of
        FirstCap ->
            WinCapUpsellFor
                { priceCents = 50000
                , label = "I'm not satisfied and want to win more"
                , raisedTier = SecondCap
                }

        SecondCap ->
            WinCapUpsellFor
                { priceCents = 850000
                , label =
                    case uncleOffer of
                        NoUncleAdvice ->
                            "I NEED TO SHOW THE WORLD I'M AN ABSOLUTE DEGENERATE"

                        UncleAdviceForSale _ ->
                            "I NEED TO SHOW UNCLE I'M AN ABSOLUTE DEGENERATE"
                , raisedTier = DegenerateCap
                }

        DegenerateCap ->
            NoFurtherUpsell


{-| The victory text for winning the final, degenerate cap.
-}
trueEndingMessage : String
trueEndingMessage =
    "\u{1F389} You win all the internet points, congrats, this is the true ending, if you believe in such things, if not it's just another beginning."


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

            HiddenRandomBias _ ->
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
    , winCapTier = FirstCap
    , autoclicker = ClickerNotBought
    , betHold = NoBetHeld
    , shopFold = ShopCollapsed
    , pendingPurchase = NoPendingPurchase
    , uncleAdviceCount = 0
    , uncleGloat = UncleHasNotGloated
    , log = [ { tone = NeutralTone, text = config.introLogLine } ]
    }


init : LevelConfig -> () -> ( Model, Cmd Msg )
init config () =
    ( initialModel config
    , case config.bias of
        KnownHeadsPercent _ ->
            Cmd.none

        HiddenRandomBias range ->
            Random.generate BiasDrawn
                (Random.map2
                    (\favored percent -> { favored = favored, favoredPercent = percent })
                    (Random.uniform Heads [ Tails ])
                    (Random.int range.minPercent range.maxPercent)
                )
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
            ( settleFlip config.uncleOffer flip model, Cmd.none )

        ClockTicked ->
            ( tickClock model, Cmd.none )

        TrackerPurchased ->
            ( purchaseTracker config.trackerOffer model, Cmd.none )

        AutoclickerPurchased ->
            ( purchaseAutoclicker config.autoclickerOffer model, Cmd.none )

        BetHoldStarted side ->
            ( { model | betHold = BetHeld side }, Cmd.none )

        BetHoldEnded ->
            ( { model | betHold = NoBetHeld }, Cmd.none )

        AutoclickerTicked ->
            case model.betHold of
                NoBetHeld ->
                    -- A tick that raced the release: no button is held,
                    -- nothing is bet.
                    ( model, Cmd.none )

                BetHeld side ->
                    placeBet side model

        ExtraTimePurchased package ->
            ( purchaseExtraTime config.extraTimeOffer package model, Cmd.none )

        WinCapRaised ->
            ( raiseWinCap config.uncleOffer model, Cmd.none )

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

                Considering AutoclickerItem _ ->
                    ( purchaseAutoclicker config.autoclickerOffer
                        { model | pendingPurchase = NoPendingPurchase }
                    , Cmd.none
                    )

                Considering (ExtraTimeItem package) _ ->
                    -- Stays open: the house sells time by the armful.
                    ( purchaseExtraTime config.extraTimeOffer package model, Cmd.none )

                Considering UncleAdviceItem _ ->
                    -- Stays open: uncle appreciates repeat customers.
                    purchaseUncleAdvice config.uncleOffer model

        PurchaseCancelled ->
            ( { model | pendingPurchase = NoPendingPurchase }, Cmd.none )

        DialogClicked ->
            -- A click inside the dialog, swallowed (stopPropagation) so
            -- the document-level dismiss listener never sees it.
            ( model, Cmd.none )

        UncleAdviceRequested ->
            purchaseUncleAdvice config.uncleOffer model

        UncleAdviceGiven phrase ->
            ( gloatIfBusted config.uncleOffer
                (logLine NeutralTone ("\u{1F9D3} Uncle: \u{201C}" ++ phrase ++ "\u{201D}") model)
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
settleFlip : UncleOffer -> { playerChoice : CoinSide, betCents : Int, landed : CoinSide } -> Model -> Model
settleFlip uncleOffer flip model =
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

            -- Flip lines carry their number, matching the multi-coin
            -- games' logs.
            outcomeText =
                String.fromInt (model.flipCount + 1)
                    ++ ": Landed "
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
                (logLine DividerTone "" counted)
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
    if model.balanceCents >= capTargetCents model.winCapTier then
        { model | phase = WonGame }

    else if model.balanceCents <= 0 then
        { model | phase = WentBust }

    else
        model


{-| The last-dollars easter egg: uncle only gloats when buying his
advice is what bankrupted you (no sane person would, hence the
reward). Called solely after his advice lands in the log; a bust in
that moment can only have come from the purchase itself, since Elm
delivers the advice command before any further player input, and a
player already bust cannot buy advice at all.
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
                        ++ ". Hold a bet button down to use it."
                    )
                    { model
                        | balanceCents = model.balanceCents - priceCents
                        , autoclicker = ClickerBought
                        , betInput = clampBetInput (model.balanceCents - priceCents) model.betInput
                    }


{-| Buy more time on the clock. Repeatable, and refused when it would
wipe the balance to $0, like the tracker. Only a package actually on
offer is honoured; a message carrying any other package does nothing.
-}
purchaseExtraTime : ExtraTimeOffer -> ExtraTimePackage -> Model -> Model
purchaseExtraTime offer package model =
    case offer of
        NoExtraTime ->
            model

        ExtraTimeForSale packages ->
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
                        , betInput = clampBetInput (model.balanceCents - package.priceCents) model.betInput
                    }


{-| "1 more minute", "30 more minutes", or the raw seconds for a
package that is not a whole number of minutes.
-}
extraTimeText : Int -> String
extraTimeText extraSeconds =
    if extraSeconds == 60 then
        "1 more minute"

    else if modBy 60 extraSeconds == 0 then
        String.fromInt (extraSeconds // 60) ++ " more minutes"

    else
        String.fromInt extraSeconds ++ " more seconds"


{-| Pay to raise the winning target and play on. Only offered on the
win screen; the winning balance always covers the price, but the guard
stays so a stray message can never charge into bankruptcy. The end
state is re-checked right away: a win that overshot the raised target
too (a huge payout can) wins again on the spot instead of leaving a
"playing" game already past its target. The bet hold is released: the
bet buttons vanished mid-hold when the win screen appeared, so no
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
                    checkEndState
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
                                , betHold = NoBetHeld
                                , betInput = clampBetInput (model.balanceCents - upsell.priceCents) model.betInput
                            }
                        )


{-| Pay uncle and draw one of his pre-programmed pearls of wisdom.
Unlike the tracker, uncle happily takes your last dollars: spending
yourself into bankruptcy on advice is a lesson the game wants to allow.
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
                ( checkEndState
                    { model
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
    Sub.batch
        [ clockSubscription model
        , autoclickerSubscription model
        , dialogDismissSubscription model
        ]


{-| While a bought autoclicker's bet button is held down, a bet on the
held side fires every 100ms.
-}
autoclickerSubscription : Model -> Sub Msg
autoclickerSubscription model =
    case model.phase of
        Playing ->
            case ( model.autoclicker, model.betHold ) of
                ( ClickerBought, BetHeld _ ) ->
                    Time.every 100 (\_ -> AutoclickerTicked)

                ( ClickerBought, NoBetHeld ) ->
                    Sub.none

                ( ClickerNotBought, BetHeld _ ) ->
                    Sub.none

                ( ClickerNotBought, NoBetHeld ) ->
                    Sub.none

        WonGame ->
            Sub.none

        WentBust ->
            Sub.none

        RanOutOfTime ->
            Sub.none


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
        [ Html.div [] [ Html.text ("Target: $" ++ String.fromInt (capTargetCents model.winCapTier // 100)) ]
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
            min 100 (100 * toFloat model.balanceCents / toFloat (capTargetCents model.winCapTier))
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
                    [ Html.button
                        (Html.Events.onClick (BetPlaced Heads)
                            :: betHoldEvents model.autoclicker Heads
                        )
                        [ Html.text (betButtonLabel config.bias Heads) ]
                    , Html.button
                        (Html.Events.onClick (BetPlaced Tails)
                            :: betHoldEvents model.autoclicker Tails
                        )
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


{-| Hold events for a bet button, mouse and touch. Only attached once
the autoclicker is bought, so an unbought game never tracks hold state.
Mouse-leave and touch-cancel count as releasing: dragging off the button
must not leave the clicker running.
-}
betHoldEvents : Autoclicker -> CoinSide -> List (Html.Attribute Msg)
betHoldEvents clicker side =
    case clicker of
        ClickerNotBought ->
            []

        ClickerBought ->
            [ Html.Events.onMouseDown (BetHoldStarted side)
            , Html.Events.onMouseUp BetHoldEnded
            , Html.Events.onMouseLeave BetHoldEnded
            , Html.Events.on "touchstart" (Decode.succeed (BetHoldStarted side))
            , Html.Events.on "touchend" (Decode.succeed BetHoldEnded)
            , Html.Events.on "touchcancel" (Decode.succeed BetHoldEnded)
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

        HiddenRandomBias _ ->
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
    case
        viewTrackerShopItem config.trackerOffer model
            ++ viewAutoclickerShopItem config.autoclickerOffer model
            ++ viewExtraTimeShopItem config.extraTimeOffer
            ++ viewUncleShopItem config.uncleOffer
    of
        [] ->
            []

        shopItems ->
            [ Html.div [ Html.Attributes.class "shop" ]
                (ShopDialog.viewShopToggle ShopToggled model.shopFold
                    :: (case model.shopFold of
                            ShopCollapsed ->
                                []

                            ShopExpanded ->
                                shopItems ++ viewPurchaseDialog config model
                       )
                )
            ]


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


viewUncleShopItem : UncleOffer -> List (Html Msg)
viewUncleShopItem offer =
    case offer of
        NoUncleAdvice ->
            []

        UncleAdviceForSale uncle ->
            [ viewShopItem (PurchaseConsidered UncleAdviceItem) "Ask uncle for advice" uncle.priceCents ]


{-| More clock time, for sale forever: the price is the deterrent, not
the stock.
-}
viewExtraTimeShopItem : ExtraTimeOffer -> List (Html Msg)
viewExtraTimeShopItem offer =
    case offer of
        NoExtraTime ->
            []

        ExtraTimeForSale packages ->
            List.map
                (\package ->
                    viewShopItem (PurchaseConsidered (ExtraTimeItem package))
                        ("Buy " ++ extraTimeText package.extraSeconds)
                        package.priceCents
                )
                packages


viewPurchaseDialog : LevelConfig -> Model -> List (Html Msg)
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
                        ++ formatCents (itemPriceCents config itemKind)
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

        AutoclickerItem ->
            "the autoclicker"

        ExtraTimeItem package ->
            extraTimeText package.extraSeconds

        UncleAdviceItem ->
            "uncle's advice"


itemExplanation : ShopItemKind -> String
itemExplanation itemKind =
    case itemKind of
        TrackerItem ->
            "Counts how often heads and tails won across your flips, so you don't have to tally the log by hand."

        AutoclickerItem ->
            "Hold a bet button down and it presses it for you, ten times a second."

        ExtraTimeItem _ ->
            "Clock running low? Nonsense. The house happily extends your opportunity to give it money."

        UncleAdviceItem ->
            "Uncle's advice speaks for itself. He's your uncle, surely he knows best."


{-| The price the dialog quotes. Zero for an absent offer, which is
never read: the dialog only opens from a shop button that an absent
offer never renders.
-}
itemPriceCents : LevelConfig -> ShopItemKind -> Int
itemPriceCents config itemKind =
    case itemKind of
        TrackerItem ->
            case config.trackerOffer of
                NoTrackerForSale ->
                    0

                TrackerForSale priceCents ->
                    priceCents

        AutoclickerItem ->
            case config.autoclickerOffer of
                NoAutoclicker ->
                    0

                AutoclickerForSale priceCents ->
                    priceCents

        ExtraTimeItem package ->
            package.priceCents

        UncleAdviceItem ->
            case config.uncleOffer of
                NoUncleAdvice ->
                    0

                UncleAdviceForSale uncle ->
                    uncle.priceCents


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
            , viewWinCapUpsellButton config.uncleOffer model
            , viewBustEnding config.bustEnding model
            , viewExplanationHint model.phase
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


viewBustEnding : BustEnding -> Model -> List (Html Msg)
viewBustEnding ending model =
    case ending of
        PlainBustEnding ->
            []

        BustEndingQuote quote ->
            -- Only after money actually went to uncle: level 2's quote
            -- reads "all the money you gave him", which is nonsense on
            -- a bust where he never got a cent.
            if model.phase == WentBust && model.uncleAdviceCount > 0 then
                [ Html.div [ Html.Attributes.class "bust-ending" ]
                    [ Html.em [] [ Html.text quote ] ]
                ]

            else
                []


{-| The lost-game pointer at the article text under the widget.
Analytics (aug 2026) showed lost players replay and refresh instead of
scrolling: black swan had more page views than level 1 while level 4
had none, so the game itself has to say where the answer is. Shared by
both engines (MultiCoinGame calls it qualified).
-}
viewExplanationHint : GamePhase -> List (Html msg)
viewExplanationHint phase =
    case phase of
        Playing ->
            []

        WonGame ->
            []

        WentBust ->
            explanationHintLine

        RanOutOfTime ->
            explanationHintLine


explanationHintLine : List (Html msg)
explanationHintLine =
    [ Html.div [ Html.Attributes.class "explanation-hint" ]
        [ Html.strong [] [ Html.text "Stuck?" ]
        , Html.text " There's an explanation further down this page \u{2193}"
        ]
    ]


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
            ( "Time's up! You failed to reach the target.", LoseTone )


{-| Only revealed at the end, so the reader can incriminate themselves
first. Level 1 calls out tails bets against its printed bias; a hidden
bias is never revealed, not even here: the reader has to earn that
knowledge through the shop and their own tallying.
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

        HiddenRandomBias _ ->
            []


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

        DividerTone ->
            Html.div [ Html.Attributes.class "log-divider" ] []


toneClass : LogTone -> String
toneClass tone =
    case tone of
        WinTone ->
            "win-text"

        LoseTone ->
            "lose-text"

        NeutralTone ->
            ""

        DividerTone ->
            ""
