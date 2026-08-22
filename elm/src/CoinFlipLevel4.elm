module CoinFlipLevel4 exposing (levelConfig, main)

{-| Level 4 of the rigged-coin game: birds of a feather
(correlation-implies-cashation.html), the correlation level.

One hidden weather roll per round (sun 60%, rain 40%). One profile
wins exactly when it is sunny paying 0.8x, another exactly when it
rains paying 1.8x: perfectly anti-correlated, exactly one wins every
round. The three profiles are dealt across Starling, Swallow, and
Cuckoo at random every game.
Their payouts form a Dutch book (0.8 * 1.8 > 1), so the right stake
split (about 61/39) nets a guaranteed ~9.6% per flip, reaching $999 in
41 presses. Either bird alone is positive EV but grows ~0.4% per flip
at best, hopeless inside the 200-flip budget. Cuckoo is the red
herring: a fat 40x payout at 2%, which is a -18% expected return.

The shop groups into Upgrades (autoclicker, flip helpers, percentage
allocator) and Intel (tracker, glasses, The Elegant Universe, uncle).
Collecting uncle's complete works unlocks asking for a refund, which
costs money and yields only his fondest farewell.

-}

import CoinFlipGame exposing (NextLevelLink(..), TrackerOffer(..), UncleOffer(..))
import MultiCoinGame exposing (AllocatorOffer(..), AutoclickerOffer(..), CoinOdds(..), CorrelationBookOffer(..), ExtraTurnsOffer(..), FlipHelperOffer(..), GlassesOffer(..), LastChanceTurnOffer(..), Model, Msg, MultiCoinConfig, ProfileAssignment(..), RefundOffer(..), TurnBudget(..), gameProgram)


levelConfig : MultiCoinConfig
levelConfig =
    { title = "\u{1FA99} Rigged Coin Trader IV: Birds of a Feather"
    , coins =
        [ { coinName = "Starling", odds = WinsWhenSunny, payoutPercent = 80 }
        , { coinName = "Swallow", odds = WinsWhenRainy, payoutPercent = 180 }
        , { coinName = "Cuckoo", odds = IndependentPercent 2, payoutPercent = 4000 }
        ]
    , weatherSunPercent = 60
    , turnBudget = FlipLimit 200
    , nextLevelLink = NoNextLevelLink

    -- Shuffled: the names carry no weather hint, so which bird got
    -- which profile must be rediscovered every game.
    , profileAssignment = ProfilesShuffledAcrossCoins
    , trackerOffer = TrackerForSale 1500
    , glassesOffer = GoldenGlassesForSale 2000
    , autoclickerOffer = AutoclickerForSale 1000
    , flipHelperOffer = FlipHelpersForSale { basePriceCents = 100, priceIncreasePercent = 10 }
    , allocatorOffer = AllocatorForSale 1000
    , bookOffer = CorrelationBookForSale 2000
    -- The per-flip prices run $6 (single), $5.50 (10-pack), $10
    -- (50-pack), and $8 for the same single flip on the out-of-flips
    -- screen: the "bulk deal" is the worst buy in the shop and
    -- desperation pays a $2 premium. Uncle recommends the big one.
    , extraTurnsOffer =
        ExtraTurnsForSale
            [ { priceCents = 600, extraFlips = 1 }
            , { priceCents = 5500, extraFlips = 10 }
            , { priceCents = 50000, extraFlips = 50 }
            ]
    , lastChanceTurnOffer = LastChanceTurnForSale { priceCents = 800, extraFlips = 1 }
    , refundOffer =
        RefundForSale
            { priceCents = 1000
            , reply = "Son, I gave you my best tricks, you're set for life now. The money though, I invested it all in a very promising cuckoo, so that ship has sailed. No need to thank me."
            }
    , uncleOffer =
        UncleAdviceForSale
            { priceCents = 500
            , firstPhrase = "Don't split your money son, pick a winner and commit."
            , morePhrases =
                [ "The cuckoo is due, I can feel it."
                , "A real gambler doesn't hedge."
                , "Diversification is for people who don't know what they're doing."
                , "Starlings and swallows are basically the same bird, so same bet really."
                , "Correlation? That's when birds fly in a V, right?"
                , "When one bird loses, bet it harder, it owes you."
                , "I read half a physics book once. Everything is strings, so nothing matters."
                , "I got this sweet deal for 50 more turns!"
                , "Your aunt once knitted a swallow. Beautiful bird. What were we talking about?"
                ]
            }
    , introLogLine = "Three birds, one sky, two hundred flips. Stake any of them on heads and press flip. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
