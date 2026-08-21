module CoinFlipLevel4 exposing (levelConfig, main)

{-| Level 4 of the rigged-coin game: birds of a feather
(birds-of-a-feather.html), the correlation level.

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

No automation in this shop: with 200 flips the game is about sizing,
not pressing.

-}

import CoinFlipGame exposing (NextLevelLink(..), TrackerOffer(..), UncleOffer(..))
import MultiCoinGame exposing (AllocatorOffer(..), AutoclickerOffer(..), CoinOdds(..), CorrelationBookOffer(..), FlipHelperOffer(..), GlassesOffer(..), Model, Msg, MultiCoinConfig, ProfileAssignment(..), TurnBudget(..), gameProgram)


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
    , autoclickerOffer = NoAutoclicker
    , flipHelperOffer = NoFlipHelpers
    , allocatorOffer = AllocatorForSale 1000
    , bookOffer = CorrelationBookForSale 2000
    , uncleOffer =
        UncleAdviceForSale
            { priceCents = 500
            , firstPhrase = "Don't split your money son, pick a winner and commit."
            , morePhrases =
                [ "Oh I don't know son, I'd put it all on the cuckoo."
                , "The cuckoo is due, I can feel it."
                , "A real gambler doesn't hedge."
                , "Diversification is for people who don't know what they're doing."
                , "Just bet whichever bird won last time, they get on streaks."
                , "Sunbird AND Rainbird? One of them always loses, that's throwing money away."
                ]
            }
    , introLogLine = "Three birds, one sky, two hundred flips. Stake any of them on heads and press flip. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
