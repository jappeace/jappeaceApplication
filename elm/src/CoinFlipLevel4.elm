module CoinFlipLevel4 exposing (levelConfig, main)

{-| Level 4 of the rigged-coin game: birds of a feather
(birds-of-a-feather.html), the correlation level.

One hidden weather roll per round (sun 60%, rain 40%). Sunbird wins
exactly when it is sunny paying 0.8x, Rainbird exactly when it rains
paying 1.8x: perfectly anti-correlated, exactly one wins every round.
Their payouts form a Dutch book (0.8 * 1.8 > 1), so the right stake
split (about 61/39) nets a guaranteed ~9.6% per flip, reaching $999 in
41 presses. Either bird alone is positive EV but grows ~0.4% per flip
at best, hopeless inside the 200-flip budget. Cuckoo is the red
herring: a fat 40x payout at 2%, which is a -18% expected return.

No automation in this shop: with 200 flips the game is about sizing,
not pressing.

-}

import CoinFlipGame exposing (NextLevelLink(..), TrackerOffer(..), UncleOffer(..))
import MultiCoinGame exposing (AutoclickerOffer(..), CoinOdds(..), FlipHelperOffer(..), GlassesOffer(..), Model, Msg, MultiCoinConfig, ProfileAssignment(..), TurnBudget(..), gameProgram)


levelConfig : MultiCoinConfig
levelConfig =
    { title = "\u{1FA99} Rigged Coin Trader IV: Birds of a Feather"
    , coins =
        [ { coinName = "Sunbird", odds = WinsWhenSunny, payoutPercent = 80 }
        , { coinName = "Rainbird", odds = WinsWhenRainy, payoutPercent = 180 }
        , { coinName = "Cuckoo", odds = IndependentPercent 2, payoutPercent = 4000 }
        ]
    , weatherSunPercent = 60
    , turnBudget = FlipLimit 200
    , nextLevelLink = NoNextLevelLink

    -- As written: the bird names are the weather hint, shuffling would
    -- detach them from their roles.
    , profileAssignment = ProfilesAsWritten
    , trackerOffer = TrackerForSale 1500
    , glassesOffer = GoldenGlassesForSale 2000
    , autoclickerOffer = NoAutoclicker
    , flipHelperOffer = NoFlipHelpers
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
