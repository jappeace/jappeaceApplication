module CoinFlipLevel3 exposing (levelConfig, main)

{-| Level 3 of the rigged-coin game: the black swan (black-swan.html).

Three birds share three hidden profiles, dealt across the names at
random on every game start: a black swan (rarely lands heads but pays
out huge, the only bet worth taking), a double-or-nothing that robs you
slowly, and a frequent winner that pays so little it bleeds you anyway.
Which bird got which profile has to be rediscovered every replay.

-}

import CoinFlipGame exposing (NextLevelLink(..), TrackerOffer(..), UncleOffer(..))
import MultiCoinGame exposing (AutoclickerOffer(..), CoinOdds(..), FlipHelperOffer(..), GlassesOffer(..), Model, Msg, MultiCoinConfig, ProfileAssignment(..), TurnBudget(..), gameProgram)


levelConfig : MultiCoinConfig
levelConfig =
    { title = "\u{1FA99} Rigged Coin Trader III: Rare Birds"
    , coins =
        [ { coinName = "Swan", odds = IndependentPercent 5, payoutPercent = 3000 }
        , { coinName = "Magpie", odds = IndependentPercent 45, payoutPercent = 100 }
        , { coinName = "Sparrow", odds = IndependentPercent 60, payoutPercent = 50 }
        ]
    , weatherSunPercent = 50
    , turnBudget = TimeLimit (30 * 60)
    , nextLevelLink = NextLevelLinkTo { url = "/birds-of-a-feather.html", label = "<<next level>>" }
    , profileAssignment = ProfilesShuffledAcrossCoins
    , trackerOffer = TrackerForSale 1500
    , glassesOffer = GoldenGlassesForSale 2000
    , autoclickerOffer = AutoclickerForSale 1000
    , flipHelperOffer = FlipHelpersForSale { basePriceCents = 100, priceIncreasePercent = 10 }
    , uncleOffer =
        UncleAdviceForSale
            { priceCents = 500
            , firstPhrase = "Oh I don't know son, sparrows never let you down."
            , morePhrases =
                [ "A swan bit me once. Never trust a swan."
                , "Magpies are drawn to money, that's gotta mean something."
                , "Bet all three, that way you always win something."
                , "The swan is due, I can feel it."
                , "If it wins more often than it loses, it's a winner. Simple."
                , "Your aunt won three magpies in a row once. Three!"
                , "So a magpie is some kinda pastry right?"
                , "I saw that movie with a pirate captain called sparrow, bet on the sparrow!"
                , "To bad dodo isn't an option, best birb."
                ]
            }
    , introLogLine = "Three coins, three payouts, all hidden. Stake any of them on heads and press flip. The clock starts on your first flip. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
