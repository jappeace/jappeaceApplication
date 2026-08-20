module CoinFlipLevel3 exposing (levelConfig, main)

{-| Level 3 of the rigged-coin game: the black swan (black-swan.html).

Three coins, everything hidden. Swan is the black swan: it almost never
lands heads, but pays out huge, and it is the only coin worth betting.
Magpie looks like a fair double-or-nothing but robs you slowly; Sparrow
wins often but pays so little it bleeds you anyway.

-}

import CoinFlipGame exposing (TrackerOffer(..), UncleOffer(..))
import MultiCoinGame exposing (Model, Msg, MultiCoinConfig, gameProgram)


levelConfig : MultiCoinConfig
levelConfig =
    { title = "\u{1FA99} Rigged Coin Trader III: Rare Birds"
    , coins =
        [ { coinName = "Swan", winPercent = 5, payoutPercent = 3000 }
        , { coinName = "Magpie", winPercent = 45, payoutPercent = 100 }
        , { coinName = "Sparrow", winPercent = 60, payoutPercent = 50 }
        ]
    , trackerOffer = TrackerForSale 1500
    , uncleOffer =
        UncleAdviceForSale
            { priceCents = 500
            , firstPhrase = "Oh I don't know son, sparrows never let you down."
            , morePhrases =
                [ "A swan bit me once. Never trust a swan."
                , "Magpies are drawn to money, that's gotta mean something."
                , "Bet all three, that way you always win something."
                , "The swan is due, I can feel it."
                , "Double your bet after every loss, that's how you win it back."
                , "Winners don't do math, son."
                ]
            }
    , introLogLine = "Three coins, three payouts, all hidden. Stake any of them on heads and press flip. The clock starts on your first flip. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
