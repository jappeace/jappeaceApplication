module CoinFlipLevel2 exposing (levelConfig, main)

{-| Level 2 of the rigged-coin game: hidden rewards. Which side wins is
drawn fifty-fifty at game start, its win percent between 55 and 65, and
neither is ever shown, not even on the end screen. The shop sells a
ratio tracker (steep) and uncle's terrible advice (cheap, worth less).
-}

import CoinFlipGame
    exposing
        ( BustEnding(..)
        , CoinBias(..)
        , LevelConfig
        , Model
        , Msg
        , NextLevelLink(..)
        , TrackerOffer(..)
        , UncleOffer(..)
        , gameProgram
        )


levelConfig : LevelConfig
levelConfig =
    { title = "\u{1FA99} Rigged Coin Trader II: Hidden Rewards"
    , bias = HiddenRandomBias { minPercent = 55, maxPercent = 65 }
    , trackerOffer = TrackerForSale 1500
    , uncleOffer =
        UncleAdviceForSale
            { priceCents = 500
            , firstPhrase = "Oh I don't know son, I'd put it all on heads."
            , morePhrases =
                [ "Tails never fails! Wait, or was it heads?"
                , "Double your bet after every loss, that's how you win it back."
                , "I've got a good feeling about this one. Bet big. Bigly!"
                , "It landed heads a lot just now, so tails is due."
                , "Winners don't do math, son."
                , "Back in my day we used our gut for gambling. Good old fashioned instincts."
                , "Ratios are deceptive because numbers aren't real."
                ]
            }
    , nextLevelLink = NextLevelLinkTo { url = "/black-swan.html", label = "<<next level>>" }
    , bustEnding = BustEndingQuote "Uncle is proud. Or perhaps he just appreciates all the money you gave him."
    , introLogLine = "The coin is rigged again, but this time I won't tell you how. The clock starts on your first bet. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
