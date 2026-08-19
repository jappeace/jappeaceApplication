module CoinFlipLevel1 exposing (levelConfig, main)

{-| The rigged-coin game on even-with-an-edge-you-lose.html: the 60% heads
bias is printed right on the bet buttons, and people still bet tails.
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
    { title = "\u{1FA99} Rigged Coin Trader"
    , bias = KnownHeadsPercent 60
    , trackerOffer = NoTrackerForSale
    , uncleOffer = NoUncleAdvice
    , nextLevelLink = NextLevelLinkTo { url = "/hidden-rewards.html", label = "<<next level>>" }
    , bustEnding = PlainBustEnding
    , introLogLine = "Heads hits 60%, tails 40%. The clock starts on your first bet. Good luck!"
    }


main : Program () Model Msg
main =
    gameProgram levelConfig
