{-# LANGUAGE OverloadedStrings #-}
module Types
  ( SiteConfig(..)
  , Article(..)
  , Page(..)
  , NavLink(..)
  , PaginationInfo(..)
  , defaultSiteConfig
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Blaze.Html (Html)

data NavLink = NavLink
  { navTitle :: Text
  , navUrl   :: Text
  , navDesc  :: Text
  , navClass :: Text
  }

data SiteConfig = SiteConfig
  { siteAuthor     :: Text
  , siteName       :: Text
  , siteUrl        :: Text
  , feedDomain     :: Text
  , feedAtom       :: Text
  , siteLinks      :: [NavLink]
  , siteSocial     :: [NavLink]
  , siteFootLinks  :: [(Text, Text)]
  , dateFormat      :: Text
  }

data Article = Article
  { articleTitle       :: Text
  , articleSlug        :: Text
  , articleCategory    :: Text
  , articleDate        :: UTCTime
  , articleModified    :: Maybe UTCTime
  , articleTags        :: [Text]
  , articleContent     :: Html
  , articleContentText :: Text   -- ^ Full article HTML as Text (for feed)
  , articleSummary     :: Maybe Html
  , articleSummaryText :: Maybe Text  -- ^ Summary HTML as Text (for feed)
  , articleFootnotesHtml :: Maybe Html  -- ^ Extracted footnotes section
  , articleUrl         :: Text
  }

data Page = Page
  { pageTitle    :: Text
  , pageSlug     :: Text
  , pageDate     :: UTCTime
  , pageContent  :: Html
  , pageUrl      :: Text
  , pageHomeTitle :: Maybe Text
  , pageHomeDesc  :: Maybe Text
  }

data PaginationInfo = PaginationInfo
  { paginationCurrent :: Int
  , paginationTotal   :: Int
  , paginationPrevUrl :: Maybe Text
  , paginationNextUrl :: Maybe Text
  }

defaultSiteConfig :: SiteConfig
defaultSiteConfig = SiteConfig
  { siteAuthor = "Jappie J. T. Klooster"
  , siteName   = "Spotify - Web Player: Music for everyone"
  , siteUrl    = "https://jappieklooster.nl"
  , feedDomain = "https://jappieklooster.nl"
  , feedAtom   = "atom"
  , siteLinks  =
      [ NavLink "About \128194" "/pages/about-me.html" "About me" "about"
      , NavLink "Hire \128039" "https://jappiesoftware.com/" "Jappie for hire" "hire"
      , NavLink "Coaching \128293" "https://jappie.me/fire" "Lern haskell by fire" "fire"
      ]
  , siteSocial =
      [ NavLink "Email \9993" "mailto:hi@jappie.me" "Contact me" "email"
      ]
  , siteFootLinks =
      [ ("Linkedin", "https://www.linkedin.com/in/jappie-klooster-1b8b4850/")
      , ("Twitch", "https://www.twitch.tv/jappiejappie")
      , ("Youtube", "https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw")
      , ("Github", "https://github.com/jappeace")
      , ("Penguin", "http://penguin.engineer/")
      , ("Raster", "https://raster.click/")
      , ("Facebook", "https://www.facebook.com/jappie.kerk")
      , ("Twitter", "https://twitter.com/jappieklooster")
      , ("Reddit", "https://www.reddit.com/r/jappie/")
      , ("Discord", "https://discord.gg/Hp4agqy")
      ]
  , dateFormat = "%Y\24180%m\26376%d\26085"
  }
