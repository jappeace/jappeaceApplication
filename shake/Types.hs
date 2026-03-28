{-# LANGUAGE OverloadedStrings #-}
module Types
  ( SiteConfig(..)
  , Article(..)
  , Page(..)
  , NavLink(..)
  , PaginationInfo(..)
  , Lang(..)
  , Translations(..)
  , translationsFor
  , langPrefix
  , langCode
  , defaultSiteConfig
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Blaze.Html (Html)

-- | Supported languages
data Lang = En | Nl deriving (Eq, Show)

-- | All translatable UI strings
data Translations = Translations
  { tPublished      :: Text  -- "published: " / "gepubliceerd: "
  , tLastModified   :: Text  -- ", last modified: " / ", laatst gewijzigd: "
  , tPostedBy       :: Text  -- "Posted by " / "Geplaatst door "
  , tReadMore       :: Text  -- "Could there be more?" / "Zou er meer zijn?"
  , tNewer          :: Text  -- "Newer" / "Nieuwer"
  , tOlder          :: Text  -- "Older" / "Ouder"
  , tBlogArchive    :: Text  -- "Blog archive" / "Blog archief"
  , tArchive        :: Text  -- "Archive" / "Archief"
  , tTags           :: Text  -- "Tags" / "Tags"
  , tCategories     :: Text  -- "Categories" / "Categorieën"
  , tRecentStuff    :: Text  -- "Recent stuff" / "Recent"
  , tTagged         :: Text  -- "Tagged: " / "Gelabeld: "
  , tFooterQuote    :: Text  -- the Lao Tzu quote / Dutch equivalent
  , tSwitchLang     :: Text  -- "NL" / "EN"
  , tSwitchLangDesc :: Text  -- "Nederlands" / "English"
  }

-- | Look up translations for a given language
translationsFor :: Lang -> Translations
translationsFor En = Translations
  { tPublished      = "published: "
  , tLastModified   = ", last modified: "
  , tPostedBy       = "Posted by "
  , tReadMore       = "Could there be more?"
  , tNewer          = "Newer"
  , tOlder          = "Older"
  , tBlogArchive    = "Blog archive"
  , tArchive        = "Archive"
  , tTags           = "Tags"
  , tCategories     = "Categories"
  , tRecentStuff    = "Recent stuff"
  , tTagged         = "Tagged: "
  , tFooterQuote    = "Those who know do not speak. Those who speak do not know."
  , tSwitchLang     = "NL"
  , tSwitchLangDesc = "Nederlands"
  }
translationsFor Nl = Translations
  { tPublished      = "gepubliceerd: "
  , tLastModified   = ", laatst gewijzigd: "
  , tPostedBy       = "Geplaatst door "
  , tReadMore       = "Zou er meer zijn?"
  , tNewer          = "Nieuwer"
  , tOlder          = "Ouder"
  , tBlogArchive    = "Blog archief"
  , tArchive        = "Archief"
  , tTags           = "Tags"
  , tCategories     = "Categorie\235n"
  , tRecentStuff    = "Recent"
  , tTagged         = "Gelabeld: "
  , tFooterQuote    = "Wie weet spreekt niet. Wie spreekt weet niet."
  , tSwitchLang     = "EN"
  , tSwitchLangDesc = "English"
  }

-- | URL prefix for a language: "" for English, "nl/" for Dutch
langPrefix :: Lang -> Text
langPrefix En = ""
langPrefix Nl = "nl/"

-- | HTML lang attribute value
langCode :: Lang -> Text
langCode En = "en"
langCode Nl = "nl"

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
  , siteLang       :: Lang
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
      , ("Youtube", "https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw")
      , ("Github", "https://github.com/jappeace")
      , ("Raster", "https://raster.click/")
      , ("Facebook", "https://www.facebook.com/jappie.kerk")
      , ("Discord", "https://discord.gg/Hp4agqy")
      ]
  , dateFormat = "%Y\24180%m\26376%d\26085"
  , siteLang   = En
  }
