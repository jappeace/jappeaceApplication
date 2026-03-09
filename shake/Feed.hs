{-# LANGUAGE OverloadedStrings #-}
module Feed
  ( generateAtomFeed
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)

import Types (Article(..), SiteConfig(..))

-- | Generate an Atom XML feed from a list of articles.
generateAtomFeed :: SiteConfig -> [Article] -> Text
generateAtomFeed config articles = T.concat
  [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  , "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
  , "  <title>" , xmlEscape (siteName config) , "</title>\n"
  , "  <link href=\"" , siteUrl config , "/\" rel=\"alternate\"/>\n"
  , "  <link href=\"" , siteUrl config , "/" , feedAtom config , "\" rel=\"self\"/>\n"
  , "  <id>" , siteUrl config , "/</id>\n"
  , case articles of
      (a:_) -> T.concat ["  <updated>", formatAtomDate (articleDate a), "</updated>\n"]
      []    -> ""
  , T.concat (map (renderEntry config) articles)
  , "</feed>\n"
  ]

renderEntry :: SiteConfig -> Article -> Text
renderEntry config article = T.concat
  [ "  <entry>\n"
  , "    <title>" , xmlEscape (articleTitle article) , "</title>\n"
  , "    <link href=\"" , siteUrl config , "/" , articleUrl article , "\" rel=\"alternate\"/>\n"
  , "    <id>" , tagUri article , "</id>\n"
  , "    <published>" , formatAtomDate (articleDate article) , "</published>\n"
  , "    <updated>"
  , case articleModified article of
      Just m  -> formatAtomDate m
      Nothing -> formatAtomDate (articleDate article)
  , "</updated>\n"
  , "    <author>\n"
  , "      <name>" , xmlEscape (siteAuthor config) , "</name>\n"
  , "    </author>\n"
  , "    <category term=\"" , xmlEscape (articleCategory article) , "\"/>\n"
  , case articleSummaryText article of
      Just s  -> T.concat ["    <summary type=\"html\">" , xmlEscape s , "</summary>\n"]
      Nothing -> ""
  , "    <content type=\"html\">" , xmlEscape (articleContentText article) , "</content>\n"
  , "  </entry>\n"
  ]

-- | Generate a tag URI for an article: tag:domain,YYYY-MM-DD:/slug.html
tagUri :: Article -> Text
tagUri article = T.concat
  [ "tag:jappieklooster.nl,"
  , T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (articleDate article))
  , ":/" , articleUrl article
  ]

formatAtomDate :: UTCTime -> Text
formatAtomDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

xmlEscape :: Text -> Text
xmlEscape = T.concatMap escapeChar
  where
    escapeChar :: Char -> Text
    escapeChar '&'  = "&amp;"
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c    = T.singleton c
