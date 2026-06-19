{-# LANGUAGE OverloadedStrings #-}
module Feed
  ( generateAtomFeed
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)

import Types (Article(..), SiteConfig(..), langPrefix)

-- | Generate an Atom XML feed from a list of articles.
-- The feed URL and article URLs respect the language prefix.
generateAtomFeed :: SiteConfig -> [Article] -> Text
generateAtomFeed config articles =
  let lang = siteLang config
      prefix = langPrefix lang
      feedUrl = siteUrl config <> "/" <> prefix <> feedAtom config
      altUrl  = siteUrl config <> "/" <> prefix <> feedDirectory config
  in T.concat
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    , "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
    , "  <title>" , xmlEscape (siteName config) , "</title>\n"
    , "  <link href=\"" , altUrl , "\" rel=\"alternate\"/>\n"
    , "  <link href=\"" , feedUrl , "\" rel=\"self\"/>\n"
    , "  <id>" , altUrl , "</id>\n"
    , case articles of
        (a:_) -> T.concat ["  <updated>", formatAtomDate (articleDate a), "</updated>\n"]
        []    -> ""
    , T.concat (map (renderEntry config) articles)
    , "</feed>\n"
    ]

-- | The directory the blog (and its feed) live in, derived from 'feedAtom'.
-- @"blog/atom"@ yields @"blog/"@ and a root-level @"atom"@ yields @""@. Used so
-- the feed's alternate and per-entry links point at the actual article paths
-- (e.g. @\/blog\/slug.html@) rather than the site root.
feedDirectory :: SiteConfig -> Text
feedDirectory config = fst (T.breakOnEnd "/" (feedAtom config))

renderEntry :: SiteConfig -> Article -> Text
renderEntry config article =
  let prefix = langPrefix (siteLang config)
      entryUrl = siteUrl config <> "/" <> prefix <> feedDirectory config <> articleUrl article
  in T.concat
    [ "  <entry>\n"
    , "    <title>" , xmlEscape (articleTitle article) , "</title>\n"
    , "    <link href=\"" , entryUrl , "\" rel=\"alternate\"/>\n"
    , "    <id>" , tagUri config article , "</id>\n"
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

-- | Generate a tag URI for an article: tag:domain,YYYY-MM-DD:/[nl/][blog/]slug.html
-- The authority is the site's own host so each brand site's feed gets stable,
-- non-colliding entry ids rather than all sharing jappieklooster.nl.
tagUri :: SiteConfig -> Article -> Text
tagUri config article =
  let prefix = langPrefix (siteLang config)
  in T.concat
    [ "tag:" , feedHost config , ","
    , T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (articleDate article))
    , ":/" , prefix , feedDirectory config , articleUrl article
    ]

-- | The bare host of a site (no scheme), for use as a tag URI authority.
feedHost :: SiteConfig -> Text
feedHost config =
  T.replace "http://" "" (T.replace "https://" "" (siteUrl config))

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
