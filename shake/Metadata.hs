{-# LANGUAGE OverloadedStrings #-}
module Metadata
  ( parseMarkdownMeta
  , parseOrgMeta
  , parseDateField
  , parseTags
  , isDraft
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

-- | Parse Pelican-style markdown frontmatter.
-- Lines of "Key: Value" until the first blank line.
-- Returns (metadata map with lowercased keys, body without frontmatter).
parseMarkdownMeta :: Text -> (Map Text Text, Text)
parseMarkdownMeta input =
  let ls = T.lines input
      (metaLines, rest) = span isMetaLine ls
      body = case rest of
        []    -> ""
        (_:t) -> T.unlines t  -- skip the blank line separator
  in (parsePairs metaLines, body)
  where
    isMetaLine :: Text -> Bool
    isMetaLine line
      | T.null (T.strip line) = False
      | otherwise = case T.breakOn ":" line of
          (_, rest') -> not (T.null rest')

    parsePairs :: [Text] -> Map Text Text
    parsePairs = Map.fromList . map parsePair

    parsePair :: Text -> (Text, Text)
    parsePair line =
      case T.breakOn ":" line of
        (key, val) -> (T.toLower (T.strip key), T.strip (T.drop 1 val))

-- | Parse org-mode frontmatter.
-- Lines of "#+KEY: VALUE" are metadata. Comment lines (# ...) are skipped.
-- Returns (metadata map with lowercased keys, body without frontmatter lines).
parseOrgMeta :: Text -> (Map Text Text, Text)
parseOrgMeta input =
  let ls = T.lines input
      (headerLines, bodyLines) = span isHeaderLine ls
      metaLines = filter isOrgMeta headerLines
      body = T.unlines bodyLines
  in (parseOrgPairs metaLines, body)
  where
    -- Lines that are part of the header block (either #+KEY or # comment or blank)
    isHeaderLine :: Text -> Bool
    isHeaderLine line
      | T.null (T.strip line)                         = True
      | T.isPrefixOf "#" (T.stripStart line)          = True
      | otherwise                                     = False

    isOrgMeta :: Text -> Bool
    isOrgMeta line = T.isPrefixOf "#+" (T.stripStart line)
                  && not (T.isPrefixOf "#+BEGIN" (T.toUpper (T.stripStart line)))

    parseOrgPairs :: [Text] -> Map Text Text
    parseOrgPairs = Map.fromList . map parseOrgPair

    parseOrgPair :: Text -> (Text, Text)
    parseOrgPair line =
      let stripped = T.drop 2 (T.stripStart line) -- remove #+
      in case T.breakOn ":" stripped of
          (key, val) -> (T.toLower (T.strip key), T.strip (T.drop 1 val))

-- | Try parsing a date string with multiple formats.
-- Supports: "%Y-%m-%d %H:%M", "%Y-%m-%d", "%Y.%m.%d", and variants.
parseDateField :: Text -> Maybe UTCTime
parseDateField txt =
  let s = T.unpack (T.strip txt)
      formats =
        [ "%Y-%m-%d %H:%M"
        , "%Y-%m-%d %-H:%M"
        , "%Y-%m-%d"
        , "%Y.%m.%d"
        , "%Y-%m-%-d %H:%M"
        , "%Y-%m-%-d %-H:%M"
        , "%Y-%m-%-d"
        , "%Y.%m.%-d"
        ]
      tryFormat :: String -> Maybe UTCTime
      tryFormat fmt = parseTimeM True defaultTimeLocale fmt s
  in foldr (\fmt acc -> case tryFormat fmt of
              Nothing -> acc
              just    -> just) Nothing formats

-- | Parse tags from a comma-separated string.
-- If no commas, treat entire value as single tag.
parseTags :: Text -> [Text]
parseTags txt
  | T.null (T.strip txt) = []
  | T.isInfixOf "," txt  = map (T.toLower . T.strip) (T.splitOn "," txt)
  | otherwise             = [T.toLower (T.strip txt)]

-- | Check if a metadata map indicates the article is a draft.
isDraft :: Map Text Text -> Bool
isDraft meta = case Map.lookup "status" meta of
  Just s  -> T.toLower (T.strip s) == "draft"
  Nothing -> False
