{-# LANGUAGE OverloadedStrings #-}
module Slug
  ( toSlug
  ) where

import Data.Char (isAscii, isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert a title to a Pelican-compatible slug.
-- Matches Pelican's python slugify: lowercase, strip non-ASCII,
-- remove non-word chars (except spaces/hyphens), collapse whitespace/hyphens.
toSlug :: Text -> Text
toSlug =
    T.dropAround (== '-')
  . collapseHyphens
  . T.map spaceToDash
  . T.filter keepChar
  . T.filter isAscii
  . T.toLower
  where
    -- Keep alphanumeric, spaces, and hyphens (like Python's [^\w\s-] removal)
    keepChar :: Char -> Bool
    keepChar c = isAlphaNum c || isSpace c || c == '-'

    spaceToDash :: Char -> Char
    spaceToDash c
      | isSpace c = '-'
      | otherwise = c

    collapseHyphens :: Text -> Text
    collapseHyphens = T.intercalate "-" . filter (not . T.null) . T.splitOn "-"
