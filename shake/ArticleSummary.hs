-- | Turns an article's rendered HTML into the short summary shown on the
-- index, tag, and category pages and in the atom feed.
module ArticleSummary (summarize, truncateHtml) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The summary text for an article: an author-provided @Summary:@
-- header wins outright; without one the first ~50 words of the rendered
-- content are taken. The override exists because deriving the summary
-- from content spoiled the coin-flip game posts: the words right after
-- the game embed are the strategy, and they ended up on the index page.
summarize :: Maybe Text -> Text -> Text
summarize authorSummary renderedHtml =
  case authorSummary of
    Just provided -> provided
    Nothing -> truncateHtml 50 renderedHtml

-- | Truncate rendered HTML to ~N words, closing any unclosed tags.
-- Counts only visible text words, skips inside tags and entities.
--
-- @\<style\>@ and @\<script\>@ blocks are dropped entirely (tags and
-- content). Summaries are embedded many-per-page: on the index a summary's
-- inline script actually executed, and because both coin-flip posts mount
-- into the same @#coin-flip-game@ id, the level-1 game hijacked the
-- hidden-rewards slot. A summary is a teaser, not a running app.
truncateHtml :: Int -> Text -> Text
truncateHtml maxWords html =
  let (result, openTags) = walkHtml maxWords 0 [] (T.unpack html)
      closingTags = concatMap (\tagName -> "</" ++ tagName ++ ">") openTags
  in T.pack result <> T.pack closingTags

-- | Walk the HTML character by character, emitting output until the word
-- limit is reached. Returns the emitted output and the stack of tags still
-- open at the cut, newest first, so the caller can close them.
walkHtml :: Int -> Int -> [String] -> String -> (String, [String])
walkHtml maxWords wordCount openTags input =
  if wordCount >= maxWords
    then ([], openTags)
    else case input of
      [] ->
        ([], openTags)
      ('<' : '/' : rest) ->
        walkClosingTag maxWords wordCount openTags rest
      ('<' : rest) ->
        walkOpeningTag maxWords wordCount openTags rest
      ('&' : rest) ->
        walkEntity maxWords wordCount openTags rest
      (currentChar : rest) ->
        if isHtmlWhitespace currentChar
          then
            let (remainder, finalTags) = walkHtml maxWords wordCount openTags rest
            in (currentChar : remainder, finalTags)
          else walkWord maxWords wordCount openTags currentChar rest

-- | A closing tag: emit it and pop its name off the open-tag stack.
-- The leading @"</"@ has already been consumed by 'walkHtml'.
walkClosingTag :: Int -> Int -> [String] -> String -> (String, [String])
walkClosingTag maxWords wordCount openTags input =
  let (tagContent, after) = span (/= '>') input
      tagName = takeWhile (\c -> c /= ' ' && c /= '>') tagContent
      closeStr = "</" ++ tagContent ++ ">"
      remainingTags = dropFirstTag tagName openTags
      (remainder, finalTags) = walkHtml maxWords wordCount remainingTags (drop 1 after)
  in (closeStr ++ remainder, finalTags)

-- | An opening tag: push it on the stack unless it is void or
-- self-closing, or drop the whole block when it is a style/script tag.
-- The leading @"<"@ has already been consumed by 'walkHtml'.
walkOpeningTag :: Int -> Int -> [String] -> String -> (String, [String])
walkOpeningTag maxWords wordCount openTags input =
  let (tagContent, after) = span (/= '>') input
      tagName = takeWhile (\c -> c /= ' ' && c /= '>' && c /= '/') tagContent
      isSelfClosing = not (null tagContent) && last tagContent == '/'
      isVoid = tagName `elem` voidElements
      tagStr = "<" ++ tagContent ++ ">"
  in if tagName == "style" || tagName == "script"
       then -- Drop the whole block: nothing emitted, nothing counted.
         let (_droppedBody, afterClose) = skipUntilClose tagName (drop 1 after)
         in walkHtml maxWords wordCount openTags afterClose
       else
         let newTags =
               if isSelfClosing || isVoid || null tagName
                 then openTags
                 else tagName : openTags
             (remainder, finalTags) = walkHtml maxWords wordCount newTags (drop 1 after)
         in (tagStr ++ remainder, finalTags)

-- | An HTML entity ("&amp;"): emit it without counting it as a word.
-- The leading @"&"@ has already been consumed by 'walkHtml'.
walkEntity :: Int -> Int -> [String] -> String -> (String, [String])
walkEntity maxWords wordCount openTags input =
  let (entity, after) = span (/= ';') input
      entityStr = "&" ++ entity ++ ";"
      (remainder, finalTags) = walkHtml maxWords wordCount openTags (drop 1 after)
  in (entityStr ++ remainder, finalTags)

-- | A visible word: emit it up to the next whitespace, tag, or entity,
-- and count it toward the limit.
walkWord :: Int -> Int -> [String] -> Char -> String -> (String, [String])
walkWord maxWords wordCount openTags firstChar input =
  let (word, after) = span (not . isWordBoundary) input
      fullWord = firstChar : word
      (remainder, finalTags) = walkHtml maxWords (wordCount + 1) openTags after
  in (fullWord ++ remainder, finalTags)

isHtmlWhitespace :: Char -> Bool
isHtmlWhitespace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

-- | Where a visible word stops: whitespace, a tag start, or an entity.
isWordBoundary :: Char -> Bool
isWordBoundary c = isHtmlWhitespace c || c == '<' || c == '&'

-- | Pop the first occurrence of a tag name from the open-tag stack.
dropFirstTag :: String -> [String] -> [String]
dropFirstTag _ [] = []
dropFirstTag name (tagName : rest) =
  if name == tagName
    then rest
    else tagName : dropFirstTag name rest

-- | Tags that never take a closing counterpart, per the HTML spec.
voidElements :: [String]
voidElements =
  [ "br", "img", "hr", "input", "meta", "link", "area"
  , "base", "col", "embed", "source", "track", "wbr"
  ]

-- | Consume everything up to and including the named closing tag,
-- returning the consumed content (with closing tag) and the remainder.
skipUntilClose :: String -> String -> (String, String)
skipUntilClose _ [] = ([], [])
skipUntilClose name ('<' : '/' : rest) =
  let (tagContent, after) = span (/= '>') rest
      closeName = takeWhile (\c -> c /= ' ' && c /= '>') tagContent
  in if closeName == name
       then ("</" ++ tagContent ++ ">", drop 1 after)
       else
         let (body, remaining) = skipUntilClose name (drop 1 after)
         in ("</" ++ tagContent ++ ">" ++ body, remaining)
skipUntilClose name (currentChar : rest) =
  let (body, remaining) = skipUntilClose name rest
  in (currentChar : body, remaining)
