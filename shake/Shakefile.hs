{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (IOException, catch, finally)
import qualified Data.ByteString as BS
import Data.List (sortBy, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Development.Shake
import Development.Shake.FilePath
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir
import System.IO (hSetEncoding, utf8, stdout, stderr)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc
  ( runIOorExplode
  , readMarkdown
  , readOrg
  , writeHtml5
  , def
  , readerExtensions
  , writerHighlightStyle
  , pandocExtensions
  )
import Text.Pandoc.Highlighting (pygments)
import WaiAppStatic.Types (ssIndices, unsafeToPiece, ssAddTrailingSlash)

import Feed (generateAtomFeed)
import Metadata (parseMarkdownMeta, parseOrgMeta, parseDateField, parseTags, isDraft)
import Slug (toSlug)
import Templates
  ( renderArticlePage
  , renderPagePage
  , renderIndexPage
  , renderArchivesPage
  , renderTagPage
  , renderTagsListPage
  , renderCategoryPage
  , renderCategoriesListPage
  )
import Types
  ( Article(..)
  , Page(..)
  , PaginationInfo(..)
  , defaultSiteConfig
  )

lockFile :: FilePath
lockFile = "_build/.shake.lock"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  finally
    (shakeArgs shakeOptions{shakeFiles="_build"} shakeRules)
    (Dir.removeFile lockFile `catch` \(_ :: IOException) -> return ())

shakeRules :: Rules ()
shakeRules = do
    phony "build" $ do
      -- Discover content files
      mds <- getDirectoryFiles "content" ["//*.md"]
      orgs <- getDirectoryFiles "content" ["//*.org"]
      let contentFiles = [(f, "md") | f <- mds, not (isStaticPath f)]
                      ++ [(f, "org") | f <- orgs, not (isStaticPath f)]

      -- Parse all articles and pages
      (articles, pages) <- liftIO $ parseAllContent contentFiles

      let sortedArticles = sortBy (\a b -> compare (Down (articleDate a)) (Down (articleDate b))) articles
          tagMap = buildTagMap sortedArticles
          catMap = buildCategoryMap sortedArticles
          config = defaultSiteConfig

      -- Generate all output
      liftIO $ do
        -- Create output directories
        Dir.createDirectoryIfMissing True "_site"
        Dir.createDirectoryIfMissing True "_site/tag"
        Dir.createDirectoryIfMissing True "_site/category"
        Dir.createDirectoryIfMissing True "_site/pages"

        -- Article pages
        mapM_ (\art -> writeHtmlFile
          ("_site" </> T.unpack (articleUrl art))
          (renderArticlePage config pages art sortedArticles tagMap))
          sortedArticles

        -- Static pages
        mapM_ (\page -> writeHtmlFile
          ("_site" </> T.unpack (pageUrl page))
          (renderPagePage config pages page))
          pages

        -- Paginated index pages
        let articlePages = chunksOf 10 sortedArticles
            totalPages = length articlePages
        mapM_ (\(pageNum, arts) ->
          let pagination = PaginationInfo
                { paginationCurrent = pageNum
                , paginationTotal   = totalPages
                , paginationPrevUrl = if pageNum > 1
                    then Just ("/" <> indexFileName (pageNum - 1))
                    else Nothing
                , paginationNextUrl = if pageNum < totalPages
                    then Just ("/" <> indexFileName (pageNum + 1))
                    else Nothing
                }
          in writeHtmlFile ("_site" </> T.unpack (indexFileName pageNum)) (renderIndexPage config pages arts pagination)
          ) (zip [1..] articlePages)

        -- Archives
        writeHtmlFile "_site/archives.html"
          (renderArchivesPage config pages sortedArticles)

        -- Tags list
        writeHtmlFile "_site/tags.html"
          (renderTagsListPage config pages tagMap)

        -- Individual tag pages
        mapM_ (\(tag, arts) -> writeHtmlFile
          ("_site/tag" </> T.unpack (tagSlugLocal tag) <.> "html")
          (renderTagPage config pages tag arts))
          tagMap

        -- Categories list
        writeHtmlFile "_site/categories.html"
          (renderCategoriesListPage config pages catMap)

        -- Individual category pages
        mapM_ (\(cat, arts) -> writeHtmlFile
          ("_site/category" </> T.unpack cat <.> "html")
          (renderCategoryPage config pages cat arts))
          catMap

        -- Atom feed
        T.writeFile "_site/atom" (generateAtomFeed config sortedArticles)

      -- Copy static assets
      copyStaticAssets

    phony "serve" $ do
      need ["clean", "build"]
      let port = 8000 :: Int
      putInfo $ "Serving _site on http://localhost:" ++ show port
      liftIO $ Warp.run port $ Static.staticApp
        (Static.defaultFileServerSettings "_site")
          { ssIndices = [unsafeToPiece "index.html"]
          , ssAddTrailingSlash = True
          }

    phony "clean" $ do
      putInfo "Cleaning _site and _build"
      removeFilesAfter "_site" ["//*"]
      removeFilesAfter "_build" ["//*"]

-- =============================================================================
-- Content parsing
-- =============================================================================

isStaticPath :: FilePath -> Bool
isStaticPath f = any (`prefixOf` f) ["files/", "images/", "raw-html/"]
  where
    prefixOf :: String -> String -> Bool
    prefixOf prefix str = take (length prefix) str == prefix

parseAllContent :: [(FilePath, String)] -> IO ([Article], [Page])
parseAllContent files = do
  results <- mapM parseContentFile files
  let articles = [a | Right (Left a) <- results]
      pages = [p | Right (Right p) <- results]
  return (articles, pages)

parseContentFile :: (FilePath, String) -> IO (Either String (Either Article Page))
parseContentFile (path, ext) = do
  content <- T.readFile ("content" </> path)
  let (meta, body) = case ext of
        "org" -> parseOrgMeta content
        _     -> parseMarkdownMeta content

  -- Skip drafts
  if isDraft meta
    then return (Left "draft")
    else do
      let title = fromMaybe (T.pack (takeBaseName path)) (Map.lookup "title" meta)
          slug = toSlug title
          category = fromMaybe "misc" (Map.lookup "category" meta)
          mDate = Map.lookup "date" meta >>= parseDateField
          mModified = Map.lookup "modified" meta >>= parseDateField
          tags = maybe [] parseTags (Map.lookup "tags" meta)
          isPage = "pages/" `prefixOf` path

      case mDate of
        Nothing -> do
          putStrLn $ "Warning: no valid date for " ++ path ++ ", skipping"
          return (Left "no date")
        Just date -> do
          (htmlContent, contentText, summaryHtml, summaryTextVal, footnotesHtml) <- renderPandocWithSummary ext body
          if isPage
            then return $ Right $ Right Page
              { pageTitle = title
              , pageSlug = slug
              , pageDate = date
              , pageContent = htmlContent
              , pageUrl = "pages/" <> slug <> ".html"
              , pageHomeTitle = Map.lookup "home-title" meta
              , pageHomeDesc = Map.lookup "home-desc" meta
              }
            else return $ Right $ Left Article
              { articleTitle = title
              , articleSlug = slug
              , articleCategory = category
              , articleDate = date
              , articleModified = mModified
              , articleTags = tags
              , articleContent = htmlContent
              , articleContentText = contentText
              , articleSummary = summaryHtml
              , articleSummaryText = summaryTextVal
              , articleFootnotesHtml = footnotesHtml
              , articleUrl = slug <> ".html"
              }
  where
    prefixOf :: String -> String -> Bool
    prefixOf prefix str = take (length prefix) str == prefix

-- | Render Pandoc document, returning content HTML (without footnotes),
-- full text, summary HTML (~50 words), and extracted footnotes HTML.
renderPandocWithSummary :: String -> Text -> IO (Html, Text, Maybe Html, Maybe Text, Maybe Html)
renderPandocWithSummary ext body = runIOorExplode $ do
  let ropts = def { readerExtensions = pandocExtensions }
      wopts = def { writerHighlightStyle = Just pygments }
  doc <- case ext of
    "org" -> readOrg ropts body
    _     -> readMarkdown ropts body
  fullHtml <- writeHtml5 wopts doc
  let fullText = lazyToStrictText (renderHtml fullHtml)
      (contentWithoutFn, mFootnotes) = splitFootnotes fullText
      contentHtml = H.preEscapedToHtml contentWithoutFn
      footnotesHtml = fmap H.preEscapedToHtml mFootnotes
      summaryText = truncateHtml 50 fullText
      summaryHtmlVal = H.preEscapedToHtml summaryText
  return (contentHtml, fullText, Just summaryHtmlVal, Just summaryText, footnotesHtml)

lazyToStrictText :: TL.Text -> Text
lazyToStrictText = TL.toStrict

-- | Split rendered HTML into (content-without-footnotes, Maybe footnotes-section).
-- Pandoc generates @<section id="footnotes" class="footnotes"...>@ at the end.
splitFootnotes :: Text -> (Text, Maybe Text)
splitFootnotes html =
  let marker = "<section id=\"footnotes\""
  in case T.breakOn marker html of
    (before, after)
      | T.null after -> (html, Nothing)
      | otherwise    -> (before, Just after)

-- | Truncate rendered HTML to ~N words, closing any unclosed tags.
-- Counts only visible text words, skips inside tags and entities.
truncateHtml :: Int -> Text -> Text
truncateHtml maxWords html =
  let (result, openTags) = go 0 [] (T.unpack html)
      closingTags = concatMap (\t -> "</" ++ t ++ ">") openTags
  in T.pack result <> T.pack closingTags
  where
    -- Track open tag stack, count words in text nodes
    go :: Int -> [String] -> String -> (String, [String])
    go _ tags [] = ([], tags)
    go wc tags _ | wc >= maxWords = ([], tags)
    go wc tags ('<':'/':rest) =
      -- Closing tag: consume until '>', pop from stack
      let (tagContent, after) = span (/= '>') rest
          tagName = takeWhile (\c -> c /= ' ' && c /= '>') tagContent
          closeStr = "</" ++ tagContent ++ ">"
          tags' = dropFirst tagName tags
          (remainder, finalTags) = go wc tags' (drop 1 after)
      in (closeStr ++ remainder, finalTags)
    go wc tags ('<':rest) =
      -- Opening tag: consume until '>', push to stack if not void/self-closing
      let (tagContent, after) = span (/= '>') rest
          tagName = takeWhile (\c -> c /= ' ' && c /= '>' && c /= '/') tagContent
          isSelfClosing = not (null tagContent) && last tagContent == '/'
          isVoid = tagName `elem` voidElements
          tagStr = "<" ++ tagContent ++ ">"
          tags' = if isSelfClosing || isVoid || null tagName
                  then tags
                  else tagName : tags
          (remainder, finalTags) = go wc tags' (drop 1 after)
      in (tagStr ++ remainder, finalTags)
    go wc tags ('&':rest) =
      -- HTML entity: pass through without counting as word
      let (entity, after) = span (/= ';') rest
          entityStr = "&" ++ entity ++ ";"
          (remainder, finalTags) = go wc tags (drop 1 after)
      in (entityStr ++ remainder, finalTags)
    go wc tags (c:rest)
      | c == ' ' || c == '\n' || c == '\t' || c == '\r' =
          let (remainder, finalTags) = go wc tags rest
          in (c : remainder, finalTags)
      | otherwise =
          -- Start of a word: consume until whitespace or tag
          let (word, after) = span (\ch -> ch /= ' ' && ch /= '\n' && ch /= '\t' && ch /= '\r' && ch /= '<' && ch /= '&') rest
              fullWord = c : word
              wc' = wc + 1
              (remainder, finalTags) = go wc' tags after
          in (fullWord ++ remainder, finalTags)

    dropFirst :: String -> [String] -> [String]
    dropFirst _ [] = []
    dropFirst x (y:ys)
      | x == y    = ys
      | otherwise = y : dropFirst x ys

    voidElements :: [String]
    voidElements = ["br", "img", "hr", "input", "meta", "link", "area",
                    "base", "col", "embed", "source", "track", "wbr"]

-- =============================================================================
-- Tag and category maps
-- =============================================================================

buildTagMap :: [Article] -> [(Text, [Article])]
buildTagMap articles =
  let allTags = nub $ concatMap articleTags articles
      tagArts :: Text -> [Article]
      tagArts tag = filter (\a -> tag `elem` articleTags a) articles
  in map (\t -> (t, tagArts t)) allTags

buildCategoryMap :: [Article] -> [(Text, [Article])]
buildCategoryMap articles =
  let allCats = nub $ map articleCategory articles
      catArts :: Text -> [Article]
      catArts cat = filter (\a -> articleCategory a == cat) articles
  in map (\c -> (c, catArts c)) allCats

-- =============================================================================
-- Output helpers
-- =============================================================================

writeHtmlFile :: FilePath -> Html -> IO ()
writeHtmlFile path html = do
  Dir.createDirectoryIfMissing True (takeDirectory path)
  TLIO.writeFile path (renderHtml html)

tagSlugLocal :: Text -> Text
tagSlugLocal = T.intercalate "-" . T.words . T.toLower

-- | Split a list into chunks of at most n elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

-- | File name for a paginated index page: page 1 = "index.html", page 2 = "index2.html", etc.
indexFileName :: Int -> Text
indexFileName 1 = "index.html"
indexFileName n = "index" <> T.pack (show n) <> ".html"

-- =============================================================================
-- Static asset copying
-- =============================================================================

copyStaticAssets :: Action ()
copyStaticAssets = do
  -- theme/static/ -> _site/theme/
  themeFiles <- getDirectoryFiles "theme/static" ["//*"]
  liftIO $ mapM_ (\f -> copyBinaryFile ("theme/static" </> f) ("_site/theme" </> f)) themeFiles

  -- content/images/ -> _site/images/
  copyDirIfExists "content/images" "_site/images"

  -- content/files/ -> _site/files/
  copyDirIfExists "content/files" "_site/files"

  -- content/raw-html/ -> _site/raw-html/
  copyDirIfExists "content/raw-html" "_site/raw-html"

  -- fire/ -> _site/fire/
  copyDirIfExists "fire" "_site/fire"

  -- root/* -> _site/ (root level files)
  rootExists <- liftIO $ Dir.doesDirectoryExist "root"
  if rootExists
    then do
      rootFiles <- getDirectoryFiles "root" ["//*"]
      liftIO $ mapM_ (\f -> copyBinaryFile ("root" </> f) ("_site" </> f)) rootFiles
    else return ()
  where
    copyDirIfExists :: FilePath -> FilePath -> Action ()
    copyDirIfExists src dst = do
      exists <- liftIO $ Dir.doesDirectoryExist src
      if exists
        then do
          files <- getDirectoryFiles src ["//*"]
          liftIO $ mapM_ (\f -> copyBinaryFile (src </> f) (dst </> f)) files
        else return ()

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dst = do
  Dir.createDirectoryIfMissing True (takeDirectory dst)
  BS.readFile src >>= BS.writeFile dst
