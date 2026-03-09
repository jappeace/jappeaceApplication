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
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
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

        -- Index
        writeHtmlFile "_site/index.html"
          (renderIndexPage config pages sortedArticles)

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
      need ["build"]
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
          subreddit = Map.lookup "subreddit" meta
          isPage = "pages/" `prefixOf` path

      case mDate of
        Nothing -> do
          putStrLn $ "Warning: no valid date for " ++ path ++ ", skipping"
          return (Left "no date")
        Just date -> do
          (htmlContent, contentText, summaryHtml, summaryTextVal) <- renderPandocWithSummary ext body
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
              , articleSubreddit = subreddit
              , articleUrl = slug <> ".html"
              }
  where
    prefixOf :: String -> String -> Bool
    prefixOf prefix str = take (length prefix) str == prefix

-- | Render Pandoc document, returning full HTML + summary HTML (~50 words)
renderPandocWithSummary :: String -> Text -> IO (Html, Text, Maybe Html, Maybe Text)
renderPandocWithSummary ext body = runIOorExplode $ do
  let ropts = def { readerExtensions = pandocExtensions }
      wopts = def { writerHighlightStyle = Just pygments }
  doc <- case ext of
    "org" -> readOrg ropts body
    _     -> readMarkdown ropts body
  fullHtml <- writeHtml5 wopts doc
  let fullText = lazyToStrictText (renderHtml fullHtml)
      summaryDoc = summarizeDoc doc
  summaryHtmlVal <- writeHtml5 wopts summaryDoc
  let summaryText = lazyToStrictText (renderHtml summaryHtmlVal)
  return (fullHtml, fullText, Just summaryHtmlVal, Just summaryText)

lazyToStrictText :: TL.Text -> Text
lazyToStrictText = TL.toStrict

-- | Take blocks from a Pandoc document until ~50 words
summarizeDoc :: Pandoc -> Pandoc
summarizeDoc (Pandoc meta blocks) = Pandoc meta (takeBlocksUntilWords 50 blocks)

takeBlocksUntilWords :: Int -> [Block] -> [Block]
takeBlocksUntilWords _ [] = []
takeBlocksUntilWords remaining (b:bs)
  | remaining <= 0 = []
  | otherwise =
      let wc = blockWordCount b
      in b : takeBlocksUntilWords (remaining - wc) bs

blockWordCount :: Block -> Int
blockWordCount block = case block of
  Para inlines        -> inlineWordsCount inlines
  Plain inlines       -> inlineWordsCount inlines
  Header _ _ inlines  -> inlineWordsCount inlines
  BlockQuote blocks   -> sum (map blockWordCount blocks)
  BulletList items    -> sum (map (sum . map blockWordCount) items)
  OrderedList _ items -> sum (map (sum . map blockWordCount) items)
  DefinitionList defs -> sum [inlineWordsCount term + sum (map (sum . map blockWordCount) defn) | (term, defn) <- defs]
  Div _ blocks        -> sum (map blockWordCount blocks)
  LineBlock lns       -> sum (map inlineWordsCount lns)
  CodeBlock _ code    -> length (T.words code)
  RawBlock _ raw      -> length (T.words raw)
  Table {}            -> 20 -- rough estimate
  Figure _ _ blocks   -> sum (map blockWordCount blocks)
  HorizontalRule      -> 0

inlineWordsCount :: [Inline] -> Int
inlineWordsCount = sum . map inlineWordCount

inlineWordCount :: Inline -> Int
inlineWordCount inline = case inline of
  Str t           -> length (T.words t)
  Space           -> 0
  SoftBreak       -> 0
  LineBreak       -> 0
  Emph inlines    -> inlineWordsCount inlines
  Underline inlines -> inlineWordsCount inlines
  Strong inlines  -> inlineWordsCount inlines
  Strikeout inlines -> inlineWordsCount inlines
  Superscript inlines -> inlineWordsCount inlines
  Subscript inlines -> inlineWordsCount inlines
  SmallCaps inlines -> inlineWordsCount inlines
  Quoted _ inlines -> inlineWordsCount inlines
  Cite _ inlines  -> inlineWordsCount inlines
  Code _ t        -> length (T.words t)
  Math _ t        -> length (T.words t)
  RawInline _ t   -> length (T.words t)
  Link _ inlines _ -> inlineWordsCount inlines
  Image _ inlines _ -> inlineWordsCount inlines
  Note blocks     -> sum (map blockWordCount blocks)
  Span _ inlines  -> inlineWordsCount inlines

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
