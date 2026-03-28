{-# LANGUAGE OverloadedStrings #-}
module Templates
  ( renderArticlePage
  , renderPagePage
  , renderIndexPage
  , renderArchivesPage
  , renderTagPage
  , renderTagsListPage
  , renderCategoryPage
  , renderCategoriesListPage
  ) where

import Data.List (groupBy, sortBy)
import Data.Maybe (isJust)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types
  ( SiteConfig(..), Article(..), Page(..), NavLink(..), PaginationInfo(..)
  , Translations(..), translationsFor, langPrefix, langCode
  )

-- Helper to convert Text to AttributeValue
toValue :: Text -> H.AttributeValue
toValue = H.toValue

toHtml :: Text -> Html
toHtml = H.toHtml

-- | Root-relative URL (works locally and in production)
absUrl :: SiteConfig -> Text -> Text
absUrl _config path = path

-- | Build a root-relative URL respecting the current language prefix.
-- For English: /foo.html, for Dutch: /nl/foo.html
langUrl :: SiteConfig -> Text -> Text
langUrl config path =
  let prefix = langPrefix (siteLang config)
  in "/" <> prefix <> T.dropWhile (== '/') path

-- Date formatting
formatDate :: Text -> UTCTime -> Text
formatDate fmt t = T.pack $ formatTime defaultTimeLocale (T.unpack fmt) t

formatLocaleDate :: UTCTime -> Text
formatLocaleDate = formatDate "%Y\24180%m\26376%d\26085"

formatIsoDate :: UTCTime -> Text
formatIsoDate = formatDate "%Y-%m-%dT%H:%M:%SZ"

formatArchiveDate :: UTCTime -> Text
formatArchiveDate = formatDate "%b %d"

-- =============================================================================
-- Base template (_base.html equivalent)
-- =============================================================================

-- | The @switchUrl@ parameter is @Just url@ when a translation exists for the
-- current page in the other language, @Nothing@ otherwise (no toggle shown).
baseTemplate :: SiteConfig -> Bool -> Maybe Text -> Text -> Html -> Html
baseTemplate config isArticle mSwitchUrl title content =
  let lang = siteLang config
      trans = translationsFor lang
  in H.docTypeHtml ! A.class_ "no-js" ! customAttribute "lang" (langCode lang) $ do
  H.head $ do
    H.title (toHtml title)
    H.meta ! A.name "author" ! A.content (toValue (siteAuthor config))
    H.meta ! customAttribute "charset" "utf-8"
    -- Atom feed
    H.link ! A.href (toValue (feedDomain config <> "/" <> langPrefix lang <> feedAtom config))
           ! A.type_ "application/atom+xml"
           ! A.rel "alternate"
           ! A.title (toValue (siteName config <> " Atom Feed"))
    -- Mobile meta
    H.meta ! A.name "HandheldFriendly" ! A.content "True"
    H.meta ! A.name "MobileOptimized" ! A.content "320"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    -- Favicon
    H.link ! A.href (toValue (absUrl config "/favicon.png")) ! A.rel "icon"
    -- CSS: base
    H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/base.css"))
    -- MathJax for articles
    if isArticle
      then do
        H.script ! A.type_ "text/x-mathjax-config" $
          "MathJax.Hub.Config({\"HTML-CSS\": {styles: {\".MathJax .mo, .MathJax .mi\": {color: \"black ! important\"}}}, tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']],processEscapes: true}});"
        H.script ! A.type_ "text/javascript"
                 ! A.src "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"
                 $ mempty
      else mempty
    -- Google Analytics
    H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=UA-120139048-1" $ mempty
    H.script $ "window.dataLayer = window.dataLayer || [];\nfunction gtag(){dataLayer.push(arguments);}\ngtag('js', new Date());\ngtag('config', 'UA-120139048-1');\ngtag('config', 'G-SJ36NEDJPD');"
    -- CSS: article or double column
    H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/all.css"))
    if isArticle
      then H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/article.css"))
      else H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/double_collumn.css"))
    -- Footnote tooltip JS
    H.script $ H.preEscapedToHtml footnoteScript
  H.body $ do
    content
    -- Footer
    H.footer ! A.class_ "window" ! customAttribute "role" "contentinfo" $ do
      H.p $ H.ul ! A.class_ "footlinks" $
        mapM_ renderFootLink (siteFootLinks config)
      H.p $ H.small $
        toHtml (tFooterQuote trans)
      H.p $ H.small $ do
        H.a ! A.href (toValue (langUrl config (feedAtom config))) $
          H.img ! A.class_ "category-glyph" ! A.alt "Atom feed" ! A.src (toValue (absUrl config "/theme/images/atom-icon.svg"))
        " Powered by "
        H.a ! A.href "https://hackage.haskell.org/package/shake" $ "Shake"
        ". "
        H.a ! A.href "https://github.com/jappeace/jappeaceApplication" $ "Source code"
        ", licensed under GPLv3."
      -- Language toggle in footer (if translation exists)
      case mSwitchUrl of
        Just switchUrl ->
          H.p ! A.class_ "lang-switch" $
            H.a ! A.href (toValue switchUrl)
                ! customAttribute "onclick" "localStorage.setItem('lang',this.href.indexOf('/nl/')>=0?'nl':'en')"
                $ toHtml (tSwitchLang trans <> " (" <> tSwitchLangDesc trans <> ")")
        Nothing -> mempty
    -- Syntax highlighting CSS last
    H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/syntax.css"))
    -- Pandoc syntax highlighting
    H.link ! A.rel "stylesheet" ! A.href (toValue (absUrl config "/theme/css/pandoc-syntax.css"))
  where
    renderFootLink :: (Text, Text) -> Html
    renderFootLink (name, url) =
      H.li $ H.a ! A.href (toValue url) $ toHtml name

-- custom HTML attribute helper
customAttribute :: Text -> Text -> H.Attribute
customAttribute name val = H.customAttribute (H.textTag name) (toValue val)

-- =============================================================================
-- Site template (base.html equivalent - header + navigation)
-- =============================================================================

siteTemplate :: SiteConfig -> [Page] -> Bool -> Maybe Text -> Text -> Html -> Html
siteTemplate config pages isArticle mSwitchUrl title body =
  let lang = siteLang config
      trans = translationsFor lang
  in baseTemplate config isArticle mSwitchUrl title $ do
    H.header ! A.id "title" ! A.class_ "window" ! customAttribute "role" "banner" $ do
      H.h1 $ H.a ! A.id "sitename" ! A.href (toValue (langUrl config "")) $ "Jappie"
      navigationHtml config pages mSwitchUrl trans
      H.div ! A.class_ "browser-warning" $
        H.span "\9888\65039 Chrome mobile not supported, please upgrade to Firefox \9888\65039"
    H.div ! A.id "main" $ body

-- =============================================================================
-- Navigation
-- =============================================================================

navigationHtml :: SiteConfig -> [Page] -> Maybe Text -> Translations -> Html
navigationHtml config pages mSwitchUrl trans = H.nav $
  H.ul ! A.class_ "navigation" $ do
    mapM_ (\nl -> renderNavLink nl >> "\n") (siteLinks config)
    mapM_ (\p -> renderPageNavLink p >> "\n") (filter (isJust . pageHomeTitle) pages)
    mapM_ (\nl -> renderNavLink nl >> "\n") (siteSocial config)
    -- Language toggle in navigation
    case mSwitchUrl of
      Just switchUrl ->
        H.li ! A.class_ "lang-switch" $
          H.a ! A.href (toValue switchUrl)
              ! customAttribute "onclick" "localStorage.setItem('lang',this.href.indexOf('/nl/')>=0?'nl':'en')"
              $ do
            H.h3 ! A.class_ "bracket left-bracket" $ H.preEscapedToHtml ("&lt;" :: Text)
            H.div ! A.class_ "link" $ do
              H.h2 $ toHtml (tSwitchLang trans)
              H.p ! A.class_ "description" $ toHtml (tSwitchLangDesc trans)
            H.h3 ! A.class_ "bracket right-bracket" $ H.preEscapedToHtml ("&gt;" :: Text)
      Nothing -> mempty
  where
    renderNavLink :: NavLink -> Html
    renderNavLink nl =
      H.li ! A.class_ (toValue (navClass nl)) $
        H.a ! A.href (toValue (navUrl nl)) $ do
          H.h3 ! A.class_ "bracket left-bracket" $ H.preEscapedToHtml ("&lt;" :: Text)
          H.div ! A.class_ "link" $ do
            H.h2 $ toHtml (navTitle nl)
            H.p ! A.class_ "description" $ toHtml (navDesc nl)
          H.h3 ! A.class_ "bracket right-bracket" $ H.preEscapedToHtml ("&gt;" :: Text)

    renderPageNavLink :: Page -> Html
    renderPageNavLink page =
      let title' = case pageHomeTitle page of
            Just ht -> ht
            Nothing -> pageTitle page
          desc = case pageHomeDesc page of
            Just hd -> hd
            Nothing -> ""
      in H.li $
          H.a ! A.href (toValue (langUrl config (pageUrl page))) $ do
            H.h3 ! A.class_ "bracket left-bracket" $ H.preEscapedToHtml ("&lt;" :: Text)
            H.div ! A.class_ "link" $ do
              H.h2 $ toHtml title'
              H.p ! A.class_ "description" $ toHtml desc
            H.h3 ! A.class_ "bracket right-bracket" $ H.preEscapedToHtml ("&gt;" :: Text)

-- =============================================================================
-- Category glyph
-- =============================================================================

categoryGlyph :: SiteConfig -> Text -> Html
categoryGlyph config catname =
  H.img ! A.class_ "category-glyph"
        ! A.src (toValue (absUrl config ("/theme/images/category-" <> catname <> ".svg")))
        ! customAttribute "onerror" onerrorVal
        ! A.alt "[]"
  where
    onerrorVal :: Text
    onerrorVal = "this.src='" <> absUrl config ("/theme/images/category-" <> catname <> ".png") <> "'; this.onerror=null;"

-- =============================================================================
-- Article page
-- =============================================================================

renderArticlePage :: SiteConfig -> [Page] -> Maybe Text -> Article -> [Article] -> [(Text, [Article])] -> Html
renderArticlePage config pages mSwitchUrl article allArticles tags =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages True mSwitchUrl (articleTitle article <> " / " <> siteName config) $ do
    H.article ! A.class_ "single" ! customAttribute "role" "article" $ do
      -- Header
      articleHead config article
      -- Content (without footnotes)
      H.div ! A.class_ "entry-content" $
        articleContent article
      -- Footer with metadata + utterances
      H.footer $ do
        articleFooter config article
        H.div ! A.class_ "comments" $
          H.script ! A.src "https://utteranc.es/client.js"
                   ! customAttribute "repo" "jappeace/jappeaceApplication"
                   ! customAttribute "issue-term" "url"
                   ! customAttribute "label" "Utterances"
                   ! customAttribute "theme" "github-light"
                   ! customAttribute "crossorigin" "anonymous"
                   ! A.async ""
                   $ mempty
      -- Footnotes section (after utterances)
      case articleFootnotesHtml article of
        Just fn -> fn
        Nothing -> mempty
    -- Recent posts
    H.section $ do
      H.h1 $ H.a ! A.href (toValue (langUrl config "")) $ toHtml (tRecentStuff trans)
      H.ul ! A.id "recent_posts" $
        mapM_ recentPost (take 8 allArticles)
    -- Tags sidebar
    H.section $ do
      H.h1 $ H.a ! A.href (toValue (langUrl config "")) $ toHtml (tTags trans)
      H.div ! A.class_ "sidebar-tags" $
        mapM_ renderSidebarTag (sortBy (comparing fst) (filter (\(_, arts) -> length arts > 1) tags))
  where
    recentPost :: Article -> Html
    recentPost a =
      H.li ! A.class_ "post" $
        H.a ! A.href (toValue (langUrl config (articleUrl a))) $ do
          categoryGlyph config (articleCategory a)
          toHtml (" " <> articleTitle a)

    renderSidebarTag :: (Text, [Article]) -> Html
    renderSidebarTag (tag, _) =
      H.a ! A.class_ "tag" ! A.href (toValue (langUrl config ("tag/" <> tagSlug tag <> ".html"))) $ toHtml tag

-- =============================================================================
-- Article header/footer helpers
-- =============================================================================

articleHead :: SiteConfig -> Article -> Html
articleHead config article = H.header $ do
  H.h1 $
    H.a ! A.href (toValue (langUrl config (articleUrl article))) $ do
      toHtml (articleTitle article <> " ")
      categoryGlyph config (articleCategory article)
  H.p ! A.class_ "meta" $
    articleTime config article

articleTime :: SiteConfig -> Article -> Html
articleTime config article =
  let trans = translationsFor (siteLang config)
  in do
    H.time ! customAttribute "datetime" (formatIsoDate (articleDate article))
           ! customAttribute "pubdate" "" $ do
      toHtml (tPublished trans)
      toHtml (formatLocaleDate (articleDate article))
    case articleModified article of
      Just m ->
        H.time ! customAttribute "datetime" (formatIsoDate m) $ do
          toHtml (tLastModified trans)
          toHtml (formatLocaleDate m)
      Nothing -> mempty

articleFooter :: SiteConfig -> Article -> Html
articleFooter config article =
  let trans = translationsFor (siteLang config)
  in H.ul ! A.class_ "meta" $ do
    H.li ! A.class_ "byline author vcard" $ do
      toHtml (tPostedBy trans)
      H.span ! A.class_ "fn" $ toHtml (("Jappie J. T. Klooster") :: Text)
      " in "
      H.a ! A.class_ "category" ! A.href (toValue (langUrl config ("category/" <> articleCategory article <> ".html"))) $ do
        categoryGlyph config (articleCategory article)
        " "
        toHtml (articleCategory article)
    H.li $ articleTime config article
    if null (articleTags article)
      then mempty
      else H.li ! A.class_ "tags" $
             mapM_ renderTag (articleTags article)
  where
    renderTag :: Text -> Html
    renderTag tag =
      H.a ! A.class_ "tag" ! A.href (toValue (langUrl config ("tag/" <> tagSlug tag <> ".html"))) $
        toHtml ("#" <> tag)

-- =============================================================================
-- Page
-- =============================================================================

renderPagePage :: SiteConfig -> [Page] -> Maybe Text -> Page -> Html
renderPagePage config pages mSwitchUrl page =
  siteTemplate config pages False mSwitchUrl (pageTitle page <> " / " <> siteName config) $
    H.article ! A.class_ "single" ! customAttribute "role" "article" $ do
      H.header $
        H.h1 $
          H.a ! A.href (toValue (langUrl config (pageUrl page))) $ toHtml (pageTitle page)
      H.div ! A.class_ "entry-content" $
        pageContent page

-- =============================================================================
-- Index page (paginated, 10 articles per page)
-- =============================================================================

renderIndexPage :: SiteConfig -> [Page] -> Maybe Text -> [Article] -> PaginationInfo -> Html
renderIndexPage config pages mSwitchUrl articles pagination =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages False mSwitchUrl (siteName config) $ do
    mapM_ (renderArticleSummary config) articles
    H.footer ! A.class_ "pagination" $ do
      case paginationPrevUrl pagination of
        Just url -> H.a ! A.class_ "prev" ! A.href (toValue (absUrl config url)) $ toHtml (tNewer trans)
        Nothing  -> H.span ! A.class_ "prev" $ mempty
      H.a ! A.href (toValue (langUrl config "archives.html")) $ toHtml (tBlogArchive trans)
      case paginationNextUrl pagination of
        Just url -> H.a ! A.class_ "next" ! A.href (toValue (absUrl config url)) $ toHtml (tOlder trans)
        Nothing  -> H.span ! A.class_ "next" $ mempty

renderArticleSummary :: SiteConfig -> Article -> Html
renderArticleSummary config article =
  let trans = translationsFor (siteLang config)
  in H.article $ do
    articleHead config article
    H.div ! A.class_ "entry-content" $
      case articleSummary article of
        Just s  -> s
        Nothing -> articleContent article
    H.footer $
      H.a ! A.rel "full-article" ! A.href (toValue (langUrl config (articleUrl article))) $
        toHtml (tReadMore trans)

-- =============================================================================
-- Archives page
-- =============================================================================

renderArchivesPage :: SiteConfig -> [Page] -> Maybe Text -> [Article] -> Html
renderArchivesPage config pages mSwitchUrl articles =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages False mSwitchUrl (tArchive trans <> " / " <> siteName config) $
    H.section ! A.class_ "archives" $ do
      H.h1 $ toHtml (tArchive trans)
      mapM_ renderYearGroup yearGroups
  where
    yearGroups :: [[Article]]
    yearGroups = groupBy sameYear (sortBy (\a b -> compare (Down (articleDate a)) (Down (articleDate b))) articles)

    sameYear :: Article -> Article -> Bool
    sameYear a b = formatDate "%Y" (articleDate a) == formatDate "%Y" (articleDate b)

    renderYearGroup :: [Article] -> Html
    renderYearGroup [] = mempty
    renderYearGroup grp@(first':_) = do
      H.hr
      H.h2 $ toHtml (formatDate "%Y" (articleDate first'))
      mapM_ renderArchiveArticle grp

    renderArchiveArticle :: Article -> Html
    renderArchiveArticle article = H.article $ do
      H.time ! customAttribute "datetime" (formatIsoDate (articleDate article))
             ! customAttribute "pubdate" "" $
        toHtml (formatArchiveDate (articleDate article))
      H.h1 ! A.class_ (toValue ("category-" <> articleCategory article)) $
        H.a ! A.href (toValue (langUrl config (articleUrl article))) $ do
          categoryGlyph config (articleCategory article)
          " "
          toHtml (articleTitle article)
      H.footer $
        H.ul ! A.class_ "meta" $
          if null (articleTags article)
            then mempty
            else H.li ! A.class_ "tags" $
                   mapM_ (\tag -> H.a ! A.class_ "tag"
                                      ! A.href (toValue (langUrl config ("tag/" <> tagSlug tag <> ".html")))
                                      $ toHtml ("#" <> tag))
                         (articleTags article)

-- =============================================================================
-- Tag pages
-- =============================================================================

renderTagsListPage :: SiteConfig -> [Page] -> Maybe Text -> [(Text, [Article])] -> Html
renderTagsListPage config pages mSwitchUrl tags =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages False mSwitchUrl (tTags trans <> " / " <> siteName config) $
    H.section ! A.class_ "tags" $ do
      H.h1 $ toHtml (tTags trans)
      H.p "This page sure is exciting, wow!"
      H.ul $
        mapM_ renderTagItem (sortBy (comparing fst) tags)
  where
    renderTagItem :: (Text, [Article]) -> Html
    renderTagItem (tag, arts) =
      H.li $ do
        H.a ! A.href (toValue (langUrl config ("tag/" <> tagSlug tag <> ".html"))) $ toHtml tag
        toHtml ((" \8212 " <> T.pack (show (length arts))) :: Text)

renderTagPage :: SiteConfig -> [Page] -> Maybe Text -> Text -> [Article] -> Html
renderTagPage config pages mSwitchUrl tag articles =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages False mSwitchUrl ("\128278 " <> tag <> " / " <> siteName config) $ do
    H.h1 $ toHtml (tTagged trans <> tag)
    mapM_ (renderArticleSummary config) articles

-- =============================================================================
-- Category pages
-- =============================================================================

-- | Fix 10: No article counts on categories page
renderCategoriesListPage :: SiteConfig -> [Page] -> Maybe Text -> [(Text, [Article])] -> Html
renderCategoriesListPage config pages mSwitchUrl categories =
  let trans = translationsFor (siteLang config)
  in siteTemplate config pages False mSwitchUrl (tCategories trans <> " / " <> siteName config) $
    H.section ! A.class_ "categories" $ do
      H.h1 $ toHtml (tCategories trans)
      H.ul $
        mapM_ renderCatItem categories
  where
    renderCatItem :: (Text, [Article]) -> Html
    renderCatItem (cat, _arts) =
      H.li $
        H.a ! A.href (toValue (langUrl config ("category/" <> cat <> ".html"))) $ do
          categoryGlyph config cat
          " "
          toHtml cat

renderCategoryPage :: SiteConfig -> [Page] -> Maybe Text -> Text -> [Article] -> Html
renderCategoryPage config pages mSwitchUrl cat articles =
  siteTemplate config pages False mSwitchUrl (cat <> " / " <> siteName config) $ do
    H.h1 ! A.id "category-title" $ do
      categoryGlyph config cat
      " "
      toHtml cat
    mapM_ (renderArticleSummary config) articles

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Simple tag slug: lowercase, spaces to hyphens
tagSlug :: Text -> Text
tagSlug = T.intercalate "-" . T.words . T.toLower

-- | JavaScript for footnote hover tooltips
footnoteScript :: Text
footnoteScript = T.unlines
  [ "document.addEventListener('DOMContentLoaded', () => {"
  , "  const links = document.querySelectorAll('a.footnote-ref[href^=\"#\"]');"
  , "  links.forEach(link => {"
  , "    const getTarget = () => {"
  , "      const id = (link.hash || '').slice(1);"
  , "      return id ? document.getElementById(id) : null;"
  , "    };"
  , "    link.addEventListener('mouseenter', (e) => {"
  , "      const t = getTarget();"
  , "      if (!t) return;"
  , "      t.style.position = 'fixed';"
  , "      t.style.zIndex = '9999';"
  , "      t.style.left = (e.clientX + 12) + 'px';"
  , "      t.style.top  = (e.clientY + 12) + 'px';"
  , "    });"
  , "    link.addEventListener('mousemove', (e) => {"
  , "      const t = getTarget();"
  , "      if (!t) return;"
  , "      t.style.left = (e.clientX + 12) + 'px';"
  , "      t.style.top  = (e.clientY + 12) + 'px';"
  , "    });"
  , "    link.addEventListener('mouseleave', () => {"
  , "      const t = getTarget();"
  , "      if (!t) return;"
  , "      reset(t);"
  , "    });"
  , "    function reset(t) {"
  , "      t.style.left = '';"
  , "      t.style.top = '';"
  , "      t.style.position = '';"
  , "      t.style.zIndex = '';"
  , "    }"
  , "    link.addEventListener('mousedown', () => {"
  , "      const t = getTarget();"
  , "      if (t) reset(t);"
  , "    });"
  , "  });"
  , "  if (/Chrome/.test(navigator.userAgent) && /Mobile/.test(navigator.userAgent)) {"
  , "      document.querySelector('.browser-warning').style.display = 'block';"
  , "  }"
  , "});"
  ]

