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
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (SiteConfig(..), Article(..), Page(..), NavLink(..))

-- Helper to convert Text to AttributeValue
toValue :: Text -> H.AttributeValue
toValue = H.toValue

toHtml :: Text -> Html
toHtml = H.toHtml

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

baseTemplate :: SiteConfig -> Bool -> Text -> Html -> Html
baseTemplate config isArticle title content = H.docTypeHtml ! A.class_ "no-js" ! customAttribute "lang" "en" $ do
  H.head $ do
    H.title (toHtml title)
    H.meta ! A.name "author" ! A.content (toValue (siteAuthor config))
    H.meta ! customAttribute "charset" "utf-8"
    -- Atom feed
    H.link ! A.href (toValue (feedDomain config <> "/" <> feedAtom config))
           ! A.type_ "application/atom+xml"
           ! A.rel "alternate"
           ! A.title (toValue (siteName config <> " Atom Feed"))
    -- Mobile meta
    H.meta ! A.name "HandheldFriendly" ! A.content "True"
    H.meta ! A.name "MobileOptimized" ! A.content "320"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    -- Favicon
    H.link ! A.href (toValue (siteUrl config <> "/favicon.png")) ! A.rel "icon"
    -- CSS: base
    H.link ! A.rel "stylesheet" ! A.href "/theme/css/base.css"
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
    H.link ! A.rel "stylesheet" ! A.href "/theme/css/all.css"
    if isArticle
      then H.link ! A.rel "stylesheet" ! A.href "/theme/css/article.css"
      else H.link ! A.rel "stylesheet" ! A.href "/theme/css/double_collumn.css"
    -- Footnote tooltip JS
    H.script $ H.preEscapedToHtml footnoteScript
  H.body $ do
    content
    -- Footer
    H.footer ! A.class_ "window" ! customAttribute "role" "contentinfo" $ do
      H.p $ H.ul ! A.class_ "footlinks" $
        mapM_ renderFootLink (siteFootLinks config)
      H.p $ H.small $
        "Those who know do not speak. Those who speak do not know."
      H.p $ H.small $ do
        H.a ! A.href "/atom" $
          H.img ! A.class_ "category-glyph" ! A.alt "Atom feed" ! A.src "/theme/images/atom-icon.svg"
        " Powered by "
        H.a ! A.href "https://github.com/jappeace/jappeaceApplication" $ "Shake"
        ". "
        H.a ! A.href "https://github.com/jappeace/jappeaceApplication" $ "Source code"
        ", licensed under GPLv3."
    -- Syntax highlighting CSS last
    H.link ! A.rel "stylesheet" ! A.href "/theme/css/syntax.css"
    -- Pandoc syntax highlighting
    H.link ! A.rel "stylesheet" ! A.href "/theme/css/pandoc-syntax.css"
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

siteTemplate :: SiteConfig -> [Page] -> Bool -> Text -> Html -> Html
siteTemplate config pages isArticle title body =
  baseTemplate config isArticle title $ do
    H.header ! A.id "title" ! A.class_ "window" ! customAttribute "role" "banner" $ do
      H.h1 $ H.a ! A.id "sitename" ! A.href (toValue (siteUrl config <> "/")) $ "Jappie"
      navigationHtml config pages
      H.div ! A.class_ "browser-warning" $
        H.span "\9888\65039 Chrome mobile not supported, please upgrade to Firefox \9888\65039"
    H.div ! A.id "main" $ body

-- =============================================================================
-- Navigation
-- =============================================================================

navigationHtml :: SiteConfig -> [Page] -> Html
navigationHtml config pages = H.nav $
  H.ul ! A.class_ "navigation" $ do
    mapM_ renderNavLink (siteLinks config)
    mapM_ renderPageNavLink pages
    mapM_ renderNavLink (siteSocial config)
  where
    renderNavLink :: NavLink -> Html
    renderNavLink nl =
      H.li ! A.class_ (toValue (navClass nl)) $
        H.a ! A.href (toValue (navUrl nl)) $ do
          H.h3 ! A.class_ "bracket left-bracket" $ "&lt;"
          H.div ! A.class_ "link" $ do
            H.h2 $ toHtml (navTitle nl)
            H.p ! A.class_ "description" $ toHtml (navDesc nl)
          H.h3 ! A.class_ "bracket right-bracket" $ "&gt;"

    renderPageNavLink :: Page -> Html
    renderPageNavLink page =
      let title' = case pageHomeTitle page of
            Just ht -> ht
            Nothing -> pageTitle page
          desc = case pageHomeDesc page of
            Just hd -> hd
            Nothing -> ""
      in H.li $
          H.a ! A.href (toValue ("/" <> pageUrl page)) $ do
            H.h3 ! A.class_ "bracket left-bracket" $ "&lt;"
            H.div ! A.class_ "link" $ do
              H.h2 $ toHtml title'
              H.p ! A.class_ "description" $ toHtml desc
            H.h3 ! A.class_ "bracket right-bracket" $ "&gt;"

-- =============================================================================
-- Category glyph
-- =============================================================================

categoryGlyph :: Text -> Html
categoryGlyph catname =
  H.img ! A.class_ "category-glyph"
        ! A.src (toValue ("/theme/images/category-" <> catname <> ".svg"))
        ! customAttribute "onerror" onerrorVal
        ! A.alt (toValue ("[" <> catname <> "]"))
  where
    onerrorVal :: Text
    onerrorVal = "this.src='/theme/images/category-" <> catname <> ".png'; this.onerror=null;"

-- =============================================================================
-- Article page
-- =============================================================================

renderArticlePage :: SiteConfig -> [Page] -> Article -> [Article] -> [(Text, [Article])] -> Html
renderArticlePage config pages article allArticles tags =
  siteTemplate config pages True (articleTitle article <> " / " <> siteName config) $ do
    H.article ! A.class_ "single" ! customAttribute "role" "article" $ do
      -- Header
      articleHead article
      -- Content
      H.div ! A.class_ "entry-content" $
        articleContent article
      -- Subreddit
      case articleSubreddit article of
        Just sr -> H.small $ toHtml sr
        Nothing -> mempty
      -- Footer with metadata
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
    -- Recent posts
    H.section $ do
      H.h1 $ H.a ! A.href "/archives.html" $ "Recent stuff"
      H.ul ! A.id "recent_posts" $
        mapM_ recentPost (take 8 allArticles)
    -- Tags sidebar
    H.section $ do
      H.h1 $ H.a ! A.href "/tags.html" $ "Tags"
      H.div ! A.class_ "sidebar-tags" $
        mapM_ renderSidebarTag (filter (\(_, arts) -> length arts > 1) tags)
  where
    recentPost :: Article -> Html
    recentPost a =
      H.li ! A.class_ "post" $
        H.a ! A.href (toValue ("/" <> articleUrl a)) $ do
          categoryGlyph (articleCategory a)
          toHtml (" " <> articleTitle a)

    renderSidebarTag :: (Text, [Article]) -> Html
    renderSidebarTag (tag, _) =
      H.a ! A.class_ "tag" ! A.href (toValue ("/tag/" <> tagSlug tag <> ".html")) $ toHtml tag

-- =============================================================================
-- Article header/footer helpers
-- =============================================================================

articleHead :: Article -> Html
articleHead article = H.header $ do
  H.h1 $
    H.a ! A.href (toValue ("/" <> articleUrl article)) $ do
      toHtml (articleTitle article <> " ")
      categoryGlyph (articleCategory article)
  H.p ! A.class_ "meta" $
    articleTime article

articleTime :: Article -> Html
articleTime article = do
  H.time ! customAttribute "datetime" (formatIsoDate (articleDate article))
         ! customAttribute "pubdate" "" $ do
    "published: "
    toHtml (formatLocaleDate (articleDate article))
  case articleModified article of
    Just m ->
      H.time ! customAttribute "datetime" (formatIsoDate m) $ do
        ", last modified: "
        toHtml (formatLocaleDate m)
    Nothing -> mempty

articleFooter :: SiteConfig -> Article -> Html
articleFooter _config article =
  H.ul ! A.class_ "meta" $ do
    H.li ! A.class_ "byline author vcard" $ do
      "Posted by "
      H.span ! A.class_ "fn" $ toHtml (("Jappie J. T. Klooster") :: Text)
      " in "
      H.a ! A.class_ "category" ! A.href (toValue ("/category/" <> articleCategory article <> ".html")) $ do
        categoryGlyph (articleCategory article)
        " "
        toHtml (articleCategory article)
    H.li $ articleTime article
    if null (articleTags article)
      then mempty
      else H.li ! A.class_ "tags" $
             mapM_ renderTag (articleTags article)
  where
    renderTag :: Text -> Html
    renderTag tag =
      H.a ! A.class_ "tag" ! A.href (toValue ("/tag/" <> tagSlug tag <> ".html")) $
        toHtml ("#" <> tag)

-- =============================================================================
-- Page
-- =============================================================================

renderPagePage :: SiteConfig -> [Page] -> Page -> Html
renderPagePage config pages page =
  siteTemplate config pages False (pageTitle page <> " / " <> siteName config) $
    H.article ! A.class_ "single" ! customAttribute "role" "article" $ do
      H.header $
        H.h1 $
          H.a ! A.href (toValue ("/" <> pageUrl page)) $ toHtml (pageTitle page)
      H.div ! A.class_ "entry-content" $
        pageContent page

-- =============================================================================
-- Index page (article list without pagination since DEFAULT_PAGINATION = False)
-- =============================================================================

renderIndexPage :: SiteConfig -> [Page] -> [Article] -> Html
renderIndexPage config pages articles =
  siteTemplate config pages False (siteName config) $
    mapM_ renderArticleSummary articles

renderArticleSummary :: Article -> Html
renderArticleSummary article =
  H.article $ do
    articleHead article
    H.div ! A.class_ "entry-content" $
      case articleSummary article of
        Just s  -> s
        Nothing -> articleContent article
    H.footer $
      H.a ! A.rel "full-article" ! A.href (toValue ("/" <> articleUrl article)) $
        "Could there be more?"

-- =============================================================================
-- Archives page
-- =============================================================================

renderArchivesPage :: SiteConfig -> [Page] -> [Article] -> Html
renderArchivesPage config pages articles =
  siteTemplate config pages False ("Archive / " <> siteName config) $
    H.section ! A.class_ "archives" $ do
      H.h1 "Archive"
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
        H.a ! A.href (toValue ("/" <> articleUrl article)) $ do
          categoryGlyph (articleCategory article)
          " "
          toHtml (articleTitle article)
      H.footer $
        H.ul ! A.class_ "meta" $
          if null (articleTags article)
            then mempty
            else H.li ! A.class_ "tags" $
                   mapM_ (\tag -> H.a ! A.class_ "tag"
                                      ! A.href (toValue ("/tag/" <> tagSlug tag <> ".html"))
                                      $ toHtml ("#" <> tag))
                         (articleTags article)

-- =============================================================================
-- Tag pages
-- =============================================================================

renderTagsListPage :: SiteConfig -> [Page] -> [(Text, [Article])] -> Html
renderTagsListPage config pages tags =
  siteTemplate config pages False ("Tags / " <> siteName config) $
    H.section ! A.class_ "tags" $ do
      H.h1 "Tags"
      H.p "This page sure is exciting, wow!"
      H.ul $
        mapM_ renderTagItem tags
  where
    renderTagItem :: (Text, [Article]) -> Html
    renderTagItem (tag, arts) =
      H.li $ do
        H.a ! A.href (toValue ("/tag/" <> tagSlug tag <> ".html")) $ toHtml tag
        toHtml ((" \8212 " <> T.pack (show (length arts))) :: Text)

renderTagPage :: SiteConfig -> [Page] -> Text -> [Article] -> Html
renderTagPage config pages tag articles =
  siteTemplate config pages False ("\128278 " <> tag <> " / " <> siteName config) $ do
    H.h1 $ toHtml (("Tagged: " <> tag) :: Text)
    mapM_ renderArticleSummary articles

-- =============================================================================
-- Category pages
-- =============================================================================

renderCategoriesListPage :: SiteConfig -> [Page] -> [(Text, [Article])] -> Html
renderCategoriesListPage config pages categories =
  siteTemplate config pages False ("Categories / " <> siteName config) $
    H.section ! A.class_ "categories" $ do
      H.h1 "Categories"
      H.ul $
        mapM_ renderCatItem categories
  where
    renderCatItem :: (Text, [Article]) -> Html
    renderCatItem (cat, arts) =
      H.li $ do
        H.a ! A.href (toValue ("/category/" <> cat <> ".html")) $ do
          categoryGlyph cat
          " "
          toHtml cat
        toHtml ((" \8212 " <> T.pack (show (length arts))) :: Text)

renderCategoryPage :: SiteConfig -> [Page] -> Text -> [Article] -> Html
renderCategoryPage config pages cat articles =
  siteTemplate config pages False (cat <> " / " <> siteName config) $ do
    H.h1 ! A.id "category-title" $ do
      categoryGlyph cat
      " "
      toHtml cat
    mapM_ renderArticleSummary articles

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
