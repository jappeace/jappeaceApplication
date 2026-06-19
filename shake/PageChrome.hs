{-# LANGUAGE OverloadedStrings #-}

-- | Chrome shared by the two Jappie Software B.V. brand sites
-- (jappiesoftware.com and webwinkelverhuis.nl): per-page SEO metadata,
-- structured data (JSON-LD), the social-share image fallback, blog-listing
-- markup and the small date/HTML helpers. Both 'PenguinTemplates' and
-- 'WebwinkelTemplates' render their own navigation and footer on top of this.
module PageChrome
  ( PageMeta(..)
  , defaultPageMeta
  , toValue
  , toHtml
  , customAttribute
  , ogLocale
  , resolveOgImage
  , companyEmail
  , organizationJsonLd
  , serviceJsonLd
  , faqPageJsonLd
  , jsonLdString
  , formatIsoDate
  , formatHumanDate
  , stripHtmlTags
  , articleMetaDescription
  , renderBlogSummary
  , renderPagination
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (Article(..), PaginationInfo(..))

-- | Convert 'Text' to a blaze attribute value.
toValue :: Text -> H.AttributeValue
toValue = H.toValue

-- | Convert 'Text' to blaze 'Html'.
toHtml :: Text -> Html
toHtml = H.toHtml

-- | Custom HTML attribute helper (for @property=@ and the like).
customAttribute :: Text -> Text -> H.Attribute
customAttribute name val = H.customAttribute (H.textTag name) (toValue val)

-- =============================================================================
-- Per-page SEO metadata
-- =============================================================================

-- | SEO metadata carried by each page for the base template to render.
data PageMeta = PageMeta
  { pageMetaTitle       :: Text
  , pageMetaDescription :: Text
  , pageMetaLang        :: Text       -- ^ "en" or "nl"
  , pageMetaCanonical   :: Maybe Text -- ^ Full canonical URL
  , pageMetaOgImage     :: Maybe Text -- ^ Full URL to OG image
  , pageMetaExtraHead   :: Html       -- ^ JSON-LD or other per-page head content
  }

-- | Default English page metadata with a generic company description.
defaultPageMeta :: Text -> PageMeta
defaultPageMeta title = PageMeta
  { pageMetaTitle       = title
  , pageMetaDescription = "Software products and expert consulting. We build reliable systems that solve real problems."
  , pageMetaLang        = "en"
  , pageMetaCanonical   = Nothing
  , pageMetaOgImage     = Nothing
  , pageMetaExtraHead   = mempty
  }

-- | Map a language code to the Open Graph locale format.
ogLocale :: Text -> Text
ogLocale "nl" = "nl_NL"
ogLocale "en" = "en_US"
ogLocale other = other

-- | Resolve the share image URL for a page, falling back to the given
-- site-default when the page sets no image of its own. The fallback is passed
-- in so each brand site can host its own default on its own domain.
resolveOgImage :: Text -> PageMeta -> Text
resolveOgImage siteDefault = fromMaybe siteDefault . pageMetaOgImage

-- =============================================================================
-- Site-wide structured data
-- =============================================================================

-- | Public contact address for the operating company, shared by both brand
-- sites and the Organization structured data.
companyEmail :: Text
companyEmail = "hallo@jappiesoftware.com"

-- | Company-level Open Graph / logo asset, used inside the Organization
-- structured data. Always points at the canonical company domain, even on
-- webwinkelverhuis.nl pages, because this describes the company entity itself.
companyOgImage :: Text
companyOgImage = "https://jappiesoftware.com/og-default.png"

-- | Site-wide Organization structured data, rendered in the head of every page.
-- Lets search engines treat Jappie Software B.V. as a known entity (name, logo,
-- contact details, profiles) rather than re-deriving it per page.
-- Decision: chose schema.org Organization over LocalBusiness because the
-- migration service is delivered remotely, not from a walk-in storefront, so the
-- LocalBusiness address/opening-hours fields would be misleading.
organizationJsonLd :: Html
organizationJsonLd =
  H.script ! A.type_ "application/ld+json" $ H.preEscapedToHtml organizationJson
  where
    organizationJson :: Text
    organizationJson = T.concat
      [ "{\"@context\":\"https://schema.org\""
      , ",\"@type\":\"Organization\""
      , ",\"name\":\"Jappie Software B.V.\""
      , ",\"url\":\"https://jappiesoftware.com/\""
      , ",\"logo\":\"https://jappiesoftware.com/logo.svg\""
      , ",\"image\":" <> jsonLdString companyOgImage
      , ",\"email\":" <> jsonLdString companyEmail
      , ",\"telephone\":\"+31644237437\""
      , ",\"vatID\":\"NL\""
      , ",\"identifier\":\"KVK 95097872\""
      , ",\"areaServed\":\"NL\""
      , ",\"sameAs\":[\"https://jappie.me/\",\"https://github.com/jappeace\"]"
      , "}"
      ]

-- | Service structured data for a migration landing page. Makes the priced
-- service offering itself eligible for rich results, complementing the FAQ
-- markup already present on these pages.
serviceJsonLd :: Text -> Text -> Text -> Html
serviceJsonLd serviceName serviceDescription pageUrl =
  H.script ! A.type_ "application/ld+json" $ H.preEscapedToHtml serviceJson
  where
    serviceJson :: Text
    serviceJson = T.concat
      [ "{\"@context\":\"https://schema.org\""
      , ",\"@type\":\"Service\""
      , ",\"serviceType\":\"Webshop migratie\""
      , ",\"name\":" <> jsonLdString serviceName
      , ",\"description\":" <> jsonLdString serviceDescription
      , ",\"url\":" <> jsonLdString pageUrl
      , ",\"areaServed\":\"NL\""
      , ",\"provider\":{\"@type\":\"Organization\""
      , ",\"name\":\"Jappie Software B.V.\""
      , ",\"url\":\"https://jappiesoftware.com/\"}"
      , ",\"offers\":{\"@type\":\"Offer\""
      , ",\"priceCurrency\":\"EUR\""
      , ",\"price\":\"999\""
      , ",\"url\":" <> jsonLdString pageUrl <> "}"
      , "}"
      ]

-- | FAQ structured data (JSON-LD) built from question/answer pairs. Makes a
-- page eligible for Google's FAQ rich snippets. Replaces the per-page copies
-- that previously duplicated this builder on every migration landing page.
faqPageJsonLd :: [(Text, Text)] -> Html
faqPageJsonLd entries =
  H.script ! A.type_ "application/ld+json" $ H.preEscapedToHtml $ T.concat
    [ "{\"@context\":\"https://schema.org\""
    , ",\"@type\":\"FAQPage\""
    , ",\"mainEntity\":["
    , T.intercalate "," (map faqEntryJson entries)
    , "]}"
    ]

-- | One @Question@/@Answer@ node of a 'faqPageJsonLd' document.
faqEntryJson :: (Text, Text) -> Text
faqEntryJson (question, answer) = T.concat
  [ "{\"@type\":\"Question\""
  , ",\"name\":" <> jsonLdString question
  , ",\"acceptedAnswer\":{\"@type\":\"Answer\""
  , ",\"text\":" <> jsonLdString answer
  , "}}"
  ]

-- | Render a 'Text' value as a JSON string literal, escaping the characters that
-- would otherwise break the surrounding JSON-LD document.
jsonLdString :: Text -> Text
jsonLdString txt = "\"" <> T.concatMap escapeJsonLdChar txt <> "\""

escapeJsonLdChar :: Char -> Text
escapeJsonLdChar '"'  = "\\\""
escapeJsonLdChar '\\' = "\\\\"
escapeJsonLdChar '\n' = "\\n"
escapeJsonLdChar '\r' = "\\r"
escapeJsonLdChar '\t' = "\\t"
escapeJsonLdChar c    = T.singleton c

-- =============================================================================
-- Blog listing markup (shared by both sites' blogs)
-- =============================================================================

-- | One article summary in a paginated blog index. Both brand blogs
-- (jappiesoftware.com and webwinkelverhuis.nl) live under @/blog/@, so the
-- link path is fixed here rather than threaded through from the SiteConfig.
renderBlogSummary :: Article -> Html
renderBlogSummary article =
  H.article ! A.class_ "post-summary" $ do
    H.h2 $
      H.a ! A.href (toValue ("/blog/" <> articleUrl article)) $
        toHtml (articleTitle article)
    H.p ! A.class_ "post-meta" $ do
      H.time ! customAttribute "datetime" (formatIsoDate (articleDate article)) $
        toHtml (formatHumanDate (articleDate article))
      case articleTags article of
        [] -> mempty
        tagList -> do
          H.preEscapedToHtml (" &middot; " :: Text)
          mapM_ (\tag -> H.span ! A.class_ "tag" $ toHtml tag) tagList
    case articleSummary article of
      Just summary -> H.div ! A.class_ "summary" $ summary
      Nothing -> mempty

-- | Newer/older navigation between paginated blog index pages.
renderPagination :: PaginationInfo -> Html
renderPagination pagination =
  H.nav ! A.class_ "pagination" $ do
    case paginationPrevUrl pagination of
      Just url -> H.a ! A.class_ "prev" ! A.href (toValue url) $ H.preEscapedToHtml ("&larr; Newer" :: Text)
      Nothing -> mempty
    case paginationNextUrl pagination of
      Just url -> H.a ! A.class_ "next" ! A.href (toValue url) $ H.preEscapedToHtml ("Older &rarr;" :: Text)
      Nothing -> mempty

-- =============================================================================
-- Helpers
-- =============================================================================

formatIsoDate :: UTCTime -> Text
formatIsoDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

formatHumanDate :: UTCTime -> Text
formatHumanDate = T.pack . formatTime defaultTimeLocale "%B %e, %Y"

-- | Strip HTML tags from text for use in meta descriptions.
stripHtmlTags :: Text -> Text
stripHtmlTags = stripTagsLoop False

-- | Worker for 'stripHtmlTags': walk the text dropping everything between a
-- '<' and the next '>'. @inTag@ records whether we are currently inside a tag.
stripTagsLoop :: Bool -> Text -> Text
stripTagsLoop inTag txt
  | T.null txt = T.empty
  | otherwise =
      let (firstChar, rest) = (T.head txt, T.tail txt)
      in case firstChar of
        '<' -> stripTagsLoop True rest
        '>' -> stripTagsLoop False rest
        _   -> if inTag
               then stripTagsLoop True rest
               else T.cons firstChar (stripTagsLoop False rest)

-- | Meta description for a blog article: its summary text (first 160 chars with
-- tags stripped), falling back to the title. Shared by both brand blogs.
articleMetaDescription :: Article -> Text
articleMetaDescription article = case articleSummaryText article of
  Just summaryText -> T.take 160 (stripHtmlTags summaryText)
  Nothing          -> articleTitle article
