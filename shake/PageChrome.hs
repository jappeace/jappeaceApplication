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
  , companyWhatsappNumber
  , whatsappFloatingButton
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
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Network.HTTP.Types.URI (urlEncode)
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
  , pageMetaSwitchUrl   :: Maybe Text -- ^ Same page in the other language, for the language toggle
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
  , pageMetaSwitchUrl   = Nothing
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

-- =============================================================================
-- Floating WhatsApp contact button ("bolletje")
-- =============================================================================

-- | The company WhatsApp number in wa.me format: country code, no leading plus,
-- no spaces. The same line shown as +31 6 4423 7437 in the site footers.
companyWhatsappNumber :: Text
companyWhatsappNumber = "31644237437"

-- | A floating WhatsApp contact button pinned to the bottom-right corner of the
-- viewport. Tapping it opens a WhatsApp chat with the company, pre-filled with
-- @prefilledMessage@. @accessibleLabel@ is the screen-reader label, passed in so
-- each brand site supplies it in its own language (English on jappiesoftware.com,
-- Dutch on webwinkelverhuis.nl). The visual styling (the green circle, position
-- and hover) lives in each site's @.whatsapp-bolletje@ stylesheet rule; the
-- inline glyph is filled with @currentColor@ so the stylesheet controls colour.
whatsappFloatingButton :: Text -> Text -> Html
whatsappFloatingButton accessibleLabel prefilledMessage =
  H.a ! A.href (toValue (whatsappChatUrl prefilledMessage))
      ! A.class_ "whatsapp-bolletje"
      ! A.target "_blank"
      ! customAttribute "rel" "noopener noreferrer"
      ! customAttribute "aria-label" accessibleLabel
      $ H.preEscapedToHtml whatsappGlyphSvg

-- | The wa.me deep link that opens a WhatsApp chat with the company, pre-filled
-- with @prefilledMessage@ (percent-encoded into the @text@ query parameter).
whatsappChatUrl :: Text -> Text
whatsappChatUrl prefilledMessage =
  "https://wa.me/" <> companyWhatsappNumber
    <> "?text=" <> percentEncodeQuery prefilledMessage

-- | Percent-encode text for use as a URL query value. Encodes the UTF-8 bytes
-- per RFC 3986 (so spaces become %20 and any non-ASCII is escaped), keeping the
-- pre-filled WhatsApp message intact regardless of punctuation or accents.
percentEncodeQuery :: Text -> Text
percentEncodeQuery = decodeUtf8 . urlEncode True . encodeUtf8

-- | The WhatsApp glyph as an inline SVG (single path, official logo outline).
-- Filled with @currentColor@ so the surrounding @.whatsapp-bolletje@ colour
-- applies. Marked @aria-hidden@ because the link itself carries the label.
whatsappGlyphSvg :: Text
whatsappGlyphSvg =
  "<svg viewBox=\"0 0 24 24\" xmlns=\"http://www.w3.org/2000/svg\" fill=\"currentColor\" aria-hidden=\"true\" focusable=\"false\"><path d=\"M17.472 14.382c-.297-.149-1.758-.867-2.03-.967-.273-.099-.471-.148-.67.15-.197.297-.767.966-.94 1.164-.173.199-.347.223-.644.075-.297-.15-1.255-.463-2.39-1.475-.883-.788-1.48-1.761-1.653-2.059-.173-.297-.018-.458.13-.606.134-.133.298-.347.446-.52.149-.174.198-.298.298-.497.099-.198.05-.371-.025-.52-.075-.149-.669-1.612-.916-2.207-.242-.579-.487-.5-.669-.51-.173-.008-.371-.01-.57-.01-.198 0-.52.074-.792.372-.272.297-1.04 1.016-1.04 2.479 0 1.462 1.065 2.875 1.213 3.074.149.198 2.096 3.2 5.077 4.487.709.306 1.262.489 1.694.625.712.227 1.36.195 1.871.118.571-.085 1.758-.719 2.006-1.413.248-.694.248-1.289.173-1.413-.074-.124-.272-.198-.57-.347m-5.421 7.403h-.004a9.87 9.87 0 0 1-5.031-1.378l-.361-.214-3.741.982.998-3.648-.235-.374a9.86 9.86 0 0 1-1.51-5.26c.001-5.45 4.436-9.884 9.888-9.884 2.64 0 5.122 1.03 6.988 2.898a9.825 9.825 0 0 1 2.893 6.994c-.003 5.45-4.437 9.885-9.885 9.885M20.52 3.449C18.24 1.245 15.24 0 12.045 0 5.463 0 .104 5.359.101 11.892c0 2.096.547 4.142 1.588 5.945L0 24l6.305-1.654a11.882 11.882 0 0 0 5.683 1.448h.005c6.582 0 11.943-5.359 11.945-11.893a11.821 11.821 0 0 0-3.418-8.45\"/></svg>"

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
