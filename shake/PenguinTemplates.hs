{-# LANGUAGE OverloadedStrings #-}

-- | Templates for jappiesoftware.com: the company's products-and-consulting
-- site and its technical blog. Carries the penguin theme (voronoi background,
-- green palette). Shared SEO/structured-data/blog markup lives in 'PageChrome';
-- the webshop-migration brand site lives in 'WebwinkelTemplates'.
module PenguinTemplates
  ( penguinIndexPage
  , penguinBlogIndexPage
  , penguinArticlePage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (SiteConfig(..), Article(..), PaginationInfo(..))
import PageChrome
  ( PageMeta(..)
  , defaultPageMeta
  , toValue
  , toHtml
  , customAttribute
  , ogLocale
  , resolveOgImage
  , companyEmail
  , organizationJsonLd
  , formatIsoDate
  , formatHumanDate
  , articleMetaDescription
  , renderBlogSummary
  , renderPagination
  )

-- | Site-default social-share image for jappiesoftware.com.
penguinOgImage :: Text
penguinOgImage = "https://jappiesoftware.com/og-default.png"

-- | The "get in touch" mailto used across the company site.
contactMailto :: H.AttributeValue
contactMailto = toValue ("mailto:" <> companyEmail)

-- =============================================================================
-- Base templates
-- =============================================================================

-- | Shared head + navigation + footer for jappiesoftware.com. @ogType@ is the
-- Open Graph type and @includeFeed@ adds the Atom feed link on blog pages.
penguinBaseWith :: Text -> Bool -> PageMeta -> Html -> Html
penguinBaseWith ogType includeFeed meta content =
  H.docTypeHtml ! A.lang (toValue (pageMetaLang meta)) $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content (toValue (pageMetaDescription meta))
      -- Open Graph tags
      H.meta ! customAttribute "property" "og:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! customAttribute "property" "og:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! customAttribute "property" "og:type" ! A.content (toValue ogType)
      H.meta ! customAttribute "property" "og:locale" ! A.content (toValue (ogLocale (pageMetaLang meta)))
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.meta ! customAttribute "property" "og:url" ! A.content (toValue canonicalUrl)
        Nothing -> mempty
      H.meta ! customAttribute "property" "og:image" ! A.content (toValue (resolveOgImage penguinOgImage meta))
      H.meta ! customAttribute "property" "og:image:width" ! A.content "1200"
      H.meta ! customAttribute "property" "og:image:height" ! A.content "630"
      -- Twitter Card tags
      H.meta ! A.name "twitter:card" ! A.content "summary_large_image"
      H.meta ! A.name "twitter:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! A.name "twitter:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! A.name "twitter:image" ! A.content (toValue (resolveOgImage penguinOgImage meta))
      -- Canonical URL
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.link ! A.rel "canonical" ! A.href (toValue canonicalUrl)
        Nothing -> mempty
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      if includeFeed
        then H.link ! A.href "/blog/atom"
                    ! A.type_ "application/atom+xml"
                    ! A.rel "alternate"
                    ! A.title "Jappie Software B.V. Atom Feed"
        else mempty
      H.script ! A.defer "" ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-FMYV1PLWZ6" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-FMYV1PLWZ6');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
      organizationJsonLd
      pageMetaExtraHead meta
    H.body $ do
      H.header $
        H.nav ! A.class_ "top-nav" $ do
          H.span ! A.class_ "logo" $
            H.a ! A.href "/" $ "Jappie Software B.V."
          H.ul $ do
            H.li $ H.a ! A.href "/#products" $ "Products"
            H.li $ H.a ! A.href "/#consulting" $ "Consulting"
            H.li $ H.a ! A.href "/blog/" $ "Blog"
            H.li $ H.a ! A.href contactMailto ! A.class_ "cta-link" $ "Get in touch"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href contactMailto $ toHtml companyEmail
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- | Landing page skeleton (Open Graph type "website").
penguinBaseTemplate :: PageMeta -> Html -> Html
penguinBaseTemplate = penguinBaseWith "website" False

-- | Blog page skeleton (Open Graph type "article", with Atom feed link).
penguinBlogBaseTemplate :: PageMeta -> Html -> Html
penguinBlogBaseTemplate = penguinBaseWith "article" True

-- =============================================================================
-- Landing page (index.html)
-- =============================================================================

penguinIndexPage :: Html
penguinIndexPage = penguinBaseTemplate indexMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "We build software that solves real problems."
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Software products and expert consulting from a team that ships reliable systems. We build tools we believe in &mdash; and help others do the same." :: Text)

    -- Products
    H.section ! A.class_ "for-who" ! A.id "products" $ do
      H.h2 "Products"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "MijnWebwinkel Migration Tool"
          H.p $ H.preEscapedToHtml ("Migrate your webshop from MijnWebwinkel to Shopify, WooCommerce or another platform &mdash; products, categories, translations, images, SEO redirects and bulk data modifications. Fully automated." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Learn more &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "CCV Shop Migration Tool"
          H.p $ H.preEscapedToHtml ("Migrate your webshop from CCV Shop to Shopify &mdash; products, categories, translations, images, inventory and SEO redirects. Fully automated." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-ccvshop.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Learn more &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Lightspeed Migration Tool"
          H.p $ H.preEscapedToHtml ("Migrate your webshop from Lightspeed to Shopify &mdash; products, categories, translations, images, inventory and SEO redirects. No traffic loss." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-lightspeed.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Learn more &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Massapp"
          H.p "Bulk WhatsApp messaging for businesses. Reach your customers at scale through the official WhatsApp Business API."
          H.p ! A.class_ "coming-soon" $ "Rebuilding on official API. Contact us for early access."
        H.li ! A.class_ "card" $ do
          H.h3 "IoT & Sensor Solutions"
          H.p $ H.preEscapedToHtml ("Custom software for sensor data collection, monitoring, and dashboards. We have deep experience with IoT systems &mdash; from firmware to cloud." :: Text)
          H.p ! A.class_ "coming-soon" $ "Looking for partners with domain expertise."

    -- Consulting
    H.section ! A.class_ "results" ! A.id "consulting" $ do
      H.h2 "Expert Consulting"
      H.p $ H.preEscapedToHtml ("We also take on consulting engagements where our expertise makes a difference. 10+ years building production systems &mdash; we make technical decisions and then build them." :: Text)
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            "Built the core platform for a "
            H.strong "reinsurance technology startup"
            H.preEscapedToHtml (" &mdash; from early architecture to handling live deals in production. " :: Text)
            H.a ! A.href "https://jappie.me/the-peculiar-event-sourced-deadlock.html" $ H.preEscapedToHtml ("Read about solving a production issue &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            "Technical lead for a "
            H.strong "construction IoT startup"
            H.preEscapedToHtml (". Architecture decisions that scaled from pilot to production. 7x device performance improvement. " :: Text)
            H.a ! A.href "https://jappie.me/stacked-against-us.html" $ H.preEscapedToHtml ("Read the full story &rarr;" :: Text)
            H.preEscapedToHtml (" &middot; " :: Text)
            H.a ! A.href "https://jappie.me/firmware-lemons.html" $ H.preEscapedToHtml ("The 7x improvement &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            "Automated "
            H.strong "e-commerce migration"
            " for a multi-language webshop with 2,400+ products across three domains and three languages."

    -- About
    H.section ! A.class_ "about" $ do
      H.h2 "About"
      H.p $ H.preEscapedToHtml ("I&rsquo;m Jappie Klooster. I build software products and help companies that need serious technical expertise. I use technologies chosen for reliability &mdash; Haskell, Nix, and whatever else gets the job done right." :: Text)
      H.p $ H.preEscapedToHtml ("Based in the Netherlands. Available for product partnerships, consulting engagements, and co-founder conversations." :: Text)
      H.p $ do
        "For more writing and case studies, visit the "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Have a problem that needs solving?"
      H.p $ do
        H.preEscapedToHtml ("Whether you need a product, a technical partner, or expert consulting &mdash; " :: Text)
        H.a ! A.href contactMailto $ "get in touch"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"
  where
    indexMeta :: PageMeta
    indexMeta = (defaultPageMeta "Jappie Software B.V. \8212 Software Products & Expert Consulting")
      { pageMetaCanonical = Just "https://jappiesoftware.com/"
      }

-- =============================================================================
-- Blog index page (paginated listing)
-- =============================================================================

penguinBlogIndexPage :: SiteConfig -> [Article] -> PaginationInfo -> Html
penguinBlogIndexPage _config articles pagination =
  penguinBlogBaseTemplate blogIndexMeta $
    H.main ! A.class_ "blog-listing" $ do
      H.h1 "Blog"
      mapM_ renderBlogSummary articles
      renderPagination pagination
  where
    blogIndexMeta :: PageMeta
    blogIndexMeta = (defaultPageMeta "Blog \8212 Jappie Software B.V.")
      { pageMetaDescription = "Technical blog about Haskell, Nix, software architecture, and building reliable production systems."
      , pageMetaCanonical   = Just "https://jappiesoftware.com/blog/"
      }

-- =============================================================================
-- Individual article page
-- =============================================================================

penguinArticlePage :: SiteConfig -> Article -> Html
penguinArticlePage _config article =
  penguinBlogBaseTemplate articleMeta $
    H.main ! A.class_ "blog-article" $
      H.article $ do
        H.header $ do
          H.h1 $ toHtml (articleTitle article)
          H.p ! A.class_ "post-meta" $ do
            H.time ! customAttribute "datetime" (formatIsoDate (articleDate article)) $
              toHtml (formatHumanDate (articleDate article))
            case articleTags article of
              [] -> mempty
              tagList -> do
                H.preEscapedToHtml (" &middot; " :: Text)
                mapM_ (\tag -> H.span ! A.class_ "tag" $ toHtml tag) tagList
        H.div ! A.class_ "entry-content" $
          articleContent article
        case articleFootnotesHtml article of
          Just fn -> fn
          Nothing -> mempty
        H.footer ! A.class_ "article-footer" $
          H.a ! A.href "/blog/" $ H.preEscapedToHtml ("&larr; Back to blog" :: Text)
  where
    articleMeta :: PageMeta
    articleMeta = (defaultPageMeta (articleTitle article <> " \8212 Jappie Software B.V."))
      { pageMetaDescription = articleMetaDescription article
      , pageMetaCanonical   = Just ("https://jappiesoftware.com/blog/" <> articleUrl article)
      }

-- =============================================================================
-- Voronoi background
-- =============================================================================

voronoiScript :: Text
voronoiScript = T.unlines
  [ "document.addEventListener(\"DOMContentLoaded\", function() {"
  , "  const svg = d3.select(\"svg.voronoi\");"
  , "  function drawVoronoi() {"
  , "    const width = window.innerWidth;"
  , "    const height = window.innerHeight;"
  , "    const area = width * height;"
  , "    const cellSize = 15000;"
  , "    const numPoints = Math.max(100, Math.ceil(area / cellSize));"
  , "    const points = d3.range(numPoints).map(() => [Math.random() * width, Math.random() * height]);"
  , "    const delaunay = d3.Delaunay.from(points);"
  , "    const voronoi = delaunay.voronoi([0, 0, width, height]);"
  , "    svg.selectAll(\"g\").remove();"
  , "    svg.append(\"g\")"
  , "      .selectAll(\"path\")"
  , "      .data(voronoi.cellPolygons())"
  , "      .join(\"path\")"
  , "        .attr(\"d\", d => \"M\" + d.join(\"L\") + \"Z\")"
  , "        .attr(\"stroke\", \"#ddd\")"
  , "        .attr(\"fill\", \"none\");"
  , "  }"
  , "  drawVoronoi();"
  , "  let resizeTimer;"
  , "  window.addEventListener(\"resize\", function() {"
  , "    clearTimeout(resizeTimer);"
  , "    resizeTimer = setTimeout(drawVoronoi, 150);"
  , "  });"
  , "});"
  ]
