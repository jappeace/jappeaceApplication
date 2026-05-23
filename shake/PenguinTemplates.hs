{-# LANGUAGE OverloadedStrings #-}
module PenguinTemplates
  ( penguinIndexPage
  , penguinBlogIndexPage
  , penguinArticlePage
  , mijnwebwinkelMigrationPage
  , PageMeta(..)
  , defaultPageMeta
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (SiteConfig(..), Article(..), PaginationInfo(..))

-- Helper to convert Text to AttributeValue
toValue :: Text -> H.AttributeValue
toValue = H.toValue

toHtml :: Text -> Html
toHtml = H.toHtml

-- | Custom HTML attribute helper
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

-- | Default English page metadata with generic company description.
defaultPageMeta :: Text -> PageMeta
defaultPageMeta title = PageMeta
  { pageMetaTitle       = title
  , pageMetaDescription = "Software products and expert consulting. We build reliable systems that solve real problems."
  , pageMetaLang        = "en"
  , pageMetaCanonical   = Nothing
  , pageMetaOgImage     = Nothing
  , pageMetaExtraHead   = mempty
  }

-- =============================================================================
-- Base template shared by all penguin pages
-- =============================================================================

penguinBaseTemplate :: PageMeta -> Html -> Html
penguinBaseTemplate meta content =
  H.docTypeHtml ! A.lang (toValue (pageMetaLang meta)) $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content (toValue (pageMetaDescription meta))
      -- Open Graph tags
      H.meta ! customAttribute "property" "og:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! customAttribute "property" "og:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! customAttribute "property" "og:type" ! A.content "website"
      H.meta ! customAttribute "property" "og:locale" ! A.content (toValue (ogLocale (pageMetaLang meta)))
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.meta ! customAttribute "property" "og:url" ! A.content (toValue canonicalUrl)
        Nothing -> mempty
      case pageMetaOgImage meta of
        Just imageUrl -> H.meta ! customAttribute "property" "og:image" ! A.content (toValue imageUrl)
        Nothing -> mempty
      -- Twitter Card tags
      H.meta ! A.name "twitter:card" ! A.content "summary"
      H.meta ! A.name "twitter:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! A.name "twitter:description" ! A.content (toValue (pageMetaDescription meta))
      -- Canonical URL
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.link ! A.rel "canonical" ! A.href (toValue canonicalUrl)
        Nothing -> mempty
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      H.script ! A.async "" ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-FMYV1PLWZ6" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-FMYV1PLWZ6');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
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
            H.li $ H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-link" $ "Get in touch"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href "mailto:hi@jappie.me" $ "hi@jappie.me"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- | Blog-specific base template (uses root-relative paths for assets)
penguinBlogBaseTemplate :: PageMeta -> Html -> Html
penguinBlogBaseTemplate meta content =
  H.docTypeHtml ! A.lang (toValue (pageMetaLang meta)) $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content (toValue (pageMetaDescription meta))
      -- Open Graph tags
      H.meta ! customAttribute "property" "og:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! customAttribute "property" "og:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! customAttribute "property" "og:type" ! A.content "article"
      H.meta ! customAttribute "property" "og:locale" ! A.content (toValue (ogLocale (pageMetaLang meta)))
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.meta ! customAttribute "property" "og:url" ! A.content (toValue canonicalUrl)
        Nothing -> mempty
      case pageMetaOgImage meta of
        Just imageUrl -> H.meta ! customAttribute "property" "og:image" ! A.content (toValue imageUrl)
        Nothing -> mempty
      -- Twitter Card tags
      H.meta ! A.name "twitter:card" ! A.content "summary"
      H.meta ! A.name "twitter:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! A.name "twitter:description" ! A.content (toValue (pageMetaDescription meta))
      -- Canonical URL
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.link ! A.rel "canonical" ! A.href (toValue canonicalUrl)
        Nothing -> mempty
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      H.link ! A.href "/blog/atom"
             ! A.type_ "application/atom+xml"
             ! A.rel "alternate"
             ! A.title "Jappie Software B.V. Atom Feed"
      H.script ! A.async "" ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-FMYV1PLWZ6" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-FMYV1PLWZ6');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
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
            H.li $ H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-link" $ "Get in touch"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href "mailto:hi@jappie.me" $ "hi@jappie.me"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- | Map language code to OG locale format
ogLocale :: Text -> Text
ogLocale "nl" = "nl_NL"
ogLocale "en" = "en_US"
ogLocale other = other

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
          H.a ! A.href "/migrate-mijnwebwinkel.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Learn more &rarr;" :: Text)
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
        H.a ! A.href "mailto:hi@jappie.me" $ "get in touch"
        "."
      H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-button" $ "Get in touch"
  where
    indexMeta :: PageMeta
    indexMeta = (defaultPageMeta "Jappie Software B.V. \8212 Software Products & Expert Consulting")
      { pageMetaCanonical = Just "https://jappiesoftware.com/"
      }

-- =============================================================================
-- MijnWebwinkel migration landing page
-- =============================================================================

mijnwebwinkelMigrationPage :: Html
mijnwebwinkelMigrationPage = penguinBaseTemplate migrationMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Migreer van MijnWebwinkel"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Volledig geautomatiseerde migratie van uw webshop naar Shopify, WooCommerce of een ander platform. Producten, categorie&euml;n, vertalingen, afbeeldingen en SEO-redirects &mdash; zonder handmatig overtypen." :: Text)
      H.a ! A.href "mailto:hi@jappie.me?subject=MijnWebwinkel%20migratie" ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar het formaat van uw doelplatform."
        H.li ! A.class_ "card" $ do
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien &mdash; ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Spaarpunten"
          H.p "Spaarpuntensaldi van uw klanten worden overgezet naar het loyaliteitsprogramma van uw nieuwe platform."
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p $ H.preEscapedToHtml ("301-redirects van elke oude URL naar de nieuwe URL. We hebben de onderliggende logica van MijnWebwinkel-artikel-ID&rsquo;s in URLs achterhaald, waardoor we alle redirects volledig automatisch kunnen genereren. Uw Google-posities en backlinks blijven behouden." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen aan data"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie &mdash; bijvoorbeeld alt-teksten voor alle afbeeldingen, prijsaanpassingen of het opschonen van beschrijvingen." :: Text)

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          " \8212 Ons programma crawlt uw MijnWebwinkel-shop en slaat alle data op."
        H.li $ do
          H.strong "Controle"
          " \8212 U krijgt een werkende testshop die u zelf kunt controleren voordat we live gaan."
        H.li $ do
          H.strong "Import"
          " \8212 We importeren alles in uw nieuwe shop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Verificatie"
          " \8212 Samen controleren we steekproefsgewijs of alles klopt."

    -- Pricing
    H.section ! A.class_ "engagement" ! A.id "pricing" $ do
      H.h2 "Prijzen"
      H.div ! A.class_ "card-grid" $ do
        H.div ! A.class_ "card" $ do
          H.h3 "Volledige migratie"
          H.p ! A.class_ "price" $ H.preEscapedToHtml ("Vanaf &euro;750" :: Text)
          H.p $ H.preEscapedToHtml ("Producten, afbeeldingen, vertalingen, categorie&euml;n, klantdata, SEO-redirects en eventuele bulk-aanpassingen. Prijs afhankelijk van de omvang van uw webshop." :: Text)
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Vaste prijs, vooraf afgesproken. Geen verrassingen. Betaling na succesvolle migratie." :: Text)

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ H.preEscapedToHtml ("Ons migratietool is gebouwd op basis van een echte migratie &mdash; een webshop met 2.400+ producten, drie talen (NL/DE/EN) en drie domeinen. Het werkt, want het is al gedaan." :: Text)
      H.ul $ do
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (" &mdash; geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (" &mdash; 301-redirects zodat uw Google-posities niet verloren gaan" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (" &mdash; vertalingen correct gekoppeld via offici&euml;le APIs" :: Text)
        H.li $ H.strong "Bulk-aanpassingen" >> H.preEscapedToHtml (" &mdash; data opschonen, alt-teksten toevoegen of prijzen aanpassen tijdens de migratie" :: Text)
        H.li $ H.strong "Platformkeuze" >> H.preEscapedToHtml (" &mdash; migreer naar Shopify, WooCommerce of een ander platform naar keuze" :: Text)
        H.li $ H.strong "Controleerbaar" >> H.preEscapedToHtml (" &mdash; u krijgt een rapport en kunt alles verifi&euml;ren voor de overstap" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (" &mdash; geen uurtarief, u weet vooraf wat het kost" :: Text)

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ do
        H.dt "Hoe lang duurt een migratie?"
        H.dd "De technische migratie duurt meestal 1-2 werkdagen. De voorbereiding en controle erbij: reken op een week totaal."
        H.dt "Kan ik mijn domeinnaam behouden?"
        H.dd "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd."
        H.dt "Wat als er iets niet klopt na de migratie?"
        H.dd "We controleren samen steekproefsgewijs. Eventuele correcties zijn inbegrepen in de vaste prijs."
        H.dt "Werkt het ook voor andere talen dan NL/DE/EN?"
        H.dd "Ja. Het programma ondersteunt elke taalcombinatie die MijnWebwinkel en uw doelplatform beide ondersteunen."
        H.dt "Kan ik ook naar een ander platform dan Shopify migreren?"
        H.dd "Ja. Shopify is het meest gekozen doelplatform, maar we kunnen ook migreren naar WooCommerce of andere platformen."
        H.dt "Worden spaarpunten ook overgezet?"
        H.dd "Ja. Spaarpuntensaldi van uw klanten worden meegenomen naar het loyaliteitsprogramma van uw nieuwe platform."
        H.dt "Hoe werken de SEO-redirects precies?"
        H.dd "We hebben de onderliggende logica achterhaald waarmee MijnWebwinkel artikel-ID's in URLs genereert. Daardoor kunnen we alle redirects volledig automatisch aanmaken, ook voor URLs met numerieke product-ID's."
        H.dt "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
        H.dd "Ja. We kunnen grootschalige wijzigingen doorvoeren, bijvoorbeeld alt-teksten genereren voor alle afbeeldingen, prijzen aanpassen of beschrijvingen opschonen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te migreren?"
      H.p $ do
        "Stuur een mail naar "
        H.a ! A.href "mailto:hi@jappie.me?subject=MijnWebwinkel%20migratie" $ "hi@jappie.me"
        " met een link naar uw webshop. U ontvangt binnen twee werkdagen een offerte."
      H.a ! A.href "mailto:hi@jappie.me?subject=MijnWebwinkel%20migratie" ! A.class_ "cta-button" $ "Vraag een offerte aan"
  where
    migrationMeta :: PageMeta
    migrationMeta = PageMeta
      { pageMetaTitle       = "MijnWebwinkel migratie \8212 Jappie Software B.V."
      , pageMetaDescription = "Geautomatiseerde migratie van MijnWebwinkel naar Shopify, WooCommerce of een ander platform. Producten, vertalingen, afbeeldingen en SEO-redirects. Vanaf \8364\&750."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://jappiesoftware.com/migrate-mijnwebwinkel.html"
      , pageMetaOgImage     = Nothing
      , pageMetaExtraHead   = migrationFaqJsonLd
      }

-- | FAQ structured data (JSON-LD) for the migration page.
-- Makes the page eligible for Google rich snippets.
migrationFaqJsonLd :: Html
migrationFaqJsonLd =
  H.script ! A.type_ "application/ld+json" $ H.preEscapedToHtml faqJson
  where
    faqJson :: Text
    faqJson = T.concat
      [ "{\"@context\":\"https://schema.org\""
      , ",\"@type\":\"FAQPage\""
      , ",\"mainEntity\":["
      , faqEntry
          "Hoe lang duurt een migratie?"
          "De technische migratie duurt meestal 1-2 werkdagen. De voorbereiding en controle erbij: reken op een week totaal."
      , ","
      , faqEntry
          "Kan ik mijn domeinnaam behouden?"
          "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd."
      , ","
      , faqEntry
          "Wat als er iets niet klopt na de migratie?"
          "We controleren samen steekproefsgewijs. Eventuele correcties zijn inbegrepen in de vaste prijs."
      , ","
      , faqEntry
          "Werkt het ook voor andere talen dan NL/DE/EN?"
          "Ja. Het programma ondersteunt elke taalcombinatie die MijnWebwinkel en uw doelplatform beide ondersteunen."
      , ","
      , faqEntry
          "Kan ik ook naar een ander platform dan Shopify migreren?"
          "Ja. Shopify is het meest gekozen doelplatform, maar we kunnen ook migreren naar WooCommerce of andere platformen."
      , ","
      , faqEntry
          "Worden spaarpunten ook overgezet?"
          "Ja. Spaarpuntensaldi van uw klanten worden meegenomen naar het loyaliteitsprogramma van uw nieuwe platform."
      , ","
      , faqEntry
          "Hoe werken de SEO-redirects precies?"
          "We hebben de onderliggende logica achterhaald waarmee MijnWebwinkel artikel-ID's in URLs genereert. Daardoor kunnen we alle redirects volledig automatisch aanmaken, ook voor URLs met numerieke product-ID's."
      , ","
      , faqEntry
          "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
          "Ja. We kunnen grootschalige wijzigingen doorvoeren, bijvoorbeeld alt-teksten genereren voor alle afbeeldingen, prijzen aanpassen of beschrijvingen opschonen."
      , "]}"
      ]

    faqEntry :: Text -> Text -> Text
    faqEntry question answer = T.concat
      [ "{\"@type\":\"Question\""
      , ",\"name\":" <> jsonString question
      , ",\"acceptedAnswer\":{\"@type\":\"Answer\""
      , ",\"text\":" <> jsonString answer
      , "}}"
      ]

    -- | Minimal JSON string escaping for known-safe Dutch text
    jsonString :: Text -> Text
    jsonString txt = "\"" <> escapeJsonText txt <> "\""

    escapeJsonText :: Text -> Text
    escapeJsonText = T.concatMap escapeJsonChar

    escapeJsonChar :: Char -> Text
    escapeJsonChar '"'  = "\\\""
    escapeJsonChar '\\' = "\\\\"
    escapeJsonChar '\n' = "\\n"
    escapeJsonChar '\r' = "\\r"
    escapeJsonChar '\t' = "\\t"
    escapeJsonChar c    = T.singleton c

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
      { pageMetaDescription = articleDescription article
      , pageMetaCanonical   = Just ("https://jappiesoftware.com/blog/" <> articleUrl article)
      }

    -- | Use the article summary text as meta description, falling back to the title.
    articleDescription :: Article -> Text
    articleDescription art = case articleSummaryText art of
      Just summaryText -> T.take 160 (stripHtmlTags summaryText)
      Nothing          -> articleTitle art

-- | Strip HTML tags from text for use in meta descriptions.
stripHtmlTags :: Text -> Text
stripHtmlTags = go False
  where
    go :: Bool -> Text -> Text
    go _ txt | T.null txt = T.empty
    go inTag txt =
      let (firstChar, rest) = (T.head txt, T.tail txt)
      in case firstChar of
        '<' -> go True rest
        '>' -> go False rest
        _   -> if inTag
               then go True rest
               else T.cons firstChar (go False rest)

-- =============================================================================
-- Helpers
-- =============================================================================

formatIsoDate :: UTCTime -> Text
formatIsoDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

formatHumanDate :: UTCTime -> Text
formatHumanDate = T.pack . formatTime defaultTimeLocale "%B %e, %Y"

voronoiScript :: Text
voronoiScript = T.unlines
  [ "const svg = d3.select(\"svg.voronoi\");"
  , "function drawVoronoi() {"
  , "  const width = window.innerWidth;"
  , "  const height = window.innerHeight;"
  , "  const area = width * height;"
  , "  const cellSize = 15000;"
  , "  const numPoints = Math.max(100, Math.ceil(area / cellSize));"
  , "  const points = d3.range(numPoints).map(() => [Math.random() * width, Math.random() * height]);"
  , "  const delaunay = d3.Delaunay.from(points);"
  , "  const voronoi = delaunay.voronoi([0, 0, width, height]);"
  , "  svg.selectAll(\"g\").remove();"
  , "  svg.append(\"g\")"
  , "    .selectAll(\"path\")"
  , "    .data(voronoi.cellPolygons())"
  , "    .join(\"path\")"
  , "      .attr(\"d\", d => \"M\" + d.join(\"L\") + \"Z\")"
  , "      .attr(\"stroke\", \"#ddd\")"
  , "      .attr(\"fill\", \"none\");"
  , "}"
  , "drawVoronoi();"
  , "let resizeTimer;"
  , "window.addEventListener(\"resize\", function() {"
  , "  clearTimeout(resizeTimer);"
  , "  resizeTimer = setTimeout(drawVoronoi, 150);"
  , "});"
  ]
