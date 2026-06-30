{-# LANGUAGE OverloadedStrings #-}

-- | Templates for jappiesoftware.com: the company's products-and-consulting
-- site and its technical blog. Carries the penguin theme (voronoi background,
-- green palette). Shared SEO/structured-data/blog markup lives in 'PageChrome';
-- the webshop-migration brand site lives in 'WebwinkelTemplates'.
module PenguinTemplates
  ( penguinIndexPage
  , penguinIndexPageNl
  , penguinWordpressPage
  , penguinWordpressPageNl
  , penguinBlogIndexPage
  , penguinArticlePage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types
  ( SiteConfig(..)
  , Article(..)
  , PaginationInfo(..)
  , Lang(..)
  , Translations(..)
  , translationsFor
  , langCode
  )
import PageChrome
  ( PageMeta(..)
  , defaultPageMeta
  , toValue
  , toHtml
  , customAttribute
  , ogLocale
  , resolveOgImage
  , companyEmail
  , whatsappFloatingButton
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

-- | Shared head + navigation + footer for jappiesoftware.com. @lang@ drives the
-- HTML lang attribute, the translated navigation and the language toggle;
-- @ogType@ is the Open Graph type and @includeFeed@ adds the Atom feed link on
-- blog pages.
penguinBaseWith :: Lang -> Text -> Bool -> PageMeta -> Html -> Html
penguinBaseWith lang ogType includeFeed meta content =
  H.docTypeHtml ! A.lang (toValue (langCode lang)) $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content (toValue (pageMetaDescription meta))
      -- Open Graph tags
      H.meta ! customAttribute "property" "og:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! customAttribute "property" "og:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! customAttribute "property" "og:type" ! A.content (toValue ogType)
      H.meta ! customAttribute "property" "og:locale" ! A.content (toValue (ogLocale (langCode lang)))
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
      H.header $ penguinNav lang (pageMetaSwitchUrl meta)
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href contactMailto $ toHtml companyEmail
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "https://jappie.me/" $ "jappie.me"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      whatsappFloatingButton (penguinWhatsappLabel lang) (penguinWhatsappMessage lang)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- | The top navigation, with labels and section links in @lang@ and an optional
-- language toggle linking to @switchUrl@ (the same page in the other language).
penguinNav :: Lang -> Maybe Text -> Html
penguinNav lang switchUrl =
  H.nav ! A.class_ "top-nav" $ do
    H.span ! A.class_ "logo" $
      H.a ! A.href (penguinHome lang) $ "Jappie Software B.V."
    H.ul $ do
      H.li $ H.a ! A.href (penguinAnchor lang "products") $ toHtml (navProducts lang)
      H.li $ H.a ! A.href (penguinAnchor lang "consulting") $ toHtml (navConsulting lang)
      H.li $ H.a ! A.href "/blog/" $ "Blog"
      H.li $ H.a ! A.href contactMailto ! A.class_ "cta-link" $ toHtml (navContact lang)
      penguinLangToggle lang switchUrl

-- | The language toggle list item, shown only when the page has a counterpart in
-- the other language. Mirrors the personal blog's toggle: it remembers the
-- choice in @localStorage@ so repeat visitors land in their language.
penguinLangToggle :: Lang -> Maybe Text -> Html
penguinLangToggle lang switchUrl = case switchUrl of
  Nothing -> mempty
  Just url ->
    H.li ! A.class_ "lang-switch" $
      H.a ! A.href (toValue url)
          ! customAttribute "onclick" "localStorage.setItem('lang',this.href.indexOf('/nl/')>=0?'nl':'en')"
          $ toHtml (tSwitchLang trans <> " (" <> tSwitchLangDesc trans <> ")")
  where
    trans :: Translations
    trans = translationsFor lang

-- | Home URL of the company site in the given language.
penguinHome :: Lang -> H.AttributeValue
penguinHome En = "/"
penguinHome Nl = "/nl/"

-- | Link to a same-page section anchor (e.g. "products") in the given language.
penguinAnchor :: Lang -> Text -> H.AttributeValue
penguinAnchor En section = toValue ("/#" <> section)
penguinAnchor Nl section = toValue ("/nl/#" <> section)

-- | Navigation label for the products section.
navProducts :: Lang -> Text
navProducts En = "Products"
navProducts Nl = "Producten"

-- | Navigation label for the consulting section.
navConsulting :: Lang -> Text
navConsulting En = "Consulting"
navConsulting Nl = "Consultancy"

-- | Navigation label for the "get in touch" call to action.
navContact :: Lang -> Text
navContact En = "Get in touch"
navContact Nl = "Neem contact op"

-- | Screen-reader label for the WhatsApp button, per language.
penguinWhatsappLabel :: Lang -> Text
penguinWhatsappLabel En = "Open a WhatsApp chat with us"
penguinWhatsappLabel Nl = "Open een WhatsApp-gesprek met ons"

-- | Pre-filled message for the WhatsApp button, per language.
penguinWhatsappMessage :: Lang -> Text
penguinWhatsappMessage En = "Hi! I have a question about Jappie Software."
penguinWhatsappMessage Nl = "Hallo, ik heb een vraag aan Jappie Software."

-- | Landing page skeleton (Open Graph type "website").
penguinBaseTemplate :: Lang -> PageMeta -> Html -> Html
penguinBaseTemplate lang = penguinBaseWith lang "website" False

-- | Blog page skeleton (Open Graph type "article", with Atom feed link).
penguinBlogBaseTemplate :: Lang -> PageMeta -> Html -> Html
penguinBlogBaseTemplate lang = penguinBaseWith lang "article" True

-- =============================================================================
-- Landing page (index.html)
-- =============================================================================

penguinIndexPage :: Html
penguinIndexPage = penguinBaseTemplate En indexMeta $
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
          H.h3 "WordPress Websites"
          H.p $ H.preEscapedToHtml ("A professional website for your business, built for a fixed price and ready to manage yourself. Often the first step before selling online." :: Text)
          H.a ! A.href "/wordpress-websites.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Learn more &rarr;" :: Text)
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
      , pageMetaSwitchUrl = Just "/nl/"
      }

-- =============================================================================
-- Landing page, Dutch (nl/index.html)
-- =============================================================================

-- | Dutch counterpart of 'penguinIndexPage'. Same structure and section ids (so
-- the in-page anchors keep working), Dutch copy. The webshop-migration cards
-- still point at webwinkelverhuis.nl, which is Dutch already.
penguinIndexPageNl :: Html
penguinIndexPageNl = penguinBaseTemplate Nl indexMetaNl $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Wij bouwen software die echte problemen oplost."
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Softwareproducten en deskundig advies van een team dat betrouwbare systemen oplevert. We bouwen tools waar we zelf in geloven, en helpen anderen hetzelfde te doen." :: Text)

    -- Producten
    H.section ! A.class_ "for-who" ! A.id "products" $ do
      H.h2 "Producten"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "MijnWebwinkel-migratie"
          H.p $ H.preEscapedToHtml ("Verhuis uw webshop van MijnWebwinkel naar Shopify, WooCommerce of een ander platform: producten, categorie\235n, vertalingen, afbeeldingen, SEO-redirects en bulk-aanpassingen. Volledig geautomatiseerd." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Lees meer &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "CCV Shop-migratie"
          H.p $ H.preEscapedToHtml ("Verhuis uw webshop van CCV Shop naar Shopify: producten, categorie\235n, vertalingen, afbeeldingen, voorraad en SEO-redirects. Volledig geautomatiseerd." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-ccvshop.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Lees meer &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Lightspeed-migratie"
          H.p $ H.preEscapedToHtml ("Verhuis uw webshop van Lightspeed naar Shopify: producten, categorie\235n, vertalingen, afbeeldingen, voorraad en SEO-redirects. Zonder verkeersverlies." :: Text)
          H.a ! A.href "https://webwinkelverhuis.nl/migrate-lightspeed.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Lees meer &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "WordPress-websites"
          H.p $ H.preEscapedToHtml ("Een professionele website voor uw bedrijf, voor een vaste prijs en zelf te beheren. Vaak de eerste stap voordat u online gaat verkopen." :: Text)
          H.a ! A.href "/nl/wordpress-websites.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Lees meer &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Massapp"
          H.p "Bulk-WhatsApp-berichten voor bedrijven. Bereik uw klanten op grote schaal via de offici\235le WhatsApp Business API."
          H.p ! A.class_ "coming-soon" $ "Wordt herbouwd op de offici\235le API. Neem contact op voor vroege toegang."
        H.li ! A.class_ "card" $ do
          H.h3 "IoT & sensoroplossingen"
          H.p $ H.preEscapedToHtml ("Software op maat voor sensordata, monitoring en dashboards. We hebben diepgaande ervaring met IoT-systemen, van firmware tot cloud." :: Text)
          H.p ! A.class_ "coming-soon" $ "Op zoek naar partners met domeinkennis."

    -- Consultancy
    H.section ! A.class_ "results" ! A.id "consulting" $ do
      H.h2 "Deskundig advies"
      H.p $ H.preEscapedToHtml ("We nemen ook adviesopdrachten aan waar onze expertise het verschil maakt. 10+ jaar bouwen aan productiesystemen: we maken technische keuzes en bouwen ze ook." :: Text)
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            "Het kernplatform gebouwd voor een "
            H.strong "herverzekerings-startup"
            ", van de eerste architectuur tot live deals in productie. "
            H.a ! A.href "https://jappie.me/the-peculiar-event-sourced-deadlock.html" $ H.preEscapedToHtml ("Lees over het oplossen van een productieprobleem &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            "Technisch lead voor een "
            H.strong "bouw-IoT-startup"
            ". Architectuurkeuzes die meeschaalden van pilot naar productie. 7x snellere apparaten. "
            H.a ! A.href "https://jappie.me/stacked-against-us.html" $ H.preEscapedToHtml ("Lees het hele verhaal &rarr;" :: Text)
            H.preEscapedToHtml (" &middot; " :: Text)
            H.a ! A.href "https://jappie.me/firmware-lemons.html" $ H.preEscapedToHtml ("De 7x verbetering &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            "Geautomatiseerde "
            H.strong "e-commerce-migratie"
            " voor een meertalige webshop met 2.400+ producten over drie domeinen en drie talen."

    -- Over
    H.section ! A.class_ "about" $ do
      H.h2 "Over ons"
      H.p $ H.preEscapedToHtml ("Ik ben Jappie Klooster. Ik bouw softwareproducten en help bedrijven die serieuze technische expertise nodig hebben. Ik kies technologie op betrouwbaarheid: Haskell, Nix, en wat het werk verder goed doet." :: Text)
      H.p $ H.preEscapedToHtml ("Gevestigd in Nederland. Beschikbaar voor productpartnerschappen, adviesopdrachten en gesprekken over medeoprichterschap." :: Text)
      H.p $ do
        "Meer schrijfsels en cases vindt u op de "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Laatste CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Een probleem dat opgelost moet worden?"
      H.p $ do
        "Of u nu een product, een technische partner of deskundig advies nodig hebt, "
        H.a ! A.href contactMailto $ "neem contact op"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"
  where
    indexMetaNl :: PageMeta
    indexMetaNl = (defaultPageMeta "Jappie Software B.V. \8212 Softwareproducten & deskundig advies")
      { pageMetaDescription = "Softwareproducten en deskundig advies. We bouwen betrouwbare systemen die echte problemen oplossen."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://jappiesoftware.com/nl/"
      , pageMetaSwitchUrl   = Just "/"
      }

-- =============================================================================
-- WordPress websites service page
-- =============================================================================

-- | The "WordPress Websites" service landing page. We build fixed-price
-- WordPress sites for small businesses (warm-intro leads so far), and this page
-- gives that work a home and frames it as the natural first step before a
-- webshop, linking onward to the migration service on webwinkelverhuis.nl. The
-- two recent builds (Voedzame Kost, Het Waardegebaar) are the proof of work.
penguinWordpressPage :: Html
penguinWordpressPage = penguinBaseTemplate En wordpressMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "A website that works for your business."
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("A clean, fast WordPress site, built for one fixed price and handed over so you can manage it yourself. No hourly billing, no surprises, and you pay on delivery." :: Text)
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"

    -- What you get
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "What you get"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "A theme built around your brand"
          H.p $ H.preEscapedToHtml ("We set up a premium-looking theme in your own colours and lay out the pages you need. You approve the look up front: two candidates with dummy content, you pick." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Mobile-first and responsive"
          H.p $ H.preEscapedToHtml ("Built mobile-first and tested on phone, laptop and desktop, so it looks right everywhere: consistent margins, sensible proportions, a layout that works on a small screen." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Contact, WhatsApp and bookings"
          H.p $ H.preEscapedToHtml ("A contact form, your email and phone in plain sight, and a WhatsApp button. Optionally a booking tool (Calendly, Google Appointments) so clients can plan an appointment themselves." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Basic SEO"
          H.p $ H.preEscapedToHtml ("Meta titles and descriptions, image alt text and a clean heading structure so Google understands the page. You supply the keywords, we build them in correctly." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "HTTPS, speed and security"
          H.p $ H.preEscapedToHtml ("SSL (the padlock) switched on, images sized right and a page cache as the final step. On managed hosting this is part of a tidy build, not a separate expensive package." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "A handover, so it stays yours"
          H.p $ H.preEscapedToHtml ("A personal video walkthrough plus a short written manual: edit text, update a page, replace a photo, manage the bookings. You are not tied to us for day-to-day changes." :: Text)

    -- Recent work
    H.section ! A.class_ "results" $ do
      H.h2 "Recent work"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            H.strong "Voedzame Kost"
            H.preEscapedToHtml (": a warm, calm site for a dietitian and ACT coach. A two-colour system separates private clients from organisations, with a workshop showcase and WhatsApp contact. " :: Text)
            H.a ! A.href "https://voedzamekost.nl/" $ H.preEscapedToHtml ("voedzamekost.nl &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            H.strong "Het Waardegebaar"
            H.preEscapedToHtml (": a one-page site built from a wireframe, with a hero, a \"who it is for\" section, a three-step approach, reviews, an inspiration gallery and a contact portal. Responsive, with basic SEO built in. " :: Text)
            H.a ! A.href "https://waardegebaar.nl/" $ H.preEscapedToHtml ("waardegebaar.nl &rarr;" :: Text)

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "How it works"
      H.ol $ do
        H.li $ do
          H.strong "Intake"
          ": we talk through your goals, the pages you need and the style you want."
        H.li $ do
          H.strong "Theme proposal"
          ": you pick from two candidates with dummy content, so you see the look before we build."
        H.li $ do
          H.strong "Build"
          ": we set up the pages and place your texts, images and brand colours."
        H.li $ do
          H.strong "Handover"
          ": the site goes live and you get a walkthrough plus a manual to manage it yourself."

    -- The webshop angle
    H.section ! A.class_ "about" $ do
      H.h2 "A first step, not the last"
      H.p $ H.preEscapedToHtml ("Plenty of businesses start with a site to be found, and later want to sell online. When you are ready to open a webshop, or to move an existing one without losing your Google rankings, we build and migrate those too." :: Text)
      H.p $ do
        "See "
        H.a ! A.href "https://webwinkelverhuis.nl/" $ "webwinkelverhuis.nl"
        " for the webshop side."

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Thinking about a new website?"
      H.p $ do
        H.preEscapedToHtml ("Tell us what your business does and we will sketch what a fixed-price site would look like. " :: Text)
        H.a ! A.href contactMailto $ "Get in touch"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"
  where
    wordpressMeta :: PageMeta
    wordpressMeta = (defaultPageMeta "WordPress Websites \8212 Jappie Software B.V.")
      { pageMetaDescription = "Fixed-price WordPress websites for small businesses: a clean, fast site in your own brand, handed over so you can manage it yourself. Often the first step before a webshop."
      , pageMetaCanonical   = Just "https://jappiesoftware.com/wordpress-websites.html"
      , pageMetaSwitchUrl   = Just "/nl/wordpress-websites.html"
      }

-- =============================================================================
-- WordPress websites service page, Dutch (nl/wordpress-websites.html)
-- =============================================================================

-- | Dutch counterpart of 'penguinWordpressPage'. This is the page the
-- warm-intro SMB leads actually land on, so the Dutch copy is the important one.
penguinWordpressPageNl :: Html
penguinWordpressPageNl = penguinBaseTemplate Nl wordpressMetaNl $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Een website die voor uw bedrijf werkt."
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Een verzorgde, snelle WordPress-site, gebouwd voor \233\233n vaste prijs en zo opgeleverd dat u hem zelf kunt beheren. Geen uurtarief, geen verrassingen, en u betaalt na oplevering." :: Text)
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"

    -- Wat u krijgt
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat u krijgt"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Een thema rond uw merk"
          H.p $ H.preEscapedToHtml ("We richten een premium-ogend thema in uw eigen kleuren in en zetten de pagina's op die u nodig hebt. U keurt de look vooraf goed: twee kandidaten met voorbeeldinhoud, u kiest." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Mobile-first en responsive"
          H.p $ H.preEscapedToHtml ("Mobile-first gebouwd en getest op telefoon, laptop en desktop, zodat het overal klopt: nette marges, kloppende verhoudingen en een indeling die op een klein scherm werkt." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Contact, WhatsApp en afspraken"
          H.p $ H.preEscapedToHtml ("Een contactformulier, uw e-mail en telefoon goed zichtbaar, en een WhatsApp-knop. Optioneel een afsprakentool (Calendly, Google Afspraken) zodat klanten zelf een afspraak inplannen." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Basis-SEO"
          H.p $ H.preEscapedToHtml ("Meta-titels en -beschrijvingen, alt-teksten bij afbeeldingen en een nette koppenstructuur zodat Google de pagina begrijpt. U levert de zoekwoorden, wij bouwen ze correct in." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "HTTPS, snelheid en beveiliging"
          H.p $ H.preEscapedToHtml ("SSL (het slotje) aangezet, afbeeldingen in de juiste maat en een page cache als laatste stap. Op managed hosting hoort dit bij een nette bouw, niet bij een apart duur pakket." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Een overdracht, zodat het van u blijft"
          H.p $ H.preEscapedToHtml ("Een persoonlijke videorondleiding plus een korte handleiding: tekst aanpassen, een pagina bijwerken, een foto vervangen, de afspraken beheren. U bent voor dagelijkse aanpassingen niet afhankelijk van ons." :: Text)

    -- Recent werk
    H.section ! A.class_ "results" $ do
      H.h2 "Recent werk"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            H.strong "Voedzame Kost"
            H.preEscapedToHtml (": een warme, rustige site voor een di\235tist en ACT-coach. Een twee-kleurensysteem scheidt particulieren van organisaties, met een workshop-showcase en WhatsApp-contact. " :: Text)
            H.a ! A.href "https://voedzamekost.nl/" $ H.preEscapedToHtml ("voedzamekost.nl &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            H.strong "Het Waardegebaar"
            H.preEscapedToHtml (": een \233\233n-pagina-site gebouwd vanaf een wireframe, met een hero, een \"voor wie\"-sectie, een werkwijze in drie stappen, reviews, een inspiratie-galerij en een contactportaal. Responsive, met basis-SEO ingebouwd. " :: Text)
            H.a ! A.href "https://waardegebaar.nl/" $ H.preEscapedToHtml ("waardegebaar.nl &rarr;" :: Text)

    -- Hoe het werkt
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Intake"
          ": we bespreken uw doelen, de pagina's die u nodig hebt en de stijl die u wilt."
        H.li $ do
          H.strong "Themavoorstel"
          ": u kiest uit twee kandidaten met voorbeeldinhoud, zodat u de look ziet voordat we bouwen."
        H.li $ do
          H.strong "Bouw"
          ": we zetten de pagina's op en verwerken uw teksten, afbeeldingen en merkkleuren."
        H.li $ do
          H.strong "Overdracht"
          ": de site gaat live en u krijgt een rondleiding plus een handleiding om hem zelf te beheren."

    -- De webshop-stap
    H.section ! A.class_ "about" $ do
      H.h2 "Een eerste stap, niet de laatste"
      H.p $ H.preEscapedToHtml ("Veel bedrijven beginnen met een site om gevonden te worden, en willen later online verkopen. Als u klaar bent om een webshop te openen, of een bestaande te verhuizen zonder uw Google-posities te verliezen, bouwen en migreren we die ook." :: Text)
      H.p $ do
        "Zie "
        H.a ! A.href "https://webwinkelverhuis.nl/" $ "webwinkelverhuis.nl"
        " voor de webshop-kant."

    -- Laatste CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Denkt u na over een nieuwe website?"
      H.p $ do
        "Vertel ons wat uw bedrijf doet, dan schetsen we hoe een site met vaste prijs eruit zou zien. "
        H.a ! A.href contactMailto $ "Neem contact op"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"
  where
    wordpressMetaNl :: PageMeta
    wordpressMetaNl = (defaultPageMeta "WordPress-websites \8212 Jappie Software B.V.")
      { pageMetaDescription = "WordPress-websites met vaste prijs voor het mkb: een verzorgde, snelle site in uw eigen merk, opgeleverd zodat u hem zelf kunt beheren. Vaak de eerste stap voor een webshop."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://jappiesoftware.com/nl/wordpress-websites.html"
      , pageMetaSwitchUrl   = Just "/wordpress-websites.html"
      }

-- =============================================================================
-- Blog index page (paginated listing)
-- =============================================================================

penguinBlogIndexPage :: SiteConfig -> [Article] -> PaginationInfo -> Html
penguinBlogIndexPage _config articles pagination =
  penguinBlogBaseTemplate En blogIndexMeta $
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
  penguinBlogBaseTemplate En articleMeta $
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
