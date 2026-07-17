{-# LANGUAGE OverloadedStrings #-}

-- | Templates for jappiesoftware.com: the company's site for fixed-price,
-- transactional services (WordPress websites, webshop migrations). Carries
-- the penguin theme (voronoi background, green palette). The technical blog
-- remains reachable but unpromoted; consulting lives on jappie.me. Shared
-- SEO/structured-data/blog markup lives in 'PageChrome'; the
-- webshop-migration brand site lives in 'WebwinkelTemplates'.
module PenguinTemplates
  ( WebwinkelverhuisUrl(..)
  , penguinIndexPage
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
  , meetLink
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

-- | Laura's LinkedIn post announcing the voedzamekost.nl launch, linked as
-- social proof next to the site itself. Tracking parameters (utm_*, rcm)
-- are stripped: rcm identifies the account the share link was copied from.
voedzameKostAnnouncementUrl :: H.AttributeValue
voedzameKostAnnouncementUrl = "https://www.linkedin.com/posts/laura-koster-852458a4_voeding-diaebtist-gedragsverandering-share-7483083081000681473-yX2e/"

-- | Origin (no trailing slash) of the webwinkelverhuis.nl site as linked from
-- jappiesoftware.com pages. Production passes the real domain; the local
-- serve mode passes http://localhost:8002 so both locally served sites
-- cross-link to each other instead of jumping to the live site.
newtype WebwinkelverhuisUrl = WebwinkelverhuisUrl Text

-- Decision: the landing page shows exactly two service blocks (websites,
-- webshop migration) instead of the previous six-card product grid. Client
-- feedback: the cards were confusing, three of them jumped straight to
-- webwinkelverhuis.nl, and the page had no pictures and no reading line.
-- Alternatives considered: keeping all six cards with images (still noisy),
-- or one combined block (hides that these are two distinct services).
-- Massapp and IoT were dropped from the landing page; they were "coming
-- soon" filler without a next step for the visitor.

-- | One service on the landing page: an illustration next to a short pitch
-- with a single call to action, rendered as a wide block.
data ServiceBlock = ServiceBlock
  { serviceImageSource :: Text
  , serviceImageAlt :: Text
  , serviceTitle :: Text
  , serviceBody :: Html
  , serviceLinkHref :: Text
  , serviceLinkLabel :: Text
  , serviceNote :: Maybe Html
  }

-- | Render a 'ServiceBlock' as an image-plus-text block with one button.
renderServiceBlock :: ServiceBlock -> Html
renderServiceBlock block =
  H.div ! A.class_ "service-block" $ do
    H.img ! A.src (toValue (serviceImageSource block))
          ! A.alt (toValue (serviceImageAlt block))
          ! A.width "400" ! A.height "300"
    H.div $ do
      H.h3 (toHtml (serviceTitle block))
      H.p (serviceBody block)
      H.a ! A.href (toValue (serviceLinkHref block)) ! A.class_ "cta-button" $
        toHtml (serviceLinkLabel block)
      case serviceNote block of
        Just note -> H.p ! A.class_ "service-note" $ note
        Nothing -> mempty

-- | English "have a website built" service block, linking within this site.
wordpressServiceEn :: ServiceBlock
wordpressServiceEn = ServiceBlock
  { serviceImageSource = "/illustratie-website.svg"
  , serviceImageAlt = "Illustration of a website being built, with a pencil editing the page"
  , serviceTitle = "Have a website built"
  , serviceBody = "A clean, fast WordPress website for your business, for one fixed price. You bring the design, we realise it, and after delivery you update everything yourself."
  , serviceLinkHref = "/wordpress-websites.html"
  , serviceLinkLabel = "What you can expect"
  , serviceNote = Nothing
  }

-- | English webshop-migration service block. This one leaves the site, so the
-- note says so explicitly instead of surprising the visitor.
migrationServiceEn :: WebwinkelverhuisUrl -> ServiceBlock
migrationServiceEn (WebwinkelverhuisUrl url) = ServiceBlock
  { serviceImageSource = "/illustratie-verhuizen.svg"
  , serviceImageAlt = "Illustration of boxes moving from an old webshop to a new one"
  , serviceTitle = "Move your webshop"
  , serviceBody = "Stuck on MijnWebwinkel, CCV Shop or Lightspeed? We move your complete webshop to Shopify or another platform: products, translations, images and the redirects that keep your Google rankings."
  , serviceLinkHref = url <> "/"
  , serviceLinkLabel = "Visit Webwinkelverhuis"
  , serviceNote = Just "Opens webwinkelverhuis.nl, our dedicated migration site."
  }

-- | Dutch "laat een website bouwen" service block, linking within this site.
wordpressServiceNl :: ServiceBlock
wordpressServiceNl = ServiceBlock
  { serviceImageSource = "/illustratie-website.svg"
  , serviceImageAlt = "Illustratie van een website in aanbouw, met een potlood dat de pagina bewerkt"
  , serviceTitle = "Laat een website bouwen"
  , serviceBody = "Een verzorgde, snelle WordPress-website voor uw bedrijf, voor \233\233n vaste prijs. U levert het ontwerp, wij realiseren het, en na oplevering past u alles zelf aan."
  , serviceLinkHref = "/nl/wordpress-websites.html"
  , serviceLinkLabel = "Wat u kunt verwachten"
  , serviceNote = Nothing
  }

-- | Dutch webshop-migration service block. This one leaves the site, so the
-- note says so explicitly instead of surprising the visitor.
migrationServiceNl :: WebwinkelverhuisUrl -> ServiceBlock
migrationServiceNl (WebwinkelverhuisUrl url) = ServiceBlock
  { serviceImageSource = "/illustratie-verhuizen.svg"
  , serviceImageAlt = "Illustratie van dozen die van een oude webshop naar een nieuwe verhuizen"
  , serviceTitle = "Verhuis uw webshop"
  , serviceBody = "Vastgelopen op MijnWebwinkel, CCV Shop of Lightspeed? Wij verhuizen uw complete webshop naar Shopify of een ander platform: producten, vertalingen, afbeeldingen en de redirects die uw Google-posities behouden."
  , serviceLinkHref = url <> "/"
  , serviceLinkLabel = "Naar Webwinkelverhuis"
  , serviceNote = Just "Opent webwinkelverhuis.nl, onze aparte migratie-site."
  }

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
    -- No Blog entry: the technical blog stays reachable (footer link, old
    -- URLs live) but is not maintained or promoted; the menu sells only the
    -- transactional services.
    H.ul $ do
      H.li $ H.a ! A.href (penguinAnchor lang "products") $ toHtml (navProducts lang)
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

-- | Navigation label for the services section (anchor id stays "products" so
-- old links keep working).
navProducts :: Lang -> Text
navProducts En = "Services"
navProducts Nl = "Diensten"

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

penguinIndexPage :: WebwinkelverhuisUrl -> Html
penguinIndexPage webwinkelUrl = penguinBaseTemplate En indexMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "A website or webshop that works for your business."
      H.p ! A.class_ "subtitle" $ "We build websites for small businesses and move webshops to a better platform. Fixed price, plain language, and you pay on delivery."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"

    -- What we do: the two services
    H.section ! A.class_ "for-who" ! A.id "products" $ do
      H.h2 "What we do"
      -- Migration first: it is the better business, so it gets the top slot.
      renderServiceBlock (migrationServiceEn webwinkelUrl)
      renderServiceBlock wordpressServiceEn

    -- How we work: the same three steps every subpage elaborates on
    H.section ! A.class_ "audit" $ do
      H.h2 "How we work"
      H.ol $ do
        H.li $ do
          H.strong "Meet"
          ": a free conversation about what you need. No obligations."
        H.li $ do
          H.strong "Fixed proposal"
          ": one fixed price, agreed up front. No hourly billing, no surprises."
        H.li $ do
          H.strong "Build and deliver"
          ": you check everything before it goes live, and you pay on delivery."

    -- Decision: no consulting section. Consulting leads never came through
    -- this site and Jappie does not want to push that work here; the site
    -- sells fixed-price, transactional services (websites, migrations).
    -- Consulting and case studies live on jappie.me, which the About section
    -- and footer still link. Alternative considered: keeping a slimmed
    -- consulting mention, rejected because it muddies the transactional
    -- positioning for the Ellen/Laura audience.

    -- About
    H.section ! A.class_ "about" $ do
      H.h2 "About"
      H.div ! A.class_ "about-grid" $ do
        H.div $ do
          H.p $ H.preEscapedToHtml ("I&rsquo;m Jappie Klooster. I build websites and webshops for entrepreneurs, for a fixed price and in plain language. Behind the scenes I bring ten-plus years of software engineering, so the technology is chosen for reliability." :: Text)
          H.p $ do
            "Based in the Netherlands. For consulting and case studies, see "
            H.a ! A.href "https://jappie.me/" $ "jappie.me"
            "."
        H.img ! A.class_ "about-portrait"
              ! A.src "/selfie.png"
              ! A.alt "Jappie Klooster"
              ! A.width "300" ! A.height "259"

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Have a problem that needs solving?"
      H.p $ do
        "Whether you need a website or a webshop migration: "
        H.a ! A.href contactMailto $ "get in touch"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"
  where
    indexMeta :: PageMeta
    indexMeta = (defaultPageMeta "Jappie Software B.V. \8212 Websites & Webshop Migrations")
      { pageMetaDescription = "We build websites for small businesses and move webshops to a better platform. Fixed price, plain language, and you pay on delivery."
      , pageMetaCanonical = Just "https://jappiesoftware.com/"
      , pageMetaSwitchUrl = Just "/nl/"
      }

-- =============================================================================
-- Landing page, Dutch (nl/index.html)
-- =============================================================================

-- | Dutch counterpart of 'penguinIndexPage'. Same structure and section ids (so
-- the in-page anchors keep working), Dutch copy. The webshop-migration cards
-- still point at webwinkelverhuis.nl, which is Dutch already.
penguinIndexPageNl :: WebwinkelverhuisUrl -> Html
penguinIndexPageNl webwinkelUrl = penguinBaseTemplate Nl indexMetaNl $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Een website of webshop die voor uw bedrijf werkt."
      H.p ! A.class_ "subtitle" $ "Wij bouwen websites voor ondernemers en verhuizen webshops naar een beter platform. Vaste prijs, gewone taal, en u betaalt na oplevering."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"

    -- Wat we doen: de twee diensten
    H.section ! A.class_ "for-who" ! A.id "products" $ do
      H.h2 "Wat we doen"
      -- Migratie eerst: dat is de betere business, dus die krijgt de toppositie.
      renderServiceBlock (migrationServiceNl webwinkelUrl)
      renderServiceBlock wordpressServiceNl

    -- Hoe we werken: dezelfde drie stappen die elke subpagina uitwerkt
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe we werken"
      H.ol $ do
        H.li $ do
          H.strong "Kennismaken"
          ": een gratis gesprek over wat u nodig hebt. Vrijblijvend."
        H.li $ do
          H.strong "Vast voorstel"
          ": \233\233n vaste prijs, vooraf afgesproken. Geen uurtarief, geen verrassingen."
        H.li $ do
          H.strong "Bouwen en opleveren"
          ": u controleert alles voordat het live gaat, en u betaalt na oplevering."

    -- Geen adviessectie: zie de Decision-notitie op de Engelse pagina.

    -- Over
    H.section ! A.class_ "about" $ do
      H.h2 "Over ons"
      H.div ! A.class_ "about-grid" $ do
        H.div $ do
          H.p "Ik ben Jappie Klooster. Ik bouw websites en webshops voor ondernemers, voor een vaste prijs en in gewone taal. Achter de schermen zit ruim tien jaar software-ervaring, dus de techniek is gekozen op betrouwbaarheid."
          H.p $ do
            "Gevestigd in Nederland. Voor adviesopdrachten en cases: "
            H.a ! A.href "https://jappie.me/" $ "jappie.me"
            "."
        H.img ! A.class_ "about-portrait"
              ! A.src "/selfie.png"
              ! A.alt "Jappie Klooster"
              ! A.width "300" ! A.height "259"

    -- Laatste CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Een probleem dat opgelost moet worden?"
      H.p $ do
        "Of u nu een website of een webshopverhuizing nodig hebt, "
        H.a ! A.href contactMailto $ "neem contact op"
        "."
      H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"
  where
    indexMetaNl :: PageMeta
    indexMetaNl = (defaultPageMeta "Jappie Software B.V. \8212 Websites & webshopverhuizingen")
      { pageMetaDescription = "Wij bouwen websites voor ondernemers en verhuizen webshops naar een beter platform. Vaste prijs, gewone taal, en u betaalt na oplevering."
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
-- webshop, linking onward to the migration service on webwinkelverhuis.nl.
-- The delivered build (Voedzame Kost, live since July 2026) is the proof of
-- work; Het Waardegebaar joins the list once it is delivered.
penguinWordpressPage :: WebwinkelverhuisUrl -> Html
penguinWordpressPage (WebwinkelverhuisUrl webwinkelUrl) = penguinBaseTemplate En wordpressMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Have a website built that works for your business."
          H.p ! A.class_ "subtitle" $ "A clean, fast WordPress site for practices, coaches and other small businesses. One fixed price, your design realised faithfully, and after delivery you update everything yourself."
          H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Get in touch"
        H.img ! A.class_ "hero-image"
              ! A.src "/huiskamer.jpg"
              ! A.alt "Watercolour of a warm living room: the feeling your website should have"
              ! A.width "350" ! A.height "451"

    -- What you can expect
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "What you can expect"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-ontwerp.svg"
                ! A.alt "Verfpalet" ! A.width "56" ! A.height "56"
          H.h3 "Your design, built faithfully"
          H.p "We realise the design you bring: a sketch, a wireframe or a theme you picked as the starting point. The design comes from you; if it is not there yet, we refine it together over a few iterations."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-apparaten.svg"
                ! A.alt "Laptop en telefoon" ! A.width "56" ! A.height "56"
          H.h3 "Looks right on phone and computer"
          H.p "Built and tested on phone, laptop and desktop, so it looks right everywhere: consistent margins, sensible proportions, a layout that works on a small screen."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-contact.svg"
                ! A.alt "Tekstballon en kalender" ! A.width "56" ! A.height "56"
          H.h3 "Contact, WhatsApp and bookings"
          H.p "A contact form, your email and phone in plain sight, and a WhatsApp button. Optionally a booking tool so clients can plan an appointment themselves."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-vindbaar.svg"
                ! A.alt "Vergrootglas boven een pagina" ! A.width "56" ! A.height "56"
          H.h3 "Clean page titles and headings"
          H.p "Meta titles, descriptions and a clear heading structure, built in correctly so Google can read your site. You decide the keywords; we do not offer SEO advice."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-veilig.svg"
                ! A.alt "Schild met bliksem" ! A.width "56" ! A.height "56"
          H.h3 "Safe and fast"
          H.p "The padlock in the browser (SSL), images sized right and a site that loads quickly. That is part of a tidy build, not a separate expensive package."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-zelf-aanpassen.svg"
                ! A.alt "Potlood op een pagina" ! A.width "56" ! A.height "56"
          H.h3 "Update it yourself, no need to call us"
          H.p "A personal video walkthrough plus a short written manual: edit text, replace a photo, update a page. After that you can do it yourself, and we stay available for bigger jobs."

    -- Recent work: Voedzame Kost was delivered (live 2026-07) and shows here;
    -- Het Waardegebaar stays hidden below until it is delivered and live.
    H.section ! A.class_ "results" $ do
      H.h2 "Recent work"
      H.div ! A.class_ "testimonials" $
        H.blockquote $
          H.p $ do
            H.strong "Voedzame Kost"
            H.preEscapedToHtml (": a warm, calm site for a dietitian and ACT coach. A two-colour system separates private clients from organisations, with a workshop showcase and WhatsApp contact. " :: Text)
            H.a ! A.href "https://voedzamekost.nl/" $ H.preEscapedToHtml ("voedzamekost.nl &rarr;" :: Text)
            " ("
            H.a ! A.href voedzameKostAnnouncementUrl $ "announcement"
            ")"
    {-
        H.blockquote $
          H.p $ do
            H.strong "Het Waardegebaar"
            H.preEscapedToHtml (": a one-page site built from a wireframe, with a hero, a \"who it is for\" section, a three-step approach, reviews, an inspiration gallery and a contact portal. Responsive, with basic SEO built in. " :: Text)
            H.a ! A.href "https://waardegebaar.nl/" $ H.preEscapedToHtml ("waardegebaar.nl &rarr;" :: Text)
    -}

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "How does it work?"
      H.ol $ do
        H.li $ do
          H.strong "Meet"
          ": we talk through your goals, the pages you need and the style you want. Free, no obligations."
        H.li $ do
          H.strong "Your design"
          ": you deliver the design: a sketch, a wireframe or a theme as the starting point. The design comes from you; if it is not there yet, we refine it together over a few iterations."
        H.li $ do
          H.strong "Build"
          ": we realise your design and place your texts, photos and brand colours."
        H.li $ do
          H.strong "Handover"
          ": the site goes live and you get a video walkthrough plus a short manual, so you can update everything yourself."

    -- The webshop angle
    H.section ! A.class_ "about" $ do
      H.h2 "A first step, not the last"
      H.p $ H.preEscapedToHtml ("Plenty of businesses start with a site to be found, and later want to sell online. When you are ready to open a webshop, or to move an existing one without losing your Google rankings, we build and migrate those too." :: Text)
      H.p $ do
        "See "
        H.a ! A.href (toValue (webwinkelUrl <> "/")) $ "webwinkelverhuis.nl"
        " for the webshop side."

    -- Final CTA: direct booking is the lowest-friction step for this
    -- audience; email stays available as the text link.
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Thinking about a new website?"
      H.p $ do
        "Tell us what your business does and we will sketch what a fixed-price site would look like. Prefer email? "
        H.a ! A.href contactMailto $ "Get in touch"
        "."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Book a free chat"
  where
    wordpressMeta :: PageMeta
    wordpressMeta = (defaultPageMeta "Have a Website Built \8212 Fixed-Price WordPress \8212 Jappie Software B.V.")
      { pageMetaDescription = "Have a website built for one fixed price: a clean, fast WordPress site realising your design, handed over so you can update everything yourself."
      , pageMetaCanonical   = Just "https://jappiesoftware.com/wordpress-websites.html"
      , pageMetaSwitchUrl   = Just "/nl/wordpress-websites.html"
      }

-- =============================================================================
-- WordPress websites service page, Dutch (nl/wordpress-websites.html)
-- =============================================================================

-- | Dutch counterpart of 'penguinWordpressPage'. This is the page the
-- warm-intro SMB leads actually land on, so the Dutch copy is the important one.
penguinWordpressPageNl :: WebwinkelverhuisUrl -> Html
penguinWordpressPageNl (WebwinkelverhuisUrl webwinkelUrl) = penguinBaseTemplate Nl wordpressMetaNl $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Laat een website bouwen die voor uw bedrijf werkt."
          H.p ! A.class_ "subtitle" $ "Een verzorgde, snelle WordPress-site voor praktijken, coaches en andere kleine ondernemers. E\233n vaste prijs, uw ontwerp trouw gerealiseerd, en na oplevering past u alles zelf aan."
          H.a ! A.href contactMailto ! A.class_ "cta-button" $ "Neem contact op"
        H.img ! A.class_ "hero-image"
              ! A.src "/huiskamer.jpg"
              ! A.alt "Aquarel van een warme huiskamer: de sfeer die uw website mag hebben"
              ! A.width "350" ! A.height "451"

    -- Wat u kunt verwachten
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat u kunt verwachten"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-ontwerp.svg"
                ! A.alt "Verfpalet" ! A.width "56" ! A.height "56"
          H.h3 "Uw ontwerp, trouw gebouwd"
          H.p "Wij realiseren het ontwerp dat u aanlevert: een schets, een wireframe of een gekozen thema als vertrekpunt. Het ontwerp komt van u; komt u er nog niet uit, dan verfijnen we het samen in een paar iteraties."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-apparaten.svg"
                ! A.alt "Laptop en telefoon" ! A.width "56" ! A.height "56"
          H.h3 "Mooi op telefoon en computer"
          H.p "Gebouwd en getest op telefoon, laptop en desktop, zodat het overal klopt: nette marges, kloppende verhoudingen en een indeling die op een klein scherm werkt."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-contact.svg"
                ! A.alt "Tekstballon en kalender" ! A.width "56" ! A.height "56"
          H.h3 "Contact, WhatsApp en afspraken"
          H.p "Een contactformulier, uw e-mail en telefoon goed zichtbaar, en een WhatsApp-knop. Optioneel een afsprakentool zodat klanten zelf een afspraak inplannen."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-vindbaar.svg"
                ! A.alt "Vergrootglas boven een pagina" ! A.width "56" ! A.height "56"
          H.h3 "Nette paginatitels en koppen"
          H.p "Meta-titels, beschrijvingen en een duidelijke koppenstructuur, correct ingebouwd zodat Google uw site kan lezen. U bepaalt de zoekwoorden; SEO-advies geven we niet."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-veilig.svg"
                ! A.alt "Schild met bliksem" ! A.width "56" ! A.height "56"
          H.h3 "Veilig en snel"
          H.p "Het slotje in de browser (SSL), afbeeldingen in de juiste maat en een site die vlot laadt. Dat hoort bij een nette bouw, niet bij een apart duur pakket."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-zelf-aanpassen.svg"
                ! A.alt "Potlood op een pagina" ! A.width "56" ! A.height "56"
          H.h3 "Zelf aanpassen zonder ons te bellen"
          H.p "Een persoonlijke videorondleiding plus een korte handleiding: tekst aanpassen, een foto vervangen, een pagina bijwerken. U kunt het daarna zelf, en voor grotere klussen blijven we bereikbaar."

    -- Recent werk: Voedzame Kost is opgeleverd (live juli 2026) en staat hier;
    -- Het Waardegebaar blijft hieronder verborgen tot het opgeleverd en live is.
    H.section ! A.class_ "results" $ do
      H.h2 "Recent werk"
      H.div ! A.class_ "testimonials" $
        H.blockquote $
          H.p $ do
            H.strong "Voedzame Kost"
            H.preEscapedToHtml (": een warme, rustige site voor een di\235tist en ACT-coach. Een twee-kleurensysteem scheidt particulieren van organisaties, met een workshop-showcase en WhatsApp-contact. " :: Text)
            H.a ! A.href "https://voedzamekost.nl/" $ H.preEscapedToHtml ("voedzamekost.nl &rarr;" :: Text)
            " ("
            H.a ! A.href voedzameKostAnnouncementUrl $ "aankondiging"
            ")"
    {-
        H.blockquote $
          H.p $ do
            H.strong "Het Waardegebaar"
            H.preEscapedToHtml (": een \233\233n-pagina-site gebouwd vanaf een wireframe, met een hero, een \"voor wie\"-sectie, een werkwijze in drie stappen, reviews, een inspiratie-galerij en een contactportaal. Responsive, met basis-SEO ingebouwd. " :: Text)
            H.a ! A.href "https://waardegebaar.nl/" $ H.preEscapedToHtml ("waardegebaar.nl &rarr;" :: Text)
    -}

    -- Hoe het werkt
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe gaat het in zijn werk?"
      H.ol $ do
        H.li $ do
          H.strong "Kennismaken"
          ": we bespreken uw doelen, de pagina's die u nodig hebt en de stijl die u wilt. Gratis en vrijblijvend."
        H.li $ do
          H.strong "Uw ontwerp"
          ": u levert het ontwerp aan: een schets, een wireframe of een thema als vertrekpunt. Het ontwerp komt van u; komt u er nog niet uit, dan verfijnen we het samen in een paar iteraties."
        H.li $ do
          H.strong "Bouw"
          ": wij realiseren uw ontwerp en verwerken uw teksten, foto's en merkkleuren."
        H.li $ do
          H.strong "Overdracht"
          ": de site gaat live en u krijgt een videorondleiding plus een korte handleiding, zodat u alles zelf kunt aanpassen."

    -- De webshop-stap
    H.section ! A.class_ "about" $ do
      H.h2 "Een eerste stap, niet de laatste"
      H.p $ H.preEscapedToHtml ("Veel bedrijven beginnen met een site om gevonden te worden, en willen later online verkopen. Als u klaar bent om een webshop te openen, of een bestaande te verhuizen zonder uw Google-posities te verliezen, bouwen en migreren we die ook." :: Text)
      H.p $ do
        "Zie "
        H.a ! A.href (toValue (webwinkelUrl <> "/")) $ "webwinkelverhuis.nl"
        " voor de webshop-kant."

    -- Laatste CTA: direct een gesprek inplannen is voor deze doelgroep de
    -- laagste drempel; mailen kan via de tekstlink.
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Denkt u na over een nieuwe website?"
      H.p $ do
        "Vertel ons wat uw bedrijf doet, dan schetsen we hoe een site met vaste prijs eruit zou zien. Liever mailen? "
        H.a ! A.href contactMailto $ "Neem contact op"
        "."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gratis kennismaking"
  where
    wordpressMetaNl :: PageMeta
    wordpressMetaNl = (defaultPageMeta "Laat een website bouwen \8212 WordPress met vaste prijs \8212 Jappie Software B.V.")
      { pageMetaDescription = "Laat een website bouwen voor \233\233n vaste prijs: een verzorgde, snelle WordPress-site die uw ontwerp realiseert. Na oplevering past u alles zelf aan."
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
