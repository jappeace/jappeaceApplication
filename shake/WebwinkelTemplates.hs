{-# LANGUAGE OverloadedStrings #-}

-- | Templates for webwinkelverhuis.nl: the dedicated webshop-migration brand
-- domain. It carries its own navigation, footer and theme (distinct from the
-- jappiesoftware.com penguin theme in 'PenguinTemplates'), a landing page, the
-- per-platform migration and "waarom" pages, and its own blog. The operating
-- company behind it is still Jappie Software B.V.; shared structured data and
-- blog markup come from 'PageChrome'.
module WebwinkelTemplates
  ( webwinkelIndexPage
  , prijzenPage
  , scanPage
  , webwinkelBlogIndexPage
  , webwinkelArticlePage
  , appPage
  , mijnwebwinkelMigrationPage
  , ccvshopMigrationPage
  , lightspeedMigrationPage
  , mijnwebwinkelWaaromPage
  , lightspeedWaaromPage
  , relativizeWebwinkelContentImages
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
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
  , percentEncodeQuery
  , migratieBasisprijsEuro
  , meetLink
  , whatsappFloatingButton
  , jsonLdString
  , serviceJsonLd
  , FaqQuestion
  , FaqAnswer
  , faqAnswerText
  , faqAnswerHtml
  , renderFaqItem
  , faqPageJsonLd
  , formatIsoDate
  , formatHumanDate
  , articleMetaDescription
  , renderBlogSummary
  , renderPagination
  )

-- =============================================================================
-- Theme constants
-- =============================================================================

-- | Site-default social-share image, hosted on this domain.
webwinkelOgImage :: Text
webwinkelOgImage = "https://webwinkelverhuis.nl/og-default.png"

-- | Rewrite absolute webwinkelverhuis.nl image sources in a rendered page to
-- root-relative ones, so images load on the local `shake-blog serve` preview
-- as well as in production.
--
-- Decision: org content must reference images by absolute production URL,
-- because pandoc's org reader turns a root-relative @[[/x.png]]@ link into a
-- broken @file:///x.png@; this pass restores same-origin loading afterwards.
-- Only @src=@ attributes are rewritten: @href=@ is left alone so canonical
-- link tags and social-share URLs stay absolute for SEO.
relativizeWebwinkelContentImages :: TL.Text -> TL.Text
relativizeWebwinkelContentImages =
  TL.replace "src=\"https://webwinkelverhuis.nl/" "src=\"/"

-- | Organization structured data for the webwinkelverhuis.nl brand. Presents
-- Webwinkelverhuis as its own entity (own name, url, email, logo) to Google and
-- rich-result consumers, with Jappie Software B.V. as the accurate legal parent,
-- instead of inheriting jappiesoftware.com's shared 'organizationJsonLd'.
webwinkelOrganizationJsonLd :: Html
webwinkelOrganizationJsonLd =
  H.script ! A.type_ "application/ld+json" $ H.preEscapedToHtml organizationJson
  where
    organizationJson :: Text
    organizationJson = mconcat
      [ "{\"@context\":\"https://schema.org\""
      , ",\"@type\":\"Organization\""
      , ",\"name\":\"Webwinkelverhuis\""
      , ",\"url\":\"https://webwinkelverhuis.nl/\""
      , ",\"logo\":\"https://webwinkelverhuis.nl/favicon.svg\""
      , ",\"image\":" <> jsonLdString webwinkelOgImage
      , ",\"email\":" <> jsonLdString webwinkelEmail
      , ",\"telephone\":\"+31644237437\""
      , ",\"identifier\":\"KVK 95097872\""
      , ",\"areaServed\":\"NL\""
      , ",\"parentOrganization\":{\"@type\":\"Organization\",\"name\":\"Jappie Software B.V.\",\"url\":\"https://jappiesoftware.com/\"}"
      , "}"
      ]

-- | Contact address for the webwinkelverhuis.nl brand. On-brand with the domain
-- the visitor is on (the jappiesoftware.com penguin site keeps its own
-- 'companyEmail'); a mismatched contact address on a trust page reads as "who am
-- I actually emailing?".
webwinkelEmail :: Text
webwinkelEmail = "jappie@webwinkelverhuis.nl"

-- | The "ask for a quote" mailto used by every call-to-action button. The body
-- is a gentle fill-in template: a blank mail box is intimidating, and these
-- prompts tell the merchant which details make for a good quote (mirroring the
-- calculator's questions) without forcing them through the calculator first.
offerteMailto :: H.AttributeValue
offerteMailto =
  toValue
    ( "mailto:" <> webwinkelEmail
        <> "?subject=Migratie%20offerte&body="
        <> percentEncodeQuery offerteBodyTemplate
    )

-- | Fill-in template for a quote request from a plain CTA button (not the
-- calculator). Kept in sync by hand with the calculator's questions.
offerteBodyTemplate :: Text
offerteBodyTemplate =
  "Hallo,\n\n"
    <> "Ik wil graag een offerte voor het verhuizen van mijn webshop. "
    <> "Om u een goede prijs te kunnen geven, alvast wat info (vul in wat u weet):\n\n"
    <> "- Huidig platform (bijv. MijnWebwinkel, CCV Shop): \n"
    <> "- Gewenst platform (Shopify of WooCommerce): \n"
    <> "- Aantal producten (ongeveer): \n"
    <> "- Aantal talen: \n"
    <> "- Meenemen (klantaccounts, bestelgeschiedenis, nieuwsbrief, voorraad, reviews): \n"
    <> "- Domeinnaam of e-mail nog bij MijnWebwinkel?: \n"
    <> "- Bijzonderheden (kassa/point-of-sale, zakelijke klanten, verzendkoppeling): \n\n"
    <> "Met vriendelijke groet,"

-- | Mailto for merchants whose migration is already running or done and who
-- want follow-up work (mass edits, theme changes, integrations). The subject
-- differs from 'offerteMailto' so these mails are recognisable as
-- existing-client work rather than new leads.
uitbreidingMailto :: H.AttributeValue
uitbreidingMailto = toValue ("mailto:" <> webwinkelEmail <> "?subject=Uitbreiding%20webshop")

-- =============================================================================
-- Base template
-- =============================================================================

-- | Shared page skeleton for webwinkelverhuis.nl. @ogType@ is the Open Graph
-- type ("website" for landing/migration pages, "article" for blog posts) and
-- @includeFeed@ adds the Atom feed link used on blog pages.
webwinkelBaseWith :: Text -> Bool -> PageMeta -> Html -> Html
webwinkelBaseWith ogType includeFeed meta content =
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
      H.meta ! customAttribute "property" "og:image" ! A.content (toValue (resolveOgImage webwinkelOgImage meta))
      H.meta ! customAttribute "property" "og:image:width" ! A.content "1200"
      H.meta ! customAttribute "property" "og:image:height" ! A.content "630"
      -- Twitter Card tags
      H.meta ! A.name "twitter:card" ! A.content "summary_large_image"
      H.meta ! A.name "twitter:title" ! A.content (toValue (pageMetaTitle meta))
      H.meta ! A.name "twitter:description" ! A.content (toValue (pageMetaDescription meta))
      H.meta ! A.name "twitter:image" ! A.content (toValue (resolveOgImage webwinkelOgImage meta))
      -- Canonical URL
      case pageMetaCanonical meta of
        Just canonicalUrl -> H.link ! A.rel "canonical" ! A.href (toValue canonicalUrl)
        Nothing -> mempty
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.type_ "image/svg+xml" ! A.href "/favicon.svg"
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      if includeFeed
        then H.link ! A.href "/blog/atom"
                    ! A.type_ "application/atom+xml"
                    ! A.rel "alternate"
                    ! A.title "Webwinkelverhuis blog"
        else mempty
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-GD4S885G6F" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-GD4S885G6F');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
      webwinkelOrganizationJsonLd
      pageMetaExtraHead meta
    H.body $ do
      H.header $
        H.nav ! A.class_ "top-nav" $ do
          H.span ! A.class_ "logo" $
            H.a ! A.href "/" $ "Webwinkelverhuis"
          H.ul $ do
            H.li $ H.a ! A.href "/migrate-mijnwebwinkel.html" $ "MijnWebwinkel"
            H.li $ H.a ! A.href "/migrate-lightspeed.html" $ "Lightspeed"
            H.li $ H.a ! A.href "/migrate-ccvshop.html" $ "CCV Shop"
            H.li $ H.a ! A.href "/prijzen.html" $ "Prijzen"
            H.li $ H.a ! A.href "/blog/" $ "Blog"
            H.li $ H.a ! A.href offerteMailto ! A.class_ "cta-link" $ "Offerte"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href (toValue ("mailto:" <> webwinkelEmail)) ! A.class_ "footer-mail" $ toHtml webwinkelEmail
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ do
          "Webwinkelverhuis is een dienst van "
          H.a ! A.href "https://jappiesoftware.com/" $ "Jappie Software B.V."
          H.preEscapedToHtml (" &middot; KVK: 95097872 &middot; Ooievaarstraat 38, 8262 AN Kampen" :: Text)
      H.script $ H.preEscapedToHtml ctaTrackScript

-- | Track clicks on the call-to-action buttons in Google Analytics, so we see
-- the dropoff per acquisition path: the plain "vraag een offerte aan" mailto
-- buttons (offerte_mailto_klik) and the "plan een gesprek" meeting links
-- (gesprek_knop_klik). Skipped: the calculator's own button (.calc-offerte),
-- which Elm reports itself with richer params, and the footer contact-mail
-- (.footer-mail), which is general contact, not an offerte-CTA, so it would
-- pollute the button metric. Page views are collected by GA4 automatically, so
-- the migration pages need no extra event here.
ctaTrackScript :: Text
ctaTrackScript =
  "document.addEventListener('DOMContentLoaded',function(){"
    <> "function track(sel,ev){document.querySelectorAll(sel).forEach(function(a){"
    <> "if(a.classList.contains('calc-offerte')||a.classList.contains('footer-mail'))return;"
    <> "a.addEventListener('click',function(){"
    <> "if(window.gtag){gtag('event',ev,{knop_tekst:(a.textContent||'').trim().slice(0,60)});}"
    <> "});});}"
    <> "track('a[href^=\"mailto:\"]','offerte_mailto_klik');"
    <> "track('a[href^=\"https://meet.jappiesoftware.com\"]','gesprek_knop_klik');"
    <> "});"

-- | Boot the Elm price calculator and forward its analytics port to gtag, so the
-- calculator's funnel events (engaged, blocked, requested) land in Google
-- Analytics alongside the page views.
prijsCalculatorInitScript :: Text
prijsCalculatorInitScript =
  "var prijsCalcApp = Elm.PrijsCalculator.init({node: document.getElementById('prijs-calculator-mount')});"
    <> "if(prijsCalcApp.ports&&prijsCalcApp.ports.analyticsEvent){"
    <> "prijsCalcApp.ports.analyticsEvent.subscribe(function(e){"
    <> "if(window.gtag){gtag('event', e.name, e.params||{});}"
    <> "});}"

-- | Landing / migration page skeleton (Open Graph type "website").
webwinkelBaseTemplate :: PageMeta -> Html -> Html
webwinkelBaseTemplate = webwinkelBaseWith "website" False

-- | Blog page skeleton (Open Graph type "article", with Atom feed link).
webwinkelBlogBaseTemplate :: PageMeta -> Html -> Html
webwinkelBlogBaseTemplate = webwinkelBaseWith "article" True

-- =============================================================================
-- Landing page (index.html)
-- =============================================================================

-- Decision: het Shopify Basic-maandbedrag staat met prijspeildatum naast onze
-- eigen prijzen (merchant-persona read-through, aug 2026): "wat ga ik daarna
-- per maand betalen" was het grootste onbeantwoorde bezwaar op de pagina.
-- Alternatief was alleen naar shopify.com linken zonder bedrag, maar een
-- bezoeker die moet doorklikken om de maandprijs te vinden leest dat als
-- verstopte kosten. De datum maakt een latere prijswijziging bij Shopify
-- geen gebroken belofte.

-- | Shopify Basic per maand bij jaarlijkse betaling, in euro's.
shopifyBasicJaarlijksEuroPerMaand :: Text
shopifyBasicJaarlijksEuroPerMaand = "21"

-- | Shopify Basic per maand bij maandelijkse betaling, in euro's.
shopifyBasicMaandelijksEuroPerMaand :: Text
shopifyBasicMaandelijksEuroPerMaand = "28"

-- | Peildatum van de getoonde Shopify-prijzen; bijwerken wanneer de bedragen
-- opnieuw gecontroleerd worden.
shopifyPrijspeil :: Text
shopifyPrijspeil = "augustus 2026"

-- | Shopify's eigen prijzenpagina, waar de getoonde bedragen vandaan komen.
shopifyPrijzenUrl :: H.AttributeValue
shopifyPrijzenUrl = "https://www.shopify.com/nl/prijzen"

-- | Note under the price sections: what the merchant pays Shopify itself per
-- month after the migration, datestamped and linked to Shopify's own pricing
-- page. Shown wherever 'prijzen' appears and on 'prijzenPage'.
shopifyKostenNote :: Html
shopifyKostenNote =
  H.p ! A.class_ "engagement-note" $ do
    H.preEscapedToHtml
      ( "Naast onze eenmalige migratieprijs betaalt u het abonnement van uw nieuwe platform. Shopify Basic kost bijvoorbeeld &euro;"
          <> shopifyBasicJaarlijksEuroPerMaand
          <> " per maand bij jaarlijkse betaling en &euro;"
          <> shopifyBasicMaandelijksEuroPerMaand
          <> " per maand bij maandelijkse betaling (prijspeil " <> shopifyPrijspeil <> ", zie "
      )
    H.a ! A.href shopifyPrijzenUrl $ "de actuele Shopify-prijzen"
    ")."

prijzen :: Html
prijzen = H.section ! A.class_ "engagement" ! A.id "prijzen" $ do
      H.h2 "Prijzen"
      H.div ! A.class_ "card-grid" $ do
        H.div ! A.class_ "card" $ do
          H.h3 $ "Volledige migratie" <> H.preEscapedToHtml (" vanaf &euro;" <> migratieBasisprijsEuro)
          H.p $ H.preEscapedToHtml ("Inclusief 1.000 producten en &eacute;&eacute;n taal: producten, afbeeldingen, categorie&euml;n, klantdata, voorraad en SEO-redirects. Grotere catalogi, extra talen en losse diensten (e-mail-setup, en domeinverhuizing als uw domein nog bij uw huidige platform staat) hebben een vaste meerprijs." :: Text)
          H.a ! A.href "/prijzen.html#rekenhulp" ! A.class_ "cta-button" $ "Bereken direct uw prijs"
      shopifyKostenNote



webwinkelIndexPage :: Html
webwinkelIndexPage = webwinkelBaseTemplate indexMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Webshop verhuizen zonder data en SEO verlies."
          H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Vastgelopen op MijnWebwinkel, CCV Shop of Lightspeed? Wij verhuizen uw complete webshop geautomatiseerd naar Shopify of een ander platform. U betaalt pas na een succesvolle migratie." :: Text)
          -- Decision: the landing-page CTAs point at the price calculator
          -- instead of an offerte-mailto. The calculator gives the visitor an
          -- immediate, tangible result (Gemini-feedback: CTAs should offer
          -- direct value) and its own offerte button follows once they have a
          -- price on screen.
          H.a ! A.href "/prijzen.html#rekenhulp" ! A.class_ "cta-button" $ "Bereken direct uw prijs"
          -- Naast de prijsvraag de twijfelvraag: "is mijn shop eigenlijk
          -- slecht af?". De gratis scan (/scan.html) beantwoordt die met een
          -- meting in plaats van een verkooppraatje.
          H.a ! A.href "/scan.html" ! A.class_ "cta-button-secondary" $ "Beoordeel mijn webshop"
        H.img ! A.class_ "hero-image"
              ! A.src "/illustratie-verhuizen.svg"
              ! A.alt "Illustratie van dozen die van een oude webshop naar een nieuwe verhuizen"
              ! A.width "400" ! A.height "300"

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.div ! A.class_ "audit-grid" $ do
        H.ol $ do
          H.li $ do
            H.strong "Scan"
            ": ons programma leest uw huidige webshop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
          H.li $ do
            H.strong "Wennen"
            ": de testshop draait naast uw huidige webshop, die gewoon doordraait. U raakt op uw gemak bekend met uw nieuwe shop."
          H.li $ do
            H.strong "DNS-overzet"
            ": bent u er klaar voor? Dan wijzen we uw domein op de nieuwe shop en bent u verhuisd."
        scanDemo

    recentWerkSection

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. U kiest het platform: Shopify, WooCommerce, of vraag ons advies voor uw situatie. Wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (": u betaalt pas na een succesvolle migratie" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (": geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "Zelfvaliderend" >> H.preEscapedToHtml (": het programma valideert zijn eigen werk" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (": elke oude link krijgt een 301-redirect en blijft werken, uw opgebouwde SEO verhuist mee" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (": geen uurtarief, u weet vooraf wat het kost" :: Text)
      H.p $ do
        "Meer weten over waarom shops vertrekken? Lees onze "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Platform cards: the routing step, at the bottom so the visitor first
    -- reads the promise, the process and the proof before picking a platform.
    -- Secondary buttons: the orange primary is reserved for the offerte and
    -- plan-een-gesprek actions.
    H.section ! A.class_ "for-who" ! A.id "platforms" $ do
      H.h2 "Vanaf welk platform verhuist u?"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-bevroren.svg"
                ! A.alt "Sneeuwvlok: het platform is bevroren"
                ! A.width "56" ! A.height "56"
          H.h3 "MijnWebwinkel"
          H.p $ H.preEscapedToHtml ("Bevroren platform, verdubbelde prijzen, gesloten community. Wij zetten alles over, inclusief de automatisch gegenereerde 301-redirects voor uw artikel-URLs." :: Text)
          H.a ! A.href "/migrate-mijnwebwinkel.html" ! A.class_ "cta-button-secondary" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-prijsstijging.svg"
                ! A.alt "Grafiek met stijgende prijzen"
                ! A.width "56" ! A.height "56"
          H.h3 "Lightspeed"
          H.p $ H.preEscapedToHtml ("Beursgenoteerd en steeds duurder voor kleine shops. Ons programma legt elke oude URL vast (allemaal, niet een steekproef) en stuurt die door met een 301-redirect. Bij onbegeleide migraties zagen we verhalen van 70% verkeersverlies; dat is precies wat wij voorkomen." :: Text)
          H.a ! A.href "/migrate-lightspeed.html" ! A.class_ "cta-button-secondary" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-beperkt.svg"
                ! A.alt "Hangslot: beperkte mogelijkheden"
                ! A.width "56" ! A.height "56"
          H.h3 "CCV Shop"
          H.p $ H.preEscapedToHtml ("Steeds duurder, terwijl het winkelbestand krimpt en het zwaartepunt na de Fiserv-overname bij betalen en kassa ligt. Wij zetten uw producten, talen, klantaccounts en voorraad volledig geautomatiseerd over." :: Text)
          H.a ! A.href "/migrate-ccvshop.html" ! A.class_ "cta-button-secondary" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)

    -- Pricing: listed openly. Hiding the price reads as evasive to a
    -- doubtful merchant; a visible "vanaf" and the breakdown radiate
    -- confidence. The offerte remains the binding guarantee, the
    -- disclaimer says so plainly so an updated price is never a broken
    -- promise.
    prijzen
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ mapM_ renderFaqItem homeFaq

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te verhuizen?"
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    indexMeta :: PageMeta
    indexMeta = PageMeta
      { pageMetaTitle       = "Webwinkelverhuis \8212 Verhuis uw webshop zonder zorgen"
      , pageMetaDescription = "Geautomatiseerde webshop-migratie van MijnWebwinkel, CCV Shop of Lightspeed naar Shopify. Producten, vertalingen, klantdata en SEO-redirects. Betaling na succesvolle migratie."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd homeFaq
      }

-- | The homepage FAQ: the questions that most often keep a visitor from
-- reaching out, answered before they leave. The same pairs feed the visible
-- list and 'faqPageJsonLd'. The platform-choice answer deliberately offers
-- advice instead of a platform list: which platform fits depends on the
-- shop's situation, and "weet ik nog niet" is a fine starting point (also
-- selectable in the price calculator).
homeFaq :: [(FaqQuestion, FaqAnswer)]
homeFaq =
  [ ( "Kan mijn webshop verhuisd worden?"
    , faqAnswerText "Vrijwel altijd. Producten, teksten, afbeeldingen, klanten en de categoriestructuur zetten we geautomatiseerd over vanaf MijnWebwinkel, CCV Shop, Lightspeed en andere platformen, inclusief 301-redirects van al uw oude URLs zodat uw Google-posities meeverhuizen." )
  , ( "Naar welk platform kan ik het beste verhuizen?"
    , faqAnswerText "Dat hangt af van uw situatie: uw assortiment, uw koppelingen en hoeveel u zelf wilt kunnen aanpassen. Shopify is het meest gekozen doelplatform omdat het makkelijk is. WooCommerce kan een goede optie zijn omdat het flexibel is. Weet u het nog niet? In een gratis gesprek adviseren we een platform op basis van uw situatie, en in de rekenhulp kunt u die keuze gewoon openlaten." )
  , ( "Hoe lang duurt een migratie?"
    , faqAnswerText "Het technische overzetten van uw producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan uw nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Wat kost een webshop-migratie?"
    , faqAnswerHtml $ do
        toHtml ("Een vaste prijs vanaf " <> migratieBasisprijsEuro <> " euro, afhankelijk van het aantal producten en talen, en u betaalt pas na een geslaagde migratie. Met ")
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp op de prijzenpagina"
        " berekent u in een minuut uw richtprijs." )
  , ( "Kan mijn shop blijven doorverkopen tijdens de migratie?"
    , faqAnswerText "Ja. De nieuwe shop bouwen we naast uw huidige webshop op, die gewoon doordraait en verkoopt. Pas bij de livegang zetten we uw domein om naar waar u heen wilt. Tegen die tijd heeft u vertrouwen in het nieuwe systeem en is alles getest." )
  ]

-- =============================================================================
-- MijnWebwinkel migration landing page
-- =============================================================================

-- | Screen-reader label for the WhatsApp button on the MijnWebwinkel migration
-- page. Dutch, matching the page's audience.
mijnwebwinkelWhatsappLabel :: Text
mijnwebwinkelWhatsappLabel = "Open een WhatsApp-gesprek met ons"

-- | Pre-filled message for the WhatsApp button on the MijnWebwinkel migration
-- page. Dutch, matching the page's audience.
mijnwebwinkelWhatsappMessage :: Text
mijnwebwinkelWhatsappMessage = "Hallo, ik heb een vraag over de migratie van mijn MijnWebwinkel-shop."

-- | The migration app's landing page (webwinkelverhuis.nl/app.html). This is
-- the App URL of the custom Shopify app we install in a client's store to run
-- the import; Shopify shows the merchant this page on install. The audience is
-- a merchant who already agreed to the migration, not a new lead, so instead
-- of a quote button it explains what the app is and why it asks for access,
-- and lists the follow-up services we offer after the migration (mass edits,
-- theme work, integrations). Marked noindex: it is a utility page, not part of
-- the marketing funnel.
-- | One line of the animated scan panel: what fills the bar, and what the
-- visitor "krijgt" when the check lands.
data ScanStap = ScanStap
  { scanStapLabel :: Text
  , scanStapResultaat :: Text
  }

-- | The steps the scan panel animates through, in the vocabulary of a
-- non-technical shop owner: what would THEY worry about in a move? Products,
-- photos, customers, loyalty points, and whether Google still finds them.
-- Deliberately no "redirects", "slugs" or other jargon.
scanStappen :: [ScanStap]
scanStappen =
  [ ScanStap "Producten" "2.400 ingepakt"
  , ScanStap "Foto's en teksten" "alles mee"
  , ScanStap "Klantaccounts" "iedereen mee"
  , ScanStap "Spaarpunten" "saldo klopt"
  , ScanStap "Linkvertaling voor Google" "werkt door"
  , ScanStap "Alles nalopen" "niets vergeten"
  ]

-- | The animated "scan panel" next to the how-it-works steps: a stylised view
-- of the migration program at work, as progress bars that fill one after
-- another and land on a plain-language result. Pure CSS (per-row keyframes in
-- style.css selected via @nth-child@, all sharing one timeline so the cycle
-- ends with every bar draining together before refilling one by one), and
-- hidden from screen readers because the steps text next to it carries the
-- same information.
--
-- Decision: an animated illustration instead of a recording of the real tool.
-- The migration program is a CLI that only ever runs on our own machine, so a
-- capture would show the customer nothing recognisable; this panel shows the
-- same work in the customer's own vocabulary (Gemini-feedback: show, don't
-- tell).
scanDemo :: Html
scanDemo =
  H.div ! A.class_ "scan-demo" $ do
    H.div ! customAttribute "aria-hidden" "true" $ do
      H.div ! A.class_ "scan-demo-kop" $ "Uw webshop verhuist"
      H.ul ! A.class_ "scan-demo-lijst" $
        mapM_ scanDemoItem scanStappen

-- | One animated row of the scan panel; the row's position in the list picks
-- its keyframes via @nth-child@ in style.css, which staggers the fills.
scanDemoItem :: ScanStap -> Html
scanDemoItem stap =
  H.li ! A.class_ "scan-item" $ do
    H.div ! A.class_ "scan-item-rij" $ do
      H.span $ toHtml (scanStapLabel stap)
      H.span ! A.class_ "scan-resultaat" $ toHtml (scanStapResultaat stap <> " \10003")
    H.div ! A.class_ "scan-balk" $
      H.div ! A.class_ "scan-balk-vulling" $ mempty

-- | The shared "Recent werk" proof section: the Panzer-ShopNL migration,
-- linking to both the case-study blog post and the live shop. Shown on the
-- index page and the MijnWebwinkel migration page.
recentWerkSection :: Html
recentWerkSection =
  H.section ! A.class_ "results" $ do
    H.h2 "Recent werk"
    H.div ! A.class_ "testimonials" $ do
      H.blockquote $
        H.p $ do
          H.strong "Panzer-ShopNL"
          H.preEscapedToHtml (": een modeltreinwinkel met 2.400+ producten over drie domeinen en drie talen, verhuisd van MijnWebwinkel naar Shopify. Inclusief vertalingen, de volledige categorieboom en link behoud, zodat de SEO meeverhuisde. " :: Text)
          H.a ! A.href "/blog/klantverhaal-panzer-shopnl-van-mijnwebwinkel-naar-shopify-in-drie-talen.html" $ H.preEscapedToHtml ("Lees het klantverhaal &rarr;" :: Text)
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "https://panzer-shop.nl/" $ H.preEscapedToHtml ("panzer-shop.nl &rarr;" :: Text)

appPage :: Html
appPage = webwinkelBaseTemplate appMeta $ do
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "De migratie-app"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("U ziet deze pagina omdat de migratie-app van Webwinkelverhuis in uw Shopify-winkel is ge&iuml;nstalleerd. Dat is precies de bedoeling: de app is het gereedschap waarmee wij uw webshop naar Shopify overzetten." :: Text)
      H.a ! A.href "#meer" ! A.class_ "cta-button" $ "Wat we verder voor u kunnen doen"

    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat de app doet"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten plaatsen"
          H.p "De app zet uw producten, varianten, afbeeldingen, prijzen en SKU's in uw nieuwe Shopify-winkel."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n en pagina&rsquo;s" :: Text)
          H.p $ H.preEscapedToHtml ("Uw categorie&euml;n worden Shopify-collections en uw informatiepagina&rsquo;s worden meegenomen, inclusief het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "De app legt 301-redirects aan van elke oude URL naar de juiste nieuwe pagina, zodat elke bestaande link blijft werken en uw opgebouwde SEO meeverhuist."
        H.li ! A.class_ "card" $ do
          H.h3 "Thema"
          H.p "De app bouwt en plaatst een Shopify-thema dat de uitstraling van uw huidige winkel volgt."

    H.section ! A.class_ "audit" $ do
      H.h2 "Waarom de app toegang vraagt"
      H.p "Om dit werk te doen vraagt de app toegang tot precies de onderdelen die hij plaatst:"
      H.ul $ do
        H.li $ H.strong "Producten" >> ": om uw artikelen, varianten en collections aan te maken."
        H.li $ H.strong "Content" >> ": om uw pagina's, navigatie en redirects over te zetten."
        H.li $ H.strong "Klanten" >> ": om bestaande klantaccounts mee te nemen."
        H.li $ H.preEscapedToHtml ("<strong>Thema&rsquo;s</strong>: om het nieuwe thema te plaatsen." :: Text)
        H.li $ H.strong "Vertalingen" >> ": om meertalige content correct te koppelen."

    H.section ! A.class_ "about" $ do
      H.h2 "Veilig en tijdelijk"
      H.p "De app is alleen nodig tijdens de migratie. Wij installeren hem in uw winkel om uw data te plaatsen, en daarna kan hij verwijderd worden. Hij maakt geen onderdeel uit van uw winkel voor uw bezoekers."
      H.p $ do
        H.a ! A.href "/migrate-mijnwebwinkel.html" $ "Lees hoe de migratie werkt"
        H.preEscapedToHtml (" &rarr;" :: Text)

    H.section ! A.class_ "for-who" ! A.id "meer" $ do
      H.h2 "Wat we verder voor u kunnen doen"
      H.p $ H.preEscapedToHtml ("Dezelfde techniek waarmee we uw shop verhuizen, zetten we ook na de migratie voor u in. Enkele voorbeelden van wat we voor andere webwinkels hebben gedaan:" :: Text)
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Massabewerkingen"
          H.p $ H.preEscapedToHtml ("Duizenden producten in &eacute;&eacute;n batch aanpassen: merknamen corrigeren, verkeerd vertaalde termen rechtzetten of prijzen bijwerken, over de hele catalogus en in alle talen tegelijk." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "SEO in bulk"
          H.p $ H.preEscapedToHtml ("Alle meta titles en meta descriptions opnieuw opbouwen in &eacute;&eacute;n uniforme stijl, per taal en per categorie." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Thema-uitbreidingen"
          H.p "Uw Shopify-thema uitbreiden met extra secties of functionaliteit, of de vormgeving verder afstemmen op uw huisstijl."
        H.li ! A.class_ "card" $ do
          H.h3 "Koppelingen en apps"
          H.p "Uw boekhouding of facturatie koppelen aan Shopify, een verhuur-app inrichten, of een extra taal toevoegen inclusief vertaalde URL's en redirects."
        H.li ! A.class_ "card" $ do
          H.h3 "Training"
          H.p $ H.preEscapedToHtml ("Een rondleiding door uw nieuwe shop met beknopte handleiding, of een cursus Shopify van twee uur, &eacute;&eacute;n-op-&eacute;&eacute;n." :: Text)
      H.p "Alles tegen een vaste prijs per klus, geen uurtarief. U weet vooraf waar u aan toe bent."
      H.a ! A.href uitbreidingMailto ! A.class_ "cta-button" $ "Bespreek uw idee met ons"
  where
    appMeta :: PageMeta
    appMeta = PageMeta
      { pageMetaTitle       = "De migratie-app van Webwinkelverhuis"
      , pageMetaDescription = "Uitleg over de migratie-app van Webwinkelverhuis: het gereedschap waarmee wij uw webshop naar Shopify overzetten, en wat we na de migratie verder voor uw shop kunnen doen."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/app.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = H.meta ! A.name "robots" ! A.content "noindex"
      }

-- | The full public price list on its own page. The homepage keeps a
-- short teaser and links here; this page carries every line so a
-- doubtful merchant who wants the whole picture finds it openly, no
-- "bel ons om te horen wat het kost". The lock-in note states plainly
-- that shown prices are indicative and only an offerte binds them, so
-- a later price change is never a broken promise.
prijzenPage :: Html
prijzenPage = webwinkelBaseTemplate prijzenMeta $
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "Prijzen"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Vaste prijzen, vooraf afgesproken. U betaalt pas na een succesvolle migratie. Hieronder ziet u precies waar u aan toe bent." :: Text)

    H.section ! A.class_ "engagement" ! A.id "rekenhulp" $ do
      H.h2 "Bereken uw richtprijs"
      H.p "Beantwoord een paar vragen over uw shop en u ziet meteen een indicatie."
      H.div ! A.id "prijs-calculator-mount" $ mempty
      H.noscript $ H.p "De rekenhulp heeft JavaScript nodig. Hieronder staat de volledige prijslijst zodat u ook zonder JavaScript alles ziet."
      H.div ! A.class_ "calc-footnotes" $ do
        H.h3 "Over de themakeuze"
        H.p "Kiest u voor zelf inrichten, dan staat uw shop na de migratie op een standaard Shopify-thema dat u zelf verzorgt of door een ontwerper naar keuze laat doen. Theming hoeft niet via ons; wij doen het ook en zijn er inmiddels aardig goed in. Probeer het gerust eerst zelf: uw oude shop blijft gewoon draaien naast de nieuwe, dus u loopt geen risico. Komt u er niet uit, dan helpen we u alsnog."
        H.p $ H.preEscapedToHtml ("Bij uitstraling overzetten (&euro;749) benaderen we uw huidige uitstraling zo dicht mogelijk; kleine aanpassingen op verzoek zitten erbij." :: Text)
        H.p "Een volledig nieuw ontwerp is los ontwerpwerk en prijzen we op aanvraag."

    H.section ! A.class_ "engagement" $ do
      H.h2 "De migratie"
      H.table ! A.class_ "price-table" $ H.tbody $ do
        H.tr $ do
          H.td "Basismigratie: t/m 1.000 producten, 1 taal"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;1.999" :: Text)
        H.tr $ do
          H.td "Extra producten, boven de 1.000 (eerste taal)"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;0,25 per product" :: Text)
        H.tr $ do
          H.td "Extra taal: per product over de hele catalogus"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;0,25 per product" :: Text)
        H.tr $ do
          H.td "Extra taal: configuratie, per taal"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
      shopifyKostenNote

    H.section ! A.class_ "engagement" $ do
      H.h2 "Modules en extra diensten"
      H.p "Losse onderdelen die u naar keuze bijschakelt. U betaalt alleen voor wat u meeneemt."
      H.table ! A.class_ "price-table" $ H.tbody $ do
        H.tr $ do
          H.td "Uitstraling overzetten"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;749" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Klantaccounts meenemen (uw klanten houden hun inlog)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td "Bestelgeschiedenis meenemen"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td "Nieuwsbrief-aanmeldingen meenemen"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td "Voorraadaantallen live overzetten"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td "Reviews / beoordelingen overzetten"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;150" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Domeinverhuizing (uw domeinnaam staat nog bij MijnWebwinkel)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("E-mail-setup (mailboxen, SPF/DKIM, doorstuurregels)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;150" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Verzendkoppeling (bijv. DHL: pakketten en labels vanuit uw shop)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;150" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("B2B-kanaal (aparte prijzen en inlog voor zakelijke klanten)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;750" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Kassa / point-of-sale (Shopify POS in uw fysieke winkel)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;750" :: Text)
        H.tr $ do
          H.td "Volledig nieuw ontwerp"
          H.td ! A.class_ "price-cell" $ "op aanvraag"
        H.tr $ do
          H.td "Catalogus-brede teksttransformaties"
          H.td ! A.class_ "price-cell" $ "op aanvraag"
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Na de migratie staat uw shop op een standaard Shopify-thema dat u zelf inricht. De vormgeving hoeft niet via ons: u kunt het zelf doen of een ontwerper naar keuze inhuren. Wilt u het uit handen geven, dan benaderen wij uw huidige uitstraling zo dicht mogelijk (&euro;749) of ontwerpen we iets nieuws (op aanvraag)." :: Text)
      H.p ! A.class_ "engagement-note" $ "Kassa/point-of-sale zetten we bij u op locatie op. Installatie en reiskosten komen daar los bij, op aanvraag."

    H.section ! A.class_ "results" $ do
      H.h2 "Altijd inbegrepen"
      H.ul $ do
        H.li $ H.preEscapedToHtml ("Producten, afbeeldingen, categorie&euml;n, klantdata en voorraad." :: Text)
        H.li $ H.preEscapedToHtml ("Uw teksten, meta-titels en informatiepagina&apos;s (zoals over-ons en blog) verhuizen mee." :: Text)
        H.li "SEO-redirects (301) voor elke oude URL, zodat uw links en opgebouwde SEO meeverhuizen."
        H.li "Een testshop naast uw draaiende winkel; DNS-overzet met zo min mogelijk downtime."
        H.li "Betaling pas na een succesvolle migratie."

    H.section ! A.class_ "final-cta" $ do
      H.h2 "Geldt deze prijs voor mij?"
      H.p "Deze prijzen kunnen we in de toekomst aanpassen, en de hier getoonde bedragen zijn een indicatie, geen garantie. Alleen een offerte legt uw prijs vast. Wilt u tegen deze prijzen verhuizen? Vraag nu een offerte aan, dan staat uw prijs zwart-op-wit."
      H.div ! A.class_ "cta-row" $ do
        H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"
        H.a ! A.href meetLink ! A.class_ "cta-button-secondary" $ "Liever eerst sparren? Plan een gesprek"

    H.script ! A.src "/prijs-calculator.js" $ mempty
    H.script $ H.preEscapedToHtml prijsCalculatorInitScript
  where
    prijzenMeta :: PageMeta
    prijzenMeta = PageMeta
      { pageMetaTitle       = "Prijzen \8212 Webwinkelverhuis"
      , pageMetaDescription = "Vaste prijzen voor uw webshop-migratie naar Shopify: vanaf \8364\&1.999 inclusief 1.000 producten en 1 taal. Domeinverhuizing \8364\&250, e-mail-setup \8364\&150. Betaling na succesvolle migratie."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/prijzen.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

-- =============================================================================
-- Webshop-scanner page (scan.html)
-- =============================================================================

-- | Boot the Elm webshop scanner and forward its analytics port to gtag,
-- mirroring 'prijsCalculatorInitScript'. The scanner's funnel events
-- (scanner_started, scanner_klaar, scanner_mislukt, gesprek_knop_klik) land in
-- Google Analytics alongside the page views. The port is named
-- scannerAnalyticsEvent because Elm port names are global per program and the
-- calculator already claims analyticsEvent.
scannerFormInitScript :: Text
scannerFormInitScript =
  "var scannerApp = Elm.ScannerForm.init({node: document.getElementById('webshop-scanner-mount')});"
    <> "if(scannerApp.ports&&scannerApp.ports.scannerAnalyticsEvent){"
    <> "scannerApp.ports.scannerAnalyticsEvent.subscribe(function(e){"
    <> "if(window.gtag){gtag('event', e.name, e.params||{});}"
    <> "});}"

-- | The "beoordeel mijn webshop" page: an Elm app (elm/src/ScannerForm.elm)
-- that posts the visitor's shop URL to /api/scan, polls the scan status and
-- renders the Dutch report with scores, kernmetingen and verbeterpunten. The
-- scan backend serves the API on the same origin.
scanPage :: Html
scanPage = webwinkelBaseTemplate scanMeta $
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "Beoordeel mijn webshop"
      H.p ! A.class_ "subtitle" $ "Vul het adres van uw webshop in. Wij meten hem door en u ziet binnen enkele minuten waar u staat: snelheid, vindbaarheid en de punten die beter kunnen."
    H.section ! A.class_ "engagement" $ do
      H.div ! A.id "webshop-scanner-mount" $ mempty
      H.noscript $ H.p "De beoordeling heeft JavaScript nodig. Liever direct contact? Plan een gratis gesprek via meet.jappiesoftware.com."
    H.script ! A.src "/scanner-form.js" $ mempty
    H.script $ H.preEscapedToHtml scannerFormInitScript
  where
    scanMeta :: PageMeta
    scanMeta = PageMeta
      { pageMetaTitle       = "Beoordeel mijn webshop \8212 Webwinkelverhuis"
      , pageMetaDescription = "Gratis beoordeling van uw webshop: wij meten snelheid en vindbaarheid met Lighthouse en laten zien welke verbeterpunten oplosbaar zijn en welke vastliggen in uw platform."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/scan.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

mijnwebwinkelMigrationPage :: Html
mijnwebwinkelMigrationPage = webwinkelBaseTemplate migrationMeta $ do
  whatsappFloatingButton mijnwebwinkelWhatsappLabel mijnwebwinkelWhatsappMessage
  H.main $ do
    -- Hero: friendly reassurance rather than pressure, with the escape
    -- illustration. The visitor already knows the platform is stuck; this
    -- page's job is making the move feel safe.
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Ontsnap MijnWebwinkel"
          H.p ! A.class_ "subtitle" $ "Uw webshop is uw broodwinning, en MijnWebwinkel staat al jaren stil. Verhuizen voelt als een grote stap, maar het hoeft geen sprong in het diepe te zijn: wij zetten uw complete shop geautomatiseerd over, zonder dataverlies en met zo min mogelijk downtime. U betaalt pas na een succesvolle migratie."
          H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"
        H.img ! A.class_ "hero-image"
              ! A.src "/illustratie-ontsnappen.svg"
              ! A.alt "Illustratie van dozen die een bevroren webshop verlaten richting een zonnige nieuwe winkel"
              ! A.width "400" ! A.height "300"

    -- What we migrate. One card per data type, each with a pictogram; the
    -- technical depth (redirect internals, platform choice) lives in the FAQ
    -- instead of a second near-duplicate card grid.
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      -- The Google card leads the grid: keeping Google rankings is the
      -- service's biggest selling point. Copy is deliberately jargon-arm
      -- (les: merchant-persona read-through, aug 2026): benefit first, the
      -- technical term at most in parentheses. "301-redirect" stays once as
      -- searchable keyword; SKU's, Collections en URL-slugs zijn vertaald
      -- naar winkelierswoorden.
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-redirects.svg"
                ! A.alt "Pijl die een nieuwe route neemt" ! A.width "56" ! A.height "56"
          H.h3 "Uw plek in Google"
          H.p "Elke oude link naar uw shop blijft werken en stuurt bezoekers automatisch naar de juiste nieuwe pagina (301-redirects). Uw opgebouwde positie in Google verhuist mee."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-producten.svg"
                ! A.alt "Doos met producten" ! A.width "56" ! A.height "56"
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, artikelnummers en varianten. Automatisch overgezet naar uw nieuwe platform."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-categorieen.svg"
                ! A.alt "Categorieboom" ! A.width "56" ! A.height "56"
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("Uw volledige categorie-indeling en het navigatiemenu verhuizen mee, inclusief vertaalde titels. Ook uw informatiepagina&apos;s (contact, voorwaarden, verzendinformatie) gaan mee." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-thema.svg"
                ! A.alt "Verfpalet" ! A.width "56" ! A.height "56"
          H.h3 "Thema"
          H.p "Uw nieuwe shop krijgt een thema dat de uitstraling van uw huidige winkel volgt. U begint niet met een kale winkel."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-talen.svg"
                ! A.alt "Twee tekstballonnen" ! A.width "56" ! A.height "56"
          H.h3 "Meerdere talen"
          H.p "Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien, ook op de vertaalde webadressen van uw pagina's."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-spaarpunten.svg"
                ! A.alt "Munt met ster" ! A.width "56" ! A.height "56"
          H.h3 "Klanten & spaarpunten"
          H.p "Klantaccounts, bestelgeschiedenis en spaarpuntensaldi verhuizen mee. Uw klanten kunnen direct inloggen, met hun spaarpunten in het loyaliteitsprogramma van uw nieuwe platform."
      H.p ! A.class_ "engagement-note" $ "Ook de rest van uw shop verhuist mee: informatiepagina's zoals over-ons en verzendinformatie, uw blog, reviews, kortingscodes en cadeaubonnen. Als aparte dienst doen we ook grootschalige aanpassingen aan uw productdata tijdens de migratie, zoals prijsaanpassingen, het opschonen van beschrijvingen of beschrijvingen voor Google bij al uw afbeeldingen (alt-teksten)."

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          ": ons programma leest uw MijnWebwinkel-shop volledig uit en bouwt er een testshop mee op, met producten, vertalingen, categorie\235n en doorverwijzingen."
        H.li $ do
          H.strong "Wennen"
          ": de testshop draait naast uw MijnWebwinkel-shop, die gewoon doordraait. U raakt op uw gemak bekend met uw nieuwe shop."
        H.li $ do
          H.strong "Livegang"
          ": bent u er klaar voor? Dan wijzen we uw domeinnaam op de nieuwe shop en bent u verhuisd. Uw shop is hooguit 5 tot 30 minuten beperkt bereikbaar, terwijl het beveiligingscertificaat (het slotje in de browser) opnieuw wordt aangemaakt."

    -- Recent werk: proof before price, so the number lands on trust.
    recentWerkSection

    -- Pricing
    prijzen

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $ do
          H.p "U weet het al: MijnWebwinkel gaat nergens meer heen. Geen nieuwe features, geen community, trage support. Trage laadtijden schaden uw SEO, en dat is op MijnWebwinkel niet te verbeteren. Gelukkig hoeft u daar niet op te wachten: verhuizen is inmiddels een gebaande weg."
          H.p $ do
            H.a ! A.href "/waarom-mijnwebwinkel.html" $ "Waarom wordt MijnWebwinkel niet meer doorontwikkeld?"
            H.preEscapedToHtml (" &rarr;" :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. U kiest het platform: Shopify, WooCommerce, of iets anders. Wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> ": u betaalt pas na succesvolle migratie"
        H.li $ H.strong "Geautomatiseerd" >> ": geen handmatig overtypen, geen kopieerfouten"
        H.li $ H.strong "Gecontroleerd" >> ": het programma telt na afloop alles na, van elk product tot elke klant"
        H.li $ H.strong "SEO-behoud" >> ": elke oude link blijft werken en uw opgebouwde positie in Google verhuist mee"
        H.li $ H.strong "Meertalig" >> ": vertalingen correct gekoppeld via de offici\235le koppelingen van uw nieuwe platform"
        H.li $ H.strong "Vaste prijs" >> ": geen uurtarief, u weet vooraf wat het kost"

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ mapM_ renderFaqItem mijnwebwinkelFaq

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven een eerlijke inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gratis gesprek"
  where
    migrationMeta :: PageMeta
    migrationMeta = PageMeta
      { pageMetaTitle       = "Ontsnap MijnWebwinkel \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van MijnWebwinkel naar Shopify, WooCommerce of een ander platform. Producten, vertalingen, afbeeldingen en SEO-redirects. Vanaf \8364" <> migratieBasisprijsEuro <> "."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd mijnwebwinkelFaq <> serviceJsonLd
          "MijnWebwinkel naar Shopify migratie"
          "Geautomatiseerde migratie van MijnWebwinkel naar Shopify, WooCommerce of een ander platform: producten, vertalingen, afbeeldingen, categorieboom en SEO-redirects."
          "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html"
      }

-- | The MijnWebwinkel-page FAQ. The extra livelihood questions (bestellingen
-- tijdens de verhuizing, e-mail, wat kost Shopify zelf, verhuizen mijn
-- pagina's en reviews) come from a merchant-persona read-through (aug 2026):
-- ze bleken de grootste stille bezwaren op de pagina. Antwoorden die de
-- lezer ergens heen sturen (rekenhulp, Shopify-prijzen) dragen echte links
-- via 'faqAnswerHtml'.
mijnwebwinkelFaq :: [(FaqQuestion, FaqAnswer)]
mijnwebwinkelFaq =
  [ ( "Hoe lang duurt een migratie?"
    , faqAnswerText "Het technische overzetten van uw producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, betaalmethoden, eventuele koppelingen, en rustig wennen aan uw nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Wat gebeurt er met bestellingen tijdens de verhuizing?"
    , faqAnswerHtml $ do
        "Uw MijnWebwinkel-shop blijft gewoon open en verkoopt door terwijl wij de nieuwe shop opbouwen. Vlak voor de livegang zetten we de laatste stand over, zodat ook recente bestellingen en actuele voorraadaantallen meekomen. Bestelgeschiedenis en voorraad kiest u als optie in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        "." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja, uw webadres blijft gewoon van u. Bij de livegang wijzen we uw domeinnaam op de nieuwe shop, en alle oude links worden automatisch doorgestuurd." )
  , ( "Wat gebeurt er met mijn e-mailadres?"
    , faqAnswerHtml $ do
        "Uw e-mailadres blijft gewoon werken. Loopt uw e-mail nu via MijnWebwinkel, dan zetten we uw mailboxen en doorstuurregels over als losse dienst (e-mail-setup in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        "), zodat u geen bericht mist." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma telt na afloop alles na. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die MijnWebwinkel en uw nieuwe platform beide ondersteunen. Ook de vertaalde webadressen verhuizen mee." )
  , ( "Kan ik ook naar een ander platform dan Shopify migreren?"
    , faqAnswerText "Ja. Shopify wordt het meest gekozen, maar we kunnen ook migreren naar WooCommerce of andere platformen." )
  , ( "Wat kost Shopify zelf per maand?"
    , faqAnswerHtml $ do
        toHtml ("Het Basic-abonnement van Shopify kost \8364" <> shopifyBasicJaarlijksEuroPerMaand
          <> " per maand bij jaarlijkse betaling, of \8364" <> shopifyBasicMaandelijksEuroPerMaand
          <> " per maand als u per maand betaalt (prijspeil " <> shopifyPrijspeil
          <> "). Voor de meeste shops die van MijnWebwinkel komen is Basic voldoende. De actuele prijzen vindt u op ")
        H.a ! A.href shopifyPrijzenUrl $ "shopify.com/nl/prijzen"
        "." )
  , ( "Kunnen jullie een website importeren naar Shopify?"
    , faqAnswerText "Ja. Producten, teksten, afbeeldingen, klanten en de categoriestructuur van uw bestaande website worden automatisch naar Shopify overgezet, inclusief automatische doorverwijzingen (301-redirects) van al uw oude links, zodat uw Google-posities meeverhuizen." )
  , ( "Worden spaarpunten ook overgezet?"
    , faqAnswerText "Ja. Spaarpuntensaldi van uw klanten worden meegenomen naar het loyaliteitsprogramma van uw nieuwe platform." )
  , ( "Verhuizen mijn pagina's, reviews en kortingscodes ook?"
    , faqAnswerHtml $ do
        "Ja. Informatiepagina's zoals over-ons en verzendinformatie en uw blog verhuizen standaard mee. Reviews neemt u als optie mee in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        ", en ook kortingscodes en cadeaubonnen zetten we voor u over. Alles wat in uw shop zit, kan mee." )
  , ( "Hoe werken de SEO-redirects precies?"
    , faqAnswerText "MijnWebwinkel bouwt zijn links op uit interne artikelnummers. We hebben uitgezocht hoe dat precies werkt, waardoor we voor elke oude link automatisch de juiste doorverwijzing (301-redirect) kunnen aanmaken, ook voor links met nummers erin." )
  , ( "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
    , faqAnswerText "Ja. We kunnen grootschalige wijzigingen doorvoeren, bijvoorbeeld prijzen aanpassen, beschrijvingen opschonen, of beschrijvingen voor Google toevoegen aan al uw afbeeldingen (alt-teksten)." )
  ]

-- =============================================================================
-- CCV Shop migration landing page
-- =============================================================================

ccvshopMigrationPage :: Html
ccvshopMigrationPage = webwinkelBaseTemplate ccvMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Ontsnap CCV Shop"
      -- Decision: hero benoemt CCV-specifieke pijn (prijsstijging,
      -- krimpend winkelbestand, Fiserv-zwaartepunt) in plaats van de
      -- generieke MWW-tekst. Bronnen in
      -- jappiesoft/research/ccv-woocommerce-market-onderzoek.org.
      -- "Thema's en functies staan al jaren stil" stond hier tot 2 aug
      -- 2026 en is verwijderd: directe metingen weerspreken het (eigen
      -- nieuwspagina meldt samengestelde producten en thema-updates in
      -- apr 2026 en een bol.com-koppeling in mrt 2026; de API-SDK kreeg
      -- in 2026 maandelijkse releases). Een CCV-winkelier die die
      -- functies gebruikt, prikt daar meteen doorheen. Wat blijft is
      -- wat we kunnen aantonen; de Fiserv-passage blijft bewust een
      -- constatering over zwaartepunt, geen EOL-voorspelling.
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Sinds de Amerikaanse betaalreus Fiserv CCV overnam ligt het zwaartepunt bij betalen en kassa: uw webshop is een tweede rang product. Wij verhuizen uw complete shop geautomatiseerd naar Shopify, WooCommerce of een ander platform van uw keuze: zonder dataverlies, met zo min mogelijk downtime." :: Text)
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar het formaat van uw doelplatform."
        H.li ! A.class_ "card" $ do
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien, ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantaccounts"
          H.p "Klantgegevens en bestelgeschiedenis worden overgezet zodat uw klanten direct kunnen inloggen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Uw backlinks blijven werken en uw opgebouwde SEO verhuist mee."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadinformatie en staffelprijzen worden meegenomen. Per-variant prijzen en voorraadbeheer werken direct in uw nieuwe shop."

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          ": ons programma leest uw CCV Shop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Wennen"
          ": de testshop draait naast uw CCV Shop, die gewoon doordraait. U raakt op uw gemak bekend met uw nieuwe shop."
        H.li $ do
          H.strong "DNS-overzet"
          ": bent u er klaar voor? Dan wijzen we uw domein op de nieuwe shop en bent u verhuisd. We houden de downtime zo klein mogelijk; het aanmaken van een nieuw SSL-certificaat kan nog 5 tot 30 minuten duren."

    -- Pricing
    prijzen

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        -- Decision: echte klachtcitaat van een CCV-gebruiker
        -- (Trustpilot, gedocumenteerd in
        -- jappiesoft/research/ccv-woocommerce-market-onderzoek.org)
        -- in plaats van een verzonnen peptalk in een testimonial-blok.
        H.blockquote $ do
          H.p $ H.preEscapedToHtml ("&bdquo;7 jaar een CCV shop gehad, de webshop is zwaar verouderd, zowel in thema&apos;s als functionaliteiten, er wordt bijna niks geupdate. Mijn omzet en conversie is door het dak gegaan sinds ik Shopify gebruik.&rdquo;" :: Text)
          H.p $ H.strong "CCV Shop-gebruiker op Trustpilot, mei 2025"
      -- Decision: het groeiverhaal leidt hier, niet verval-retoriek. Uit
      -- de leave-reden-scan (jappiesoft research, 2 aug 2026): de
      -- grootste groep CCV-vertrekkers groeit uit het platform (top-
      -- bestemming is een custom cart, 13 van 28), dus de pagina spreekt
      -- ambitie aan in plaats van angst.
      H.p $ H.preEscapedToHtml ("De meeste winkeliers verlaten CCV Shop niet omdat het kapot is, maar omdat ze eruit groeien: maatwerk of functies die er niet in zitten. Shopify en WooCommerce groeien w&eacute;l met u mee, van extra verkoopkanalen en talen tot B2B-maatwerk en duizenden apps. En het lastigste deel van uitgroeien, alles heelhuids overzetten, is precies ons vak." :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. U kiest het platform: Shopify, WooCommerce, of iets anders. Wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (": u betaalt pas na succesvolle migratie" :: Text)
        H.li $ H.strong "Platformonafhankelijk" >> H.preEscapedToHtml (": u kiest de bestemming, wij migreren naar elk platform" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (": geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (": elke oude link krijgt een 301-redirect en blijft werken, uw opgebouwde SEO verhuist mee" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (": vertalingen correct gekoppeld via offici&euml;le APIs" :: Text)
        H.li $ H.strong "Zelfvaliderend" >> H.preEscapedToHtml (": het programma valideert zijn eigen werk" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (": geen uurtarief, u weet vooraf wat het kost" :: Text)

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ mapM_ renderFaqItem ccvshopFaq

    -- Technical details
    H.section ! A.class_ "for-who" $ do
      H.h2 "Technische details"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "Volledige 301-redirect mapping van elke oude URL naar het nieuwe adres. Uw Google-rankings, backlinks en organisch verkeer blijven behouden."
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen, alles in &eacute;&eacute;n keer." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & staffelprijzen"
          H.p "Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar uw nieuwe shop. Inclusief prijsverschillen per maat of kleur."
        H.li ! A.class_ "card" $ do
          H.h3 "Vertalingen & URL-slugs"
          H.p $ H.preEscapedToHtml ("Meertalige content wordt correct gekoppeld via de offici&euml;le API van uw doelplatform. Inclusief vertaalde URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantdata & accounts"
          H.p "Klantaccounts en bestelgeschiedenis worden overgezet zodat uw klanten direct verder kunnen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "Testshop"
          H.p "U krijgt een volledige testshop naast uw huidige shop om alvast te wennen. Pas na uw akkoord gaan we live; eventuele correcties zijn inbegrepen."
        H.li ! A.class_ "card" $ do
          H.h3 "Fysieke winkel & kassa"
          H.p "Verkoopt u ook in een fysieke winkel? Bij WooCommerce als bestemming kan uw huidige CCV-pinterminal via een kassakoppeling blijven; bij Shopify richten we Shopify POS voor u in zodat winkel en webshop weer dezelfde voorraad delen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p $ H.preEscapedToHtml ("U hoeft niet langer te wachten tot CCV Shop beter wordt. Neem de controle terug over uw webshop." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    ccvMeta :: PageMeta
    ccvMeta = PageMeta
      { pageMetaTitle       = "Ontsnap CCV Shop \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van CCV Shop naar Shopify, WooCommerce of een ander platform. Producten, vertalingen, afbeeldingen, voorraad en SEO-redirects. Vanaf \8364" <> migratieBasisprijsEuro <> "."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-ccvshop.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd ccvshopFaq <> serviceJsonLd
          "CCV Shop naar Shopify migratie"
          "Geautomatiseerde migratie van CCV Shop naar Shopify, WooCommerce of een ander platform: producten, vertalingen, afbeeldingen, voorraad, klantdata en SEO-redirects."
          "https://webwinkelverhuis.nl/migrate-ccvshop.html"
      }

ccvshopFaq :: [(FaqQuestion, FaqAnswer)]
ccvshopFaq =
  [ ( "Hoe lang duurt een migratie?"
    , faqAnswerText "Het technische overzetten van uw producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan uw nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma valideert zijn eigen werk. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die CCV Shop en uw doelplatform beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , faqAnswerText "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat uw klanten direct verder kunnen." )
  , ( "Hoe werken de SEO-redirects precies?"
    , faqAnswerText "We genereren automatisch 301-redirects van elke oude URL naar het nieuwe adres. Uw Google-rankings en backlinks blijven behouden." )
  -- Decision: POS-antwoord is bewust eerlijk over de beperking en
  -- maakt er een bestemmingskeuze van: bij WooCommerce blijft de
  -- CCV-terminal via Nederlandse kassasoftware gekoppeld, bij
  -- Shopify kan hij alleen als losse (niet-geïntegreerde) pin
  -- blijven, want geïntegreerd ondersteunt Shopify POS uitsluitend
  -- eigen terminals via Shopify Payments (help.shopify.com, gecheckt
  -- 2 aug 2026). "Moet vervangen" zeggen we dus niet: dat geldt
  -- alleen voor wie een geïntegreerde kassa wil. Bron-onderzoek in
  -- jappiesoft/research/ccv-woocommerce-market-onderzoek.org.
  , ( "Ik heb ook een fysieke winkel met een CCV-pinterminal. Kan die mee?"
    , faqAnswerText $ "Dat hangt af van uw bestemming. Kiest u WooCommerce, dan kan uw CCV-terminal gewoon gekoppeld blijven: Nederlandse kassasoftware verbindt de terminal en synchroniseert de voorraad met uw webshop, en uw pincontract loopt door. Kiest u Shopify, dan koppelt de terminal niet meer met de kassa; hij kan wel als losse pin blijven werken, maar dan typt u elk bedrag over. Wilt u winkel en webshop weer als \233\233n geheel, dan vervangt u hem door een Shopify-terminal. Die overstap is eenmalig en bescheiden (\8364" <> "59 tot \8364" <> "249) en daarna bent u ook voor het pinnen van CCV af. Wij hebben ervaring met het inrichten van kassa's en pinterminals en nemen dit gewoon in het migratietraject mee." )
  ]

-- =============================================================================
-- Lightspeed migration landing page
-- =============================================================================

lightspeedMigrationPage :: Html
lightspeedMigrationPage = webwinkelBaseTemplate lightspeedMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Ontsnap Lightspeed"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Lightspeed duwt u richting hun nieuwe platform of de deur uit. Ondertussen draait uw webshop op verouderde software die steeds minder krijgt. Wij verhuizen uw complete shop naar Shopify: geautomatiseerd, zonder dataverlies, zonder SEO-verlies." :: Text)
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar het formaat van uw doelplatform."
        H.li ! A.class_ "card" $ do
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien, ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantaccounts & bestellingen"
          H.p "Klantgegevens, bestelgeschiedenis en accountdata worden overgezet zodat uw klanten direct kunnen inloggen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Uw backlinks blijven werken en uw opgebouwde SEO verhuist mee; bij onbegeleide migraties zagen we verhalen van 70% verkeersverlies."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n & navigatie" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadbeheer, staffelprijzen en per-variant pricing worden correct overgezet. Uw voorraadniveaus kloppen direct in uw nieuwe shop."

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          ": ons programma leest uw Lightspeed-shop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Wennen"
          ": de testshop draait naast uw Lightspeed-shop, die gewoon doordraait. U raakt op uw gemak bekend met uw nieuwe shop."
        H.li $ do
          H.strong "DNS-overzet"
          ": bent u er klaar voor? Dan wijzen we uw domein op de nieuwe shop en bent u verhuisd. We houden de downtime zo klein mogelijk; het aanmaken van een nieuw SSL-certificaat kan nog 5 tot 30 minuten duren."

    -- Pricing
    prijzen

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $ do
          H.p $ H.preEscapedToHtml ("U bent niet de enige: 59% van alle Lightspeed-vertrekkers kiest Shopify. Maar zonder begeleiding gaan bij de overstap vaak oude URLs kapot; wij hebben verhalen gezien van 70% verkeersverlies bij een onbegeleide migratie. Wij zorgen dat elke oude URL blijft doorverwijzen (ons programma legt ze allemaal vast, niet een steekproef) en uw opgebouwde SEO meeverhuist." :: Text)
          H.p $ do
            H.a ! A.href "/waarom-lightspeed.html" $ "Waarom verlaten steeds meer webshops Lightspeed?"
            H.preEscapedToHtml (" &rarr;" :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. U kiest het platform: Shopify, WooCommerce, of iets anders. Wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (": u betaalt pas na succesvolle migratie" :: Text)
        H.li $ H.strong "Platformonafhankelijk" >> H.preEscapedToHtml (": u kiest de bestemming, wij migreren naar elk platform" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (": elke oude link krijgt een 301-redirect en blijft werken, uw opgebouwde SEO verhuist mee" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (": geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (": vertalingen correct gekoppeld via offici&euml;le APIs" :: Text)
        H.li $ H.strong "Zelfvaliderend" >> H.preEscapedToHtml (": het programma valideert zijn eigen werk" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (": geen uurtarief, u weet vooraf wat het kost" :: Text)

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ mapM_ renderFaqItem lightspeedFaq

    -- Technical details
    H.section ! A.class_ "for-who" $ do
      H.h2 "Technische details"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "Volledige 301-redirect mapping van elke oude URL naar het nieuwe adres. Uw Google-rankings, backlinks en organisch verkeer blijven behouden."
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen, alles in &eacute;&eacute;n keer." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & staffelprijzen"
          H.p "Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar uw nieuwe shop. Inclusief prijsverschillen per maat of kleur."
        H.li ! A.class_ "card" $ do
          H.h3 "Vertalingen & URL-slugs"
          H.p $ H.preEscapedToHtml ("Meertalige content wordt correct gekoppeld via de offici&euml;le API van uw doelplatform. Inclusief vertaalde URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantdata & accounts"
          H.p "Klantaccounts en bestelgeschiedenis worden overgezet zodat uw klanten direct verder kunnen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "Testshop"
          H.p "U krijgt een volledige testshop naast uw huidige shop om alvast te wennen. Pas na uw akkoord gaan we live; eventuele correcties zijn inbegrepen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p $ H.preEscapedToHtml ("Lightspeed gaat u niet helpen met deze overstap. Wij wel." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    lightspeedMeta :: PageMeta
    lightspeedMeta = PageMeta
      { pageMetaTitle       = "Ontsnap Lightspeed \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van Lightspeed naar Shopify, WooCommerce of een ander platform. Producten, vertalingen, afbeeldingen, voorraad en SEO-redirects. Vanaf \8364" <> migratieBasisprijsEuro <> "."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-lightspeed.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd lightspeedFaq <> serviceJsonLd
          "Lightspeed naar Shopify migratie"
          "Geautomatiseerde migratie van Lightspeed naar Shopify, WooCommerce of een ander platform: producten, vertalingen, afbeeldingen, voorraad en SEO-redirects."
          "https://webwinkelverhuis.nl/migrate-lightspeed.html"
      }

lightspeedFaq :: [(FaqQuestion, FaqAnswer)]
lightspeedFaq =
  [ ( "Hoe lang duurt een migratie?"
    , faqAnswerText "Het technische overzetten van uw producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan uw nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma valideert zijn eigen werk. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Verlies ik mijn Google-posities?"
    , faqAnswerText "Elke oude URL krijgt automatisch een 301-redirect, zodat al uw links en backlinks blijven werken en de opgebouwde SEO meeverhuist. Google kan na elke grote sitewijziging tijdelijk schommelen; het blijvende verlies uit de horrorverhalen komt door ontbrekende redirects, en dat dekken wij volledig af." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die Lightspeed en Shopify beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , faqAnswerText "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat uw klanten direct verder kunnen." )
  ]

-- =============================================================================
-- MijnWebwinkel "Waarom wordt het verwaarloosd?" article page
-- =============================================================================

mijnwebwinkelWaaromPage :: Html
mijnwebwinkelWaaromPage = webwinkelBaseTemplate waaromMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Waarom wordt MijnWebwinkel niet meer doorontwikkeld?"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Het korte antwoord: MijnWebwinkel is in 2021 verkocht aan een Noors softwareconglomeraat. Sindsdien is de code bevroren en zijn de prijzen verdubbeld. Dit is geen complottheorie, het is gewoon de financi&euml;le logica van private equity." :: Text)

    -- Timeline
    H.section ! A.class_ "for-who" $ do
      H.h2 "De tijdlijn"
      H.ol $ do
        H.li $ do
          H.strong "2005"
          H.preEscapedToHtml (": Alex Pansier richt MijnWebwinkel op in Oss. Het platform groeit organisch naar bijna 7.000 webshops." :: Text)
        H.li $ do
          H.strong "November 2021"
          H.preEscapedToHtml (": " :: Text)
          H.a ! A.href "https://www.visma.com/news/visma-strengthens-its-position-in-the-benelux-e-commerce-market-with-the-acquisition-of-mijnwebwinkel" $ "Visma neemt MijnWebwinkel over"
          ". De oprichter vertrekt. Visma is een Noors softwareconglomeraat (15.000 medewerkers, 170+ bedrijven) in handen van het Britse private-equityfonds Hg Capital."
        H.li $ do
          H.strong "2022\8211\&2025"
          H.preEscapedToHtml (": de &euro;20-tier wordt beperkt tot 25 producten. Serieuze shops betalen nu &euro;40\8211\&70/maand. Ontwikkeling stopt. Het aantal webshops daalt van ~7.000 naar ~4.500." :: Text)
        H.li $ do
          H.strong "November 2025"
          H.preEscapedToHtml (": " :: Text)
          H.a ! A.href "https://www.emerce.nl/wire/mijnwebwinkel-mystore-lanceren-acendy-nieuw-tijdperk-ecommerce" $ "MijnWebwinkel wordt samengevoegd met het Noorse Mystore"
          H.preEscapedToHtml (" tot &ldquo;Acendy&rdquo;. Dit werd kort daarna weer teruggedraaid." :: Text)
        H.li $ do
          H.strong "Februari 2026"
          H.preEscapedToHtml (": " :: Text)
          H.a ! A.href "https://www.privateequitywire.co.uk/hg-spins-out-e500m-of-visma-assets-as-ipo-plans-stall/" $ H.preEscapedToHtml ("Visma stoot &euro;500 miljoen aan bedrijven af" :: Text)
          " (waaronder Acendy/MijnWebwinkel) in een nieuw vehikel genaamd Norvato. Reden: Visma bereidt een beursgang voor en wil alleen kernproducten behouden."

    -- Why this happens
    H.section ! A.class_ "audit" $ do
      H.h2 "Waarom wordt het verwaarloosd?"
      H.p $ H.preEscapedToHtml ("MijnWebwinkel is geen slecht bedrijf met incompetente ontwikkelaars. Het is een <strong>winstgevend platform dat bewust wordt leeggemolken</strong> door de eigenaren. Dit is het standaard private-equity draaiboek:" :: Text)
      H.ol $ do
        H.li $ do
          H.strong "Koop goedkoop"
          ": een winstgevend SaaS-platform met duizenden betalende klanten"
        H.li $ do
          H.strong "Verhoog prijzen"
          H.preEscapedToHtml (": beperk het goedkope plan tot 25 producten, duw serieuze shops naar &euro;40\8211\&70/maand" :: Text)
        H.li $ do
          H.strong "Verlaag kosten"
          ": stop alle ontwikkeling, minimaliseer support"
        H.li $ do
          H.strong "Melk de marge"
          H.preEscapedToHtml (": 4.500 shops &times; &euro;40/maand = &euro;2,1 miljoen per jaar aan inkomsten met minimale kosten" :: Text)
        H.li $ do
          H.strong "Voeg samen of verkoop"
          ": als de melkkoe opdroogt, fuseer met een ander product of stoot af"

    -- The numbers
    H.section ! A.class_ "for-who" $ do
      H.h2 "De cijfers"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 $ H.a ! A.href "https://storeleads.app/reports/mijnwebwinkel" $ "-35%"
          H.p "Daling in het aantal MijnWebwinkel-shops sinds de piek in 2022. Van ~7.000 naar ~4.500."
        H.li ! A.class_ "card" $ do
          H.h3 "10:1"
          H.p "Verhouding vertrek vs. aankomst in de afgelopen 90 dagen. 40 shops vertrokken, 4 bijgekomen."
        H.li ! A.class_ "card" $ do
          H.h3 "55%"
          H.p "Van de vertrekkende MijnWebwinkel-shops kiest 55% voor Shopify als bestemming."
        H.li ! A.class_ "card" $ do
          H.h3 "25 producten"
          H.p $ H.preEscapedToHtml ("Het goedkoopste plan (&euro;20/maand) is beperkt tot 25 producten. Serieuze shops betalen &euro;40\8211\&70/maand." :: Text)
      H.p ! A.class_ "engagement-note" $ do
        "Bron: "
        H.a ! A.href "https://storeleads.app/reports/mijnwebwinkel" $ "StoreLeads.app"
        " (mei 2026)"

    -- Sources
    H.section ! A.class_ "about" $ do
      H.h2 "Bronnen"
      H.ul $ do
        H.li $ do
          H.a ! A.href "https://www.visma.com/news/visma-strengthens-its-position-in-the-benelux-e-commerce-market-with-the-acquisition-of-mijnwebwinkel" $ "Visma persbericht: overname MijnWebwinkel"
          " (2021)"
        H.li $ do
          H.a ! A.href "https://www.emerce.nl/wire/mijnwebwinkel-mystore-lanceren-acendy-nieuw-tijdperk-ecommerce" $ "Emerce: MijnWebwinkel en Mystore lanceren Acendy"
          " (november 2025)"
        H.li $ do
          H.a ! A.href "https://www.privateequitywire.co.uk/hg-spins-out-e500m-of-visma-assets-as-ipo-plans-stall/" $ H.preEscapedToHtml ("Private Equity Wire: Hg stoot &euro;500M aan Visma-assets af" :: Text)
          " (mei 2026)"
        H.li $
          H.a ! A.href "https://storeleads.app/reports/mijnwebwinkel" $ "StoreLeads: MijnWebwinkel platformrapport"

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te vertrekken?"
      H.p $ H.preEscapedToHtml ("MijnWebwinkel wordt niet meer beter. Het platform is verkocht, de code is bevroren, en de opvolger kost het dubbele. U kunt wachten tot u <em>gedwongen</em> wordt te migreren naar Acendy, of u kunt nu zelf kiezen waar u naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-mijnwebwinkel.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (": volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    waaromMeta :: PageMeta
    waaromMeta = PageMeta
      { pageMetaTitle       = "Waarom wordt MijnWebwinkel niet meer doorontwikkeld? \8212 Webwinkelverhuis"
      , pageMetaDescription = "MijnWebwinkel is in 2021 overgenomen door Visma/Hg Capital en wordt sindsdien niet meer doorontwikkeld. De code is bevroren, prijzen zijn verdubbeld, en het platform wordt afgebouwd. Dit is waarom."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/waarom-mijnwebwinkel.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

-- =============================================================================
-- Lightspeed "Waarom verlaten webshops Lightspeed?" article page
-- =============================================================================

lightspeedWaaromPage :: Html
lightspeedWaaromPage = webwinkelBaseTemplate waaromLsMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Waarom verlaten steeds meer webshops Lightspeed?"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Lightspeed is een beursgenoteerd bedrijf. Dat klinkt als stabiliteit, maar het betekent het tegenovergestelde: het management moet elk kwartaal de aandeelhouders laten zien dat de omzet per klant stijgt. De makkelijkste manier? Prijzen verhogen en kleine webshops eruit drukken." :: Text)

    -- The stock market logic
    H.section ! A.class_ "audit" $ do
      H.h2 "De beurslogica"
      H.p $ H.preEscapedToHtml ("Lightspeed Commerce (NYSE/TSX: LSPD) ging in 2019 naar de beurs in Toronto en in 2020 naar New York. Sindsdien is het bedrijf niet meer van de oprichter, maar van de aandeelhouders. En aandeelhouders willen &eacute;&eacute;n ding: groei." :: Text)
      H.p "Groei kan op twee manieren:"
      H.ol $ do
        H.li $ do
          H.strong "Meer klanten"
          ": maar de markt is verzadigd en Lightspeed verliest netto klanten"
        H.li $ do
          H.strong "Meer omzet per klant"
          H.preEscapedToHtml (": en d&aacute;t is precies wat er gebeurt" :: Text)
      H.p $ H.preEscapedToHtml ("Het gevolg: de goedkoopste plannen worden duurder, features worden weggehaald uit lagere tiers, en het hele platform wordt opgeschoven richting grotere winkeliers die meer betalen. <strong>U als kleine webshop bent niet de doelgroep, u bent de ballast.</strong>" :: Text)
      H.p $ H.preEscapedToHtml ("Wij vinden dat fundamenteel verkeerd. Kleine webshops groeien; dat is het hele punt. De webshop van vandaag met 200 producten is de webshop van volgend jaar met 2.000 producten. Maar als Lightspeed niet in die groei gelooft, hoeft u daar niet op te wachten. Wij helpen u graag naar een platform dat w&eacute;l in u investeert." :: Text)

    -- Timeline
    H.section ! A.class_ "for-who" $ do
      H.h2 "Wat er is gebeurd"
      H.ol $ do
        H.li $ do
          H.strong "2019"
          H.preEscapedToHtml (": Lightspeed gaat naar de beurs in Toronto. Haalt $240 miljoen op. Het bedrijf moet nu elk kwartaal groeicijfers laten zien." :: Text)
        H.li $ do
          H.strong "2020"
          H.preEscapedToHtml (": tweede beursnotering in New York. Nog eens $376 miljoen opgehaald. Begint agressief bedrijven op te kopen: ShopKeep, Vend, Ecwid, NuORDER. Dit lijkt op een leveraged-buyout-strategie: groei door overnames in plaats van door het eigen product te verbeteren. Het probleem is dat Lightspeed hun eigen platform niet kon laten groeien, en nu alle toekomstige groei opoffert voor kortetermijnwinst." :: Text)
        H.li $ do
          H.strong "2021"
          H.preEscapedToHtml (": aandeel piekt rond $125. Kort daarna publiceert " :: Text)
          H.a ! A.href "https://www.sprucepointcap.com/lightspeed-commerce-inc" $ "Spruce Point Capital"
          H.preEscapedToHtml (" een vernietigend rapport dat de groeicijfers in twijfel trekt. Het aandeel keldert." :: Text)
        H.li $ do
          H.strong "2022\8211\&2024"
          H.preEscapedToHtml (": prijzen worden verhoogd. Het goedkoopste plan (Essential) kost nu &euro;68/maand voor slechts 250 productvarianten. Het oude eCom-platform (C-Series) wordt afgebouwd richting E-Series (Ecwid)." :: Text)
        H.li $ do
          H.strong "2025\8211\&2026"
          H.preEscapedToHtml (": het aantal actieve Lightspeed-webshops daalt van 23.700 naar 18.500. Een verlies van 22% in drie jaar." :: Text)

    -- The numbers
    H.section ! A.class_ "for-who" $ do
      H.h2 "De cijfers"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 $ H.a ! A.href "https://storeleads.app/reports/lightspeed" $ "-22%"
          H.p "Daling in het aantal Lightspeed-webshops sinds de piek in Q3 2023. Van 23.700 naar 18.500."
        H.li ! A.class_ "card" $ do
          H.h3 "10:1"
          H.p "Verhouding vertrek vs. aankomst in de afgelopen 90 dagen. 160 shops vertrokken, 16 bijgekomen."
        H.li ! A.class_ "card" $ do
          H.h3 "59%"
          H.p "Van de vertrekkende Lightspeed-shops kiest 59% voor Shopify als bestemming."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("&euro;68/mnd" :: Text)
          H.p $ H.preEscapedToHtml ("Het goedkoopste plan (Essential) kost &euro;68/maand en is beperkt tot 250 productvarianten en 1 taal. Serieuze shops betalen &euro;120\8211\&259/maand." :: Text)
      H.p ! A.class_ "engagement-note" $ do
        "Bron: "
        H.a ! A.href "https://storeleads.app/reports/lightspeed" $ "StoreLeads.app"
        " (mei 2026)"

    -- Why this is structural
    H.section ! A.class_ "audit" $ do
      H.h2 "Dit wordt niet beter"
      H.p $ do
        "Bij andere platforms zoals "
        H.a ! A.href "/waarom-mijnwebwinkel.html" $ "MijnWebwinkel"
        H.preEscapedToHtml (" (een Nederlands webshopplatform dat hetzelfde overkomt) kon je nog hopen dat een nieuwe eigenaar het platform nieuw leven zou inblazen. Bij Lightspeed is dat uitgesloten. Dit is een beursgenoteerd bedrijf met een duidelijke strategie:" :: Text)
      H.ol $ do
        H.li $ do
          H.strong "Consolideer platforms"
          H.preEscapedToHtml (": het oude eCom (C-Series) wordt afgebouwd. De opvolger is E-Series, gebaseerd op het opgekochte Ecwid. Uw huidige shop is legacy." :: Text)
        H.li $ do
          H.strong "Verschuif naar enterprise"
          H.preEscapedToHtml (": het Professional-plan kost &euro;259/maand, Enterprise is op offerte. Lightspeed wil minder klanten die meer betalen." :: Text)
        H.li $ do
          H.strong "Verhoog de opbrengst per klant"
          H.preEscapedToHtml (": via hogere abonnementen, betaalverwerking (Lightspeed Payments), en kapitaalverstrekking (Lightspeed Capital). Elk kwartaal moet dit cijfer omhoog." :: Text)
        H.li $ do
          H.strong "Laat kleine shops vanzelf vertrekken"
          H.preEscapedToHtml (": door de prijs hoog genoeg te maken dat het voor kleine shops niet meer rendabel is. Dat is geen vergissing, dat is de strategie." :: Text)
      H.p $ H.preEscapedToHtml ("De aandelenkoers is gedaald van $125 naar rond de $20. Het management staat onder druk om winstgevender te worden. Dat betekent: <strong>hogere prijzen, minder support, en focus op grote klanten</strong>. Kleine webshops passen niet in dat plaatje." :: Text)

    -- Nederland specifiek
    H.section ! A.class_ "for-who" $ do
      H.h2 "Nederland als melkkoe"
      H.p $ H.preEscapedToHtml ("27% van alle Lightspeed-webshops zit in Nederland: bijna 5.000 shops. Nederland is veruit de grootste markt, groter dan de VS en Canada samen. Toch wordt het platform bestuurd vanuit Montreal, met Amerikaanse prioriteiten." :: Text)
      H.p $ H.preEscapedToHtml ("De E-Series migratie is <strong>alleen beschikbaar in Noord-Amerika</strong>. Nederlandse shops zitten vast op de C-Series, een platform dat niet meer wordt doorontwikkeld, met prijzen die wel doorstijgen." :: Text)

    -- Sources
    H.section ! A.class_ "about" $ do
      H.h2 "Bronnen"
      H.ul $ do
        H.li $ do
          H.a ! A.href "https://storeleads.app/reports/lightspeed" $ "StoreLeads: Lightspeed platformrapport"
          " (mei 2026)"
        H.li $
          H.a ! A.href "https://www.lightspeedhq.nl/ecommerce/prijzen/" $ "Lightspeed prijzen Nederland"
        H.li $
          H.a ! A.href "https://stockanalysis.com/stocks/lspd/" $ "LSPD aandelenkoers"
        H.li $ do
          H.a ! A.href "https://www.sprucepointcap.com/lightspeed-commerce-inc" $ "Spruce Point Capital: short-seller rapport"
          " (2021)"
        H.li $
          H.a ! A.href "https://ecom-support.lightspeedhq.com/hc/en-us/articles/9034086949531-Lightspeed-eCom-E-Series-upgrade-FAQ" $ "Lightspeed E-Series upgrade FAQ"

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te vertrekken?"
      H.p $ H.preEscapedToHtml ("Lightspeed wordt elk kwartaal duurder en elk kwartaal minder gericht op u. U kunt wachten tot de volgende prijsverhoging, of u kunt nu zelf kiezen waar u naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-lightspeed.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (": volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    waaromLsMeta :: PageMeta
    waaromLsMeta = PageMeta
      { pageMetaTitle       = "Waarom verlaten steeds meer webshops Lightspeed? \8212 Webwinkelverhuis"
      , pageMetaDescription = "Lightspeed is beursgenoteerd en verschuift richting enterprise-klanten. Prijzen stijgen, kleine shops worden eruit gedrukt. 22% minder webshops in drie jaar. Dit is waarom."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/waarom-lightspeed.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd lightspeedWaaromFaq
      }

lightspeedWaaromFaq :: [(FaqQuestion, FaqAnswer)]
lightspeedWaaromFaq =
  [ ( "Waarom wordt Lightspeed steeds duurder?"
    , faqAnswerText "Lightspeed is beursgenoteerd (NYSE/TSX: LSPD) en moet elk kwartaal groei laten zien aan aandeelhouders. Omdat de markt verzadigd is, verhoogt het management de prijs per klant in plaats van meer klanten te werven." )
  , ( "Wat is er met Lightspeed eCom C-Series?"
    , faqAnswerText "De C-Series wordt afgebouwd. De opvolger is E-Series, gebaseerd op het opgekochte Ecwid. De E-Series migratie is alleen beschikbaar in Noord-Amerika; Nederlandse shops zitten vast op het oude platform." )
  , ( "Hoeveel webshops verlaten Lightspeed?"
    , faqAnswerText "In de afgelopen 90 dagen vertrokken 160 webshops terwijl er slechts 16 bijkwamen. Sinds Q3 2023 is het totaal gedaald van 23.700 naar 18.500 shops, een daling van 22%." )
  , ( "Waar gaan vertrekkende Lightspeed-shops naartoe?"
    , faqAnswerText "59% van de vertrekkende Lightspeed-shops kiest Shopify als bestemming." )
  ]

-- =============================================================================
-- Blog index page (paginated listing)
-- =============================================================================

webwinkelBlogIndexPage :: SiteConfig -> [Article] -> PaginationInfo -> Html
webwinkelBlogIndexPage _config articles pagination =
  webwinkelBlogBaseTemplate blogIndexMeta $
    H.main ! A.class_ "blog-listing" $ do
      H.h1 "Blog"
      mapM_ renderBlogSummary articles
      renderPagination pagination
  where
    blogIndexMeta :: PageMeta
    blogIndexMeta = PageMeta
      { pageMetaTitle       = "Blog \8212 Webwinkelverhuis"
      , pageMetaDescription = "Praktische gidsen over webshop-migratie: SEO behouden, platformen vergelijken, en veilig overstappen naar Shopify."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/blog/"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

-- =============================================================================
-- Individual article page
-- =============================================================================

webwinkelArticlePage :: SiteConfig -> Article -> Html
webwinkelArticlePage _config article =
  webwinkelBlogBaseTemplate articleMeta $
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
          H.a ! A.href "/blog/" $ H.preEscapedToHtml ("&larr; Terug naar blog" :: Text)
  where
    articleMeta :: PageMeta
    articleMeta = (defaultPageMeta (articleTitle article <> " \8212 Webwinkelverhuis"))
      { pageMetaDescription = articleMetaDescription article
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just ("https://webwinkelverhuis.nl/blog/" <> articleUrl article)
      }
