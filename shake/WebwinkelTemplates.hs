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
  , vierNulVierPagina
  , webwinkelBlogIndexPage
  , webwinkelArticlePage
  , appPage
  , mijnwebwinkelMigrationPage
  , ccvshopMigrationPage
  , lightspeedMigrationPage
  , mijnwebwinkelWaaromPage
  , lightspeedWaaromPage
  , overOnsPage
  , contactPage
  , relativizeWebwinkelContentImages
  , webwinkelverhuisSitemap
  , webwinkelverhuisStaticPages
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (Day, UTCTime(..), defaultTimeLocale, formatTime, fromGregorian)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (SiteConfig(..), Article(..), PaginationInfo(..), articleLastmod)
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
  , renderFaqItemCollapsible
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

-- | Site-default social-share image, hosted on this domain. 1200x630,
-- aangeleverd bij het joepa-ontwerp.
webwinkelOgImage :: Text
webwinkelOgImage = "https://webwinkelverhuis.nl/assets/beeld/og.jpg"

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
      , ",\"logo\":\"https://webwinkelverhuis.nl/assets/beeld/logo-breed.png\""
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
    <> "Om je een goede prijs te kunnen geven, alvast wat info (vul in wat je weet):\n\n"
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
      H.link ! A.rel "preload" ! A.href "/assets/fonts/bricolage-grotesque-var.woff2"
             ! customAttribute "as" "font" ! A.type_ "font/woff2"
             ! customAttribute "crossorigin" ""
      H.link ! A.rel "preload" ! A.href "/assets/fonts/figtree-var.woff2"
             ! customAttribute "as" "font" ! A.type_ "font/woff2"
             ! customAttribute "crossorigin" ""
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.type_ "image/png" ! A.sizes "32x32" ! A.href "/assets/beeld/favicon-32.png"
      H.link ! A.rel "icon" ! A.type_ "image/png" ! A.sizes "64x64" ! A.href "/assets/beeld/favicon-64.png"
      if includeFeed
        then H.link ! A.href "/blog/atom"
                    ! A.type_ "application/atom+xml"
                    ! A.rel "alternate"
                    ! A.title "Webwinkelverhuis blog"
        else mempty
      -- Decision: Google Analytics 4 zonder cookiebanner (Jappie, 8 aug
      -- 2026). Gekozen: de kaalst mogelijke gtag-config (alleen een
      -- measurement-id; geen user_id, geen Google Signals, geen
      -- advertentiekoppelingen) en geen toestemmingsbanner. Overwogen
      -- alternatieven: een consentbanner (afgewezen: de banner-gekte doet
      -- meer kwaad voor de bezoeker dan deze meting), cookieloze analytics
      -- zoals GoatCounter of Plausible (afgewezen: extra infra), of
      -- helemaal geen analytics (afgewezen: de funnel-metingen sturen de
      -- outreach). Waarom dit mag: art. 11.7a lid 3b Telecommunicatiewet
      -- zondert analytics uit van het toestemmingsvereiste "mits dit geen
      -- of geringe gevolgen heeft voor de persoonlijke levenssfeer"; de
      -- memorie van toelichting (Kamerstukken II 33902, nr. 3) rekent ook
      -- third-party-analytics daaronder zolang er niet geprofileerd wordt
      -- en een verwerkersovereenkomst het eigen gebruik door de derde
      -- inperkt. De AP heeft geen formeel GA4-standpunt en handhaaft in de
      -- praktijk op misleidende banners en tracking-vóór-toestemming, niet
      -- op kale analytics. Randvoorwaarden die dit overeind houden: in de
      -- GA4-beheerconsole Google Signals en ads-personalisatie uit, geen
      -- Google Ads-koppeling, bewaartermijn op het minimum en de
      -- verwerkersvoorwaarden van Google geaccepteerd. Wordt de config
      -- hieronder ooit rijker dan een kaal 'config'-aanroep, dan vervalt
      -- de grond onder deze keuze en moet hij opnieuw gemaakt worden.
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-GD4S885G6F" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-GD4S885G6F');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
      webwinkelOrganizationJsonLd
      pageMetaExtraHead meta
    H.body $ do
      H.header webwinkelTopNav
      content
      whatsappFloatingButton webwinkelWhatsappLabel webwinkelWhatsappMessage
      H.footer $ do
        H.div ! A.class_ "voet-boven" $ do
          H.p ! A.class_ "voet-merk" $ do
            "Webwinkel"
            H.span "verhuis"
            ".nl"
          H.ul $ do
            H.li $ H.a ! A.href "/migrate-mijnwebwinkel.html" $ "MijnWebwinkel"
            H.li $ H.a ! A.href "/migrate-lightspeed.html" $ "Lightspeed"
            H.li $ H.a ! A.href "/migrate-ccvshop.html" $ "CCV Shop"
            H.li $ H.a ! A.href (toValue ("mailto:" <> webwinkelEmail)) ! A.class_ "footer-mail" $ toHtml webwinkelEmail
            H.li $ H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
            H.li $ H.a ! A.href "/blog/" $ "Blog"
        H.div ! A.class_ "voet-onder" $
          H.p $ H.small $ do
            "Webwinkelverhuis is een dienst van "
            H.a ! A.href "https://jappiesoftware.com/" $ "Jappie Software B.V."
            H.preEscapedToHtml (" &middot; KVK: 95097872 &middot; Ooievaarstraat 38, 8262 AN Kampen" :: Text)
      H.script $ H.preEscapedToHtml menuToggleScript
      H.script $ H.preEscapedToHtml bandSpeelScript
      H.script $ H.preEscapedToHtml ctaTrackScript

-- | Het ene menu van webwinkelverhuis.nl. Elke pagina rendert zijn header
-- via deze definitie (uit 'webwinkelBaseWith'); er bestaan geen
-- per-pagina-menu's, dus een menuwijziging hier is overal doorgevoerd.
webwinkelTopNav :: Html
webwinkelTopNav =
  H.nav ! A.class_ "top-nav" $ do
    H.span ! A.class_ "logo" $
      H.a ! A.href "/" ! customAttribute "aria-label" "Webwinkelverhuis, home" $
        H.img ! A.src "/assets/beeld/logo-breed.png" ! A.alt "Webwinkelverhuis"
              ! A.width "1292" ! A.height "150"
    H.button ! A.class_ "menu-knop" ! A.type_ "button"
             ! customAttribute "aria-expanded" "false"
             ! customAttribute "aria-controls" "hoofdnav"
             ! customAttribute "aria-label" "Menu" $
      H.preEscapedToHtml menuKnopSvg
    H.div ! A.class_ "hoofdnav" ! A.id "hoofdnav" $ do
      -- Decision: geen platformlinks in het menu (review Jappie, 8 aug
      -- 2026): een bezoeker geeft maar om een van die pagina's en de lijst
      -- groeit alleen maar. De platformkeuze loopt via de kaarten op de
      -- landingspagina; de platformpagina's staan wel in de footer zodat
      -- ze intern gelinkt blijven.
      H.ul $ do
        H.li $ H.a ! A.href "/prijzen.html" $ "Prijzen"
        H.li $ H.a ! A.href "/blog/" $ "Blog"
        H.li $ H.a ! A.href "/over-ons.html" $ "Over ons"
        H.li $ H.a ! A.href "/contact.html" $ "Contact"
      -- Decision: de Offerte-knop in de navigatie is bewust de omlijnde
      -- secundaire stijl (review Jappie, 8 aug 2026): hij staat op elke
      -- pagina en mag niet blijvend aandacht trekken; de groene primaire
      -- knop is voor de call-to-actions in de content.
      H.a ! A.href offerteMailto ! A.class_ "cta-button-secondary" $ "Offerte"

-- | Hamburger-icoon van de mobiele menuknop: twee lijnen, kleur volgt
-- @currentColor@.
menuKnopSvg :: Text
menuKnopSvg =
  "<svg width=\"26\" height=\"26\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.2\" stroke-linecap=\"round\" aria-hidden=\"true\">"
    <> "<line x1=\"3.5\" y1=\"7\" x2=\"20.5\" y2=\"7\"/>"
    <> "<line x1=\"3.5\" y1=\"17\" x2=\"20.5\" y2=\"17\"/></svg>"

-- | Vinkje-icoon, gebruikt in de hero-chip, hero-noot, inpaklijst en
-- prijs-kaart-garantie. Kleur volgt @currentColor@ van de omliggende regel.
vinkjeSvg :: Text
vinkjeSvg =
  "<svg width=\"15\" height=\"15\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"3\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 12.5 9.5 18 20 6.5\"/></svg>"

-- | Pijl-icoon tussen de haltes van de van-naar-route in de recent-werk-sectie.
routePijlSvg :: Text
routePijlSvg =
  "<svg width=\"26\" height=\"16\" viewBox=\"0 0 26 16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M2 8h20\"/><path d=\"m17 3 5 5-5 5\"/></svg>"

-- | Open/dicht-gedrag van het mobiele menu: knop toggelt de open-klasse,
-- een linkkeuze of Escape sluit het menu weer.
menuToggleScript :: Text
menuToggleScript =
  "(function(){"
    <> "var knop=document.querySelector('.menu-knop');"
    <> "var nav=document.getElementById('hoofdnav');"
    <> "if(!knop||!nav)return;"
    <> "function zet(open){nav.classList.toggle('open',open);knop.setAttribute('aria-expanded',String(open));}"
    <> "knop.addEventListener('click',function(){zet(!nav.classList.contains('open'));});"
    <> "nav.addEventListener('click',function(e){if(e.target.closest('a'))zet(false);});"
    <> "document.addEventListener('keydown',function(e){"
    <> "if(e.key==='Escape'&&nav.classList.contains('open')){zet(false);knop.focus();}"
    <> "});})();"

-- | Start de band-animatie op de landingspagina zodra de band in beeld
-- scrolt: de IntersectionObserver zet eenmalig de klasse "speel", waarna de
-- CSS-animatie precies een keer speelt en de tabel gevuld blijft staan.
-- Zonder JavaScript (of zonder .band op de pagina) gebeurt er niets en
-- toont de basisstijl de tabel statisch compleet.
bandSpeelScript :: Text
bandSpeelScript =
  "(function(){"
    <> "var band=document.querySelector('.band');"
    <> "if(!band||!('IntersectionObserver' in window)){return;}"
    <> "var kijker=new IntersectionObserver(function(entries){"
    <> "entries.forEach(function(e){"
    <> "if(e.isIntersecting){band.classList.add('speel');kijker.disconnect();}"
    <> "});},{threshold:0.15});"
    <> "kijker.observe(band);"
    <> "})();"

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
      ( "Naast onze eenmalige migratieprijs betaal je het abonnement van je nieuwe platform. Shopify Basic kost bijvoorbeeld &euro;"
          <> shopifyBasicJaarlijksEuroPerMaand
          <> " per maand bij jaarlijkse betaling en &euro;"
          <> shopifyBasicMaandelijksEuroPerMaand
          <> " per maand bij maandelijkse betaling (prijspeil " <> shopifyPrijspeil <> ", zie "
      )
    H.a ! A.href shopifyPrijzenUrl $ "de actuele Shopify-prijzen"
    ")."

-- | De gedeelde prijzen-sectie: de prijs-kaart uit het joepa-ontwerp, met de
-- rekenhulp-knop als vervolgstap en de betaal-na-succes-garantie eronder.
prijzen :: Html
prijzen = H.section ! A.class_ "prijs-sectie" ! A.id "prijzen" $
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Volledige migratie"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("vanaf &euro;" <> migratieBasisprijsEuro <> " ")
      H.small "eenmalig"
    H.p ! A.class_ "inbegrepen" $ H.preEscapedToHtml ("Inclusief 1.000 producten: producten, afbeeldingen, categorie&euml;n, klantdata, voorraad en SEO-redirects." :: Text)
    H.p ! A.class_ "meerprijs" $ H.preEscapedToHtml ("Grotere catalogi, extra talen en losse diensten (e-mail-setup, een cursus Shopify, en domeinverhuizing als je domein nog bij je huidige platform staat) hebben een vaste meerprijs." :: Text)
    H.hr
    H.p ! A.class_ "abonnement" $ do
      H.preEscapedToHtml
        ( "Naast onze eenmalige migratieprijs betaal je het abonnement van je nieuwe platform. Shopify Basic kost bijvoorbeeld &euro;"
            <> shopifyBasicJaarlijksEuroPerMaand
            <> " per maand bij jaarlijkse betaling en &euro;"
            <> shopifyBasicMaandelijksEuroPerMaand
            <> " per maand bij maandelijkse betaling (prijspeil " <> shopifyPrijspeil <> ", zie "
        )
      H.a ! A.href shopifyPrijzenUrl $ "de actuele Shopify-prijzen"
      ")."
    H.a ! A.href "/prijzen.html#rekenhulp" ! A.class_ "cta-button" $ "Bereken direct je prijs"
    H.p ! A.class_ "garantie" $ do
      H.preEscapedToHtml vinkjeSvg
      " Je betaalt pas na een succesvolle migratie"



webwinkelIndexPage :: Html
webwinkelIndexPage = webwinkelBaseTemplate indexMeta $
  H.main $ do
    -- Hero: foto met merkverloop-echo, garantie-chip en de Panzer-Shop-noot
    -- op de foto als direct bewijs.
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.p ! A.class_ "hero-chip" $ do
            H.preEscapedToHtml vinkjeSvg
            " Je betaalt pas na een succesvolle migratie"
          H.h1 $ H.preEscapedToHtml ("Webshop verhuizen zonder data- en SEO&#8209;verlies." :: Text)
          H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Vastgelopen op MijnWebwinkel, CCV Shop of Lightspeed? Wij verhuizen je webshop geautomatiseerd naar Shopify of een ander platform." :: Text)
          -- Decision: hero-CTA blijft de gratis scan (/scan.html): de scan
          -- geeft de bezoeker direct een persoonlijk resultaat en voedt het
          -- migratie-aanbod met gemeten feiten. De rekenhulp blijft
          -- bereikbaar via de tweede knop en het Prijzen-menu. Interne
          -- links: GA4's automatische pageview is het kliksignaal, geen
          -- extra event (zie ctaTrackScript).
          H.div ! A.class_ "hero-knoppen" $ do
            H.a ! A.href "/scan.html" ! A.class_ "cta-button" $ "Beoordeel mijn webshop"
            H.a ! A.href "/prijzen.html" ! A.class_ "cta-button-secondary" $ "Bekijk de prijzen"
        H.div ! A.class_ "hero-beeld" $ do
          H.img ! A.src "/assets/beeld/hero-inpakken.jpg"
                ! A.alt "Webshop-eigenaar plakt verhuisdozen dicht naast een geopende laptop"
                ! A.width "1600" ! A.height "1600"
                ! customAttribute "fetchpriority" "high"
          H.p ! A.class_ "hero-noot" $ do
            H.preEscapedToHtml vinkjeSvg
            H.preEscapedToHtml (" 3.500+ producten verhuisd voor o.a. Panzer&#8209;Shop en Kruidje&#8209;Roer&#8209;Me&#8209;Niet" :: Text)

    -- Navy checklist-band: wat er allemaal meeverhuist, in winkelierswoorden.
    H.section ! A.class_ "band" $ do
      H.h2 "Je webshop verhuist"
      H.ul ! A.class_ "inpaklijst" $ mapM_ inpaklijstItem scanStappen

    -- How it works, met de testshop-foto ernaast.
    hoeHetWerktSectie
      [ HoeHetWerktStap "Scan" "Ons programma leest je huidige webshop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
      , HoeHetWerktStap "Wennen" "De testshop draait naast je huidige webshop, die gewoon doordraait. Je raakt op je gemak bekend met je nieuwe shop."
      , HoeHetWerktStap "DNS-overzet" "Ben je er klaar voor? Dan wijzen we je domein op de nieuwe shop en ben je verhuisd."
      ]

    recentWerkSection

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. Je kiest het platform: Shopify, WooCommerce, of vraag ons advies voor je situatie. Wij regelen de techniek." :: Text)
      H.dl ! A.class_ "waarom-rijen" $ do
        H.div $ do
          H.dt "Geen risico"
          H.dd "Je betaalt pas na een succesvolle migratie."
        H.div $ do
          H.dt "Geautomatiseerd"
          H.dd "Geen handmatig overtypen, geen kopieerfouten."
        H.div $ do
          H.dt "Zelfvaliderend"
          H.dd "Het programma valideert zijn eigen werk."
        H.div $ do
          H.dt "SEO-behoud"
          H.dd "Elke oude link krijgt een 301-redirect en blijft werken; je opgebouwde SEO verhuist mee."
        H.div $ do
          H.dt "Vaste prijs"
          H.dd "Geen uurtarief, je weet vooraf wat het kost."
      H.p $ do
        "Meer weten over waarom shops vertrekken? Lees onze "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Platform cards: the routing step, at the bottom so the visitor first
    -- reads the promise, the process and the proof before picking a platform.
    -- Secondary buttons: the green primary is reserved for the offerte and
    -- plan-een-gesprek actions.
    H.section ! A.class_ "for-who" ! A.id "platforms" $ do
      H.h2 "Vanaf welk platform verhuis je?"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-bevroren.svg"
                ! A.alt "Sneeuwvlok: het platform is bevroren"
                ! A.width "56" ! A.height "56"
          H.h3 "MijnWebwinkel"
          H.p $ H.preEscapedToHtml ("Bevroren platform, verdubbelde prijzen, gesloten community. Wij zetten alles over, inclusief de automatisch gegenereerde 301-redirects voor je artikel-URLs." :: Text)
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
          H.p $ H.preEscapedToHtml ("Steeds duurder, terwijl het winkelbestand krimpt en het zwaartepunt na de Fiserv-overname bij betalen en kassa ligt. Wij zetten je producten, talen, klantaccounts en voorraad volledig geautomatiseerd over." :: Text)
          H.a ! A.href "/migrate-ccvshop.html" ! A.class_ "cta-button-secondary" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)

    -- Pricing: listed openly. Hiding the price reads as evasive to a
    -- doubtful merchant; a visible "vanaf" and the breakdown radiate
    -- confidence. The offerte remains the binding guarantee, the
    -- disclaimer says so plainly so an updated price is never a broken
    -- promise.
    prijzen
    H.section ! A.class_ "about" ! A.id "vragen" $ do
      H.h2 "Veelgestelde vragen"
      H.div ! A.class_ "faq" $ mapM_ renderFaqItemCollapsible homeFaq

    -- Afsluiting zoals in het joepa-ontwerp: gesprek plannen naast de
    -- contactfoto, in een lichte sectie.
    H.section ! A.id "contact" $
      H.div ! A.class_ "contact" $ do
        H.div $ do
          H.h2 "Klaar om te verhuizen?"
          H.p ! A.class_ "contact-intro" $ "Plan een gratis, vrijblijvend gesprek. We bekijken samen je webshop en geven direct een inschatting."
          H.div ! A.class_ "contact-acties" $ do
            H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
            H.a ! A.href offerteMailto ! A.class_ "cta-button-secondary" $ "Offerte per e-mail"
          H.p ! A.class_ "contact-direct" $ do
            "Liever direct? Mail "
            H.a ! A.href (toValue ("mailto:" <> webwinkelEmail)) $ toHtml webwinkelEmail
            " of bel "
            H.a ! A.href "tel:+31644237437" $ H.preEscapedToHtml ("+31&nbsp;6&nbsp;4423&nbsp;7437" :: Text)
            "."
        H.div ! A.class_ "contact-beeld" $
          H.img ! A.src "/assets/beeld/contact-gesprek.jpg"
                ! A.alt "Webshop-eigenaar in gesprek aan de keukentafel, met pakketdozen op de achtergrond"
                ! A.width "1376" ! A.height "768" ! customAttribute "loading" "lazy"
  where
    indexMeta :: PageMeta
    indexMeta = PageMeta
      { pageMetaTitle       = "Webwinkelverhuis \8212 Verhuis je webshop zonder zorgen"
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
    , faqAnswerText "Vrijwel altijd. Producten, teksten, afbeeldingen, klanten en de categoriestructuur zetten we geautomatiseerd over vanaf MijnWebwinkel, CCV Shop, Lightspeed en andere platformen, inclusief 301-redirects van al je oude URLs zodat je Google-posities meeverhuizen." )
  , ( "Naar welk platform kan ik het beste verhuizen?"
    , faqAnswerText "Dat hangt af van je situatie: je assortiment, je koppelingen en hoeveel je zelf wilt kunnen aanpassen. Shopify is het meest gekozen doelplatform omdat het makkelijk is. WooCommerce kan een goede optie zijn omdat het flexibel is. Weet je het nog niet? In een gratis gesprek adviseren we een platform op basis van je situatie, en in de rekenhulp kun je die keuze gewoon openlaten." )
  , ( "Hoe lang duurt een migratie?"
    , faqAnswerText "Het technische overzetten van je producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan je nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Wat kost een webshop-migratie?"
    , faqAnswerHtml $ do
        toHtml ("Een vaste prijs vanaf " <> migratieBasisprijsEuro <> " euro, afhankelijk van het aantal producten en talen, en je betaalt pas na een geslaagde migratie. Met ")
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp op de prijzenpagina"
        " bereken je in een minuut je richtprijs." )
  , ( "Kan mijn shop blijven doorverkopen tijdens de migratie?"
    , faqAnswerText "Ja. De nieuwe shop bouwen we naast je huidige webshop op, die gewoon doordraait en verkoopt. Pas bij de livegang zetten we je domein om naar waar je heen wilt. Tegen die tijd heb je vertrouwen in het nieuwe systeem en is alles getest." )
  ]

-- =============================================================================
-- MijnWebwinkel migration landing page
-- =============================================================================

-- | Screen-reader label for the floating WhatsApp button, shown on every
-- webwinkelverhuis.nl page via the base template.
webwinkelWhatsappLabel :: Text
webwinkelWhatsappLabel = "Open een WhatsApp-gesprek met ons"

-- | Pre-filled message for the floating WhatsApp button. Platform-neutraal,
-- want de knop staat op elke pagina.
webwinkelWhatsappMessage :: Text
webwinkelWhatsappMessage = "Hallo, ik heb een vraag over het verhuizen van mijn webshop."

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

-- | One row of the navy checklist-band on the landing page: what moves, and
-- the plain-language result with a checkmark.
--
-- Decision: the joepa-ontwerp replaced the earlier animated scan panel with
-- this static checklist band. Same content, same winkelierswoorden
-- (Gemini-feedback: show, don't tell), maar rustiger.
inpaklijstItem :: ScanStap -> Html
inpaklijstItem stap = H.li $ do
  H.span ! A.class_ "wat" $ toHtml (scanStapLabel stap)
  H.span ! A.class_ "status" $ do
    toHtml (scanStapResultaat stap <> " ")
    H.preEscapedToHtml vinkjeSvg

-- | Een stap in de gedeelde "Hoe het werkt"-sectie: een korte titel met
-- daaronder de toelichting.
data HoeHetWerktStap = HoeHetWerktStap
  { hoeHetWerktStapTitel :: Text
  , hoeHetWerktStapTekst :: Text
  }

-- | De "Hoe het werkt"-sectie van de landingspagina, herbruikt op de
-- migratiepagina's (review Jappie, 8 aug 2026): genummerde stappen links,
-- de testshop-foto rechts. De stapteksten verschillen per pagina (de
-- migratiepagina's noemen hun platform), de vorm is overal gelijk.
hoeHetWerktSectie :: [HoeHetWerktStap] -> Html
hoeHetWerktSectie stappen =
  H.section ! A.id "hoe-het-werkt" $
    H.div ! A.class_ "proces" $ do
      H.div $ do
        H.h2 "Hoe het werkt"
        H.ol $ mapM_ hoeHetWerktStapItem stappen
      H.div ! A.class_ "proces-beeld" $
        H.img ! A.src "/assets/beeld/proces-testshop.jpg"
              ! A.alt "Twee laptops naast elkaar: de testshop draait naast de huidige webshop"
              ! A.width "1376" ! A.height "768" ! customAttribute "loading" "lazy"

hoeHetWerktStapItem :: HoeHetWerktStap -> Html
hoeHetWerktStapItem stap = H.li $ do
  H.h3 (toHtml (hoeHetWerktStapTitel stap))
  H.p ! A.class_ "stap-tekst" $ toHtml (hoeHetWerktStapTekst stap)

-- | Een reden in de gedeelde "Waarom via ons?"-sectie: een korte titel met
-- de toelichting, gerenderd als de definitierijen van de landingspagina.
data WaaromPunt = WaaromPunt
  { waaromPuntTitel :: Text
  , waaromPuntTekst :: Text
  }

-- | Het beeld naast de gedeelde waarom-sectie: pad, alt-tekst en de
-- werkelijke afmetingen van het bestand.
data WaaromBeeld = WaaromBeeld
  { waaromBeeldPad :: Text
  , waaromBeeldAlt :: Text
  , waaromBeeldBreedte :: Text
  , waaromBeeldHoogte :: Text
  }

-- | De gedeelde "Waarom via ons?"-sectie van de migratiepagina's (review
-- Jappie, 8 aug 2026): links de pagina-specifieke inleiding (testimonial of
-- cijfers), de vaste specialisten-alinea en de redenen als definitierijen;
-- rechts een stockfoto zodat de lege ruimte naast de leestekst gevuld is.
-- Decision: per pagina een eigen stockfoto (review Jappie: niet overal
-- dezelfde), gekozen uit Pexels (licentie: vrij commercieel te gebruiken,
-- geen naamsvermelding nodig) in dezelfde warme interieurstijl als de
-- joepa-foto's. Niet het eigen portret (dat bleef op over-ons, waar
-- bezoekers juist in de mensen geinteresseerd zijn) en niet het
-- migratiediagram (onleesbaar op kolombreedte, bronlabel klopt alleen voor
-- MWW). De landingspagina houdt zijn eigen paginabrede variant.
waaromViaOnsSectie :: WaaromBeeld -> Html -> [WaaromPunt] -> Html
waaromViaOnsSectie beeld inleiding punten =
  H.section ! A.class_ "results" $ do
    H.h2 "Waarom via ons?"
    H.div ! A.class_ "audit-grid" $ do
      H.div $ do
        inleiding
        H.p $ H.preEscapedToHtml ("Wij zijn migratie-specialisten, geen verlengstuk van &eacute;&eacute;n platform. Je kiest het platform: Shopify, WooCommerce, of iets anders. Wij regelen de techniek." :: Text)
        H.dl ! A.class_ "waarom-rijen" $ mapM_ waaromPuntRij punten
      H.div ! A.class_ "portret-beeld" $
        H.img ! A.src (toValue (waaromBeeldPad beeld))
              ! A.alt (toValue (waaromBeeldAlt beeld))
              ! A.width (toValue (waaromBeeldBreedte beeld))
              ! A.height (toValue (waaromBeeldHoogte beeld))
              ! customAttribute "loading" "lazy"

waaromPuntRij :: WaaromPunt -> Html
waaromPuntRij punt = H.div $ do
  H.dt (toHtml (waaromPuntTitel punt))
  H.dd (toHtml (waaromPuntTekst punt))

-- | The shared "Recent werk" proof section: the Panzer-ShopNL and
-- Kruidje Roer Me Niet migrations, each linking to the case-study blog
-- post and the live shop. Shown on the index page and the MijnWebwinkel
-- migration page.
recentWerkSection :: Html
recentWerkSection =
  H.section ! A.class_ "case-sectie" $ do
    H.h2 "Recent werk"
    H.p ! A.class_ "route" ! customAttribute "aria-label" "Verhuisd van MijnWebwinkel naar Shopify" $ do
      H.span ! A.class_ "halte" $ "MijnWebwinkel"
      H.preEscapedToHtml routePijlSvg
      H.span ! A.class_ "halte" $ "Shopify"
    H.div ! A.class_ "case-cijfers" $ do
      H.div $ do
        H.div ! A.class_ "cijfer" $ "2.400+"
        H.div ! A.class_ "label" $ "producten"
      H.div $ do
        H.div ! A.class_ "cijfer" $ "3"
        H.div ! A.class_ "label" $ "talen"
      H.div $ do
        H.div ! A.class_ "cijfer" $ "3"
        H.div ! A.class_ "label" $ "domeinen"
    H.div ! A.class_ "case-tekst" $
      H.p $ do
        H.strong "Panzer-ShopNL"
        H.preEscapedToHtml (": een modeltreinwinkel met 2.400+ producten over drie domeinen en drie talen, verhuisd van MijnWebwinkel naar Shopify. Inclusief vertalingen, de volledige categorieboom en link behoud, zodat de SEO meeverhuisde." :: Text)
    H.div ! A.class_ "case-links" $ do
      H.a ! A.href "/blog/klantverhaal-panzer-shopnl-van-mijnwebwinkel-naar-shopify-in-drie-talen.html" $ H.preEscapedToHtml ("Lees het klantverhaal &rarr;" :: Text)
      H.a ! A.href "https://panzer-shop.nl/" $ H.preEscapedToHtml ("panzer-shop.nl &rarr;" :: Text)
    H.div ! A.class_ "case-cijfers" $ do
      H.div $ do
        H.div ! A.class_ "cijfer" $ "1.100+"
        H.div ! A.class_ "label" $ "producten"
      H.div $ do
        H.div ! A.class_ "cijfer" $ "200"
        H.div ! A.class_ "label" $ "klantaccounts"
      H.div $ do
        H.div ! A.class_ "cijfer" $ "1"
        H.div ! A.class_ "label" $ "spaarprogramma"
    H.div ! A.class_ "case-tekst" $
      H.p $ do
        H.strong "Kruidje Roer Me Niet"
        H.preEscapedToHtml (": een reformwinkel uit Amersfoort (sinds 1975), verhuisd van MijnWebwinkel naar Shopify. Het spaarprogramma van de fysieke winkel telt nu ook online mee, en klanten kiezen een DHL-pakketpunt gewoon op het Basic-abonnement." :: Text)
    H.div ! A.class_ "case-links" $ do
      H.a ! A.href "/blog/klantverhaal-kruidje-roer-me-niet-van-mijnwebwinkel-naar-shopify-met-spaarpunten-en-al.html" $ H.preEscapedToHtml ("Lees het klantverhaal &rarr;" :: Text)
      H.a ! A.href "https://kruidje-roer-me-niet.nl/" $ H.preEscapedToHtml ("kruidje-roer-me-niet.nl &rarr;" :: Text)

appPage :: Html
appPage = webwinkelBaseTemplate appMeta $ do
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "De migratie-app"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Je ziet deze pagina omdat de migratie-app van Webwinkelverhuis in je Shopify-winkel is ge&iuml;nstalleerd. Dat is precies de bedoeling: de app is het gereedschap waarmee wij je webshop naar Shopify overzetten." :: Text)
      H.a ! A.href "#meer" ! A.class_ "cta-button" $ "Wat we verder voor je kunnen doen"

    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat de app doet"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten plaatsen"
          H.p "De app zet je producten, varianten, afbeeldingen, prijzen en SKU's in je nieuwe Shopify-winkel."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n en pagina&rsquo;s" :: Text)
          H.p $ H.preEscapedToHtml ("Je categorie&euml;n worden Shopify-collections en je informatiepagina&rsquo;s worden meegenomen, inclusief het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "De app legt 301-redirects aan van elke oude URL naar de juiste nieuwe pagina, zodat elke bestaande link blijft werken en je opgebouwde SEO meeverhuist."
        H.li ! A.class_ "card" $ do
          H.h3 "Thema"
          H.p "De app bouwt en plaatst een Shopify-thema dat de uitstraling van je huidige winkel volgt."

    H.section ! A.class_ "audit" $ do
      H.h2 "Waarom de app toegang vraagt"
      H.p "Om dit werk te doen vraagt de app toegang tot precies de onderdelen die hij plaatst:"
      H.ul $ do
        H.li $ H.strong "Producten" >> ": om je artikelen, varianten en collections aan te maken."
        H.li $ H.strong "Content" >> ": om je pagina's, navigatie en redirects over te zetten."
        H.li $ H.strong "Klanten" >> ": om bestaande klantaccounts mee te nemen."
        H.li $ H.preEscapedToHtml ("<strong>Thema&rsquo;s</strong>: om het nieuwe thema te plaatsen." :: Text)
        H.li $ H.strong "Vertalingen" >> ": om meertalige content correct te koppelen."

    H.section ! A.class_ "about" $ do
      H.h2 "Veilig en tijdelijk"
      H.p "De app is alleen nodig tijdens de migratie. Wij installeren hem in je winkel om je data te plaatsen, en daarna kan hij verwijderd worden. Hij maakt geen onderdeel uit van je winkel voor je bezoekers."
      H.p $ do
        H.a ! A.href "/migrate-mijnwebwinkel.html" $ "Lees hoe de migratie werkt"
        H.preEscapedToHtml (" &rarr;" :: Text)

    H.section ! A.class_ "for-who" ! A.id "meer" $ do
      H.h2 "Wat we verder voor je kunnen doen"
      H.p $ H.preEscapedToHtml ("Dezelfde techniek waarmee we je shop verhuizen, zetten we ook na de migratie voor je in. Enkele voorbeelden van wat we voor andere webwinkels hebben gedaan:" :: Text)
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Massabewerkingen"
          H.p $ H.preEscapedToHtml ("Duizenden producten in &eacute;&eacute;n batch aanpassen: merknamen corrigeren, verkeerd vertaalde termen rechtzetten of prijzen bijwerken, over de hele catalogus en in alle talen tegelijk." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "SEO in bulk"
          H.p $ H.preEscapedToHtml ("Alle meta titles en meta descriptions opnieuw opbouwen in &eacute;&eacute;n uniforme stijl, per taal en per categorie." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Thema-uitbreidingen"
          H.p "Je Shopify-thema uitbreiden met extra secties of functionaliteit, of de vormgeving verder afstemmen op je huisstijl."
        H.li ! A.class_ "card" $ do
          H.h3 "Koppelingen en apps"
          H.p "Je boekhouding of facturatie koppelen aan Shopify, een verhuur-app inrichten, of een extra taal toevoegen inclusief vertaalde URL's en redirects."
        H.li ! A.class_ "card" $ do
          H.h3 "Training"
          H.p $ H.preEscapedToHtml ("Een rondleiding door je nieuwe shop met beknopte handleiding, of een cursus Shopify van twee uur, &eacute;&eacute;n-op-&eacute;&eacute;n." :: Text)
      H.p "Alles tegen een vaste prijs per klus, geen uurtarief. Je weet vooraf waar je aan toe bent."
      H.a ! A.href uitbreidingMailto ! A.class_ "cta-button" $ "Bespreek je idee met ons"
  where
    appMeta :: PageMeta
    appMeta = PageMeta
      { pageMetaTitle       = "De migratie-app van Webwinkelverhuis"
      , pageMetaDescription = "Uitleg over de migratie-app van Webwinkelverhuis: het gereedschap waarmee wij je webshop naar Shopify overzetten, en wat we na de migratie verder voor je shop kunnen doen."
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
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Vaste prijzen, vooraf afgesproken. Je betaalt pas na een succesvolle migratie. Hieronder zie je precies waar je aan toe bent." :: Text)

    H.section ! A.class_ "engagement" ! A.id "rekenhulp" $ do
      H.h2 "Bereken je richtprijs"
      H.p "Beantwoord een paar vragen over je shop en je ziet meteen een indicatie."
      H.div ! A.id "prijs-calculator-mount" $ mempty
      H.noscript $ H.p "De rekenhulp heeft JavaScript nodig. Hieronder staat de volledige prijslijst zodat je ook zonder JavaScript alles ziet."
      H.div ! A.class_ "calc-footnotes" $ do
        H.h3 "Over de themakeuze"
        H.p "Kies je voor zelf inrichten, dan staat je shop na de migratie op een standaard Shopify-thema dat je zelf verzorgt of door een ontwerper naar keuze laat doen. Theming hoeft niet via ons; wij doen het ook en zijn er inmiddels aardig goed in. Probeer het gerust eerst zelf: je oude shop blijft gewoon draaien naast de nieuwe, dus je loopt geen risico. Kom je er niet uit, dan helpen we je alsnog."
        H.p $ H.preEscapedToHtml ("Bij uitstraling overzetten (&euro;749) benaderen we je huidige uitstraling zo dicht mogelijk; kleine aanpassingen op verzoek zitten erbij." :: Text)
        H.p "Een volledig nieuw ontwerp is los ontwerpwerk en prijzen we op aanvraag."

    H.section ! A.class_ "engagement" $ do
      H.h2 "De migratie"
      H.table ! A.class_ "price-table" $ H.tbody $ do
        H.tr $ do
          H.td "Basismigratie: 1.000 producten inbegrepen"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;1.999" :: Text)
        H.tr $ do
          H.td "Extra producten boven die 1.000 (elk product telt per taal \233\233n keer mee)"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;0,25 per product" :: Text)
        H.tr $ do
          H.td "Extra taal: configuratie, per taal"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
      shopifyKostenNote

    H.section ! A.class_ "engagement" $ do
      H.h2 "Modules en extra diensten"
      H.p "Losse onderdelen die je naar keuze bijschakelt. Je betaalt alleen voor wat je meeneemt."
      H.table ! A.class_ "price-table" $ H.tbody $ do
        H.tr $ do
          H.td "Uitstraling overzetten"
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;749" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Klantaccounts meenemen (je klanten houden hun inlog)" :: Text)
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
          H.td $ H.preEscapedToHtml ("Domeinverhuizing (je domeinnaam staat nog bij MijnWebwinkel)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;250" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("E-mail-setup (mailboxen, SPF/DKIM, doorstuurregels)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;150" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Verzendkoppeling (bijv. DHL: pakketten en labels vanuit je shop)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;150" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("B2B-kanaal (aparte prijzen en inlog voor zakelijke klanten)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;750" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Kassa / point-of-sale (Shopify POS in je fysieke winkel)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;750" :: Text)
        H.tr $ do
          H.td $ H.preEscapedToHtml ("Cursus Shopify (2 uur, 1-op-1, samen door je nieuwe shop)" :: Text)
          H.td ! A.class_ "price-cell" $ H.preEscapedToHtml ("&euro;300" :: Text)
        H.tr $ do
          H.td "Volledig nieuw ontwerp"
          H.td ! A.class_ "price-cell" $ "op aanvraag"
        H.tr $ do
          H.td "Catalogus-brede teksttransformaties"
          H.td ! A.class_ "price-cell" $ "op aanvraag"
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Na de migratie staat je shop op een standaard Shopify-thema dat je zelf inricht. De vormgeving hoeft niet via ons: je kunt het zelf doen of een ontwerper naar keuze inhuren. Wil je het uit handen geven, dan benaderen wij je huidige uitstraling zo dicht mogelijk (&euro;749) of ontwerpen we iets nieuws (op aanvraag)." :: Text)
      H.p ! A.class_ "engagement-note" $ "Kassa/point-of-sale zetten we bij je op locatie op. Installatie en reiskosten komen daar los bij, op aanvraag."

    H.section ! A.class_ "results" $ do
      H.h2 "Altijd inbegrepen"
      H.ul $ do
        H.li $ H.preEscapedToHtml ("Producten, afbeeldingen, categorie&euml;n, klantdata en voorraad." :: Text)
        H.li $ H.preEscapedToHtml ("Je teksten, meta-titels en informatiepagina&apos;s (zoals over-ons en blog) verhuizen mee." :: Text)
        H.li "SEO-redirects (301) voor elke oude URL, zodat je links en opgebouwde SEO meeverhuizen."
        H.li "Een testshop naast je draaiende winkel; DNS-overzet met zo min mogelijk downtime."
        H.li "Betaling pas na een succesvolle migratie."

    H.section ! A.class_ "final-cta" $ do
      H.h2 "Geldt deze prijs voor mij?"
      H.p "Deze prijzen kunnen we in de toekomst aanpassen, en de hier getoonde bedragen zijn een indicatie, geen garantie. Alleen een offerte legt je prijs vast. Wil je tegen deze prijzen verhuizen? Vraag nu een offerte aan, dan staat je prijs zwart-op-wit."
      H.div ! A.class_ "cta-row" $ do
        H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"
        H.a ! A.href meetLink ! A.class_ "cta-button-secondary" $ "Liever eerst sparren? Plan een gesprek"

    H.script ! A.src "/prijs-calculator.js" $ mempty
    H.script $ H.preEscapedToHtml prijsCalculatorInitScript
  where
    prijzenMeta :: PageMeta
    prijzenMeta = PageMeta
      { pageMetaTitle       = "Prijzen \8212 Webwinkelverhuis"
      , pageMetaDescription = "Vaste prijzen voor je webshop-migratie naar Shopify: vanaf \8364\&1.999 inclusief 1.000 producten. Domeinverhuizing \8364\&250, e-mail-setup \8364\&150. Betaling na succesvolle migratie."
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
    -- Eén gecentreerd blok (review Jappie, 8 aug 2026): kop, uitleg en het
    -- scannerformulier samen in de hero-sectie, die de hele pagina vult.
    H.section ! A.class_ "hero scan-blok" $ do
      H.h1 "Beoordeel mijn webshop"
      H.p ! A.class_ "subtitle" $ "Vul het adres van je webshop in. Wij meten hem door en je ziet binnen enkele minuten waar je staat: snelheid, vindbaarheid en de punten die beter kunnen."
      H.div ! A.id "webshop-scanner-mount" $ mempty
      H.noscript $ H.p "De beoordeling heeft JavaScript nodig. Liever direct contact? Plan een gratis gesprek via meet.jappiesoftware.com."
    H.script ! A.src "/scanner-form.js" $ mempty
    H.script $ H.preEscapedToHtml scannerFormInitScript
  where
    scanMeta :: PageMeta
    scanMeta = PageMeta
      { pageMetaTitle       = "Beoordeel mijn webshop \8212 Webwinkelverhuis"
      , pageMetaDescription = "Gratis beoordeling van je webshop: wij meten snelheid en vindbaarheid met Lighthouse en laten zien welke verbeterpunten oplosbaar zijn en welke vastliggen in je platform."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/scan.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

-- =============================================================================
-- 404-pagina (404.html)
-- =============================================================================

-- | De foutpagina die nginx via error_page serveert bij elke onbekende URL,
-- in plaats van de kale standaard-404. noindex: een foutpagina hoort niet in
-- de zoekindex, maar de links erop mogen gewoon gevolgd worden.
vierNulVierPagina :: Html
vierNulVierPagina = webwinkelBaseTemplate vierNulVierMeta $
  H.main $
    H.section ! A.class_ "hero scan-blok" $ do
      H.h1 "Pagina niet gevonden"
      H.p ! A.class_ "subtitle" $ "Deze pagina bestaat niet (meer). Alles wat wij verhuizen krijgt een doorverwijzing, maar deze link leidde nergens heen."
      H.div ! A.class_ "hero-knoppen" $ do
        H.a ! A.href "/" ! A.class_ "cta-button" $ "Naar de homepagina"
        H.a ! A.href "/contact.html" ! A.class_ "cta-button-secondary" $ "Neem contact op"

vierNulVierMeta :: PageMeta
vierNulVierMeta = PageMeta
  { pageMetaTitle       = "Pagina niet gevonden \8212 Webwinkelverhuis"
  , pageMetaDescription = "Deze pagina bestaat niet op webwinkelverhuis.nl. Ga naar de homepagina of neem contact op."
  , pageMetaLang        = "nl"
  , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/404.html"
  , pageMetaOgImage     = Nothing
  , pageMetaSwitchUrl   = Nothing
  , pageMetaExtraHead   = H.meta ! A.name "robots" ! A.content "noindex, follow"
  }

mijnwebwinkelMigrationPage :: Html
mijnwebwinkelMigrationPage = webwinkelBaseTemplate migrationMeta $
  H.main $ do
    -- Hero: friendly reassurance rather than pressure, with the escape
    -- illustration. The visitor already knows the platform is stuck; this
    -- page's job is making the move feel safe.
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Verhuizen van MijnWebwinkel"
          H.p ! A.class_ "subtitle" $ "Je webshop is je broodwinning, en MijnWebwinkel staat al jaren stil. Verhuizen voelt als een grote stap, maar het hoeft geen sprong in het diepe te zijn: wij zetten je complete shop geautomatiseerd over, zonder dataverlies en met zo min mogelijk downtime. Je betaalt pas na een succesvolle migratie."
          -- Decision: de hero-CTA wijst naar de scanner, niet naar de
          -- offerte (Jappie, 3 aug 2026): niemand vraagt een offerte aan
          -- direct na één alinea, maar de webshop-beoordeling is een
          -- geloofwaardige eerste stap. De offerte-route blijft verderop
          -- (rekenhulp in de prijssectie, gesprek in de final-cta).
          H.a ! A.href "/scan.html" ! A.class_ "cta-button" $ "Beoordeel mijn webshop"
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
          H.h3 "Je plek in Google"
          H.p "Elke oude link naar je shop blijft werken en stuurt bezoekers automatisch naar de juiste nieuwe pagina (301-redirects). Je opgebouwde positie in Google verhuist mee."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-producten.svg"
                ! A.alt "Doos met producten" ! A.width "56" ! A.height "56"
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, artikelnummers en varianten. Automatisch overgezet naar je nieuwe platform."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-categorieen.svg"
                ! A.alt "Categorieboom" ! A.width "56" ! A.height "56"
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("Je volledige categorie-indeling en het navigatiemenu verhuizen mee, inclusief vertaalde titels. Ook je informatiepagina&apos;s (contact, voorwaarden, verzendinformatie) gaan mee." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-thema.svg"
                ! A.alt "Verfpalet" ! A.width "56" ! A.height "56"
          H.h3 "Thema"
          H.p "Je nieuwe shop krijgt een thema dat de uitstraling van je huidige winkel volgt. Je begint niet met een kale winkel."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-talen.svg"
                ! A.alt "Twee tekstballonnen" ! A.width "56" ! A.height "56"
          H.h3 "Meerdere talen"
          H.p "Vertalingen worden correct gekoppeld. Je klanten blijven je shop in hun eigen taal zien, ook op de vertaalde webadressen van je pagina's."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-spaarpunten.svg"
                ! A.alt "Munt met ster" ! A.width "56" ! A.height "56"
          H.h3 "Klanten & spaarpunten"
          H.p "Klantaccounts, bestelgeschiedenis en spaarpuntensaldi verhuizen mee. Je klanten kunnen direct inloggen, met hun spaarpunten in het loyaliteitsprogramma van je nieuwe platform."
      H.p ! A.class_ "engagement-note" $ "Ook de rest van je shop verhuist mee: informatiepagina's zoals over-ons en verzendinformatie, je blog, reviews, kortingscodes en cadeaubonnen. Als aparte dienst doen we ook grootschalige aanpassingen aan je productdata tijdens de migratie, zoals prijsaanpassingen, het opschonen van beschrijvingen of beschrijvingen voor Google bij al je afbeeldingen (alt-teksten)."

    -- En daarna: het blijvende gemak. Decision (Jappie, 12 aug 2026): de
    -- pagina verkocht alleen het verhuisproces; de eerste afgeronde
    -- migratieklant benoemt na livegang juist het dagelijkse gemak en
    -- zelf-kunnen als ervaren waarde, dus de dag-na-de-overstap krijgt
    -- een eigen sectie (jappeaceApplication issue #143). De zoek-claim
    -- is onderbouwd met screenshots van een echte MijnWebwinkel-shop
    -- (10 aug 2026: "wouter" vindt vier producten, "wouter streekhoning"
    -- geeft "geen artikelen gevonden"; jappiesoft
    -- projects/kruidje/voor-materiaal). De koppelingen-voorbeelden zijn
    -- dezelfde die de vs-Shopify-pagina al publiek noemt. Correctie uit
    -- review (Jappie): MWW heeft wel degelijk een eigen
    -- spaarpuntensysteem, alleen compleet aan MWW gebonden; het
    -- zelf-bijschrijven was specifiek de externe Piggy/Leat-koppeling
    -- van de eerste klant. De kaart claimt daarom het eerlijke
    -- verschil: eigen systemen kunnen aankoppelen versus vastzitten aan
    -- wat het platform zelf aanbiedt.
    H.section ! A.class_ "for-who" ! A.id "daarna" $ do
      H.h2 "En daarna: elke dag makkelijker"
      H.p "De verhuizing is eenmalig, het gemak is blijvend. Dit merk je na de overstap elke dag:"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-beperkt.svg"
                ! A.alt "Vergrootglas met beperking" ! A.width "56" ! A.height "56"
          H.h3 "Zoeken dat verkoopt"
          H.p $ H.preEscapedToHtml ("De zoekfunctie op MijnWebwinkel struikelt al over een extra zoekwoord: wie de productnaam bijna letterlijk intikt, kan alsnog &quot;geen artikelen gevonden&quot; te zien krijgen. Moderne platformen begrijpen meerdere woorden en typefouten, zodat klanten vinden wat ze zoeken." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-bulk.svg"
                ! A.alt "Stapel taken die vanzelf verwerkt wordt" ! A.width "56" ! A.height "56"
          H.h3 "Minder handwerk per bestelling"
          H.p "Verzendlabels maak je vanuit je beheer, je eigen spaarsysteem koppel je gewoon aan, en voor reviews, boekhouding of nieuwsbrieven bestaat een app. Op MijnWebwinkel kan alleen wat MijnWebwinkel zelf aanbiedt; wat je nu overtypt, doet straks een koppeling."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-thema.svg"
                ! A.alt "Verfpalet" ! A.width "56" ! A.height "56"
          H.h3 "Zelf aan de knoppen"
          H.p "Teksten, foto's, pagina's en blokken pas je zelf aan, wanneer jij wilt, zonder ticket of wachttijd. En je platform ontwikkelt gewoon door, dus het wordt vanzelf beter."

    -- How it works: de gedeelde sectie van de landingspagina, met
    -- MijnWebwinkel-specifieke stapteksten.
    hoeHetWerktSectie
      [ HoeHetWerktStap "Scan" "Ons programma leest je MijnWebwinkel-shop volledig uit en bouwt er een testshop mee op, met producten, vertalingen, categorie\235n en doorverwijzingen."
      , HoeHetWerktStap "Wennen" "De testshop draait naast je MijnWebwinkel-shop, die gewoon doordraait. Je raakt op je gemak bekend met je nieuwe shop."
      , HoeHetWerktStap "Livegang" "Ben je er klaar voor? Dan wijzen we je domeinnaam op de nieuwe shop en ben je verhuisd. We houden de downtime zo klein mogelijk."
      ]

    -- Recent werk: proof before price, so the number lands on trust.
    recentWerkSection

    -- Why us. Decision: het waarom-blok staat tussen recent werk en de
    -- prijzen (review Jappie, 8 aug 2026): bewijs, dan vertrouwen, dan de
    -- prijs, en de fond-witte sectieachtergronden wisselen zo netjes af in
    -- plaats van twee grijze secties op elkaar.
    waaromViaOnsSectie
      (WaaromBeeld "/assets/beeld/contact-gesprek.jpg"
        "Webshop-eigenaar in gesprek aan de keukentafel, met pakketdozen op de achtergrond"
        "1376" "768")
      (H.div ! A.class_ "testimonials" $
        H.blockquote $ do
          H.p "Je weet het al: MijnWebwinkel gaat nergens meer heen. Geen nieuwe features, geen community, trage support. Trage laadtijden schaden je SEO, en dat is op MijnWebwinkel niet te verbeteren. Gelukkig hoef je daar niet op te wachten: verhuizen is inmiddels een gebaande weg."
          H.p $ do
            H.a ! A.href "/waarom-mijnwebwinkel.html" $ "Waarom wordt MijnWebwinkel niet meer doorontwikkeld?"
            H.preEscapedToHtml (" &rarr;" :: Text)
          H.p $ do
            H.a ! A.href "/blog/het-miljard-van-shopify-waarom-geen-nederlands-platform-kan-bijbenen.html" $ "Het miljard van Shopify: waarom geen Nederlands platform kan bijbenen"
            H.preEscapedToHtml (" &rarr;" :: Text))
      [ WaaromPunt "Geen risico" "Je betaalt pas na een succesvolle migratie."
      , WaaromPunt "Geautomatiseerd" "Geen handmatig overtypen, geen kopieerfouten."
      , WaaromPunt "Gecontroleerd" "Het programma telt na afloop alles na, van elk product tot elke klant."
      , WaaromPunt "SEO-behoud" "Elke oude link blijft werken en je opgebouwde positie in Google verhuist mee."
      , WaaromPunt "Meertalig" "Vertalingen correct gekoppeld via de offici\235le koppelingen van je nieuwe platform."
      , WaaromPunt "Vaste prijs" "Geen uurtarief, je weet vooraf wat het kost."
      ]

    -- Pricing
    prijzen

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.div ! A.class_ "faq" $ mapM_ renderFaqItemCollapsible mijnwebwinkelFaq

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar voor de overstap?"
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen je webshop en geven een eerlijke inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gratis gesprek"
  where
    migrationMeta :: PageMeta
    migrationMeta = PageMeta
      { pageMetaTitle       = "Verhuizen van MijnWebwinkel \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
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
    , faqAnswerText "Het technische overzetten van je producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, betaalmethoden, eventuele koppelingen, en rustig wennen aan je nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Wat gebeurt er met bestellingen tijdens de verhuizing?"
    , faqAnswerHtml $ do
        "Je MijnWebwinkel-shop blijft gewoon open en verkoopt door terwijl wij de nieuwe shop opbouwen. Vlak voor de livegang zetten we de laatste stand over, zodat ook recente bestellingen en actuele voorraadaantallen meekomen. Bestelgeschiedenis en voorraad kies je als optie in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        "." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja, je webadres blijft gewoon van je. Bij de livegang wijzen we je domeinnaam op de nieuwe shop, en alle oude links worden automatisch doorgestuurd." )
  , ( "Wat gebeurt er met mijn e-mailadres?"
    , faqAnswerHtml $ do
        "Je e-mailadres blijft gewoon werken. Loopt je e-mail nu via MijnWebwinkel, dan zetten we je mailboxen en doorstuurregels over als losse dienst (e-mail-setup in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        "), zodat je geen bericht mist." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma telt na afloop alles na. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die MijnWebwinkel en je nieuwe platform beide ondersteunen. Ook de vertaalde webadressen verhuizen mee." )
  , ( "Kan ik ook naar een ander platform dan Shopify migreren?"
    , faqAnswerText "Ja. Shopify wordt het meest gekozen, maar we kunnen ook migreren naar WooCommerce of andere platformen." )
  , ( "Wat kost Shopify zelf per maand?"
    , faqAnswerHtml $ do
        toHtml ("Het Basic-abonnement van Shopify kost \8364" <> shopifyBasicJaarlijksEuroPerMaand
          <> " per maand bij jaarlijkse betaling, of \8364" <> shopifyBasicMaandelijksEuroPerMaand
          <> " per maand als je per maand betaalt (prijspeil " <> shopifyPrijspeil
          <> "). Voor de meeste shops die van MijnWebwinkel komen is Basic voldoende. De actuele prijzen vind je op ")
        H.a ! A.href shopifyPrijzenUrl $ "shopify.com/nl/prijzen"
        "." )
  , ( "Kunnen jullie een website importeren naar Shopify?"
    , faqAnswerText "Ja. Producten, teksten, afbeeldingen, klanten en de categoriestructuur van je bestaande website worden automatisch naar Shopify overgezet, inclusief automatische doorverwijzingen (301-redirects) van al je oude links, zodat je Google-posities meeverhuizen." )
  , ( "Worden spaarpunten ook overgezet?"
    , faqAnswerText "Ja. Spaarpuntensaldi van je klanten worden meegenomen naar het loyaliteitsprogramma van je nieuwe platform." )
  , ( "Verhuizen mijn pagina's, reviews en kortingscodes ook?"
    , faqAnswerHtml $ do
        "Ja. Informatiepagina's zoals over-ons en verzendinformatie en je blog verhuizen standaard mee. Reviews neem je als optie mee in "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        ", en ook kortingscodes en cadeaubonnen zetten we voor je over. Alles wat in je shop zit, kan mee." )
  , ( "Hoe werken de SEO-redirects precies?"
    , faqAnswerText "MijnWebwinkel bouwt zijn links op uit interne artikelnummers. We hebben uitgezocht hoe dat precies werkt, waardoor we voor elke oude link automatisch de juiste doorverwijzing (301-redirect) kunnen aanmaken, ook voor links met nummers erin." )
  , ( "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
    , faqAnswerText "Ja. We kunnen grootschalige wijzigingen doorvoeren, bijvoorbeeld prijzen aanpassen, beschrijvingen opschonen, of beschrijvingen voor Google toevoegen aan al je afbeeldingen (alt-teksten)." )
  , ( "Kan ik de nieuwe shop straks zelf beheren?"
    , faqAnswerHtml $ do
        "Ja, daar is Shopify juist op gebouwd: producten toevoegen, prijzen wijzigen en een banner plaatsen doe je zonder technische kennis. Wil je een vliegende start, dan is er een cursus Shopify van twee uur, \233\233n-op-\233\233n door je eigen nieuwe shop (zie "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        ")." )
  ]

-- =============================================================================
-- CCV Shop migration landing page
-- =============================================================================

ccvshopMigrationPage :: Html
ccvshopMigrationPage = webwinkelBaseTemplate ccvMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Verhuizen van CCV Shop"
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
          H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Sinds de Amerikaanse betaalreus Fiserv CCV overnam ligt het zwaartepunt bij betalen en kassa: je webshop is een tweede rang product. Wij verhuizen je complete shop geautomatiseerd naar Shopify, WooCommerce of een ander platform van je keuze: zonder dataverlies, met zo min mogelijk downtime." :: Text)
          -- Zelfde besluit als de MWW-hero (3 aug 2026): scanner als eerste
          -- stap in plaats van de offerte.
          H.a ! A.href "/scan.html" ! A.class_ "cta-button" $ "Beoordeel mijn webshop"
        H.img ! A.class_ "hero-image"
              ! A.src "/illustratie-ontsnappen.svg"
              ! A.alt "Illustratie van dozen die een bevroren webshop verlaten richting een zonnige nieuwe winkel"
              ! A.width "400" ! A.height "300"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-producten.svg"
                ! A.alt "Doos met producten" ! A.width "56" ! A.height "56"
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar het formaat van je doelplatform."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-talen.svg"
                ! A.alt "Twee tekstballonnen" ! A.width "56" ! A.height "56"
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Je klanten blijven je shop in hun eigen taal zien, ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-spaarpunten.svg"
                ! A.alt "Munt met ster" ! A.width "56" ! A.height "56"
          H.h3 "Klantaccounts"
          H.p "Klantgegevens en bestelgeschiedenis worden overgezet zodat je klanten direct kunnen inloggen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-redirects.svg"
                ! A.alt "Pijl die een nieuwe route neemt" ! A.width "56" ! A.height "56"
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Je backlinks blijven werken en je opgebouwde SEO verhuist mee."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-categorieen.svg"
                ! A.alt "Categorieboom" ! A.width "56" ! A.height "56"
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-bulk.svg"
                ! A.alt "Stapel dozen" ! A.width "56" ! A.height "56"
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadinformatie en staffelprijzen worden meegenomen. Per-variant prijzen en voorraadbeheer werken direct in je nieuwe shop."

    -- How it works: de gedeelde sectie van de landingspagina, met
    -- CCV-specifieke stapteksten.
    hoeHetWerktSectie
      [ HoeHetWerktStap "Scan" "Ons programma leest je CCV Shop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
      , HoeHetWerktStap "Wennen" "De testshop draait naast je CCV Shop, die gewoon doordraait. Je raakt op je gemak bekend met je nieuwe shop."
      , HoeHetWerktStap "DNS-overzet" "Ben je er klaar voor? Dan wijzen we je domein op de nieuwe shop en ben je verhuisd. We houden de downtime zo klein mogelijk."
      ]

    -- Pricing
    prijzen

    -- Why us
    waaromViaOnsSectie
      (WaaromBeeld "/assets/beeld/stock-eigenaar-dozen.jpg"
        "Webshop-eigenaar met een stapel dozen in een lichte werkkamer"
        "1200" "1800")
      (do
        H.div ! A.class_ "testimonials" $
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
        H.p $ H.preEscapedToHtml ("De meeste winkeliers verlaten CCV Shop niet omdat het kapot is, maar omdat ze eruit groeien: maatwerk of functies die er niet in zitten. Shopify en WooCommerce groeien w&eacute;l met je mee, van extra verkoopkanalen en talen tot B2B-maatwerk en duizenden apps. En het lastigste deel van uitgroeien, alles heelhuids overzetten, is precies ons vak." :: Text)
        H.p $ do
          H.a ! A.href "/blog/het-miljard-van-shopify-waarom-geen-nederlands-platform-kan-bijbenen.html" $ "Het miljard van Shopify: waarom geen Nederlands platform kan bijbenen"
          H.preEscapedToHtml (" &rarr;" :: Text))
      [ WaaromPunt "Geen risico" "Je betaalt pas na een succesvolle migratie."
      , WaaromPunt "Platformonafhankelijk" "Je kiest de bestemming, wij migreren naar elk platform."
      , WaaromPunt "Geautomatiseerd" "Geen handmatig overtypen, geen kopieerfouten."
      , WaaromPunt "SEO-behoud" "Elke oude link krijgt een 301-redirect en blijft werken, je opgebouwde SEO verhuist mee."
      , WaaromPunt "Meertalig" "Vertalingen correct gekoppeld via offici\235le APIs."
      , WaaromPunt "Zelfvaliderend" "Het programma valideert zijn eigen werk."
      , WaaromPunt "Vaste prijs" "Geen uurtarief, je weet vooraf wat het kost."
      ]

    -- FAQ. Decision: de aparte "Technische details"-kaartensectie is hierin
    -- opgegaan (review Jappie, 8 aug 2026): hij oogde dubbelop met "Wat we
    -- migreren"; de unieke inhoud leeft nu als platform-specifieke vragen.
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.div ! A.class_ "faq" $ mapM_ renderFaqItemCollapsible ccvshopFaq

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar voor de overstap?"
      H.p $ H.preEscapedToHtml ("Je hoeft niet langer te wachten tot CCV Shop beter wordt. Neem de controle terug over je webshop." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen je webshop en geven direct een inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    ccvMeta :: PageMeta
    ccvMeta = PageMeta
      { pageMetaTitle       = "Verhuizen van CCV Shop \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
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
    , faqAnswerText "Het technische overzetten van je producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan je nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja. Na de migratie wijs je je domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma valideert zijn eigen werk. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die CCV Shop en je doelplatform beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , faqAnswerText "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat je klanten direct verder kunnen." )
  , ( "Hoe werken de SEO-redirects precies?"
    , faqAnswerText "We genereren automatisch 301-redirects van elke oude URL naar het nieuwe adres. Je Google-rankings en backlinks blijven behouden." )
  , ( "Worden voorraad en staffelprijzen correct overgezet?"
    , faqAnswerText "Ja. Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar je nieuwe shop, inclusief prijsverschillen per maat of kleur." )
  , ( "Verhuizen vertaalde URL-slugs ook mee?"
    , faqAnswerText "Ja. Meertalige content wordt correct gekoppeld via de offici\235le API van je doelplatform, inclusief de vertaalde URL-slugs van je pagina's." )
  , ( "Krijg ik een testshop om te wennen?"
    , faqAnswerText "Ja. Je krijgt een volledige testshop naast je huidige shop om alvast te wennen. Pas na je akkoord gaan we live; eventuele correcties zijn inbegrepen." )
  , ( "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
    , faqAnswerText "Ja. We kunnen grootschalige wijzigingen doorvoeren tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen, alles in \233\233n keer." )
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
    , faqAnswerText $ "Dat hangt af van je bestemming. Kies je WooCommerce, dan kan je CCV-terminal gewoon gekoppeld blijven: Nederlandse kassasoftware verbindt de terminal en synchroniseert de voorraad met je webshop, en je pincontract loopt door. Kies je Shopify, dan koppelt de terminal niet meer met de kassa; hij kan wel als losse pin blijven werken, maar dan typ je elk bedrag over. Wil je winkel en webshop weer als \233\233n geheel, dan vervang je hem door een Shopify-terminal. Die overstap is eenmalig en bescheiden (\8364" <> "59 tot \8364" <> "249) en daarna ben je ook voor het pinnen van CCV af. Wij hebben ervaring met het inrichten van kassa's en pinterminals en nemen dit gewoon in het migratietraject mee." )
  , ( "Kan ik de nieuwe shop straks zelf beheren?"
    , faqAnswerHtml $ do
        "Ja, daar is Shopify juist op gebouwd: producten toevoegen, prijzen wijzigen en een banner plaatsen doe je zonder technische kennis. Wil je een vliegende start, dan is er een cursus Shopify van twee uur, \233\233n-op-\233\233n door je eigen nieuwe shop (zie "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        ")." )
  ]

-- =============================================================================
-- Lightspeed migration landing page
-- =============================================================================

lightspeedMigrationPage :: Html
lightspeedMigrationPage = webwinkelBaseTemplate lightspeedMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $
      H.div ! A.class_ "hero-grid" $ do
        H.div $ do
          H.h1 "Verhuizen van Lightspeed"
          H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Lightspeed duwt je richting hun nieuwe platform, maar het offici&euml;le upgradeprogramma slaat Nederland over en verliest onderweg je orderhistorie. Ondertussen draait je webshop op software die alleen nog onderhoud krijgt. Wij verhuizen je complete shop naar Shopify: geautomatiseerd, zonder dataverlies, zonder SEO-verlies, en je betaalt pas na succes." :: Text)
          -- Zelfde besluit als de MWW-hero (3 aug 2026): scanner als eerste
          -- stap in plaats van de offerte.
          H.a ! A.href "/scan.html" ! A.class_ "cta-button" $ "Beoordeel mijn webshop"
        H.img ! A.class_ "hero-image"
              ! A.src "/illustratie-ontsnappen.svg"
              ! A.alt "Illustratie van dozen die een bevroren webshop verlaten richting een zonnige nieuwe winkel"
              ! A.width "400" ! A.height "300"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-producten.svg"
                ! A.alt "Doos met producten" ! A.width "56" ! A.height "56"
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar het formaat van je doelplatform."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-talen.svg"
                ! A.alt "Twee tekstballonnen" ! A.width "56" ! A.height "56"
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Je klanten blijven je shop in hun eigen taal zien, ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-spaarpunten.svg"
                ! A.alt "Munt met ster" ! A.width "56" ! A.height "56"
          H.h3 "Klanten, spaarpunten & reviews"
          H.p "Klantgegevens en bestelgeschiedenis worden overgezet zodat je klanten direct verder kunnen, en ook spaarpunten-saldo's en beoordelingen verhuizen mee. Geen enkele andere migratiepartij die we kennen biedt dat."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-redirects.svg"
                ! A.alt "Pijl die een nieuwe route neemt" ! A.width "56" ! A.height "56"
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Je backlinks blijven werken en je opgebouwde SEO verhuist mee; bij onbegeleide migraties zagen we verhalen van 70% verkeersverlies."
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-categorieen.svg"
                ! A.alt "Categorieboom" ! A.width "56" ! A.height "56"
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n & navigatie" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.img ! A.class_ "card-icon" ! A.src "/icoon-bulk.svg"
                ! A.alt "Stapel dozen" ! A.width "56" ! A.height "56"
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadbeheer, staffelprijzen en per-variant pricing worden correct overgezet. Je voorraadniveaus kloppen direct in je nieuwe shop."

    -- How it works: de gedeelde sectie van de landingspagina, met
    -- Lightspeed-specifieke stapteksten.
    hoeHetWerktSectie
      [ HoeHetWerktStap "Scan" "Ons programma leest je Lightspeed-shop volledig uit en zet alles over naar een testshop: producten, vertalingen, collections, redirects."
      , HoeHetWerktStap "Wennen" "De testshop draait naast je Lightspeed-shop, die gewoon doordraait. Je raakt op je gemak bekend met je nieuwe shop."
      , HoeHetWerktStap "DNS-overzet" "Ben je er klaar voor? Dan wijzen we je domein op de nieuwe shop en ben je verhuisd. We houden de downtime zo klein mogelijk."
      ]

    -- Pricing
    prijzen

    -- Why us
    waaromViaOnsSectie
      (WaaromBeeld "/assets/beeld/stock-productfoto-raam.jpg"
        "Webshop-eigenaar fotografeert bij het raam een pakket voor haar webshop"
        "1200" "1800")
      (H.div ! A.class_ "testimonials" $
        H.blockquote $ do
          H.p $ H.preEscapedToHtml ("Je bent niet de enige: het aantal Lightspeed-shops in Nederland daalde van 6.904 eind 2023 naar 4.842 in augustus 2026, en 59% van de vertrekkers kiest Shopify. Maar zonder begeleiding gaan bij de overstap vaak oude URLs kapot; wij hebben verhalen gezien van 70% verkeersverlies bij een onbegeleide migratie. Wij zorgen dat elke oude URL blijft doorverwijzen (ons programma legt ze allemaal vast, niet een steekproef) en je opgebouwde SEO meeverhuist." :: Text)
          H.p $ do
            H.a ! A.href "/waarom-lightspeed.html" $ "Waarom verlaten steeds meer webshops Lightspeed?"
            H.preEscapedToHtml (" &rarr;" :: Text))
      [ WaaromPunt "Geen risico" "Je betaalt pas na een succesvolle migratie."
      , WaaromPunt "Platformonafhankelijk" "Je kiest de bestemming, wij migreren naar elk platform."
      , WaaromPunt "SEO-behoud" "Elke oude link krijgt een 301-redirect en blijft werken, je opgebouwde SEO verhuist mee."
      , WaaromPunt "Geautomatiseerd" "Geen handmatig overtypen, geen kopieerfouten."
      , WaaromPunt "Meertalig" "Vertalingen correct gekoppeld via offici\235le APIs."
      , WaaromPunt "Zelfvaliderend" "Het programma valideert zijn eigen werk."
      , WaaromPunt "Vaste prijs" "Geen uurtarief, je weet vooraf wat het kost."
      ]

    -- FAQ. Decision: de aparte "Technische details"-kaartensectie is hierin
    -- opgegaan (review Jappie, 8 aug 2026), net als op de CCV-pagina.
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.div ! A.class_ "faq" $ mapM_ renderFaqItemCollapsible lightspeedFaq

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar voor de overstap?"
      H.p $ H.preEscapedToHtml ("Lightspeed ga je niet helpen met deze overstap. Wij wel." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen je webshop en geven direct een inschatting."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    lightspeedMeta :: PageMeta
    lightspeedMeta = PageMeta
      { pageMetaTitle       = "Verhuizen van Lightspeed \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
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
    , faqAnswerText "Het technische overzetten van je producten duurt maar enkele uren. Maar er komt bij een verhuizing meestal meer kijken: het thema, apps en plugins, betaalmethoden, en rustig wennen aan je nieuwe shop. Reken daarom op ongeveer een maand van start tot livegang." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , faqAnswerText "Ja. Na de migratie wijs je je domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , faqAnswerText "Die kans is klein: de migratie is volledig geautomatiseerd en het programma valideert zijn eigen werk. Inmiddels hebben we dit ook meermaals gedaan. Maar fouten kunnen gebeuren, en als er toch iets niet klopt, lossen we het gratis op." )
  , ( "Verlies ik mijn Google-posities?"
    , faqAnswerText "Elke oude URL krijgt automatisch een 301-redirect, zodat al je links en backlinks blijven werken en de opgebouwde SEO meeverhuist. Google kan na elke grote sitewijziging tijdelijk schommelen; het blijvende verlies uit de horrorverhalen komt door ontbrekende redirects, en dat dekken wij volledig af." )
  , ( "Werkt het ook voor meertalige webshops?"
    , faqAnswerText "Ja. Nederlands, Duits, Engels, Frans of een andere taal: het programma ondersteunt elke taalcombinatie die Lightspeed en Shopify beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , faqAnswerText "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat je klanten direct verder kunnen." )
  , ( "Worden voorraad en staffelprijzen correct overgezet?"
    , faqAnswerText "Ja. Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar je nieuwe shop, inclusief prijsverschillen per maat of kleur." )
  , ( "Verhuizen vertaalde URL-slugs ook mee?"
    , faqAnswerText "Ja. Meertalige content wordt correct gekoppeld via de offici\235le API van je doelplatform, inclusief de vertaalde URL-slugs van je pagina's." )
  , ( "Krijg ik een testshop om te wennen?"
    , faqAnswerText "Ja. Je krijgt een volledige testshop naast je huidige shop om alvast te wennen. Pas na je akkoord gaan we live; eventuele correcties zijn inbegrepen." )
  , ( "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
    , faqAnswerText "Ja. We kunnen grootschalige wijzigingen doorvoeren tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen, alles in \233\233n keer." )
  , ( "Kan ik de nieuwe shop straks zelf beheren?"
    , faqAnswerHtml $ do
        "Ja, daar is Shopify juist op gebouwd: producten toevoegen, prijzen wijzigen en een banner plaatsen doe je zonder technische kennis. Wil je een vliegende start, dan is er een cursus Shopify van twee uur, \233\233n-op-\233\233n door je eigen nieuwe shop (zie "
        H.a ! A.href "/prijzen.html#rekenhulp" $ "de rekenhulp"
        ")." )
  , ( "Moet ik niet gewoon upgraden naar Lightspeed E-Series?"
    , faqAnswerText "Dat kan, maar weet waar je aan begint. Het offici\235le upgradeprogramma is alleen beschikbaar voor Noord-Amerikaanse winkels, dus als Nederlandse shop doe je die overstap sowieso op eigen kracht. Zelfs bij het offici\235le pad migreert je orderhistorie niet mee, moeten redirects en apps opnieuw, en gaan alleen twee specifieke thema's mee. E-Series is bovendien een ander product (het overgenomen Ecwid); Europese gebruikers melden er problemen mee rond verplichte e-facturatie. Als je toch opnieuw moet beginnen, kies dan zelf je platform." )
  , ( "Kunnen mijn klanten met hun oude wachtwoord inloggen?"
    , faqAnswerText "Nee, en wees op je hoede voor wie iets anders belooft: Shopify accepteert om veiligheidsredenen geen wachtwoorden van andere platformen. Wat wel kan, en wat wij doen: alle accounts verhuizen mee en je klanten zetten bij hun eerste bezoek in \233\233n stap een nieuw wachtwoord via een nette reset-flow. In de praktijk merken klanten daar nauwelijks iets van." )
  , ( "Verhuizen mijn spaarpunten en beoordelingen mee?"
    , faqAnswerText "Ja. Spaarpunten-saldo's zetten we per klant over naar een loyaliteitsapp op je nieuwe shop, gekoppeld aan de meeverhuisde klantaccounts, zodat iedereen met hetzelfde saldo aankomt. Beoordelingen importeren we in een reviews-app zodat je sociale bewijs zichtbaar blijft." )
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
      H.p $ H.preEscapedToHtml ("MijnWebwinkel is geen slecht bedrijf met incompetente ontwikkelaars. Het is een <strong>winstgevend platform waar bewust niet meer in wordt ge&iuml;nvesteerd</strong>. Dit is het standaard private-equity draaiboek:" :: Text)
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
          H.strong "Inkomsten zonder investering"
          H.preEscapedToHtml (": 4.500 shops &times; &euro;40/maand = &euro;2,1 miljoen per jaar aan inkomsten met minimale kosten" :: Text)
        H.li $ do
          H.strong "Voeg samen of verkoop"
          ": lopen de inkomsten terug, fuseer dan met een ander product of stoot af"

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
      H.h2 "Zelf het moment kiezen?"
      H.p $ H.preEscapedToHtml ("MijnWebwinkel wordt niet meer beter. Het platform is verkocht, de code is bevroren, en de opvolger kost het dubbele. Je kunt wachten tot je <em>gedwongen</em> wordt te migreren naar Acendy, of je kunt nu zelf kiezen waar je naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-mijnwebwinkel.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (": volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
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
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Lightspeed is een beursgenoteerd bedrijf. Dat klinkt als stabiliteit, maar het betekent het tegenovergestelde: het management moet elk kwartaal de aandeelhouders laten zien dat de omzet per klant stijgt. De makkelijkste manier: de prijzen verhogen bij de klanten die er al zijn." :: Text)

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
      H.p $ H.preEscapedToHtml ("Het gevolg: de goedkoopste plannen worden duurder, functies verhuizen naar de duurdere pakketten, en het hele platform wordt opgeschoven richting grotere winkeliers die meer betalen. In dat plan is voor kleine webshops steeds minder plek." :: Text)
      H.p $ H.preEscapedToHtml ("Voor jou betekent dit dat de prijsverhogingen geen incidenten zijn maar beleid. Elke brief over een nieuw tarief is geen pech, het is de strategie die zijn werk doet. Reken er dus op dat je volgend jaar meer betaalt voor dezelfde winkel, en het jaar daarna weer." :: Text)
      H.p $ H.preEscapedToHtml ("Wij vinden dat fundamenteel verkeerd. Kleine webshops groeien; dat is het hele punt. De webshop van vandaag met 200 producten is de webshop van volgend jaar met 2.000 producten. Maar als Lightspeed niet in die groei gelooft, hoef je daar niet op te wachten. Wij helpen je graag naar een platform dat w&eacute;l in je investeert." :: Text)

    -- Timeline
    H.section ! A.class_ "for-who" $ do
      H.h2 "Wat er is gebeurd"
      H.ol $ do
        H.li $ do
          H.strong "2019"
          H.preEscapedToHtml (": Lightspeed gaat naar de beurs in Toronto. Haalt $240 miljoen op. Het bedrijf moet nu elk kwartaal groeicijfers laten zien." :: Text)
        H.li $ do
          H.strong "2020"
          H.preEscapedToHtml (": tweede beursnotering in New York. Nog eens $376 miljoen opgehaald. Begint agressief bedrijven op te kopen: ShopKeep, Vend, Ecwid, NuORDER. Groeien door andere bedrijven te kopen in plaats van door het eigen product te verbeteren dus; als winkelier merk je dat doordat het geld naar overnames gaat en niet naar de software waar jij dagelijks in werkt." :: Text)
        H.li $ do
          H.strong "2021"
          H.preEscapedToHtml (": aandeel piekt rond $125. Kort daarna publiceert " :: Text)
          H.a ! A.href "https://www.sprucepointcap.com/lightspeed-commerce-inc" $ "Spruce Point Capital"
          H.preEscapedToHtml (" een vernietigend rapport dat de groeicijfers in twijfel trekt. Het aandeel keldert." :: Text)
        H.li $ do
          H.strong "2022\8211\&2024"
          H.preEscapedToHtml (": prijzen worden verhoogd. Het goedkoopste plan (Essential) kost nu &euro;68/maand voor slechts 250 productvarianten. Lightspeed heeft intussen twee webshopsystemen: de C-Series (de versie waar vrijwel alle Nederlandse shops op draaien, waarschijnlijk ook de jouwe) en de nieuwere E-Series, gebouwd op het opgekochte Ecwid. Al het bouwwerk gaat voortaan naar die nieuwe versie; die van jou wordt alleen nog onderhouden." :: Text)
        H.li $ do
          H.strong "2024"
          H.preEscapedToHtml (": twee ontslagrondes (10% in april, nog eens ~200 banen in december) en een \"strategische review\" waarin openlijk een verkoop of beursexit wordt verkend." :: Text)
        H.li $ do
          H.strong "2025"
          H.preEscapedToHtml (": de review eindigt in februari zonder koper: Lightspeed blijft beursgenoteerd en kondigt aan tot $400 miljoen eigen aandelen terug te kopen, geld dat naar beleggers gaat in plaats van naar het product, op dezelfde dag als een kwartaalverlies van $266 miljoen. De nieuwe strategie noemt als groeimotoren alleen nog <strong>retail in Noord-Amerika en horeca in Europa</strong>. Europese e-commerce, jouw shop dus, valt daarbuiten." :: Text)
        H.li $ do
          H.strong "2026"
          H.preEscapedToHtml (": boekjaar 2026 sluit op $1,23 miljard omzet met $144 miljoen nettoverlies; het Amerikaanse horecaproduct Upserve wordt met ruim 80% verlies verkocht en verdere afstotingen zijn volgens de CEO een kwestie van tijd. Het aantal actieve Lightspeed-webshops wereldwijd daalde intussen van 23.700 naar onder de 18.500." :: Text)
      H.p $ H.preEscapedToHtml ("Je hoeft geen beurskenner te zijn om dit lijstje samen te vatten: het geld en de aandacht gingen jarenlang naar overnames en aandeelhouders, niet naar de software waar jouw winkel op draait. En elke stap is uiteindelijk betaald uit de abonnementen van winkeliers zoals jij." :: Text)

    -- The numbers
    H.section ! A.class_ "for-who" $ do
      H.h2 "De cijfers"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 $ H.a ! A.href "https://storeleads.app/reports/lightspeed/NL/top-stores" $ "-30%"
          H.p "Daling van het aantal Lightspeed-shops in Nederland sinds de piek in Q3 2023: van 6.904 naar 4.842 (augustus 2026)."
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
        " (augustus 2026)"
      H.p $ H.preEscapedToHtml ("Waarom zou het jou iets uitmaken dat andere shops vertrekken? Omdat een webshopplatform op zijn winkeliers drijft. Minder shops betekent minder abonnementsgeld, en dus nog minder reden voor Lightspeed om in jouw platform te investeren. De bouwers van apps, thema's en koppelingen volgen dezelfde beweging: die stoppen hun tijd in platforms waar de klanten z&iacute;jn, waardoor hulp en koppelingen op Lightspeed steeds schaarser en duurder worden. Zo wordt vertrek een spiraal die zichzelf versnelt." :: Text)
      H.p $ H.preEscapedToHtml ("De cijfers hierboven vertellen ook wat je opties zijn. Tien vertrekkers voor elke nieuwkomer betekent dat je niet de vraag moet stellen &oacute;f de rij richting de uitgang loopt, maar wanneer jij zelf wilt lopen: nu, op je eigen tempo, of straks, wanneer het moet. En dat 59% van de vertrekkers Shopify kiest scheelt je uitzoekwerk; de winkeliers voor je hebben die vergelijking al gemaakt." :: Text)

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
          H.preEscapedToHtml (": het oude webshopsysteem (C-Series) wordt afgebouwd; de opvolger (E-Series) is een ander product, gebouwd op het opgekochte Ecwid. Je huidige shop is wat softwarebouwers legacy noemen: het draait nog, maar niemand bouwt er meer aan." :: Text)
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
      H.p $ H.preEscapedToHtml ("In de praktijk merk je dat zo: je betaalt meer, maar er komt niets bij. Geen nieuwe functies voor je winkel, langere wachttijden als er iets stuk is, en bij elke vraag eerst een chatbot. Wachten tot het beter wordt heeft bij deze strategie geen zin, want stilstand is hier niet het probleem maar het plan." :: Text)

    -- Nederland specifiek
    H.section ! A.class_ "for-who" $ do
      H.h2 "De grootste markt, de laagste prioriteit"
      H.p $ H.preEscapedToHtml ("27% van alle Lightspeed-webshops zit in Nederland: bijna 5.000 shops. Nederland is veruit de grootste markt, groter dan de VS en Canada samen. Toch wordt het platform bestuurd vanuit Montreal, met Amerikaanse prioriteiten." :: Text)
      H.p $ H.preEscapedToHtml ("De E-Series migratie, de offici&euml;le route om van het oude naar het nieuwe systeem over te stappen, is <strong>alleen beschikbaar in Noord-Amerika</strong>. Nederlandse shops zitten vast op de C-Series, een systeem dat alleen nog onderhouds-updates krijgt, met prijzen die wel doorstijgen. En zelfs wie het offici&euml;le upgradepad w&eacute;l zou krijgen, verliest daarbij de orderhistorie; redirects, apps en (op twee na) thema's moeten opnieuw, en negentig dagen na activatie wordt de oude shop uitgezet." :: Text)
      H.p $ H.preEscapedToHtml ("Onderhouds-updates klinkt geruststellend, maar het betekent alleen dat de lampen blijven branden. Alles wat je vandaag aan je winkel mist, mis je over twee jaar nog steeds; wat er bij Shopify of WooCommerce elk jaar bijkomt, gaat aan jouw shop voorbij. Je abonnement betaalt intussen mee aan ontwikkeling die jij nooit te zien krijgt, en het enige echte besluit dat overblijft is of j&iacute;j het moment van vertrek kiest of Lightspeed dat ooit voor je doet." :: Text)

    -- Merchant voice
    H.section ! A.class_ "audit" $ do
      H.h2 "Wat gebruikers zelf zeggen"
      H.p $ do
        H.preEscapedToHtml ("Op " :: Text)
        H.a ! A.href "https://nl.trustpilot.com/review/www.lightspeedhq.nl" $ "Trustpilot"
        H.preEscapedToHtml (" staat Lightspeed Nederland in augustus 2026 op een 2,9 van 5. Een greep uit recente reviews:" :: Text)
      H.ul $ do
        H.li $ H.preEscapedToHtml ("\"Een prijsverhoging van meer dan 10%, zonder verwittiging of uitleg. Ze zetten je eenvoudig met de rug tegen de muur.\" (augustus 2026)" :: Text)
        H.li $ H.preEscapedToHtml ("\"Het is een wurgcontract met kleine lettertjes.\" (maart 2026)" :: Text)
        H.li $ H.preEscapedToHtml ("\"Ik betaal voor een professioneel e-commerceplatform, niet om na een fout van de leverancier zelf projectleider te worden van het herstel\", nadat de supportafdeling tot twee keer toe het webshopthema van deze winkelier had gewist. (juli 2026)" :: Text)
      H.p $ H.preEscapedToHtml ("Voor de goede orde: de vele vijfsterren-reviews op dezelfde pagina zijn er ook, grotendeels op uitnodiging geschreven tijdens de onboarding. De \233\233nsterren-reviews hierboven zijn spontaan." :: Text)

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
        H.li $ do
          H.a ! A.href "https://nl.trustpilot.com/review/www.lightspeedhq.nl" $ "Trustpilot: Lightspeed Nederland"
          " (augustus 2026)"
        H.li $ do
          H.a ! A.href "https://www.emerce.nl/nieuws/helft-nieuwe-webwinkels-nederland-draait-shopify" $ "Emerce: helft nieuwe webwinkels Nederland draait Shopify"
          " (maart 2026)"
        H.li $ do
          H.a ! A.href "https://www.prnewswire.com/news-releases/lightspeed-announces-fourth-quarter-and-full-year-2026-financial-results-and-provides-outlook-for-fiscal-2027-302778407.html" $ "Lightspeed jaarcijfers FY2026"
          " (mei 2026)"

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Zelf het moment kiezen?"
      H.p $ H.preEscapedToHtml ("Lightspeed wordt elk kwartaal duurder en elk kwartaal minder gericht op je. Je kunt wachten tot de volgende prijsverhoging, of je kunt nu zelf kiezen waar je naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-lightspeed.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (": volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    waaromLsMeta :: PageMeta
    waaromLsMeta = PageMeta
      { pageMetaTitle       = "Waarom verlaten steeds meer webshops Lightspeed? \8212 Webwinkelverhuis"
      , pageMetaDescription = "Lightspeed is beursgenoteerd en verschuift richting enterprise-klanten. Prijzen stijgen en de focus verschuift naar grote klanten. 22% minder webshops in drie jaar. Dit is waarom."
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
    , faqAnswerText "De C-Series is het webshopsysteem waar vrijwel alle Nederlandse Lightspeed-shops op draaien. Het wordt afgebouwd; de opvolger (E-Series) is een ander product, gebaseerd op het opgekochte Ecwid. De offici\235le overstaproute naar E-Series is alleen beschikbaar in Noord-Amerika; Nederlandse shops zitten vast op het oude systeem." )
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

-- | Generate sitemap.xml for webwinkelverhuis.nl. Every entry carries a
-- lastmod: Google's recrawl prioritisation reads it, and without one on
-- the static pages a copy rewrite stays invisible until the next
-- organic crawl (the index stood at 4 of 15 pages on 2026-08-03 while
-- the whole site had just been rewritten).
webwinkelverhuisSitemap :: [Article] -> Text
webwinkelverhuisSitemap articles = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
  ]
  ++ map webwinkelStaticSitemapEntry webwinkelverhuisStaticPages
  ++ [webwinkelBlogIndexSitemapEntry articles]
  ++ map webwinkelArticleSitemapEntry articles
  ++ ["</urlset>"]

-- | The static pages with the day their copy last changed. Bump the day
-- when editing a page's copy; the blog index and articles get their
-- dates from the article metadata automatically. Current dates: the
-- site-wide rewrite of 2/3 aug 2026 (scanner-pagina, CCV/MWW-copy,
-- migratie-heroes, tagline).
--
-- Decision: hand-maintained lastmod days for the static pages,
-- reversing the earlier "we don't fabricate dates" stance (which
-- emitted the static entries dateless). Deriving dates from git at
-- build time is not possible (nix builds from a gitignoreSource copy
-- without .git), and dateless entries provably starved recrawl: on
-- 2026-08-03 Google had indexed 4 of 15 pages with the newest crawl
-- weeks old while the whole site had just been rewritten. The dates
-- here are real copy-change days from git history, kept honest by the
-- bump-on-edit rule above.
webwinkelverhuisStaticPages :: [(Text, Day)]
webwinkelverhuisStaticPages =
  [ ("https://webwinkelverhuis.nl/", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/prijzen.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/scan.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/migrate-ccvshop.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/migrate-lightspeed.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/waarom-mijnwebwinkel.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/waarom-lightspeed.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/over-ons.html", fromGregorian 2026 8 8)
  , ("https://webwinkelverhuis.nl/contact.html", fromGregorian 2026 8 8)
  ]

webwinkelStaticSitemapEntry :: (Text, Day) -> Text
webwinkelStaticSitemapEntry (url, changedOn) =
  webwinkelSitemapLine url (UTCTime changedOn 0)

-- | The blog index changes whenever an article is added or edited, so
-- it advertises the newest article lastmod. An empty article list means
-- a broken site build; crash loudly rather than emit a dateless entry.
webwinkelBlogIndexSitemapEntry :: [Article] -> Text
webwinkelBlogIndexSitemapEntry articles =
  case articles of
    [] -> error "webwinkelverhuis sitemap: blog index entry needs at least one article"
    _oneOrMore ->
      webwinkelSitemapLine "https://webwinkelverhuis.nl/blog/"
        (maximum (map articleLastmod articles))

webwinkelArticleSitemapEntry :: Article -> Text
webwinkelArticleSitemapEntry article =
  webwinkelSitemapLine
    ("https://webwinkelverhuis.nl/blog/" <> articleUrl article)
    (articleLastmod article)

webwinkelSitemapLine :: Text -> UTCTime -> Text
webwinkelSitemapLine url modified =
  "  <url><loc>" <> url <> "</loc><lastmod>"
    <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" modified)
    <> "</lastmod></url>"

-- =============================================================================
-- Over ons (over-ons.html)
-- =============================================================================

-- | The over-ons page: the origin story behind the service, requested via
-- klantfeedback (review Jappies vader, aug 2026): "vertel over jezelf,
-- van kind af aan geinteresseerd in computers, opleiding en ervaring in
-- binnen- en buitenland". Facts sourced from the blog archive (Windesheim
-- and the MSc AI at Utrecht in tool-survey/starting-at-daisee, the
-- Australia year in back-to-netherlands) so the story stays verifiable.
overOnsPage :: Html
overOnsPage = webwinkelBaseTemplate overOnsMeta $
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "Over ons"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Webwinkelverhuis is een dienst van Jappie Software, een klein softwarebedrijf uit Kampen. Met z&rsquo;n twee&euml;n, korte lijnen: je spreekt direct met degene die je migratie ook echt uitvoert." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Het verhaal"
      H.div ! A.class_ "audit-grid" $ do
        H.div $ do
          H.p $ H.preEscapedToHtml ("Ik ben Jappie Klooster en ik zit van kinds af aan achter de computer. Wat begon met spelen en dingen uit elkaar halen werd al snel programmeren, en dat ben ik nooit meer gestopt." :: Text)
          H.p $ H.preEscapedToHtml ("Na mijn hbo software engineering aan Windesheim in Zwolle deed ik een master kunstmatige intelligentie aan de Universiteit Utrecht. Daarna werkte ik als software engineer in binnen- en buitenland, onder meer een jaar in Sydney, Australi&euml;. Terug in Nederland bouwde ik jarenlang software voor bedrijven van startup tot enterprise, en inmiddels doe ik dat vanuit mijn eigen bedrijf." :: Text)
          H.p $ do
            H.preEscapedToHtml ("Webwinkelverhuis ontstond uit de eerste migratie die we deden: " :: Text)
            H.a ! A.href "/blog/klantverhaal-panzer-shopnl-van-mijnwebwinkel-naar-shopify-in-drie-talen.html" $ "panzer-shop.nl"
            H.preEscapedToHtml (", 2.400 producten in drie talen. In plaats van alles met de hand over te tikken bouwden we er gereedschap voor dat elke link, elk product en elke vertaling controleerbaar overzet. Dat gereedschap is sindsdien met elke verhuizing beter geworden. We hebben er inmiddels zoveel vertrouwen in dat we pas kosten in rekening brengen na een succesvolle verhuizing." :: Text)
          H.p $ H.preEscapedToHtml ("Na de verhuizing draait je winkel bovendien op een standaard platform: elke ontwikkelaar kan ermee verder, je bent nooit van ons afhankelijk." :: Text)
          H.p $ H.preEscapedToHtml ("Inmiddels doen we dit met z&rsquo;n twee&euml;n. Leana kwam bij het bedrijf om een opdracht voor de Haskell Foundation uit te voeren en is een expert in build-systemen; het migratievak leert ze er in de praktijk bij." :: Text)
        H.div ! A.class_ "portret-beeld" $
          H.img ! A.src "/assets/beeld/jappie-fit.jpg"
                ! A.alt "Jappie Klooster, lachend met duim omhoog"
                ! A.width "1100" ! A.height "1467" ! customAttribute "loading" "lazy"

    H.section ! A.class_ "audit" $ do
      H.h2 "Meer dan webshops"
      H.p $ do
        H.preEscapedToHtml ("Naast webshopmigraties bouwen we websites en maatwerksoftware onder de vlag van " :: Text)
        H.a ! A.href "https://jappiesoftware.com/" $ "jappiesoftware.com"
        H.preEscapedToHtml (". Dezelfde mensen, hetzelfde principe: degelijk werk, geen gedoe." :: Text)

    H.section ! A.class_ "cta-section" $ do
      H.h2 "Kennismaken?"
      H.p "Een gesprek kost niets en je weet meteen met wie je te maken heeft."
      H.a ! A.href meetLink ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    overOnsMeta :: PageMeta
    overOnsMeta = PageMeta
      { pageMetaTitle       = "Over ons \8212 Webwinkelverhuis"
      , pageMetaDescription = "Webwinkelverhuis is een dienst van Jappie Software, een klein softwarebedrijf uit Kampen. Het verhaal achter de dienst: van hobbyprogrammeur tot webshopmigraties met eigen gereedschap."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/over-ons.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

-- =============================================================================
-- Contact (contact.html)
-- =============================================================================

-- | The contact page: same klantfeedback-ronde as 'overOnsPage'. The
-- contactgegevens stood only in the footer; a menu tab makes them
-- findable. Details mirror the footer and bedrijfsgegevens (KvK 95097872).
contactPage :: Html
contactPage = webwinkelBaseTemplate contactMeta $
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "Contact"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Geen helpdesk, geen wachtrij: je krijgt antwoord van degene die je migratie ook uitvoert." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Zo bereik je ons"
      H.ul $ do
        H.li $ do
          H.strong "E-mail: "
          H.a ! A.href (toValue ("mailto:" <> webwinkelEmail)) $ toHtml webwinkelEmail
        H.li $ do
          H.strong "Telefoon of WhatsApp: "
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
        H.li $ do
          H.strong "Liever meteen inplannen: "
          H.a ! A.href meetLink $ "plan een gratis gesprek"

    H.section ! A.class_ "audit" $ do
      H.h2 "Bedrijfsgegevens"
      H.p $ H.preEscapedToHtml ("Webwinkelverhuis is een dienst van Jappie Software B.V.<br>Ooievaarstraat 38, 8262 AN Kampen<br>KvK: 95097872 &middot; BTW: NL867000569B01" :: Text)

    H.section ! A.class_ "cta-section" $ do
      H.h2 "Benieuwd wat je webshop zou kosten?"
      H.p "Vraag vrijblijvend een offerte aan; je betaalt pas na een geslaagde migratie."
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"
  where
    contactMeta :: PageMeta
    contactMeta = PageMeta
      { pageMetaTitle       = "Contact \8212 Webwinkelverhuis"
      , pageMetaDescription = "Neem contact op met Webwinkelverhuis: e-mail, telefoon, WhatsApp of plan direct een gratis gesprek. Je spreekt met degene die je migratie uitvoert."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/contact.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }
