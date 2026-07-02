{-# LANGUAGE OverloadedStrings #-}

-- | Templates for webwinkelverhuis.nl: the dedicated webshop-migration brand
-- domain. It carries its own navigation, footer and theme (distinct from the
-- jappiesoftware.com penguin theme in 'PenguinTemplates'), a landing page, the
-- per-platform migration and "waarom" pages, and its own blog. The operating
-- company behind it is still Jappie Software B.V.; shared structured data and
-- blog markup come from 'PageChrome'.
module WebwinkelTemplates
  ( webwinkelIndexPage
  , webwinkelBlogIndexPage
  , webwinkelArticlePage
  , appPage
  , mijnwebwinkelMigrationPage
  , ccvshopMigrationPage
  , lightspeedMigrationPage
  , mijnwebwinkelWaaromPage
  , lightspeedWaaromPage
  ) where

import Data.Text (Text)
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
  , whatsappFloatingButton
  , organizationJsonLd
  , serviceJsonLd
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

-- | The "ask for a quote" mailto used by every call-to-action button.
offerteMailto :: H.AttributeValue
offerteMailto = toValue ("mailto:" <> companyEmail <> "?subject=Migratie%20offerte")

-- | Mailto for merchants whose migration is already running or done and who
-- want follow-up work (mass edits, theme changes, integrations). The subject
-- differs from 'offerteMailto' so these mails are recognisable as
-- existing-client work rather than new leads.
uitbreidingMailto :: H.AttributeValue
uitbreidingMailto = toValue ("mailto:" <> companyEmail <> "?subject=Uitbreiding%20webshop")

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
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      if includeFeed
        then H.link ! A.href "/blog/atom"
                    ! A.type_ "application/atom+xml"
                    ! A.rel "alternate"
                    ! A.title "Webwinkelverhuis blog"
        else mempty
      H.script ! A.async "" ! A.src "https://www.googletagmanager.com/gtag/js?id=G-FMYV1PLWZ6" $ mempty
      H.script $ H.preEscapedToHtml ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-FMYV1PLWZ6');" :: Text)
      H.title (toHtml (pageMetaTitle meta))
      organizationJsonLd
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
            H.li $ H.a ! A.href "/blog/" $ "Blog"
            H.li $ H.a ! A.href offerteMailto ! A.class_ "cta-link" $ "Offerte"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href (toValue ("mailto:" <> companyEmail)) $ toHtml companyEmail
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ do
          "Webwinkelverhuis is een dienst van "
          H.a ! A.href "https://jappiesoftware.com/" $ "Jappie Software B.V."
          H.preEscapedToHtml (" &middot; KVK: 95097872" :: Text)

-- | Landing / migration page skeleton (Open Graph type "website").
webwinkelBaseTemplate :: PageMeta -> Html -> Html
webwinkelBaseTemplate = webwinkelBaseWith "website" False

-- | Blog page skeleton (Open Graph type "article", with Atom feed link).
webwinkelBlogBaseTemplate :: PageMeta -> Html -> Html
webwinkelBlogBaseTemplate = webwinkelBaseWith "article" True

-- =============================================================================
-- Landing page (index.html)
-- =============================================================================

webwinkelIndexPage :: Html
webwinkelIndexPage = webwinkelBaseTemplate indexMeta $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Verhuis uw webshop. Zonder dataverlies, zonder SEO-verlies."
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Vastgelopen op MijnWebwinkel, CCV Shop of Lightspeed? Wij verhuizen uw complete webshop geautomatiseerd naar Shopify of een ander platform &mdash; producten, vertalingen, afbeeldingen, klantdata en SEO-redirects. U betaalt pas na een succesvolle migratie." :: Text)
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- Platform cards
    H.section ! A.class_ "for-who" ! A.id "platforms" $ do
      H.h2 "Vanaf welk platform verhuist u?"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "MijnWebwinkel"
          H.p $ H.preEscapedToHtml ("Bevroren platform, verdubbelde prijzen, gesloten community. Wij zetten alles over &mdash; inclusief de automatisch gegenereerde 301-redirects voor uw artikel-URLs." :: Text)
          H.a ! A.href "/migrate-mijnwebwinkel.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Lightspeed"
          H.p $ H.preEscapedToHtml ("Beursgenoteerd en steeds duurder voor kleine shops. Wij verhuizen u veilig, met behoud van uw Google-posities &mdash; geen 70% verkeersverlies." :: Text)
          H.a ! A.href "/migrate-lightspeed.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "CCV Shop"
          H.p $ H.preEscapedToHtml ("Beperkte features en achterblijvende ontwikkeling. Wij zetten uw producten, talen, klantaccounts en voorraad volledig geautomatiseerd over." :: Text)
          H.a ! A.href "/migrate-ccvshop.html" ! A.class_ "cta-button" $ H.preEscapedToHtml ("Bekijk migratie &rarr;" :: Text)

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          " \8212 Ons programma leest uw huidige webshop volledig uit en slaat alle data op."
        H.li $ do
          H.strong "Controle"
          " \8212 U krijgt een werkende testshop die u zelf kunt controleren voordat we live gaan."
        H.li $ do
          H.strong "Import"
          " \8212 We importeren alles in uw nieuwe shop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Verificatie"
          " \8212 Samen controleren we steekproefsgewijs of alles klopt."

    -- Recent werk
    H.section ! A.class_ "results" $ do
      H.h2 "Recent werk"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            H.strong "Panzer-ShopNL"
            H.preEscapedToHtml (": een modeltreinwinkel met 2.400+ producten over drie domeinen en drie talen, verhuisd van MijnWebwinkel naar Shopify. Inclusief vertalingen, de volledige categorieboom en automatisch gegenereerde 301-redirects, zodat de Google-posities behouden bleven. " :: Text)
            H.a ! A.href "https://panzer-shop.nl/" $ H.preEscapedToHtml ("panzer-shop.nl &rarr;" :: Text)

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.p $ H.preEscapedToHtml ("Wij zijn geen Shopify-partner die commissie verdient op uw overstap. Wij zijn migratie-specialisten. U kiest het platform &mdash; Shopify, WooCommerce, of iets anders &mdash; wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (" &mdash; u betaalt pas na een succesvolle migratie" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (" &mdash; geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (" &mdash; 301-redirects zodat uw Google-posities niet verloren gaan" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (" &mdash; geen uurtarief, u weet vooraf wat het kost" :: Text)
      H.p $ do
        "Meer weten over waarom shops vertrekken? Lees onze "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te verhuizen?"
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Plan een gesprek"
  where
    indexMeta :: PageMeta
    indexMeta = PageMeta
      { pageMetaTitle       = "Webwinkelverhuis \8212 Verhuis uw webshop naar Shopify"
      , pageMetaDescription = "Geautomatiseerde webshop-migratie van MijnWebwinkel, CCV Shop of Lightspeed naar Shopify. Producten, vertalingen, klantdata en SEO-redirects. Betaling na succesvolle migratie."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = mempty
      }

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
          H.p "De app legt 301-redirects aan van elke oude URL naar de juiste nieuwe pagina, zodat uw Google-posities behouden blijven."
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

mijnwebwinkelMigrationPage :: Html
mijnwebwinkelMigrationPage = webwinkelBaseTemplate migrationMeta $ do
  whatsappFloatingButton mijnwebwinkelWhatsappLabel mijnwebwinkelWhatsappMessage
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 "Ontsnap MijnWebwinkel"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Uw webshop is uw broodwinning. MijnWebwinkel wordt al jaren niet meer doorontwikkeld, de community is gesloten, en support reageert niet. Hoelang blijft u nog wachten? Wij verhuizen uw complete shop naar Shopify &mdash; geautomatiseerd, zonder dataverlies, zonder downtime." :: Text)
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
          H.p ! A.class_ "price" $ H.preEscapedToHtml ("Vanaf &euro;999" :: Text)
          H.p $ H.preEscapedToHtml ("Producten, afbeeldingen, vertalingen, categorie&euml;n, klantdata, SEO-redirects en eventuele bulk-aanpassingen. Prijs afhankelijk van de omvang van uw webshop." :: Text)
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Vaste prijs, vooraf afgesproken. Geen verrassingen. Betaling na succesvolle migratie." :: Text)

    -- Recent werk
    H.section ! A.class_ "results" $ do
      H.h2 "Recent werk"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            H.strong "Panzer-ShopNL"
            H.preEscapedToHtml (": een modeltreinwinkel met 2.400+ producten over drie domeinen en drie talen, verhuisd van MijnWebwinkel naar Shopify. Inclusief vertalingen, de volledige categorieboom en automatisch gegenereerde 301-redirects, zodat de Google-posities behouden bleven. " :: Text)
            H.a ! A.href "https://panzer-shop.nl/" $ H.preEscapedToHtml ("panzer-shop.nl &rarr;" :: Text)

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $ do
          H.p $ H.preEscapedToHtml ("U weet het al: MijnWebwinkel gaat nergens meer heen. Geen nieuwe features, geen community, trage support. Elke dag dat u wacht is een dag dat uw concurrent op Shopify u inhaalt. Het werkt, want het is al gedaan." :: Text)
          H.p $ do
            H.a ! A.href "/waarom-mijnwebwinkel.html" $ "Waarom wordt MijnWebwinkel niet meer doorontwikkeld?"
            H.preEscapedToHtml (" &rarr;" :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn geen Shopify-partner die commissie verdient op uw overstap. Wij zijn migratie-specialisten. U kiest het platform &mdash; Shopify, WooCommerce, of iets anders &mdash; wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (" &mdash; u betaalt pas na succesvolle migratie" :: Text)
        H.li $ H.strong "Platformonafhankelijk" >> H.preEscapedToHtml (" &mdash; u kiest de bestemming, wij migreren naar elk platform" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (" &mdash; geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (" &mdash; 301-redirects zodat uw Google-posities niet verloren gaan" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (" &mdash; vertalingen correct gekoppeld via offici&euml;le APIs" :: Text)
        H.li $ H.strong "Controleerbaar" >> H.preEscapedToHtml (" &mdash; u krijgt een testshop en kunt alles verifi&euml;ren voor de overstap" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (" &mdash; geen uurtarief, u weet vooraf wat het kost" :: Text)

    -- FAQ
    H.section ! A.class_ "about" $ do
      H.h2 "Veelgestelde vragen"
      H.dl $ mapM_ renderFaqItem mijnwebwinkelFaq

    -- Technical details
    H.section ! A.class_ "for-who" $ do
      H.h2 "Technische details"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p $ H.preEscapedToHtml ("Volledige 301-redirect mapping van elke oude URL. We hebben de interne logica van MijnWebwinkel artikel-ID&rsquo;s in URLs achterhaald &mdash; alle redirects worden volledig automatisch gegenereerd, inclusief URLs met numerieke product-ID&rsquo;s." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie: alt-teksten genereren voor alle afbeeldingen, prijzen aanpassen, beschrijvingen opschonen, HTML-tags verwijderen &mdash; alles in &eacute;&eacute;n keer." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Platformkeuze"
          H.p $ H.preEscapedToHtml ("Shopify is het meest gekozen doelplatform, maar we ondersteunen ook WooCommerce of andere platformen. U kiest, wij regelen de techniek." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Vertalingen & URL-slugs"
          H.p $ H.preEscapedToHtml ("Meertalige content wordt correct gekoppeld via offici&euml;le platform APIs. Inclusief vertaalde URL-slugs &mdash; niet alleen productteksten." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantdata & spaarpunten"
          H.p "Klantaccounts, bestelgeschiedenis en spaarpuntensaldi worden overgezet naar het loyaliteitsprogramma van uw nieuwe platform."
        H.li ! A.class_ "card" $ do
          H.h3 "Testshop & verificatie"
          H.p "U krijgt een volledige testshop om alles te controleren. Pas na uw goedkeuring gaan we live. Correcties zijn inbegrepen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p $ H.preEscapedToHtml ("U hoeft niet langer te wachten tot MijnWebwinkel beter wordt &mdash; dat gaat niet gebeuren. " :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    migrationMeta :: PageMeta
    migrationMeta = PageMeta
      { pageMetaTitle       = "Ontsnap MijnWebwinkel \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van MijnWebwinkel naar Shopify, WooCommerce of een ander platform. Producten, vertalingen, afbeeldingen en SEO-redirects. Vanaf \8364\&999."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd mijnwebwinkelFaq <> serviceJsonLd
          "MijnWebwinkel naar Shopify migratie"
          "Geautomatiseerde migratie van MijnWebwinkel naar Shopify, WooCommerce of een ander platform: producten, vertalingen, afbeeldingen, categorieboom en SEO-redirects."
          "https://webwinkelverhuis.nl/migrate-mijnwebwinkel.html"
      }

mijnwebwinkelFaq :: [(Text, Text)]
mijnwebwinkelFaq =
  [ ( "Hoe lang duurt een migratie?"
    , "De technische migratie duurt meestal 1-2 werkdagen. De voorbereiding en controle erbij: reken op een week totaal." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , "We controleren samen steekproefsgewijs. Eventuele correcties zijn inbegrepen in de vaste prijs." )
  , ( "Werkt het ook voor andere talen dan NL/DE/EN?"
    , "Ja. Het programma ondersteunt elke taalcombinatie die MijnWebwinkel en uw doelplatform beide ondersteunen." )
  , ( "Kan ik ook naar een ander platform dan Shopify migreren?"
    , "Ja. Shopify is het meest gekozen doelplatform, maar we kunnen ook migreren naar WooCommerce of andere platformen." )
  , ( "Worden spaarpunten ook overgezet?"
    , "Ja. Spaarpuntensaldi van uw klanten worden meegenomen naar het loyaliteitsprogramma van uw nieuwe platform." )
  , ( "Hoe werken de SEO-redirects precies?"
    , "We hebben de onderliggende logica achterhaald waarmee MijnWebwinkel artikel-ID's in URLs genereert. Daardoor kunnen we alle redirects volledig automatisch aanmaken, ook voor URLs met numerieke product-ID's." )
  , ( "Kunnen jullie mijn productdata aanpassen tijdens de migratie?"
    , "Ja. We kunnen grootschalige wijzigingen doorvoeren, bijvoorbeeld alt-teksten genereren voor alle afbeeldingen, prijzen aanpassen of beschrijvingen opschonen." )
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
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Uw webshop is uw broodwinning. CCV Shop voelt steeds beperkter, de features blijven achter, en u weet dat er betere opties zijn &mdash; maar hoe krijgt u alles veilig overgezet? Wij verhuizen uw complete shop naar Shopify. Geautomatiseerd, zonder dataverlies, zonder downtime." :: Text)
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar Shopify-formaat."
        H.li ! A.class_ "card" $ do
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien &mdash; ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantaccounts"
          H.p "Klantgegevens en bestelgeschiedenis worden overgezet zodat uw klanten direct kunnen inloggen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Uw Google-posities en backlinks blijven behouden."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Shopify Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadinformatie en staffelprijzen worden meegenomen. Per-variant prijzen en voorraadbeheer werken direct in Shopify."

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          " \8212 Ons programma leest uw CCV Shop uit en slaat alle data op."
        H.li $ do
          H.strong "Controle"
          " \8212 U krijgt een werkende testshop die u zelf kunt controleren voordat we live gaan."
        H.li $ do
          H.strong "Import"
          " \8212 We importeren alles in uw Shopify-shop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Verificatie"
          " \8212 Samen controleren we steekproefsgewijs of alles klopt."

    -- Pricing
    H.section ! A.class_ "engagement" ! A.id "pricing" $ do
      H.h2 "Prijzen"
      H.div ! A.class_ "card-grid" $ do
        H.div ! A.class_ "card" $ do
          H.h3 "Volledige migratie"
          H.p ! A.class_ "price" $ H.preEscapedToHtml ("Vanaf &euro;999" :: Text)
          H.p $ H.preEscapedToHtml ("Producten, afbeeldingen, vertalingen, categorie&euml;n, klantdata, SEO-redirects en voorraad. Prijs afhankelijk van de omvang van uw webshop." :: Text)
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Vaste prijs, vooraf afgesproken. Geen verrassingen. Betaling na succesvolle migratie." :: Text)

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ H.preEscapedToHtml ("U voelt het al langer: CCV Shop houdt u tegen. Beperkte features, achterblijvende ontwikkeling, en het gevoel dat uw shop kwetsbaar is op een platform dat niet meegroeit. Elke dag dat u wacht is een dag dat uw concurrent op Shopify u inhaalt. Wij hebben al meerdere webshops succesvol gemigreerd &mdash; inclusief shops met duizenden producten en meerdere talen." :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn geen Shopify-partner die commissie verdient op uw overstap. Wij zijn migratie-specialisten. U kiest het platform &mdash; Shopify, WooCommerce, of iets anders &mdash; wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (" &mdash; u betaalt pas na succesvolle migratie" :: Text)
        H.li $ H.strong "Platformonafhankelijk" >> H.preEscapedToHtml (" &mdash; u kiest de bestemming, wij migreren naar elk platform" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (" &mdash; geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (" &mdash; 301-redirects zodat uw Google-posities niet verloren gaan" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (" &mdash; vertalingen correct gekoppeld via offici&euml;le Shopify APIs" :: Text)
        H.li $ H.strong "Controleerbaar" >> H.preEscapedToHtml (" &mdash; u krijgt een testshop en kunt alles verifi&euml;ren voor de overstap" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (" &mdash; geen uurtarief, u weet vooraf wat het kost" :: Text)

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
          H.p "Volledige 301-redirect mapping van elke oude URL naar het nieuwe Shopify-adres. Uw Google-rankings, backlinks en organisch verkeer blijven behouden."
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen &mdash; alles in &eacute;&eacute;n keer." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & staffelprijzen"
          H.p "Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar Shopify. Inclusief prijsverschillen per maat of kleur."
        H.li ! A.class_ "card" $ do
          H.h3 "Vertalingen & URL-slugs"
          H.p $ H.preEscapedToHtml ("Meertalige content wordt correct gekoppeld via de offici&euml;le Shopify API. Inclusief vertaalde URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantdata & accounts"
          H.p "Klantaccounts en bestelgeschiedenis worden overgezet zodat uw klanten direct verder kunnen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "Testshop & verificatie"
          H.p "U krijgt een volledige testshop om alles te controleren. Pas na uw goedkeuring gaan we live. Correcties zijn inbegrepen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p $ H.preEscapedToHtml ("U hoeft niet langer te wachten tot CCV Shop beter wordt. Neem de controle terug over uw webshop." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    ccvMeta :: PageMeta
    ccvMeta = PageMeta
      { pageMetaTitle       = "Ontsnap CCV Shop \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van CCV Shop naar Shopify. Producten, vertalingen, afbeeldingen, voorraad en SEO-redirects. Vanaf \8364\&999."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-ccvshop.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd ccvshopFaq <> serviceJsonLd
          "CCV Shop naar Shopify migratie"
          "Geautomatiseerde migratie van CCV Shop naar Shopify: producten, vertalingen, afbeeldingen, voorraad, klantdata en SEO-redirects."
          "https://webwinkelverhuis.nl/migrate-ccvshop.html"
      }

ccvshopFaq :: [(Text, Text)]
ccvshopFaq =
  [ ( "Hoe lang duurt een migratie?"
    , "De technische migratie duurt meestal 1-2 werkdagen. De voorbereiding en controle erbij: reken op een week totaal." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , "We controleren samen steekproefsgewijs. Eventuele correcties zijn inbegrepen in de vaste prijs." )
  , ( "Werkt het ook voor meerdere talen?"
    , "Ja. Het programma ondersteunt elke taalcombinatie die CCV Shop en Shopify beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat uw klanten direct verder kunnen." )
  , ( "Hoe werken de SEO-redirects precies?"
    , "We genereren automatisch 301-redirects van elke oude URL naar het nieuwe Shopify-adres. Uw Google-rankings en backlinks blijven behouden." )
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
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Lightspeed duwt u richting hun nieuwe platform of de deur uit. Ondertussen draait uw webshop op verouderde software die steeds minder krijgt. Wij verhuizen uw complete shop naar Shopify &mdash; geautomatiseerd, zonder dataverlies, zonder SEO-verlies." :: Text)
      H.a ! A.href offerteMailto ! A.class_ "cta-button" $ "Vraag een offerte aan"

    -- What we migrate
    H.section ! A.class_ "for-who" ! A.id "what" $ do
      H.h2 "Wat we migreren"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Producten & varianten"
          H.p "Alle producten inclusief titels, beschrijvingen, prijzen, afbeeldingen, SKU's en varianten. Automatisch overgezet naar Shopify-formaat."
        H.li ! A.class_ "card" $ do
          H.h3 "Meerdere talen"
          H.p $ H.preEscapedToHtml ("Vertalingen worden correct gekoppeld. Uw klanten blijven uw shop in hun eigen taal zien &mdash; ook de URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantaccounts & bestellingen"
          H.p "Klantgegevens, bestelgeschiedenis en accountdata worden overgezet zodat uw klanten direct kunnen inloggen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "SEO-redirects"
          H.p "301-redirects van elke oude URL naar de nieuwe URL. Uw Google-posities en backlinks blijven behouden. Geen 70% verkeersverlies zoals bij een onbegeleide migratie."
        H.li ! A.class_ "card" $ do
          H.h3 $ H.preEscapedToHtml ("Categorie&euml;n & navigatie" :: Text)
          H.p $ H.preEscapedToHtml ("De volledige categorieboom wordt overgezet naar Shopify Collections met vertaalde titels en het navigatiemenu." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & prijzen"
          H.p "Voorraadbeheer, staffelprijzen en per-variant pricing worden correct overgezet. Uw voorraadniveaus kloppen direct in Shopify."

    -- How it works
    H.section ! A.class_ "audit" $ do
      H.h2 "Hoe het werkt"
      H.ol $ do
        H.li $ do
          H.strong "Scan"
          " \8212 Ons programma leest uw Lightspeed-shop volledig uit en slaat alle data op."
        H.li $ do
          H.strong "Controle"
          " \8212 U krijgt een werkende testshop die u zelf kunt controleren voordat we live gaan."
        H.li $ do
          H.strong "Import"
          " \8212 We importeren alles in uw Shopify-shop: producten, vertalingen, collections, redirects."
        H.li $ do
          H.strong "Verificatie"
          " \8212 Samen controleren we steekproefsgewijs of alles klopt."

    -- Pricing
    H.section ! A.class_ "engagement" ! A.id "pricing" $ do
      H.h2 "Prijzen"
      H.div ! A.class_ "card-grid" $ do
        H.div ! A.class_ "card" $ do
          H.h3 "Volledige migratie"
          H.p ! A.class_ "price" $ H.preEscapedToHtml ("Vanaf &euro;999" :: Text)
          H.p $ H.preEscapedToHtml ("Producten, afbeeldingen, vertalingen, categorie&euml;n, klantdata, SEO-redirects en voorraad. Prijs afhankelijk van de omvang van uw webshop." :: Text)
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("Vaste prijs, vooraf afgesproken. Geen verrassingen. Betaling na succesvolle migratie." :: Text)

    -- Why us
    H.section ! A.class_ "results" $ do
      H.h2 "Waarom via ons?"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $ do
          H.p $ H.preEscapedToHtml ("U bent niet de enige: 59% van alle Lightspeed-vertrekkers kiest Shopify. Maar zonder begeleiding raakt u uw Google-posities kwijt &mdash; wij hebben verhalen gezien van 70% verkeersverlies bij een onbegeleide migratie. Wij zorgen dat uw SEO intact blijft." :: Text)
          H.p $ do
            H.a ! A.href "/waarom-lightspeed.html" $ "Waarom verlaten steeds meer webshops Lightspeed?"
            H.preEscapedToHtml (" &rarr;" :: Text)
      H.p $ H.preEscapedToHtml ("Wij zijn geen Shopify-partner die commissie verdient op uw overstap. Wij zijn migratie-specialisten. U kiest het platform &mdash; Shopify, WooCommerce, of iets anders &mdash; wij regelen de techniek." :: Text)
      H.ul $ do
        H.li $ H.strong "Geen risico" >> H.preEscapedToHtml (" &mdash; u betaalt pas na succesvolle migratie" :: Text)
        H.li $ H.strong "Platformonafhankelijk" >> H.preEscapedToHtml (" &mdash; u kiest de bestemming, wij migreren naar elk platform" :: Text)
        H.li $ H.strong "SEO-behoud" >> H.preEscapedToHtml (" &mdash; 301-redirects zodat uw Google-posities niet verloren gaan" :: Text)
        H.li $ H.strong "Geautomatiseerd" >> H.preEscapedToHtml (" &mdash; geen handmatig overtypen, geen kopieerfouten" :: Text)
        H.li $ H.strong "Meertalig" >> H.preEscapedToHtml (" &mdash; vertalingen correct gekoppeld via offici&euml;le Shopify APIs" :: Text)
        H.li $ H.strong "Controleerbaar" >> H.preEscapedToHtml (" &mdash; u krijgt een testshop en kunt alles verifi&euml;ren voor de overstap" :: Text)
        H.li $ H.strong "Vaste prijs" >> H.preEscapedToHtml (" &mdash; geen uurtarief, u weet vooraf wat het kost" :: Text)

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
          H.p "Volledige 301-redirect mapping van elke oude URL naar het nieuwe Shopify-adres. Uw Google-rankings, backlinks en organisch verkeer blijven behouden."
        H.li ! A.class_ "card" $ do
          H.h3 "Bulk-aanpassingen"
          H.p $ H.preEscapedToHtml ("Grootschalige wijzigingen aan uw productdata tijdens de migratie: alt-teksten genereren, prijzen aanpassen, beschrijvingen opschonen &mdash; alles in &eacute;&eacute;n keer." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Voorraad & staffelprijzen"
          H.p "Per-variant voorraadbeheer en staffelprijzen worden correct overgezet naar Shopify. Inclusief prijsverschillen per maat of kleur."
        H.li ! A.class_ "card" $ do
          H.h3 "Vertalingen & URL-slugs"
          H.p $ H.preEscapedToHtml ("Meertalige content wordt correct gekoppeld via de offici&euml;le Shopify API. Inclusief vertaalde URL-slugs." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Klantdata & accounts"
          H.p "Klantaccounts en bestelgeschiedenis worden overgezet zodat uw klanten direct verder kunnen op de nieuwe shop."
        H.li ! A.class_ "card" $ do
          H.h3 "Testshop & verificatie"
          H.p "U krijgt een volledige testshop om alles te controleren. Pas na uw goedkeuring gaan we live. Correcties zijn inbegrepen."

    -- CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 "Klaar om te ontsnappen?"
      H.p $ H.preEscapedToHtml ("Lightspeed gaat u niet helpen met deze overstap. Wij wel." :: Text)
      H.p "Plan een gratis, vrijblijvend gesprek. We bekijken samen uw webshop en geven direct een inschatting."
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Ontsnap nu"
  where
    lightspeedMeta :: PageMeta
    lightspeedMeta = PageMeta
      { pageMetaTitle       = "Ontsnap Lightspeed \8212 Migratie naar Shopify \8212 Webwinkelverhuis"
      , pageMetaDescription = "Geautomatiseerde migratie van Lightspeed naar Shopify. Producten, vertalingen, afbeeldingen, voorraad en SEO-redirects. Geen verkeersverlies. Vanaf \8364\&999."
      , pageMetaLang        = "nl"
      , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/migrate-lightspeed.html"
      , pageMetaOgImage     = Nothing
      , pageMetaSwitchUrl   = Nothing
      , pageMetaExtraHead   = faqPageJsonLd lightspeedFaq <> serviceJsonLd
          "Lightspeed naar Shopify migratie"
          "Geautomatiseerde migratie van Lightspeed naar Shopify: producten, vertalingen, afbeeldingen, voorraad en SEO-redirects, zonder verkeersverlies."
          "https://webwinkelverhuis.nl/migrate-lightspeed.html"
      }

lightspeedFaq :: [(Text, Text)]
lightspeedFaq =
  [ ( "Hoe lang duurt een migratie?"
    , "De technische migratie duurt meestal 1-2 werkdagen. De voorbereiding en controle erbij: reken op een week totaal." )
  , ( "Kan ik mijn domeinnaam behouden?"
    , "Ja. Na de migratie wijst u uw domein naar Shopify. Alle oude URLs worden automatisch doorgestuurd." )
  , ( "Wat als er iets niet klopt na de migratie?"
    , "We controleren samen steekproefsgewijs. Eventuele correcties zijn inbegrepen in de vaste prijs." )
  , ( "Verlies ik mijn Google-posities?"
    , "Nee. Wij genereren automatisch 301-redirects voor elke URL. Dit is het belangrijkste onderdeel van een veilige migratie en de reden dat u dit niet zelf wilt doen." )
  , ( "Werkt het ook voor meerdere talen?"
    , "Ja. Het programma ondersteunt elke taalcombinatie die Lightspeed en Shopify beide ondersteunen." )
  , ( "Worden mijn klantaccounts overgezet?"
    , "Ja. Klantgegevens en bestelgeschiedenis worden meegenomen zodat uw klanten direct verder kunnen." )
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
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Het korte antwoord: MijnWebwinkel is in 2021 verkocht aan een Noors softwareconglomeraat. Sindsdien is de code bevroren en zijn de prijzen verdubbeld. Dit is geen complottheorie &mdash; het is gewoon de financi&euml;le logica van private equity." :: Text)

    -- Timeline
    H.section ! A.class_ "for-who" $ do
      H.h2 "De tijdlijn"
      H.ol $ do
        H.li $ do
          H.strong "2005"
          H.preEscapedToHtml (" &mdash; Alex Pansier richt MijnWebwinkel op in Oss. Het platform groeit organisch naar bijna 7.000 webshops." :: Text)
        H.li $ do
          H.strong "November 2021"
          H.preEscapedToHtml (" &mdash; " :: Text)
          H.a ! A.href "https://www.visma.com/news/visma-strengthens-its-position-in-the-benelux-e-commerce-market-with-the-acquisition-of-mijnwebwinkel" $ "Visma neemt MijnWebwinkel over"
          ". De oprichter vertrekt. Visma is een Noors softwareconglomeraat (15.000 medewerkers, 170+ bedrijven) in handen van het Britse private-equityfonds Hg Capital."
        H.li $ do
          H.strong "2022\8211\&2025"
          H.preEscapedToHtml (" &mdash; De &euro;20-tier wordt beperkt tot 25 producten. Serieuze shops betalen nu &euro;40\8211\&70/maand. Ontwikkeling stopt. Het aantal webshops daalt van ~7.000 naar ~4.500." :: Text)
        H.li $ do
          H.strong "November 2025"
          H.preEscapedToHtml (" &mdash; " :: Text)
          H.a ! A.href "https://www.emerce.nl/wire/mijnwebwinkel-mystore-lanceren-acendy-nieuw-tijdperk-ecommerce" $ "MijnWebwinkel wordt samengevoegd met het Noorse Mystore"
          H.preEscapedToHtml (" tot &ldquo;Acendy&rdquo;. Dit werd kort daarna weer teruggedraaid." :: Text)
        H.li $ do
          H.strong "Februari 2026"
          H.preEscapedToHtml (" &mdash; " :: Text)
          H.a ! A.href "https://www.privateequitywire.co.uk/hg-spins-out-e500m-of-visma-assets-as-ipo-plans-stall/" $ H.preEscapedToHtml ("Visma stoot &euro;500 miljoen aan bedrijven af" :: Text)
          " (waaronder Acendy/MijnWebwinkel) in een nieuw vehikel genaamd Norvato. Reden: Visma bereidt een beursgang voor en wil alleen kernproducten behouden."

    -- Why this happens
    H.section ! A.class_ "audit" $ do
      H.h2 "Waarom wordt het verwaarloosd?"
      H.p $ H.preEscapedToHtml ("MijnWebwinkel is geen slecht bedrijf met incompetente ontwikkelaars. Het is een <strong>winstgevend platform dat bewust wordt leeggemolken</strong> door de eigenaren. Dit is het standaard private-equity draaiboek:" :: Text)
      H.ol $ do
        H.li $ do
          H.strong "Koop goedkoop"
          " \8212 een winstgevend SaaS-platform met duizenden betalende klanten"
        H.li $ do
          H.strong "Verhoog prijzen"
          H.preEscapedToHtml (" &mdash; beperk het goedkope plan tot 25 producten, duw serieuze shops naar &euro;40\8211\&70/maand" :: Text)
        H.li $ do
          H.strong "Verlaag kosten"
          " \8212 stop alle ontwikkeling, minimaliseer support"
        H.li $ do
          H.strong "Melk de marge"
          H.preEscapedToHtml (" &mdash; 4.500 shops &times; &euro;40/maand = &euro;2,1 miljoen per jaar aan inkomsten met minimale kosten" :: Text)
        H.li $ do
          H.strong "Voeg samen of verkoop"
          " \8212 als de melkkoe opdroogt, fuseer met een ander product of stoot af"

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
      H.p $ H.preEscapedToHtml ("MijnWebwinkel wordt niet meer beter. Het platform is verkocht, de code is bevroren, en de opvolger kost het dubbele. U kunt wachten tot u <em>gedwongen</em> wordt te migreren naar Acendy &mdash; of u kunt nu zelf kiezen waar u naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-mijnwebwinkel.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (" &mdash; volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Ontsnap nu"
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
      H.p $ H.preEscapedToHtml ("Lightspeed Commerce (NYSE/TSX: LSPD) ging in 2019 naar de beurs in Toronto en in 2020 naar New York. Sindsdien is het bedrijf niet meer van de oprichter &mdash; het is van de aandeelhouders. En aandeelhouders willen &eacute;&eacute;n ding: groei." :: Text)
      H.p "Groei kan op twee manieren:"
      H.ol $ do
        H.li $ do
          H.strong "Meer klanten"
          " \8212 maar de markt is verzadigd en Lightspeed verliest netto klanten"
        H.li $ do
          H.strong "Meer omzet per klant"
          H.preEscapedToHtml (" &mdash; en d&aacute;t is precies wat er gebeurt" :: Text)
      H.p $ H.preEscapedToHtml ("Het gevolg: de goedkoopste plannen worden duurder, features worden weggehaald uit lagere tiers, en het hele platform wordt opgeschoven richting grotere winkeliers die meer betalen. <strong>U als kleine webshop bent niet de doelgroep &mdash; u bent de ballast.</strong>" :: Text)
      H.p $ H.preEscapedToHtml ("Wij vinden dat fundamenteel verkeerd. Kleine webshops groeien &mdash; dat is het hele punt. De webshop van vandaag met 200 producten is de webshop van volgend jaar met 2.000 producten. Maar als Lightspeed niet in die groei gelooft, hoeft u daar niet op te wachten. Wij helpen u graag naar een platform dat w&eacute;l in u investeert." :: Text)

    -- Timeline
    H.section ! A.class_ "for-who" $ do
      H.h2 "Wat er is gebeurd"
      H.ol $ do
        H.li $ do
          H.strong "2019"
          H.preEscapedToHtml (" &mdash; Lightspeed gaat naar de beurs in Toronto. Haalt $240 miljoen op. Het bedrijf moet nu elk kwartaal groeicijfers laten zien." :: Text)
        H.li $ do
          H.strong "2020"
          H.preEscapedToHtml (" &mdash; Tweede beursnotering in New York. Nog eens $376 miljoen opgehaald. Begint agressief bedrijven op te kopen: ShopKeep, Vend, Ecwid, NuORDER. Dit lijkt op een leveraged-buyout-strategie: groei door overnames in plaats van door het eigen product te verbeteren. Het probleem is dat Lightspeed hun eigen platform niet kon laten groeien &mdash; en nu alle toekomstige groei opoffert voor kortetermijnwinst." :: Text)
        H.li $ do
          H.strong "2021"
          H.preEscapedToHtml (" &mdash; Aandeel piekt rond $125. Kort daarna publiceert " :: Text)
          H.a ! A.href "https://www.sprucepointcap.com/lightspeed-commerce-inc" $ "Spruce Point Capital"
          H.preEscapedToHtml (" een vernietigend rapport dat de groeicijfers in twijfel trekt. Het aandeel keldert." :: Text)
        H.li $ do
          H.strong "2022\8211\&2024"
          H.preEscapedToHtml (" &mdash; Prijzen worden verhoogd. Het goedkoopste plan (Essential) kost nu &euro;68/maand voor slechts 250 productvarianten. Het oude eCom-platform (C-Series) wordt afgebouwd richting E-Series (Ecwid)." :: Text)
        H.li $ do
          H.strong "2025\8211\&2026"
          H.preEscapedToHtml (" &mdash; Het aantal actieve Lightspeed-webshops daalt van 23.700 naar 18.500. Een verlies van 22% in drie jaar." :: Text)

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
          H.preEscapedToHtml (" &mdash; het oude eCom (C-Series) wordt afgebouwd. De opvolger is E-Series, gebaseerd op het opgekochte Ecwid. Uw huidige shop is legacy." :: Text)
        H.li $ do
          H.strong "Verschuif naar enterprise"
          H.preEscapedToHtml (" &mdash; het Professional-plan kost &euro;259/maand, Enterprise is op offerte. Lightspeed wil minder klanten die meer betalen." :: Text)
        H.li $ do
          H.strong "Verhoog de opbrengst per klant"
          H.preEscapedToHtml (" &mdash; via hogere abonnementen, betaalverwerking (Lightspeed Payments), en kapitaalverstrekking (Lightspeed Capital). Elk kwartaal moet dit cijfer omhoog." :: Text)
        H.li $ do
          H.strong "Laat kleine shops vanzelf vertrekken"
          H.preEscapedToHtml (" &mdash; door de prijs hoog genoeg te maken dat het voor kleine shops niet meer rendabel is. Dat is geen vergissing &mdash; dat is de strategie." :: Text)
      H.p $ H.preEscapedToHtml ("De aandelenkoers is gedaald van $125 naar rond de $20. Het management staat onder druk om winstgevender te worden. Dat betekent: <strong>hogere prijzen, minder support, en focus op grote klanten</strong>. Kleine webshops passen niet in dat plaatje." :: Text)

    -- Nederland specifiek
    H.section ! A.class_ "for-who" $ do
      H.h2 "Nederland als melkkoe"
      H.p $ H.preEscapedToHtml ("27% van alle Lightspeed-webshops zit in Nederland &mdash; bijna 5.000 shops. Nederland is veruit de grootste markt, groter dan de VS en Canada samen. Toch wordt het platform bestuurd vanuit Montreal, met Amerikaanse prioriteiten." :: Text)
      H.p $ H.preEscapedToHtml ("De E-Series migratie is <strong>alleen beschikbaar in Noord-Amerika</strong>. Nederlandse shops zitten vast op de C-Series &mdash; een platform dat niet meer wordt doorontwikkeld, met prijzen die wel doorstijgen." :: Text)

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
      H.p $ H.preEscapedToHtml ("Lightspeed wordt elk kwartaal duurder en elk kwartaal minder gericht op u. U kunt wachten tot de volgende prijsverhoging &mdash; of u kunt nu zelf kiezen waar u naartoe gaat." :: Text)
      H.p $ do
        H.a ! A.href "/migrate-lightspeed.html" $ "Bekijk onze migratieservice"
        H.preEscapedToHtml (" &mdash; volledig geautomatiseerd, vaste prijs, betaling na succes." :: Text)
      H.a ! A.href "https://calendar.app.google/9h9uTzsPQoryEc6S7" ! A.class_ "cta-button" $ "Ontsnap nu"
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

lightspeedWaaromFaq :: [(Text, Text)]
lightspeedWaaromFaq =
  [ ( "Waarom wordt Lightspeed steeds duurder?"
    , "Lightspeed is beursgenoteerd (NYSE/TSX: LSPD) en moet elk kwartaal groei laten zien aan aandeelhouders. Omdat de markt verzadigd is, verhoogt het management de prijs per klant in plaats van meer klanten te werven." )
  , ( "Wat is er met Lightspeed eCom C-Series?"
    , "De C-Series wordt afgebouwd. De opvolger is E-Series, gebaseerd op het opgekochte Ecwid. De E-Series migratie is alleen beschikbaar in Noord-Amerika \8212 Nederlandse shops zitten vast op het oude platform." )
  , ( "Hoeveel webshops verlaten Lightspeed?"
    , "In de afgelopen 90 dagen vertrokken 160 webshops terwijl er slechts 16 bijkwamen. Sinds Q3 2023 is het totaal gedaald van 23.700 naar 18.500 shops \8212 een daling van 22%." )
  , ( "Waar gaan vertrekkende Lightspeed-shops naartoe?"
    , "59% van de vertrekkende Lightspeed-shops kiest Shopify als bestemming." )
  ]

-- =============================================================================
-- FAQ rendering (visible accordion-free definition list)
-- =============================================================================

-- | Render a single question/answer pair as a @<dt>/<dd>@ pair. The same pairs
-- feed 'faqPageJsonLd' so the visible FAQ and the structured data never drift.
renderFaqItem :: (Text, Text) -> Html
renderFaqItem (question, answer) = do
  H.dt (toHtml question)
  H.dd (toHtml answer)

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
