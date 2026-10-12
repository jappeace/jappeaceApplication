{-# LANGUAGE OverloadedStrings #-}

-- | The /ondersteuning page of webwinkelverhuis.nl: the two support
-- subscriptions a shop can take after its migration.
--
-- The €249 terms (volumes, the 07:00–22:45 incident window, the three-hour
-- reply, the 25% credit, the 10% moving discount and cancellation) are the
-- ones offered to Kralenstulpje in jappiesoft offertes/offerte-kralenstulpje-
-- esther.org, bijlage 2 and 3. The €99 tier was never quoted; its volumes
-- (two questions, one change) come from the werkdagpakket in jappiesoft
-- strategy/webshopsupport-sla-concept.md.
--
-- Decision: a module of its own instead of another page in
-- 'WebwinkelTemplates', which is already far past the 1000-line limit.
module OndersteuningPage
  ( ondersteuningPage
  ) where

import Data.Text (Text)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PageChrome (PageMeta(..), webwinkelverhuisMeetLink)
import WebwinkelTemplates (webwinkelBaseTemplate)

ondersteuningPage :: Html
ondersteuningPage = webwinkelBaseTemplate ondersteuningMeta $
  H.main $ do
    H.section ! A.class_ "hero" $ do
      H.h1 "Ondersteuning na je verhuizing"
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("Je nieuwe webshop staat live. Wil je daarna iemand achter de hand die je winkel kent? Kies een vast maandbedrag voor je vragen, kleine aanpassingen en hulp bij storingen. Je koopt geen uren, maar afgesproken resultaten." :: Text)

    H.section ! A.class_ "prijs-sectie" $ do
      H.div ! A.class_ "prijs-kaarten" $ do
        basisKaart
        uitgebreidKaart
      H.p ! A.class_ "engagement-note" $ "Alle prijzen zijn exclusief btw en gelden voor één webshop."

    H.section ! A.class_ "audit" $ do
      H.h2 "Wat telt als vraag, wijziging of storing"
      H.p $ H.preEscapedToHtml ("Een korte hulpvraag gaat over &eacute;&eacute;n concrete handeling in je bestaande winkel." :: Text)
      H.p $ H.preEscapedToHtml ("Een kleine wijziging heeft &eacute;&eacute;n vooraf afgesproken resultaat. Dit gaat bijvoorbeeld over een banner aanpassen, een kleine aanpassing aan je thema of een afgebakende app-installatie." :: Text)
      H.p "Vragen en wijzigingen behandelen we van maandag tot en met vrijdag, van 09:00 tot 17:00, behalve op Nederlandse feestdagen. De uitvoering spreken we met je af. Ongebruikte ruimte gaat niet mee naar een volgende maand."
      H.p "Urgent is bijvoorbeeld een onbereikbare webshop, of een storing waardoor klanten niet kunnen bestellen of betalen. Storingshulp gaat niet af van je ruimte voor vragen en wijzigingen."
      H.p "Bij een storing onderzoeken we het probleem en werken we aan herstel van je bestaande winkel. Ligt het herstel bij een leverancier, dan zetten we de melding door en volgen we die op. Een hersteltermijn van een leverancier kunnen we niet garanderen."

    H.section ! A.class_ "audit" $ do
      H.h2 "De drie-uursafspraak bij uitgebreide ondersteuning"
      H.p "Meld je een urgente storing tussen 07:00 en 22:45, dan krijg je binnen drie uur per e-mail een beoordeling en een concreet plan voor de vervolgstappen. Die termijn loopt ook na 22:45 door: een melding om 22:00 krijgt uiterlijk om 01:00 antwoord. Meld je buiten dat tijdvak, dan begint de termijn om 07:00 en krijg je uiterlijk om 10:00 antwoord. Dit is geen garantie dat de storing binnen drie uur is opgelost."
      H.p $ H.preEscapedToHtml ("Halen we de drie uur niet, dan krijg je 25% van het maandbedrag terug: &euro;62,25 per gemiste termijn, met een maximum van &euro;249 per maand. Vervolgmeldingen over dezelfde storing tellen niet als een nieuwe storing." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Korting op je verhuizing"
      H.p $ H.preEscapedToHtml ("Betaal je het eerste jaar uitgebreide ondersteuning in &eacute;&eacute;n keer (&euro;2.988), dan krijg je 10% korting op je verhuizing. Het supportjaar begint bij de livegang en staat op dezelfde factuur als je verhuizing. Gaat de verhuizing niet door, dan start het supportjaar ook niet." :: Text)
      H.p "De korting geldt alleen voor ons verhuiswerk, niet voor de ondersteuning zelf of onze andere diensten. Ook kosten van derden vallen erbuiten. Dit gaat bijvoorbeeld over apps, een gekocht thema of een ontwerper."

    H.section ! A.class_ "audit" $ do
      H.h2 "Opzeggen"
      H.p $ H.preEscapedToHtml ("Betaal je per maand, dan zeg je op wanneer je wilt, met een opzegtermijn van &eacute;&eacute;n maand. Er is geen jaarverplichting." :: Text)
      H.p $ H.preEscapedToHtml ("Betaal je het eerste jaar vooruit, dan loopt je ondersteuning twaalf maanden vanaf de livegang. Daarna loopt het abonnement per maand door, met een opzegtermijn van &eacute;&eacute;n maand. Stop je tijdens dat eerste jaar, dan krijg je het resterende deel niet terug. Je ondersteuning loopt dan door tot het einde van het jaar." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Wat er niet onder valt"
      H.p "Nieuwe functies, een nieuw ontwerp, bulkbewerkingen en uitgebreide training spreken we vooraf met je af, met een vaste prijs."
      H.p "Algemene IT-hulp valt erbuiten. Dit gaat bijvoorbeeld over e-mail instellen op je telefoon of computer, of problemen met printers en wifi."
      H.p "Apps en andere diensten van derden betaal je rechtstreeks aan de aanbieder."

    H.section ! A.class_ "cta-section" $ do
      H.h2 "Ondersteuning afnemen?"
      H.p "We bespreken graag welk pakket bij je winkel past. Neem je het af bij je verhuizing, dan zetten we het in je offerte."
      H.a ! A.href webwinkelverhuisMeetLink ! A.class_ "cta-button" $ "Plan een gesprek"
      H.a ! A.href "/contact.html" ! A.class_ "cta-button-secondary" $ "Neem contact op"

basisKaart :: Html
basisKaart =
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Ondersteuning"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("&euro;99 " :: Text)
      H.small "per maand"
    H.p ! A.class_ "inbegrepen" $ "Voor je vragen en kleine aanpassingen, op werkdagen."
    H.ul $ do
      H.li "Twee korte hulpvragen per maand"
      H.li $ H.preEscapedToHtml ("&Eacute;&eacute;n kleine wijziging per maand, zoals een banner of een kleine aanpassing aan je thema" :: Text)
      H.li "Hulp bij urgente storingen op werkdagen, van 09:00 tot 17:00"
      H.li "Geen maximum aan storingsmeldingen"

uitgebreidKaart :: Html
uitgebreidKaart =
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Uitgebreide ondersteuning"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("&euro;249 " :: Text)
      H.small "per maand"
    H.p ! A.class_ "inbegrepen" $ H.preEscapedToHtml ("Meer vragen en aanpassingen, en hulp bij storingen ook &apos;s avonds en in het weekend." :: Text)
    H.ul $ do
      H.li "Vier korte hulpvragen per maand"
      H.li "Twee kleine wijzigingen per maand"
      H.li "Hulp bij urgente storingen iedere dag van 07:00 tot 22:45, ook in het weekend en op feestdagen"
      H.li "Binnen drie uur een beoordeling en een concreet plan per e-mail"
      H.li "Geen maximum aan storingsmeldingen"

ondersteuningMeta :: PageMeta
ondersteuningMeta = PageMeta
  { pageMetaTitle       = "Ondersteuning na je verhuizing \8212 Webwinkelverhuis"
  , pageMetaDescription = "Webshopondersteuning na je verhuizing naar Shopify: vanaf \8364\&99 per maand voor vragen en kleine aanpassingen, of \8364\&249 met hulp bij storingen iedere dag van 07:00 tot 22:45."
  , pageMetaLang        = "nl"
  , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/ondersteuning.html"
  , pageMetaOgImage     = Nothing
  , pageMetaSwitchUrl   = Nothing
  , pageMetaExtraHead   = mempty
  }
