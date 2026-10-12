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
-- Jappie limited the €99 incident window to working hours on 12 October.
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
      H.h1 "Ondersteuning: iemand die je webshop kent"
      H.p ! A.class_ "subtitle" $ "Je webshop loopt goed. Je wilt bezig zijn met je klanten en producten, niet met uitzoeken waarom een instelling ineens anders werkt. Met ondersteuning heb je iemand die je winkel kent en met je meekijkt als je twijfelt of iets misgaat."

    H.section ! A.class_ "audit" $ do
      H.h2 "Een vertrouwd aanspreekpunt, ook na de verhuizing"
      H.p "Even twijfelen over een instelling hoort erbij. Je hoeft niet eerst de juiste technische woorden te kennen: vertel ons wat je wilt doen of wat je opvalt. We kennen je winkel en helpen je verder, binnen de afspraken van je pakket."
      H.p "Voor een vast maandbedrag is er ruimte voor korte vragen en kleine aanpassingen. En als klanten niet meer kunnen bestellen, weet je bij wie je terechtkunt en wanneer je antwoord krijgt. Zo hoef je niet alles zelf uit te zoeken."
      H.p "Regel je dit liever zelf? Dan heb je geen abonnement nodig. Ondersteuning is een keuze, geen verplicht onderdeel van je verhuizing."

    H.section ! A.class_ "prijs-sectie" $ do
      H.div ! A.class_ "prijs-kaarten" $ do
        basisKaart
        uitgebreidKaart
      H.p ! A.class_ "engagement-note" $ "Alle prijzen zijn exclusief btw en gelden voor één webshop."

    H.section ! A.class_ "audit" $ do
      H.h2 "Voor je vragen en kleine aanpassingen"
      H.p "Wil je weten hoe je een kortingscode instelt of een tekst aanpast? Een korte hulpvraag gaat over één concrete handeling in je winkel. Wil je dat wij iets aanpassen, zoals een banner vervangen of een eenvoudige app installeren? Bij een kleine aanpassing spreken we vooraf één concreet resultaat af en wanneer we het uitvoeren."
      H.p "Het aantal inbegrepen vragen en kleine aanpassingen staat bij je pakket. We behandelen die in beide pakketten van maandag tot en met vrijdag, van 09:00 tot 17:00, behalve op Nederlandse feestdagen. Ongebruikte vragen en aanpassingen gaan niet mee naar de volgende maand."

    H.section ! A.class_ "audit" $ do
      H.h2 "En als er toch iets misgaat"
      H.p "Is je webshop onbereikbaar, of kunnen klanten niet meer bestellen of betalen? Dat is een urgente storing. We onderzoeken wat er misgaat en werken aan herstel van je bestaande winkel. Er is geen maximum aan het aantal storingen waarbij we helpen, binnen de tijden van je pakket. Die hulp gaat niet af van je ruimte voor vragen of kleine aanpassingen."
      H.p "Je hoeft niet eerst zelf de oorzaak te vinden. Je vertelt ons wat je merkt, wij zoeken uit wat er aan de hand is. Moet Shopify of de maker van een app iets herstellen? Dan zetten wij de melding door en volgen die op."
      H.p "We kunnen niet beloven hoe snel een storing is opgelost, bijvoorbeeld als Shopify of een andere leverancier iets moet herstellen. Wel spreken we af wanneer je een eerste inhoudelijk antwoord van ons krijgt."

    H.section ! A.class_ "audit" $ do
      H.h2 "Ondersteuning van €99: antwoord binnen acht werkuren"
      H.p "Bij een urgente storing krijg je binnen acht werkuren een inhoudelijk antwoord. We tellen alleen maandag tot en met vrijdag, van 09:00 tot 17:00 Nederlandse tijd, behalve op Nederlandse feestdagen. Buiten die uren staat de klok stil en hervatten we de storingshulp op de volgende werkdag."
      H.p "Een melding op vrijdag om 16:00 krijgt dus uiterlijk maandag om 16:00 antwoord, als maandag geen feestdag is. Dit is een afspraak over ons antwoord, niet over wanneer de storing opgelost is."

    H.section ! A.class_ "audit" $ do
      H.h2 "Uitgebreide ondersteuning: antwoord en plan binnen drie uur"
      H.p "Meld je een urgente storing tussen 07:00 en 22:45 Nederlandse tijd, dan krijg je binnen drie uur per e-mail onze beoordeling van het probleem en een concreet plan voor wat er daarna moet gebeuren. Dit geldt iedere dag, ook in het weekend en op feestdagen. Het betekent niet dat de storing binnen drie uur opgelost is."
      H.p "De drie uur lopen ook na 22:45 door: een melding om 22:00 krijgt uiterlijk om 01:00 antwoord. Meld je buiten de tijden van het pakket, dan begint de termijn om 07:00 en krijg je uiterlijk om 10:00 antwoord."
      H.p $ H.preEscapedToHtml ("Halen we de drie uur niet, dan krijg je 25% van het maandbedrag terug: &euro;62,25 per gemiste termijn, met een maximum van &euro;249 per maand. Vervolgmeldingen over dezelfde storing tellen niet als een nieuwe storing." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Korting op je verhuizing"
      H.p $ H.preEscapedToHtml ("Betaal je het eerste jaar uitgebreide ondersteuning in &eacute;&eacute;n keer (&euro;2.988), dan krijg je 10% korting op je verhuizing. Dat jaar begint zodra je nieuwe webshop live staat. Het jaarbedrag staat op dezelfde factuur als je verhuizing. Gaat de verhuizing niet door, dan begint het jaar ondersteuning ook niet." :: Text)
      H.p "De korting geldt alleen voor ons verhuiswerk, niet voor de ondersteuning zelf of onze andere diensten. Ook kosten van derden vallen erbuiten. Dit gaat bijvoorbeeld over apps, een gekocht thema of een ontwerper."

    H.section ! A.class_ "audit" $ do
      H.h2 "Opzeggen"
      H.p $ H.preEscapedToHtml ("Beide pakketten kun je per maand afnemen. Dan zeg je op wanneer je wilt, met een opzegtermijn van &eacute;&eacute;n maand. Er is geen jaarverplichting." :: Text)
      H.p $ H.preEscapedToHtml ("Alleen als je bij je verhuizing kiest voor een vooruitbetaald jaar uitgebreide ondersteuning met verhuiskorting, neem je eerst twaalf maanden af. Die beginnen zodra je nieuwe webshop live staat. Daarna loopt het abonnement per maand door, met een opzegtermijn van &eacute;&eacute;n maand." :: Text)
      H.p "Stop je vrijwillig tijdens dat eerste jaar, dan krijg je het resterende deel niet terug. Je ondersteuning blijft beschikbaar tot de afgesproken einddatum. Dit verandert niets aan de vergoeding voor gemiste reactietermijnen of je rechten als wij onze afspraken niet nakomen."

    H.section ! A.class_ "audit" $ do
      H.h2 "Groter werk spreken we apart af"
      H.p "Een nieuwe functie laten bouwen, je winkel opnieuw ontwerpen, veel producten tegelijk aanpassen of een uitgebreide training volgen valt buiten het pakket. Daarvoor spreken we vooraf een vaste prijs af."
      H.p "De ondersteuning is voor je webshop, niet voor je apparaten. E-mail instellen op je telefoon of computer, of problemen met printers en wifi vallen erbuiten."
      H.p "Apps en andere diensten van derden betaal je rechtstreeks aan de aanbieder."

    H.section ! A.class_ "cta-section" $ do
      H.h2 "Wil je iemand achter de hand voor je webshop?"
      H.p "Vertel ons waar je hulp bij wilt en op welke momenten. Dan bespreken we welk pakket bij je winkel past. Neem je ondersteuning af bij je verhuizing, dan zetten we het in je offerte."
      H.a ! A.href webwinkelverhuisMeetLink ! A.class_ "cta-button" $ "Plan een gesprek"
      H.a ! A.href "/contact.html" ! A.class_ "cta-button-secondary" $ "Neem contact op"

basisKaart :: Html
basisKaart =
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Ondersteuning"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("&euro;99 " :: Text)
      H.small "per maand"
    H.p ! A.class_ "inbegrepen" $ "Iemand die met je meekijkt en kleine aanpassingen doet, op werkdagen."
    H.ul $ do
      H.li "Twee korte hulpvragen per maand"
      H.li "Eén kleine aanpassing per maand, zoals een banner vervangen"
      H.li "Hulp bij urgente storingen van maandag tot en met vrijdag, van 09:00 tot 17:00, behalve op Nederlandse feestdagen"
      H.li "Bij een urgente storing binnen acht werkuren een inhoudelijk antwoord"
      H.li "Geen maximum aan storingsmeldingen"

uitgebreidKaart :: Html
uitgebreidKaart =
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Uitgebreide ondersteuning"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("&euro;249 " :: Text)
      H.small "per maand"
    H.p ! A.class_ "inbegrepen" $ "Meer ruimte voor vragen en aanpassingen, met hulp bij storingen ook in de avond en het weekend."
    H.ul $ do
      H.li "Vier korte hulpvragen per maand"
      H.li "Twee kleine aanpassingen per maand"
      H.li "Hulp bij urgente storingen iedere dag van 07:00 tot 22:45, ook in het weekend en op feestdagen"
      H.li "Bij een urgente storing binnen drie uur onze beoordeling en een concreet plan per e-mail, volgens de tijden hieronder"
      H.li "Geen maximum aan storingsmeldingen"

ondersteuningMeta :: PageMeta
ondersteuningMeta = PageMeta
  { pageMetaTitle       = "Ondersteuning na je verhuizing \8212 Webwinkelverhuis"
  , pageMetaDescription = "Je webshop loopt goed. Met ondersteuning heb je iemand die je winkel kent, voor vragen, kleine aanpassingen en hulp bij storingen. Vanaf \8364\&99 per maand."
  , pageMetaLang        = "nl"
  , pageMetaCanonical   = Just "https://webwinkelverhuis.nl/ondersteuning.html"
  , pageMetaOgImage     = Nothing
  , pageMetaSwitchUrl   = Nothing
  , pageMetaExtraHead   = mempty
  }
