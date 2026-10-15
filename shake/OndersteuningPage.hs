{-# LANGUAGE OverloadedStrings #-}

-- | The /ondersteuning page of webwinkelverhuis.nl: the two support
-- subscriptions a shop can take after its migration.
--
-- The response, credit, discount and cancellation terms originate in
-- jappiesoft offertes/offerte-kralenstulpje-esther.org, appendices 2 and 3.
-- Jappie's 12 October revisions set five working hours for €99 incidents
-- and five questions plus three changes for €249, without a question-length
-- limit. The €99 volumes remain proposals from webshopsupport-sla-concept.md.
-- His 13 October decisions add the €39 and €79 extras, theme security fixes
-- in both packages and a periodic check, monthly for €249 and quarterly for
-- €99 (confirmed 14 October). On 14 October he also made both packages
-- answer an urgent incident with an assessment and plan, and included every
-- technical shop change required by new legislation at no extra cost.
-- On 15 October he set the migration warranty at twelve months after go-live:
-- migration errors are fixed free and never use up a package's allowance.
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
      H.p "Voor een vast maandbedrag is er ruimte voor vragen en kleine aanpassingen. En als klanten niet meer kunnen bestellen, weet je bij wie je terechtkunt en wanneer je antwoord krijgt. Zo hoef je niet alles zelf uit te zoeken."
      H.p "Regel je dit liever zelf? Dan heb je geen abonnement nodig. Ondersteuning is een keuze, geen verplicht onderdeel van je verhuizing."

    H.section ! A.class_ "prijs-sectie" $ do
      H.div ! A.class_ "prijs-kaarten" $ do
        basisKaart
        uitgebreidKaart
      H.p ! A.class_ "engagement-note" $ "Alle prijzen zijn exclusief btw en gelden voor één webshop."

    H.section ! A.class_ "audit" $ do
      H.h2 "Voor je vragen en kleine aanpassingen"
      H.p "Wil je weten hoe je een kortingscode instelt of waarom een instelling anders werkt dan verwacht? Een hulpvraag gaat over één onderwerp in je webshop. Je vraag hoeft niet kort te zijn. Doorvragen over hetzelfde antwoord hoort bij diezelfde vraag."
      H.p "Een kleine aanpassing is één afgebakende wijziging in je bestaande webshop. Denk aan een app installeren en instellen, een banner vervangen of een kleine wijziging aan de indeling of opmaak van je thema. We spreken vooraf af wat het resultaat wordt en wanneer we het uitvoeren."
      H.p "Het aantal inbegrepen vragen en kleine aanpassingen staat bij je pakket. We behandelen die in beide pakketten van maandag tot en met vrijdag, van 09:00 tot 17:00, behalve op Nederlandse feestdagen. Ongebruikte vragen en aanpassingen gaan niet mee naar de volgende maand."
      H.p $ H.preEscapedToHtml ("Heb je in een maand meer nodig? Een extra hulpvraag kost &euro;39 exclusief btw. Doorvragen over hetzelfde onderwerp hoort daar ook bij. Een extra kleine aanpassing kost &euro;79 exclusief btw. Extra kosten spreken we altijd vooraf met je af." :: Text)

    H.section ! A.class_ "audit" $ do
      H.h2 "Fouten uit je verhuizing horen niet bij je pakket"
      H.p "Is er bij je verhuizing iets niet goed overgezet, of werkt iets anders dan in je offerte staat? Dan herstellen we dat kosteloos, ook als je het pas na de livegang ontdekt. Dat valt onder de garantie op je verhuizing, niet onder je ondersteuning. Het gaat dus niet af van je hulpvragen of kleine aanpassingen."
      H.p "Je kunt zulke fouten tot twaalf maanden na de livegang melden, ook zonder ondersteuningspakket. Is er na de livegang iets gewijzigd door jou of door iemand anders? Dan telt het herstel daarvan als een gewone hulpvraag of kleine aanpassing uit je pakket."

    H.section ! A.class_ "audit" $ do
      H.h2 "En als er toch iets misgaat"
      H.p "Is je webshop onbereikbaar, of kunnen klanten niet meer bestellen of betalen? Dat is een urgente storing. We onderzoeken wat er misgaat en werken aan herstel van je bestaande winkel. Er is geen maximum aan het aantal storingen waarbij we helpen, binnen de tijden van je pakket. Die hulp gaat niet af van je ruimte voor vragen of kleine aanpassingen."
      H.p "Je hoeft niet eerst zelf de oorzaak te vinden. Je vertelt ons wat je merkt, wij zoeken uit wat er aan de hand is. Moet Shopify of de maker van een app iets herstellen? Dan zetten wij de melding door en volgen die op."
      H.p $ H.preEscapedToHtml ("Je thema en apps krijgen regelmatig updates. Daarom controleren we zelf of je webshop daarna nog werkt. Bij uitgebreide ondersteuning doen we dat elke maand. Bij ondersteuning van &euro;99 doen we dat elk kwartaal. We lopen dan je homepage, een categoriepagina, een productpagina, de winkelwagen en de checkout tot aan de betaling door. Dit is een korte controle van de belangrijkste stappen, geen test van elke app of pagina." :: Text)
      H.p "Zien we dat er iets kapot is, dan laten we je dat weten. Is je webshop daardoor onbereikbaar, of kunnen klanten niet bestellen of betalen? Dan pakken we het op als urgente storing. Anders telt het herstel als een kleine aanpassing."
      H.p "Zit er een beveiligingsprobleem in een thema dat wij hebben geleverd? Dan lossen we dat in beide pakketten op. Daar zit geen maximum aan. Het gaat ook niet af van je vragen of kleine aanpassingen. Een preventieve beveiligingsaudit van je webshop hoort er niet bij."
      H.p "Komt er nieuwe wetgeving waardoor je webshop technisch aangepast moet worden? Dan voeren wij die aanpassingen in beide pakketten zonder extra kosten uit. Ze gaan ook niet af van je kleine aanpassingen. Welke regels voor jouw winkel gelden, bepaal je zelf of met je adviseur: wij geven geen juridisch advies."
      H.p "We kunnen niet beloven hoe snel een storing is opgelost, bijvoorbeeld als Shopify of een andere leverancier iets moet herstellen. Wel spreken we af wanneer je een eerste inhoudelijk antwoord van ons krijgt."

    H.section ! A.class_ "audit" $ do
      H.h2 "Ondersteuning van €99: antwoord en plan binnen vijf werkuren"
      H.p "Bij een urgente storing krijg je binnen vijf werkuren per e-mail onze beoordeling van het probleem en een concreet plan. We tellen alleen maandag tot en met vrijdag, van 09:00 tot 17:00 Nederlandse tijd, behalve op Nederlandse feestdagen. Buiten die uren staat de klok stil en hervatten we de storingshulp op de volgende werkdag."
      H.p "Een melding op vrijdag om 16:00 krijgt dus uiterlijk maandag om 13:00 antwoord, als maandag geen feestdag is. Dit is een afspraak over ons antwoord, niet over wanneer de storing opgelost is."

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
      H.p "Een nieuwe functie of koppeling op maat laten bouwen, je winkel opnieuw ontwerpen, veel producten tegelijk aanpassen of een uitgebreide training volgen valt buiten het pakket. Daarvoor spreken we vooraf een vaste prijs af."
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
      H.li "Twee hulpvragen per maand"
      H.li "Eén kleine aanpassing per maand, zoals een banner vervangen"
      H.li "Hulp bij urgente storingen van maandag tot en met vrijdag, van 09:00 tot 17:00, behalve op Nederlandse feestdagen"
      H.li "Bij een urgente storing binnen vijf werkuren onze beoordeling en een concreet plan per e-mail"
      H.li "Geen maximum aan storingsmeldingen"
      H.li "Elk kwartaal een controle of je webshop na updates nog werkt"
      H.li "Beveiligingsproblemen in een thema dat wij leverden lossen we op, zonder maximum"
      H.li "Technische aanpassingen door nieuwe wetgeving zonder extra kosten"

uitgebreidKaart :: Html
uitgebreidKaart =
  H.div ! A.class_ "prijs-kaart" $ do
    H.p ! A.class_ "wat" $ "Uitgebreide ondersteuning"
    H.p ! A.class_ "prijs" $ do
      H.preEscapedToHtml ("&euro;249 " :: Text)
      H.small "per maand"
    H.p ! A.class_ "inbegrepen" $ "Meer ruimte voor vragen en aanpassingen, met hulp bij storingen ook in de avond en het weekend."
    H.ul $ do
      H.li "Vijf hulpvragen per maand"
      H.li "Drie kleine aanpassingen per maand"
      H.li "Hulp bij urgente storingen iedere dag van 07:00 tot 22:45, ook in het weekend en op feestdagen"
      H.li "Bij een urgente storing binnen drie uur onze beoordeling en een concreet plan per e-mail, volgens de tijden hieronder"
      H.li "Geen maximum aan storingsmeldingen"
      H.li "Elke maand een controle of je webshop na updates nog werkt"
      H.li "Beveiligingsproblemen in een thema dat wij leverden lossen we op, zonder maximum"
      H.li "Technische aanpassingen door nieuwe wetgeving zonder extra kosten"

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
