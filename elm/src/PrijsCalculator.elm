port module PrijsCalculator exposing
    ( BronPlatform(..)
    , DoelPlatform(..)
    , ItemStaffel
    , Model
    , Msg(..)
    , PrijsRegel
    , Regelniveau(..)
    , StaffelSegment
    , ThemaKeuze(..)
    , bronKeuzes
    , bronNaarWaarde
    , bronOmschrijving
    , doelKeuzes
    , doelNaarWaarde
    , doelOmschrijving
    , formulierGeldig
    , initieelModel
    , invoerEventParams
    , isGroteCatalogus
    , itemStaffelSegmenten
    , offerteEventParams
    , orderhistorieStaffel
    , prijsRegels
    , productStaffelSegmenten
    , leesBron
    , leesDoel
    , main
    , totaalCenten
    , update
    )

{-| Interactieve prijsindicatie voor een webshop-migratie op webwinkelverhuis.nl.

De prijslogica is een 1-op-1 kopie van de standaard prijslijst (jappiesoft
strategy/standaard-prijslijst.org) en de tabel op /prijzen: een vaste basis
van €999 voor de overhead van elke migratie, daarbovenop elke
productvertaling (producten maal talen) vanaf de eerste in een aflopende
staffel per vijfhonderd (€0,20, €0,15, €0,15, €0,10, daarna €0,10), plus
€250 configuratie per extra taal. De modules die met de shop meegroeien
(klantaccounts, orderhistorie, nieuwsbrief, voorraad) kosten €100 met de
eerste 1.000 items inbegrepen en daarboven een aflopend tarief per item;
de bezoeker vult daarvoor een schatting van het aantal in. De overige
modules en diensten (thema, reviews, domeinverhuizing, e-mail-setup,
verzendkoppeling, B2B, kassa, cursus) zijn vaste bedragen. Alle bedragen worden
intern in hele centen gerekend zodat er geen afrondingsfouten op de komma
ontstaan; pas bij het tonen zetten we centen om naar euro's.

De vragen zijn met opzet in gewone taal gesteld, voor een webshop-eigenaar
zonder technische kennis: geen "registrar" of "MX-records", maar "je domeinnaam"
en "je e-mailadressen".

Deze indicatie is bewust geen offerte: alleen een offerte legt de prijs vast.
Dat staat ook onder de uitkomst, zodat de bezoeker weet dat dit een richtprijs
is en niet een toezegging.
-}

import Browser
import Html exposing (Html, a, button, details, div, fieldset, form, h3, input, label, legend, li, option, p, select, span, strong, summary, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode as Encode
import Url


-- Decision: analytics loopt via een uitgaande Elm-port naar JS, dat het aan
-- gtag (GA4) doorgeeft. Gekozen boven (a) JS dat in de Elm-DOM/-state graait
-- (breekt de Elm-garanties) en (b) niets meten. De port houdt Elm puur; JS raakt
-- de state niet aan. De bedragwaarde sturen we als GA4's gereserveerde
-- "value"/"currency" (native herkend), niet als eigen param; "bron"/"doel" zijn
-- custom params die je als GA4 custom dimension moet registreren om erop uit te
-- splitsen (registratie is niet retroactief).


{-| Stuurt een analytics-event naar JavaScript, waar de pagina het aan Google
Analytics (gtag) doorgeeft. De waarde is een object {name, params}. -}
port analyticsEvent : Encode.Value -> Cmd msg


{-| Bouw een gtag-event met een naam en losse parameters. -}
gaEvent : String -> List ( String, Encode.Value ) -> Cmd msg
gaEvent naam params =
    analyticsEvent
        (Encode.object
            [ ( "name", Encode.string naam )
            , ( "params", Encode.object params )
            ]
        )



-- CONSTANTEN (centen), gelijk aan standaard-prijslijst.org en /prijzen


-- Decision: basis naar 99900 zonder inbegrepen producten (besluit
-- Jappie 19 sep 2026, jappiesoft pricing-business-model.org, sectie
-- "typist999"). De meting van 19 sep: op 1499 (sinds 4 sep) kwam de
-- enige offerte van een middelgrote CCV-shop, en van 138 mails aan
-- MijnWebwinkel-shops kwam nul reactie waar CCV er vier gaf; de
-- kleine MWW-shop klikt, ziet de prijs en loopt weg. De 999 dekt de
-- afsluit- en communicatie-overhead die elke migratie heeft; de
-- omvang van de shop zit vanaf de eerste vertaling in de staffel en
-- in de meegroeiende modules. Alternatieven overwogen: 1.299 met 250
-- inbegrepen (de oktoberafspraak van 4 sep; helpt geen van beide
-- segmenten) en 999 met 500 inbegrepen (de 9-sep-offerteversie;
-- geeft de middenmaat een korting die niemand vroeg). Eerdere
-- stappen: 1999 (aug, nul conversies), 1499 met 500 (4 sep).


basisMigratieCenten : Int
basisMigratieCenten =
    99900


inbegrepenProducten : Int
inbegrepenProducten =
    0


-- Decision: de productstaffel is degressief (besluit Jappie 1 sep 2026,
-- na plotterenzo.nl en het 50.000-producten-anker van 15 aug): grote
-- catalogi kosten het migratieprogramma nauwelijks extra werk, dus een
-- vlak tarief prijst juist de goedkoopste meerschaal het hardst en
-- jaagt grote shops weg met bedragen die niets met de kostprijs te
-- maken hebben. Sinds 19 sep 2026 telt elke vertaling vanaf de eerste
-- mee, in treden van vijfhonderd: 20, 15, 15, 10 cent en daarna de
-- bodem van 10 cent. Het ijkpunt is de winkelier die het met de hand
-- laat doen (een typist haalt twaalf producten per uur, wat bij de
-- goedkoopste tarieven op zo'n 30 cent per product uitkomt); de
-- eerste trede is daar bewust onder gelegd omdat het productaantal de
-- catalogus meet en niet de omzet (een kleine zaak kan een grote
-- catalogus hebben), het gewicht ligt bij de meegroeiende modules.
-- Alternatief overwogen: een tweede maatwerkgrens op 5.000 euro
-- richtprijs (staat gebouwd op de geparkeerde branch
-- calculator-richtprijs-grens); afgewezen omdat de degressieve staffel
-- die bedragen gewoon eerlijk toonbaar maakt en de bestaande
-- grote-catalogus-grens de echt grote gevallen al naar het gesprek
-- stuurt.


staffelTredenCenten : List Int
staffelTredenCenten =
    [ 20, 15, 15, 10 ]


staffelBodemCenten : Int
staffelBodemCenten =
    10


tredeGrootte : Int
tredeGrootte =
    500


perTaalConfiguratieCenten : Int
perTaalConfiguratieCenten =
    25000


themaOverzettenCenten : Int
themaOverzettenCenten =
    74900


-- Decision: klantaccounts, orderhistorie, nieuwsbrief en voorraad zijn
-- sinds 19 sep 2026 geen vaste 250 meer maar een staffel per item
-- (besluit Jappie, jappiesoft pricing-business-model.org, sectie
-- "modules die met de shop meegroeien"). Orders en klanten meten hoe
-- groot de zaak is, beter dan het productaantal; wie er veel heeft,
-- heeft er ook het meeste aan en kan het dragen. Vast deel van €100
-- voor het gedoe om bij de data te komen (beheer-toegang, secret) met
-- de eerste 1.000 items inbegrepen, daarboven per item, na een trede
-- een lager tarief. Geen ander vast deel: de basis dekt de overhead en
-- de module-tooling is eenmalig betaald. Alternatief overwogen: de
-- staffel op orders in de basisprijs stoppen; afgewezen omdat orders
-- en accounts achter het beheer zitten en dus niet vooraf telbaar zijn,
-- terwijl modules toch pas na opgave van de winkelier geoffreerd
-- worden. De vier blijven aparte regels (besluit 25 jul 2026).


{-| Het tarief per item boven de inbegrepen items van een meegroeiende
module: eerst 'tariefCenten' per item, na 'tredeOmvang' items het lagere
'tariefDaarbovenCenten'. -}
type alias ItemStaffel =
    { tariefCenten : Int
    , tredeOmvang : Int
    , tariefDaarbovenCenten : Int
    }


moduleVastCenten : Int
moduleVastCenten =
    10000


moduleInbegrepenItems : Int
moduleInbegrepenItems =
    1000


orderhistorieStaffel : ItemStaffel
orderhistorieStaffel =
    { tariefCenten = 8, tredeOmvang = 10000, tariefDaarbovenCenten = 4 }


klantaccountsStaffel : ItemStaffel
klantaccountsStaffel =
    { tariefCenten = 15, tredeOmvang = 5000, tariefDaarbovenCenten = 8 }


nieuwsbriefStaffel : ItemStaffel
nieuwsbriefStaffel =
    { tariefCenten = 5, tredeOmvang = 5000, tariefDaarbovenCenten = 3 }


voorraadStaffel : ItemStaffel
voorraadStaffel =
    { tariefCenten = 5, tredeOmvang = 5000, tariefDaarbovenCenten = 3 }


reviewsCenten : Int
reviewsCenten =
    15000


verzendkoppelingCenten : Int
verzendkoppelingCenten =
    15000


domeinverhuizingCenten : Int
domeinverhuizingCenten =
    25000


emailSetupCenten : Int
emailSetupCenten =
    15000


b2bKanaalCenten : Int
b2bKanaalCenten =
    75000


pointOfSaleCenten : Int
pointOfSaleCenten =
    75000


cursusCenten : Int
cursusCenten =
    30000



-- MODEL


{-| Waar draait de webshop nu? MijnWebwinkel, CCV en Lightspeed kennen we en
prijzen we gelijk (het extra werk zit alleen in de eenmalige import-laag per
platform, niet per klant). Een onbekend platform prijzen we op aanvraag omdat de
import dan per geval verschilt. De keuze is verder informatief voor de offerte.
-}
type BronPlatform
    = BronMijnwebwinkel
    | BronCcvShop
    | BronLightspeed
    | BronWoocommerce
    | BronAnders


{-| Waar migreren we naartoe? Beïnvloedt de prijs niet, maar is nuttig voor de
offerte. Shopify is onze standaard; WooCommerce doen we ook. "Weet ik nog niet"
is een volwaardige keuze: welk platform past hangt af van de situatie van de
shop, en dat adviseren we in het gratis gesprek. -}
type DoelPlatform
    = DoelShopify
    | DoelWoocommerce
    | DoelAnders
    | DoelWeetNiet


{-| Hoe moet de nieuwe shop eruitzien? Een net standaard-uiterlijk zit in de
basis; de huidige look exact nabouwen is los werk; een volledig nieuw ontwerp
prijzen we op aanvraag.
-}
type ThemaKeuze
    = ThemaStandaard
    | ThemaOverzetten
    | ThemaNieuw


type alias Model =
    { productenInvoer : String
    , talenInvoer : String
    , bron : BronPlatform
    , doel : DoelPlatform
    , thema : ThemaKeuze
    , klantaccounts : Bool
    , klantaccountsInvoer : String
    , orderhistorie : Bool
    , bestellingenInvoer : String
    , nieuwsbrief : Bool
    , abonneesInvoer : String
    , voorraad : Bool
    , reviews : Bool
    , domeinBijMijnwebwinkel : Bool
    , emailBijMijnwebwinkel : Bool
    , verzendkoppeling : Bool
    , b2bKanaal : Bool
    , pointOfSale : Bool
    , cursus : Bool
    , naam : String
    , webshopDomein : String
    , emailInvoer : String
    , offertePoging : Bool
    , analyticsEngaged : Bool
    , groteCatalogusGemeld : Bool
    }


initieelModel : Model
initieelModel =
    { productenInvoer = "500"
    , talenInvoer = "1"
    , bron = BronMijnwebwinkel
    , doel = DoelShopify
    , thema = ThemaStandaard
    , klantaccounts = False
    , klantaccountsInvoer = ""
    , orderhistorie = False
    , bestellingenInvoer = ""
    , nieuwsbrief = False
    , abonneesInvoer = ""
    , voorraad = False
    , reviews = False
    , domeinBijMijnwebwinkel = False
    , emailBijMijnwebwinkel = False
    , verzendkoppeling = False
    , b2bKanaal = False
    , pointOfSale = False
    , cursus = False
    , naam = ""
    , webshopDomein = ""
    , emailInvoer = ""
    , offertePoging = False
    , analyticsEngaged = False
    , groteCatalogusGemeld = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initieelModel, Cmd.none )



-- UPDATE


type Msg
    = ProductenGewijzigd String
    | TalenGewijzigd String
    | BronGewijzigd String
    | DoelGewijzigd String
    | ThemaGewijzigd String
    | KlantaccountsGewijzigd Bool
    | KlantaccountsAantalGewijzigd String
    | OrderhistorieGewijzigd Bool
    | BestellingenGewijzigd String
    | NieuwsbriefGewijzigd Bool
    | AbonneesGewijzigd String
    | VoorraadGewijzigd Bool
    | ReviewsGewijzigd Bool
    | DomeinGewijzigd Bool
    | EmailGewijzigd Bool
    | VerzendkoppelingGewijzigd Bool
    | B2bKanaalGewijzigd Bool
    | PointOfSaleGewijzigd Bool
    | CursusGewijzigd Bool
    | NaamGewijzigd String
    | WebshopDomeinGewijzigd String
    | EmailInvoerGewijzigd String
    | OfferteGepoogd
    | OfferteVerzonden
    | GroteCatalogusContact


leesBron : String -> BronPlatform
leesBron waarde =
    if waarde == "ccv" then
        BronCcvShop

    else if waarde == "lightspeed" then
        BronLightspeed

    else if waarde == "woocommerce" then
        BronWoocommerce

    else if waarde == "anders" then
        BronAnders

    else
        BronMijnwebwinkel


leesDoel : String -> DoelPlatform
leesDoel waarde =
    if waarde == "woocommerce" then
        DoelWoocommerce

    else if waarde == "anders" then
        DoelAnders

    else if waarde == "weetniet" then
        DoelWeetNiet

    else
        DoelShopify


leesThema : String -> ThemaKeuze
leesThema waarde =
    if waarde == "overzetten" then
        ThemaOverzetten

    else if waarde == "nieuw" then
        ThemaNieuw

    else
        ThemaStandaard


{-| Inverse van leesBron: de keuzewaarde die bij een bronplatform hoort, zodat
de juiste optie in de dropdown geselecteerd staat. -}
bronNaarWaarde : BronPlatform -> String
bronNaarWaarde bron =
    case bron of
        BronMijnwebwinkel ->
            "mijnwebwinkel"

        BronCcvShop ->
            "ccv"

        BronLightspeed ->
            "lightspeed"

        BronWoocommerce ->
            "woocommerce"

        BronAnders ->
            "anders"


doelNaarWaarde : DoelPlatform -> String
doelNaarWaarde doel =
    case doel of
        DoelShopify ->
            "shopify"

        DoelWoocommerce ->
            "woocommerce"

        DoelAnders ->
            "anders"

        DoelWeetNiet ->
            "weetniet"


doelOmschrijving : DoelPlatform -> String
doelOmschrijving doel =
    case doel of
        DoelShopify ->
            "Shopify"

        DoelWoocommerce ->
            "WooCommerce"

        DoelAnders ->
            "Een ander platform"

        DoelWeetNiet ->
            "Weet ik nog niet / ik wil advies"


{-| Inverse van leesThema: de keuzewaarde die bij een themakeuze hoort. -}
themaNaarWaarde : ThemaKeuze -> String
themaNaarWaarde thema =
    case thema of
        ThemaStandaard ->
            "standaard"

        ThemaOverzetten ->
            "overzetten"

        ThemaNieuw ->
            "nieuw"


{-| Leesbare omschrijving van een bronplatform, gebruikt in de dropdown en in
de vooringevulde offerte-mail. -}
bronOmschrijving : BronPlatform -> String
bronOmschrijving bron =
    case bron of
        BronMijnwebwinkel ->
            "MijnWebwinkel"

        BronCcvShop ->
            "CCV Shop"

        BronLightspeed ->
            "Lightspeed"

        BronWoocommerce ->
            "WooCommerce"

        BronAnders ->
            "Een ander systeem / weet ik niet"


{-| Leesbare omschrijving van een themakeuze, gebruikt in de dropdown en in de
vooringevulde offerte-mail. -}
themaOmschrijving : ThemaKeuze -> String
themaOmschrijving thema =
    case thema of
        ThemaStandaard ->
            "Zelf inrichten (standaard thema)"

        ThemaOverzetten ->
            "Uitstraling overzetten"

        ThemaNieuw ->
            "Nieuw ontwerp"


{-| Werk het model bij en stuur eenmalig een "calculator_engaged"-event zodra de
bezoeker voor het eerst iets in de rekenhulp verandert. -}
markeerEngagement : Model -> ( Model, Cmd Msg )
markeerEngagement model =
    if model.analyticsEngaged then
        ( model, Cmd.none )

    else
        ( { model | analyticsEngaged = True }, gaEvent "calculator_engaged" [] )


{-| De parameters van het rollende "calculator_invoer"-event: de actuele
invoer en richtprijs, zodat een sessie in GA4 laat zien wat een bezoeker
invulde en waar die op uitkwam (issue megavid#244). Pure functie zodat de
test kan vaststellen wat er meegaat, en vooral wat er nooit meegaat: naam,
webshopdomein en e-mailadres blijven eruit, die maken de meting herleidbaar
tot een persoon en dan vervalt de grond onder de bannerloze GA4-opzet (zie
de Decision in shake/WebwinkelTemplates.hs). -}
invoerEventParams : Model -> List ( String, Encode.Value )
invoerEventParams model =
    [ ( "value", Encode.int (totaalCenten model // 100) )
    , ( "currency", Encode.string "EUR" )
    , ( "producten", Encode.int (aantalProducten model) )
    , ( "talen", Encode.int (aantalTalen model) )
    , ( "bron", Encode.string (bronOmschrijving model.bron) )
    , ( "doel", Encode.string (doelOmschrijving model.doel) )
    ]


{-| Rollend event bij elke prijsbepalende wijziging, bovenop de eenmalige
engagement-markering. GA4's sessielimieten kunnen dit makkelijk hebben: een
bezoeker die uitgebreid speelt komt op tientallen events, niet honderden. -}
markeerInvoer : Model -> ( Model, Cmd Msg )
markeerInvoer model =
    metRollendeInvoer (markeerEngagement model)


metRollendeInvoer : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
metRollendeInvoer ( model, cmd ) =
    ( model, Cmd.batch [ cmd, gaEvent "calculator_invoer" (invoerEventParams model) ] )


{-| Conversie-event met de richtprijs en de gekozen platforms. "value" en
"currency" zijn GA4's gereserveerde geldparameters, dus de waarde wordt native
herkend; "bron" en "doel" zijn custom parameters die in GA4 als custom dimension
geregistreerd moeten worden voordat je erop kunt uitsplitsen. -}
offerteAangevraagdEvent : Model -> Cmd Msg
offerteAangevraagdEvent model =
    gaEvent "offerte_aangevraagd" (offerteEventParams model)


{-| De parameters van het offerte-event, als pure functie zodat dezelfde
herleidbaarheidstest als bij invoerEventParams geldt: naam, webshopdomein en
e-mailadres uit het formulier gaan nooit mee naar GA4 (zie de Decision in
shake/WebwinkelTemplates.hs). -}
offerteEventParams : Model -> List ( String, Encode.Value )
offerteEventParams model =
    [ ( "value", Encode.int (totaalCenten model // 100) )
    , ( "currency", Encode.string "EUR" )
    , ( "bron", Encode.string (bronOmschrijving model.bron) )
    , ( "doel", Encode.string (doelOmschrijving model.doel) )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProductenGewijzigd waarde ->
            meldGroteCatalogus (markeerInvoer { model | productenInvoer = waarde })

        TalenGewijzigd waarde ->
            meldGroteCatalogus (markeerInvoer { model | talenInvoer = waarde })

        BronGewijzigd waarde ->
            markeerInvoer { model | bron = leesBron waarde }

        DoelGewijzigd waarde ->
            markeerInvoer { model | doel = leesDoel waarde }

        ThemaGewijzigd waarde ->
            markeerInvoer { model | thema = leesThema waarde }

        KlantaccountsGewijzigd aan ->
            markeerInvoer { model | klantaccounts = aan }

        KlantaccountsAantalGewijzigd waarde ->
            markeerInvoer { model | klantaccountsInvoer = waarde }

        OrderhistorieGewijzigd aan ->
            markeerInvoer { model | orderhistorie = aan }

        BestellingenGewijzigd waarde ->
            markeerInvoer { model | bestellingenInvoer = waarde }

        NieuwsbriefGewijzigd aan ->
            markeerInvoer { model | nieuwsbrief = aan }

        AbonneesGewijzigd waarde ->
            markeerInvoer { model | abonneesInvoer = waarde }

        VoorraadGewijzigd aan ->
            markeerInvoer { model | voorraad = aan }

        ReviewsGewijzigd aan ->
            markeerInvoer { model | reviews = aan }

        CursusGewijzigd aan ->
            markeerInvoer { model | cursus = aan }

        DomeinGewijzigd aan ->
            markeerInvoer { model | domeinBijMijnwebwinkel = aan }

        EmailGewijzigd aan ->
            markeerInvoer { model | emailBijMijnwebwinkel = aan }

        VerzendkoppelingGewijzigd aan ->
            markeerInvoer { model | verzendkoppeling = aan }

        B2bKanaalGewijzigd aan ->
            markeerInvoer { model | b2bKanaal = aan }

        PointOfSaleGewijzigd aan ->
            markeerInvoer { model | pointOfSale = aan }

        NaamGewijzigd waarde ->
            markeerEngagement { model | naam = waarde }

        WebshopDomeinGewijzigd waarde ->
            markeerEngagement { model | webshopDomein = waarde }

        EmailInvoerGewijzigd waarde ->
            markeerEngagement { model | emailInvoer = waarde }

        OfferteGepoogd ->
            ( { model | offertePoging = True }, gaEvent "offerte_geblokkeerd" [] )

        OfferteVerzonden ->
            ( model, offerteAangevraagdEvent model )

        GroteCatalogusContact ->
            ( model, gaEvent "grote_catalogus_contact" [] )



-- PRIJSBEREKENING


{-| Parse een getalinvoer naar een niet-negatief geheel getal. Lege of
onleesbare invoer telt als 0, zodat de calculator nooit crasht op typwerk;
de minimale zinvolle waarde (1 taal) dwingen we af bij het gebruik.
-}
leesGetal : String -> Int
leesGetal invoer =
    case String.toInt (String.trim invoer) of
        Just getal ->
            if getal < 0 then
                0

            else
                getal

        Nothing ->
            0


aantalProducten : Model -> Int
aantalProducten model =
    leesGetal model.productenInvoer


aantalTalen : Model -> Int
aantalTalen model =
    Basics.max 1 (leesGetal model.talenInvoer)


extraTalen : Model -> Int
extraTalen model =
    aantalTalen model - 1


-- Decision: productvertalingen tellen samen tegen de inbegrepen
-- producten van de basismigratie (besluit Jappie 2026-08-08, n.a.v. de
-- bybjor-offerte, destijds met 1.000 inbegrepen; sinds 4 sep 2026 zijn
-- dat er 500). Elk product telt per taal één keer mee: 160 producten
-- in 3 talen zijn 480 productvertalingen en passen dus in de basisruimte,
-- terwijl het oude model elke extra taal over de hele catalogus liet
-- betalen zonder die ruimte. Alternatief was de oude
-- twee-staffels-opzet houden; afgewezen omdat die kleine meertalige
-- shops liet betalen voor ruimte die ze al gekocht hadden.


productVertalingen : Model -> Int
productVertalingen model =
    aantalProducten model * aantalTalen model


{-| Vanaf dit aantal productvertalingen (producten maal talen) toont de
rekenhulp een neem-contact-melding in plaats van een richtprijs. De grens
stond vanaf 15 aug 2026 op 10.000, omdat het toenmalige vlakke tarief daar
stil bedragen van ruim veertienduizend euro toonde (het
50.000-producten-anker). Met de degressieve staffel is datzelfde anker
7.199 euro en gewoon eerlijk toonbaar, dus de grens is per 1 sep 2026
verruimd naar 100.000 (besluit Jappie): elke realistische shop ziet nu
direct zijn prijs, en de melding blijft alleen over als vangnet tegen
absurde of vertikte invoer, waar een kaal bedrag van tienduizenden euro's
niemand helpt. -}
groteCatalogusGrens : Int
groteCatalogusGrens =
    100000


isGroteCatalogus : Model -> Bool
isGroteCatalogus model =
    productVertalingen model >= groteCatalogusGrens


{-| Vuurt eenmalig het event "calculator_grote_catalogus" zodra de invoer de
grens passeert, zodat GA4 telt hoe vaak grote catalogi de rekenhulp raken:
precies het verkeer dat anders stil zou wegklikken. -}
meldGroteCatalogus : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
meldGroteCatalogus ( model, cmd ) =
    if isGroteCatalogus model && not model.groteCatalogusGemeld then
        ( { model | groteCatalogusGemeld = True }
        , Cmd.batch
            [ cmd
            , gaEvent "calculator_grote_catalogus"
                [ ( "producten", Encode.int (aantalProducten model) )
                , ( "talen", Encode.int (aantalTalen model) )
                ]
            ]
        )

    else
        ( model, cmd )


extraProductVertalingen : Model -> Int
extraProductVertalingen model =
    Basics.max 0 (productVertalingen model - inbegrepenProducten)


extraProductVertalingenCenten : Model -> Int
extraProductVertalingenCenten model =
    segmentenCenten (productStaffelSegmenten (productVertalingen model))



-- STAFFELSEGMENTEN


{-| Eén stuk van een staffel zoals de bezoeker hem te zien krijgt: de
items 'van' tot en met 'tot' (1-gebaseerd, over de hele telling) tegen
één tarief. De uitsplitsing op het scherm toont per segment een regel, en
de totalen zijn de som van dezelfde segmenten, zodat regels en totaal
nooit uit elkaar kunnen lopen. -}
type alias StaffelSegment =
    { van : Int
    , tot : Int
    , tariefCenten : Int
    }


{-| Verdeel 'aantal' items, te beginnen bij itemnummer 'start', over de
treden (omvang, tarief) en daarna de bodem. Lege treden en een aantal van
nul geven geen segmenten. -}
segmentenVanTreden : Int -> Int -> List ( Int, Int ) -> Int -> List StaffelSegment
segmentenVanTreden start aantal treden bodemCenten =
    if aantal <= 0 then
        []

    else
        case treden of
            [] ->
                [ { van = start, tot = start + aantal - 1, tariefCenten = bodemCenten } ]

            ( omvang, tarief ) :: rest ->
                let
                    inDezeTrede =
                        Basics.min aantal omvang
                in
                { van = start, tot = start + inDezeTrede - 1, tariefCenten = tarief }
                    :: segmentenVanTreden (start + inDezeTrede) (aantal - inDezeTrede) rest bodemCenten


{-| Twee opeenvolgende treden met hetzelfde tarief leest de bezoeker als
één stap ("501 t/m 1.500 om 15 cent"), dus die voegen we samen. -}
voegGelijkeSegmentenSamen : List StaffelSegment -> List StaffelSegment
voegGelijkeSegmentenSamen segmenten =
    case segmenten of
        eerste :: tweede :: rest ->
            if eerste.tariefCenten == tweede.tariefCenten then
                voegGelijkeSegmentenSamen ({ eerste | tot = tweede.tot } :: rest)

            else
                eerste :: voegGelijkeSegmentenSamen (tweede :: rest)

        _ ->
            segmenten


segmentAantal : StaffelSegment -> Int
segmentAantal segment =
    segment.tot - segment.van + 1


segmentCenten : StaffelSegment -> Int
segmentCenten segment =
    segmentAantal segment * segment.tariefCenten


segmentenCenten : List StaffelSegment -> Int
segmentenCenten segmenten =
    List.sum (List.map segmentCenten segmenten)


{-| De productstaffel voor een aantal productvertalingen: treden van
'tredeGrootte' tegen 'staffelTredenCenten', daarna de bodem, met gelijke
buurtreden samengevoegd. -}
productStaffelSegmenten : Int -> List StaffelSegment
productStaffelSegmenten vertalingen =
    voegGelijkeSegmentenSamen
        (segmentenVanTreden
            (inbegrepenProducten + 1)
            (Basics.max 0 (vertalingen - inbegrepenProducten))
            (List.map (\tarief -> ( tredeGrootte, tarief )) staffelTredenCenten)
            staffelBodemCenten
        )


{-| De staffel van een meegroeiende module boven de inbegrepen items:
eerst 'tredeOmvang' items tegen het eerste tarief, daarna het lagere. -}
itemStaffelSegmenten : ItemStaffel -> Int -> List StaffelSegment
itemStaffelSegmenten staffel aantal =
    voegGelijkeSegmentenSamen
        (segmentenVanTreden
            (moduleInbegrepenItems + 1)
            (Basics.max 0 (aantal - moduleInbegrepenItems))
            [ ( staffel.tredeOmvang, staffel.tariefCenten ) ]
            staffel.tariefDaarbovenCenten
        )


extraTaalConfiguratieCenten : Model -> Int
extraTaalConfiguratieCenten model =
    extraTalen model * perTaalConfiguratieCenten


themaCenten : Model -> Int
themaCenten model =
    case model.thema of
        ThemaStandaard ->
            0

        ThemaOverzetten ->
            themaOverzettenCenten

        ThemaNieuw ->
            0


{-| Platforms die het domein en de e-mail vaak zelf bundelen, dus waar een
registrar-transfer nodig kán zijn. Zelf-gehoste platforms zoals WooCommerce
niet: daar staat het domein al bij een gewone registrar (alleen DNS-cutover).
Een onbekend platform laten we ook weg; dat vragen we in de discovery uit. -}
bundeltDomeinEnEmail : BronPlatform -> Bool
bundeltDomeinEnEmail bron =
    case bron of
        BronMijnwebwinkel ->
            True

        BronCcvShop ->
            True

        BronLightspeed ->
            True

        BronWoocommerce ->
            False

        BronAnders ->
            False


{-| Domeinverhuizing telt alleen als het bronplatform het domein bundelt én de
bezoeker aangeeft dat het domein daar staat. -}
domeinGekozen : Model -> Bool
domeinGekozen model =
    bundeltDomeinEnEmail model.bron && model.domeinBijMijnwebwinkel


emailGekozen : Model -> Bool
emailGekozen model =
    bundeltDomeinEnEmail model.bron && model.emailBijMijnwebwinkel


{-| De prijs van een meegroeiende module bij een aantal items: het vaste
deel dekt de eerste 'moduleInbegrepenItems', daarboven het tarief van de
staffel, na de trede het lagere tarief. Een lege of onleesbare invoer telt
als 0 items en geeft dus het vaste deel; dat is de laagste prijs die de
module kan hebben, geen verzonnen bedrag. -}
itemStaffelCenten : ItemStaffel -> Int -> Int
itemStaffelCenten staffel aantal =
    moduleVastCenten + segmentenCenten (itemStaffelSegmenten staffel aantal)


aantalKlantaccounts : Model -> Int
aantalKlantaccounts model =
    leesGetal model.klantaccountsInvoer


aantalBestellingen : Model -> Int
aantalBestellingen model =
    leesGetal model.bestellingenInvoer


aantalAbonnees : Model -> Int
aantalAbonnees model =
    leesGetal model.abonneesInvoer


klantaccountsCenten : Model -> Int
klantaccountsCenten model =
    itemStaffelCenten klantaccountsStaffel (aantalKlantaccounts model)


orderhistorieCenten : Model -> Int
orderhistorieCenten model =
    itemStaffelCenten orderhistorieStaffel (aantalBestellingen model)


nieuwsbriefCenten : Model -> Int
nieuwsbriefCenten model =
    itemStaffelCenten nieuwsbriefStaffel (aantalAbonnees model)


{-| Voorraad gaat per product, niet per vertaling: de voorraad van een
product is in elke taal dezelfde. -}
voorraadCenten : Model -> Int
voorraadCenten model =
    itemStaffelCenten voorraadStaffel (aantalProducten model)


{-| Tel een module alleen mee als de bezoeker hem heeft aangevinkt. -}
indienAan : Bool -> Int -> Int
indienAan aan centen =
    if aan then
        centen

    else
        0


totaalCenten : Model -> Int
totaalCenten model =
    basisMigratieCenten
        + extraProductVertalingenCenten model
        + extraTaalConfiguratieCenten model
        + themaCenten model
        + indienAan model.klantaccounts (klantaccountsCenten model)
        + indienAan model.orderhistorie (orderhistorieCenten model)
        + indienAan model.nieuwsbrief (nieuwsbriefCenten model)
        + indienAan model.voorraad (voorraadCenten model)
        + indienAan model.reviews reviewsCenten
        + indienAan (domeinGekozen model) domeinverhuizingCenten
        + indienAan (emailGekozen model) emailSetupCenten
        + indienAan model.verzendkoppeling verzendkoppelingCenten
        + indienAan model.b2bKanaal b2bKanaalCenten
        + indienAan model.pointOfSale pointOfSaleCenten
        + indienAan model.cursus cursusCenten



-- WEERGAVE VAN BEDRAGEN


pad2 : Int -> String
pad2 getal =
    if getal < 10 then
        "0" ++ String.fromInt getal

    else
        String.fromInt getal


{-| Voeg Nederlandse duizendtal-punten toe aan de cijfers van een geheel
euro-bedrag: "4049" wordt "4.049".
-}
voegDuizendtallenToe : String -> String
voegDuizendtallenToe cijfers =
    if String.length cijfers <= 3 then
        cijfers

    else
        voegDuizendtallenToe (String.dropRight 3 cijfers) ++ "." ++ String.right 3 cijfers


formatteerEuro : Int -> String
formatteerEuro centen =
    let
        euros =
            centen // 100

        restCenten =
            modBy 100 centen
    in
    "\u{20AC}" ++ voegDuizendtallenToe (String.fromInt euros) ++ "," ++ pad2 restCenten



-- INVOERVELDEN


getalVeld : String -> String -> String -> (String -> Msg) -> Html Msg
getalVeld veldLabel waarde tekstNaVeld naarBericht =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text veldLabel ]
        , input
            [ Attr.type_ "number"
            , Attr.min "0"
            , Attr.value waarde
            , onInput naarBericht
            ]
            []
        , span [ Attr.class "calc-hint" ] [ text tekstNaVeld ]
        ]


isLeeg : String -> Bool
isLeeg waarde =
    String.trim waarde == ""


{-| Een verplicht tekstveld: bij een verzendpoging met lege waarde krijgt het
een foutrand en een korte melding, zodat de bezoeker ziet wat nog moet. -}
verplichtVeld : Bool -> String -> String -> String -> (String -> Msg) -> Html Msg
verplichtVeld poging veldLabel plaatshouder waarde naarBericht =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text veldLabel ]
        , input
            ([ Attr.type_ "text"
             , Attr.value waarde
             , Attr.placeholder plaatshouder
             , onInput naarBericht
             ]
                ++ foutRandKlasse poging waarde
            )
            []
        , foutMelding poging waarde
        ]


foutRandKlasse : Bool -> String -> List (Html.Attribute msg)
foutRandKlasse poging waarde =
    if poging && isLeeg waarde then
        [ Attr.class "calc-veld-fout" ]

    else
        []


foutMelding : Bool -> String -> Html msg
foutMelding poging waarde =
    if poging && isLeeg waarde then
        span [ Attr.class "calc-fout" ] [ text "Vul dit in om een offerte aan te vragen." ]

    else
        text ""


keuzeOptie : String -> String -> String -> Html Msg
keuzeOptie huidig waarde omschrijving =
    option
        [ Attr.value waarde
        , Attr.selected (huidig == waarde)
        ]
        [ text omschrijving ]


{-| De bron-opties als (keuzewaarde, label): de ene lijst waar zowel de
rekenhulp-dropdown als het offerteformulier (OfferteForm.elm) uit
rendert, zodat een platform erbij of eraf op beide plekken tegelijk
landt. -}
bronKeuzes : List ( String, String )
bronKeuzes =
    [ ( "mijnwebwinkel", bronOmschrijving BronMijnwebwinkel )
    , ( "ccv", bronOmschrijving BronCcvShop )
    , ( "lightspeed", bronOmschrijving BronLightspeed )
    , ( "woocommerce", bronOmschrijving BronWoocommerce )
    , ( "anders", bronOmschrijving BronAnders )
    ]


{-| De doel-opties, zelfde rol als 'bronKeuzes'. -}
doelKeuzes : List ( String, String )
doelKeuzes =
    [ ( "shopify", doelOmschrijving DoelShopify )
    , ( "woocommerce", doelOmschrijving DoelWoocommerce )
    , ( "anders", doelOmschrijving DoelAnders )
    , ( "weetniet", doelOmschrijving DoelWeetNiet )
    ]


bronVeld : BronPlatform -> Html Msg
bronVeld bron =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Waar draait je webshop nu?" ]
        , select [ onInput BronGewijzigd ]
            (List.map (\( waarde, omschrijving ) -> keuzeOptie (bronNaarWaarde bron) waarde omschrijving) bronKeuzes)
        ]


doelVeld : DoelPlatform -> Html Msg
doelVeld doel =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Waar wil je naartoe?" ]
        , select [ onInput DoelGewijzigd ]
            (List.map (\( waarde, omschrijving ) -> keuzeOptie (doelNaarWaarde doel) waarde omschrijving) doelKeuzes)
        ]


themaVeld : ThemaKeuze -> Html Msg
themaVeld thema =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Hoe moet je nieuwe shop eruitzien?" ]
        , select [ onInput ThemaGewijzigd ]
            [ keuzeOptie (themaNaarWaarde thema) "standaard" (themaOmschrijving ThemaStandaard)
            , keuzeOptie (themaNaarWaarde thema) "overzetten" (themaOmschrijving ThemaOverzetten)
            , keuzeOptie (themaNaarWaarde thema) "nieuw" (themaOmschrijving ThemaNieuw)
            ]
        ]


{-| Het aantal-veld van een meegroeiende module, alleen zichtbaar als de
module is aangevinkt. Leeg laten mag: dan rekent de richtprijs met het vaste
deel (tot 1.000 items), en bij de offerte tellen we het echte aantal na. -}
aantalVeld : Bool -> String -> String -> (String -> Msg) -> List (Html Msg)
aantalVeld aan veldLabel waarde naarBericht =
    if aan then
        [ getalVeld veldLabel waarde "een schatting is genoeg; leeg = tot 1.000" naarBericht ]

    else
        []


aanvinkVeld : String -> String -> Bool -> (Bool -> Msg) -> Html Msg
aanvinkVeld veldLabel toelichting aan naarBericht =
    label [ Attr.class "calc-check" ]
        [ input
            [ Attr.type_ "checkbox"
            , Attr.checked aan
            , onCheck naarBericht
            ]
            []
        , span [ Attr.class "calc-check-text" ]
            [ span [ Attr.class "calc-check-label" ] [ text veldLabel ]
            , span [ Attr.class "calc-hint" ] [ text toelichting ]
            ]
        ]



-- UITSPLITSING


{-| Een hoofdregel is een post die in het totaal telt; een subregel staat
ingesprongen onder zijn hoofdregel en legt uit hoe dat bedrag is opgebouwd
(de staffelstappen). Subregels tellen dus niet nog eens mee: het totaal is
de som van de hoofdregels. -}
type Regelniveau
    = Hoofdregel
    | Subregel


{-| Eén prijsregel: omschrijving, bedrag in centen en het niveau. -}
type alias PrijsRegel =
    { omschrijving : String
    , centen : Int
    , niveau : Regelniveau
    }


optioneleRegel : Bool -> String -> Int -> List PrijsRegel
optioneleRegel toon omschrijving centen =
    if toon then
        [ PrijsRegel omschrijving centen Hoofdregel ]

    else
        []


{-| Zet een aantal voor een enkelvoud- of meervoud-zelfstandignaamwoord, zodat
"1 extra taal" en "2 extra talen" allebei goed lopen. -}
aantalLabel : Int -> String -> String -> String
aantalLabel aantal enkelvoud meervoud =
    String.fromInt aantal
        ++ " "
        ++ (if aantal == 1 then
                enkelvoud

            else
                meervoud
           )


{-| Eén subregel per staffelstap: "501 t/m 1.500 (1.000 x \u{20AC}0,15)" met
het bedrag van die stap, ingesprongen onder de hoofdregel met het totaal,
zodat de bezoeker ziet hoeveel items tegen welk tarief tellen. -}
segmentRegel : StaffelSegment -> PrijsRegel
segmentRegel segment =
    PrijsRegel
        (voegDuizendtallenToe (String.fromInt segment.van)
            ++ " t/m "
            ++ voegDuizendtallenToe (String.fromInt segment.tot)
            ++ " ("
            ++ voegDuizendtallenToe (String.fromInt (segmentAantal segment))
            ++ " \u{00D7} "
            ++ formatteerEuro segment.tariefCenten
            ++ ")"
        )
        (segmentCenten segment)
        Subregel


{-| De productpost: een hoofdregel met het totaal en de telling, daaronder
per staffelstap een subregel. Bij meer talen telt elk product per taal, dus
dan heet de telling productvertalingen. Zonder producten geen regels. -}
productRegels : Model -> List PrijsRegel
productRegels model =
    let
        vertalingen =
            productVertalingen model
    in
    if vertalingen <= 0 then
        []

    else
        PrijsRegel (productLabel model vertalingen) (extraProductVertalingenCenten model) Hoofdregel
            :: List.map segmentRegel (productStaffelSegmenten vertalingen)


productLabel : Model -> Int -> String
productLabel model vertalingen =
    if aantalTalen model > 1 then
        "Productvertalingen (" ++ voegDuizendtallenToe (String.fromInt (aantalProducten model)) ++ " producten \u{00D7} " ++ String.fromInt (aantalTalen model) ++ " talen = " ++ voegDuizendtallenToe (String.fromInt vertalingen) ++ ")"

    else
        "Producten (" ++ voegDuizendtallenToe (String.fromInt vertalingen) ++ ")"


{-| De regels van een meegroeiende module: een hoofdregel met het totaal en
de opgegeven telling, daaronder ingesprongen het vaste deel (toegang en de
eerste 1.000 items) en per staffelstap een subregel voor de items daarboven.
Zonder opgave, of tot 1.000 items, is het vaste deel de enige subregel. -}
moduleRegels : Bool -> String -> String -> ItemStaffel -> Int -> List PrijsRegel
moduleRegels aan omschrijving eenheidMeervoud staffel aantal =
    if aan then
        PrijsRegel (omschrijving ++ moduleTelling aantal eenheidMeervoud) (itemStaffelCenten staffel aantal) Hoofdregel
            :: PrijsRegel
                ("toegang en de eerste "
                    ++ voegDuizendtallenToe (String.fromInt moduleInbegrepenItems)
                    ++ " "
                    ++ eenheidMeervoud
                )
                moduleVastCenten
                Subregel
            :: List.map segmentRegel (itemStaffelSegmenten staffel aantal)

    else
        []


moduleTelling : Int -> String -> String
moduleTelling aantal eenheidMeervoud =
    if aantal <= 0 then
        " (tot " ++ voegDuizendtallenToe (String.fromInt moduleInbegrepenItems) ++ " " ++ eenheidMeervoud ++ ")"

    else
        " (" ++ voegDuizendtallenToe (String.fromInt aantal) ++ " " ++ eenheidMeervoud ++ ")"


themaRegels : Model -> List PrijsRegel
themaRegels model =
    case model.thema of
        ThemaStandaard ->
            []

        ThemaOverzetten ->
            [ PrijsRegel "Uitstraling overzetten" themaOverzettenCenten Hoofdregel ]

        ThemaNieuw ->
            []


{-| De volledige lijst prijsregels voor de huidige keuzes. Eén bron voor zowel
de uitsplitsing op het scherm als de vooringevulde offerte-mail, zodat die twee
nooit uit elkaar lopen. -}
prijsRegels : Model -> List PrijsRegel
prijsRegels model =
    [ PrijsRegel "Basismigratie" basisMigratieCenten Hoofdregel ]
        ++ productRegels model
        ++ optioneleRegel
            (extraTalen model > 0)
            (aantalLabel (extraTalen model) "extra taal: configuratie \u{00D7} \u{20AC}250" "extra talen: configuratie \u{00D7} \u{20AC}250")
            (extraTaalConfiguratieCenten model)
        ++ themaRegels model
        ++ moduleRegels model.klantaccounts "Klantaccounts meenemen" "accounts" klantaccountsStaffel (aantalKlantaccounts model)
        ++ moduleRegels model.orderhistorie "Bestelgeschiedenis meenemen" "bestellingen" orderhistorieStaffel (aantalBestellingen model)
        ++ moduleRegels model.nieuwsbrief "Nieuwsbrief-aanmeldingen meenemen" "adressen" nieuwsbriefStaffel (aantalAbonnees model)
        ++ moduleRegels model.voorraad "Voorraadaantallen live overzetten" "producten" voorraadStaffel (aantalProducten model)
        ++ optioneleRegel model.reviews "Reviews / beoordelingen overzetten" reviewsCenten
        ++ optioneleRegel (domeinGekozen model) "Domeinverhuizing" domeinverhuizingCenten
        ++ optioneleRegel (emailGekozen model) "E-mail-setup" emailSetupCenten
        ++ optioneleRegel model.verzendkoppeling "Verzendkoppeling (bijv. DHL)" verzendkoppelingCenten
        ++ optioneleRegel model.b2bKanaal "B2B-kanaal (zakelijke prijzen)" b2bKanaalCenten
        ++ optioneleRegel model.pointOfSale "Kassa / point-of-sale" pointOfSaleCenten
        ++ optioneleRegel model.cursus "Cursus Shopify (2 uur, 1-op-1)" cursusCenten


regelNaarHtml : PrijsRegel -> Html Msg
regelNaarHtml prijsregel =
    li [ Attr.class (regelKlasse prijsregel.niveau) ]
        [ span [ Attr.class "calc-line-label" ] [ text prijsregel.omschrijving ]
        , span [ Attr.class "calc-line-price" ] [ text (formatteerEuro prijsregel.centen) ]
        ]


regelKlasse : Regelniveau -> String
regelKlasse niveau =
    case niveau of
        Hoofdregel ->
            "calc-line"

        Subregel ->
            "calc-line calc-line-sub"


uitsplitsing : Model -> Html Msg
uitsplitsing model =
    ul [ Attr.class "calc-lines" ] (List.map regelNaarHtml (prijsRegels model))


{-| Korte signaalregel bij een keuze die we niet kant-en-klaar prijzen, zodat
het totaal niet stilzwijgend een op-aanvraag-post weglaat. De volledige uitleg
staat als voetnoot onder de rekenhulp op de pagina, niet in de app zelf. -}
themaNoot : ThemaKeuze -> List (Html Msg)
themaNoot thema =
    case thema of
        ThemaNieuw ->
            [ p [ Attr.class "calc-note" ]
                [ text "Nieuw ontwerp: op aanvraag, nog niet meegerekend in het totaal." ]
            ]

        ThemaStandaard ->
            []

        ThemaOverzetten ->
            []


bronNoot : BronPlatform -> List (Html Msg)
bronNoot bron =
    case bron of
        BronAnders ->
            [ p [ Attr.class "calc-note" ]
                [ text "Ander platform: prijs op aanvraag, dit totaal is dan een ondergrens." ]
            ]

        BronMijnwebwinkel ->
            []

        BronCcvShop ->
            []

        BronLightspeed ->
            []

        BronWoocommerce ->
            []


{-| Geruststelling bij "weet ik nog niet": geen platformkeuze is geen blokkade,
we adviseren in het gratis gesprek op basis van de situatie van de shop. De
richtprijs rekent dan met Shopify, onze standaard, als uitgangspunt. -}
doelNoot : DoelPlatform -> List (Html Msg)
doelNoot doel =
    case doel of
        DoelWeetNiet ->
            [ p [ Attr.class "calc-note" ]
                [ text "Nog geen platform op het oog? Prima: in het gratis gesprek adviseren we een platform op basis van je situatie. De richtprijs rekent met Shopify als uitgangspunt." ]
            ]

        DoelShopify ->
            []

        DoelWoocommerce ->
            []

        DoelAnders ->
            []


{-| Losse waarschuwingen en geruststellingen bij keuzes die uitleg vragen: een
nieuw ontwerp en een onbekend bronplatform gaan op aanvraag, en een nog
onbekend doelplatform krijgt de advies-toezegging. -}
opAanvraagNoten : Model -> List (Html Msg)
opAanvraagNoten model =
    themaNoot model.thema ++ bronNoot model.bron ++ doelNoot model.doel ++ pointOfSaleNoot model.pointOfSale


{-| Bij point-of-sale komt de installatie op locatie: die en de reiskosten
rekenen we los, op aanvraag, dus ze zitten niet in het getoonde totaal. -}
pointOfSaleNoot : Bool -> List (Html Msg)
pointOfSaleNoot pointOfSale =
    if pointOfSale then
        [ p [ Attr.class "calc-note" ]
            [ text "Kassa/point-of-sale zetten we bij je op locatie op. Installatie en reiskosten rekenen we daar los bij, op aanvraag." ]
        ]

    else
        []



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "prijs-calculator" ]
        [ fieldset [ Attr.class "calc-inputs" ]
            [ legend [] [ text "Je webshop" ]
            , bronVeld model.bron
            , doelVeld model.doel
            , getalVeld "Hoeveel producten heeft je webshop ongeveer?" model.productenInvoer "vanaf 20 cent per product, hoe meer hoe goedkoper per stuk" ProductenGewijzigd
            , p [ Attr.class "calc-hint" ]
                [ text "Een schatting is genoeg: bij het maken van de offerte tellen we het exacte aantal voor je na." ]
            , getalVeld "In hoeveel talen staat je webshop?" model.talenInvoer "1 taal zit in de basisprijs" TalenGewijzigd
            , themaVeld model.thema
              -- Decision: de aanvinkgroepen zitten in een natief
              -- details/summary-element en staan standaard dicht.
              -- Gekozen boven een eigen open/dicht-Msg in het model:
              -- de browser regelt het klappen, er is geen state of
              -- analytics-ruis bij, en aangevinkte hokjes blijven
              -- gewoon meetellen als de groep weer dichtklapt (de
              -- inputs blijven in de DOM). Aanleiding: de rekenhulp
              -- oogde als een muur van opties, en wie alles aanvinkt
              -- schrikt van het totaal (plotterenzo-les, 31 aug 2026).
            , details [ Attr.class "calc-check-group" ] <|
                [ summary [ Attr.class "calc-label" ] [ text "Wat wil je meenemen naar de nieuwe shop?" ]
                , aanvinkVeld "Klantaccounts" "Je klanten houden hun eigen inlog. \u{20AC}100 tot 1.000 accounts, daarboven per account" model.klantaccounts KlantaccountsGewijzigd
                ]
                    ++ aantalVeld model.klantaccounts "Hoeveel klantaccounts ongeveer?" model.klantaccountsInvoer KlantaccountsAantalGewijzigd
                    ++ [ aanvinkVeld "Bestelgeschiedenis" "Alle eerdere bestellingen van je klanten. \u{20AC}100 tot 1.000 bestellingen, daarboven per bestelling" model.orderhistorie OrderhistorieGewijzigd ]
                    ++ aantalVeld model.orderhistorie "Hoeveel bestellingen staan er in je shop ongeveer?" model.bestellingenInvoer BestellingenGewijzigd
                    ++ [ aanvinkVeld "Nieuwsbrief-aanmeldingen" "De adressenlijst van je nieuwsbrief. \u{20AC}100 tot 1.000 adressen, daarboven per adres" model.nieuwsbrief NieuwsbriefGewijzigd ]
                    ++ aantalVeld model.nieuwsbrief "Hoeveel nieuwsbrief-adressen ongeveer?" model.abonneesInvoer AbonneesGewijzigd
                    ++ [ aanvinkVeld "Voorraadaantallen" "De actuele voorraad per product. \u{20AC}100 tot 1.000 producten, daarboven per product" model.voorraad VoorraadGewijzigd
                       , aanvinkVeld "Reviews / beoordelingen" "Je opgebouwde productbeoordelingen" model.reviews ReviewsGewijzigd
                       ]
            , details [ Attr.class "calc-check-group" ] <|
                [ summary [ Attr.class "calc-label" ] [ text "Extra diensten en koppelingen" ] ]
                    ++ domeinEmailVelden model
                    ++ [ aanvinkVeld "Verzendkoppeling (bijv. DHL)" "Pakketten en labels rechtstreeks vanuit je shop" model.verzendkoppeling VerzendkoppelingGewijzigd
                       , aanvinkVeld "B2B-kanaal (zakelijke klanten)" "Aparte prijzen en inlog voor zakelijke klanten" model.b2bKanaal B2bKanaalGewijzigd
                       , aanvinkVeld "Kassa / point-of-sale voor mijn fysieke winkel" "Verkopen in de winkel \u{00E9}n online met \u{00E9}\u{00E9}n systeem (Shopify POS)" model.pointOfSale PointOfSaleGewijzigd
                       , aanvinkVeld "Cursus Shopify (2 uur, 1-op-1)" "Samen door je nieuwe shop, zodat je hem daarna zelf beheert" model.cursus CursusGewijzigd
                       ]
            ]
        , div [ Attr.class "calc-result" ] <|
            if isGroteCatalogus model then
                groteCatalogusPaneel model

            else
                [ h3 [] [ text "Je richtprijs" ]
                , uitsplitsing model
                , p [ Attr.class "calc-total" ]
                    [ span [] [ text "Totaal (excl. BTW)" ]
                    , strong [] [ text (formatteerEuro (totaalCenten model)) ]
                    ]
                ]
                    ++ opAanvraagNoten model
                    ++ [ lockInNoot, offerteFormulier model, vrijblijvendNoot ]
        ]


{-| Boven de grens tonen we geen richtprijs: een vlak tarief zegt daar niets
meer en een kaal totaal van veertienduizend euro jaagt de bezoeker stil weg.
In plaats daarvan een uitnodiging tot contact, met de ingevulde aantallen al
in de mail. De grens in de tekst komt uit 'groteCatalogusGrens', zodat tekst
en gedrag niet uit elkaar kunnen lopen. -}
groteCatalogusPaneel : Model -> List (Html Msg)
groteCatalogusPaneel model =
    [ h3 [] [ text "Je richtprijs" ]
    , p [ Attr.class "calc-note calc-grote-catalogus" ]
        [ text
            ("Vanaf "
                ++ voegDuizendtallenToe (String.fromInt groteCatalogusGrens)
                ++ " producten (over alle talen samen) is jouw winkel geen standaardmigratie meer. Zo'n catalogus verdient een eigen doorrekening in plaats van een standaardtarief; neem contact op en we rekenen een passende prijs voor je door."
            )
        ]
    , form
        [ Attr.action "/api/offerte", Attr.method "post", Attr.class "calc-offerte-formulier" ]
        [ label [ Attr.class "calc-field" ]
            [ span [ Attr.class "calc-label" ] [ text "Je e-mailadres (hierop reageren we)" ]
            , input
                [ Attr.type_ "email"
                , Attr.name "email"
                , Attr.required True
                , Attr.value model.emailInvoer
                , Attr.placeholder "naam@voorbeeld.nl"
                , onInput EmailInvoerGewijzigd
                ]
                []
            ]
        , input [ Attr.type_ "hidden", Attr.name "bericht", Attr.value (groteCatalogusBericht model) ] []
        , input [ Attr.type_ "hidden", Attr.name "shop", Attr.value model.webshopDomein ] []
        , input [ Attr.type_ "hidden", Attr.name "soort", Attr.value "rekenhulp" ] []
        , input [ Attr.type_ "hidden", Attr.name "website", Attr.value "" ] []
        , button
            [ Attr.type_ "submit"
            , Attr.class "cta-button calc-offerte"
            , onClick GroteCatalogusContact
            ]
            [ text "Neem contact op" ]
        ]
    , vrijblijvendNoot
    ]


{-| Berichttekst voor de grote-catalogus-route, met de al ingevulde
aantallen en platforms zodat het gesprek meteen ergens over gaat. -}
groteCatalogusBericht : Model -> String
groteCatalogusBericht model =
    String.join "\n"
        [ "Grote catalogus (rekenhulp-melding):"
        , ""
        , "Mijn webshop heeft ongeveer " ++ String.fromInt (aantalProducten model) ++ " producten in " ++ String.fromInt (aantalTalen model) ++ " taal/talen."
        , "Huidig platform: " ++ bronOmschrijving model.bron
        , "Gewenst platform: " ++ doelOmschrijving model.doel
        , ""
        , "Ik hoor graag wat een migratie voor mijn winkel zou kosten."
        ]


{-| De domein- en e-mailvragen tonen we alleen voor bronplatforms die die zaken
zelf bundelen, met de platformnaam erin. Bij een zelf-gehost of onbekend
platform laten we ze weg. -}
domeinEmailVelden : Model -> List (Html Msg)
domeinEmailVelden model =
    if bundeltDomeinEnEmail model.bron then
        [ aanvinkVeld
            ("Mijn domeinnaam staat nog bij " ++ bronOmschrijving model.bron)
            "Het internetadres van je shop (bijv. uwshop.nl). Weet je het niet zeker? Dan zoeken we het samen uit."
            model.domeinBijMijnwebwinkel
            DomeinGewijzigd
        , aanvinkVeld
            ("Mijn e-mailadressen horen bij " ++ bronOmschrijving model.bron)
            "Bijvoorbeeld info@uwshop.nl die je via dat platform gebruikt"
            model.emailBijMijnwebwinkel
            EmailGewijzigd
        ]

    else
        []


{-| De drie verplichte contactvelden onder de richtprijs: naam, het
webshop-domein en het e-mailadres waarop we de offerte terugsturen. Alle
drie moeten ingevuld zijn voordat de offerte-knop verstuurt. Het
e-mailveld heeft een @name@ omdat het als formulierveld meegaat in de
POST naar /api/offerte; naam en domein reizen mee in het
bericht-verborgenveld. -}
contactVelden : Model -> Html Msg
contactVelden model =
    div [ Attr.class "calc-contact" ]
        [ verplichtVeld model.offertePoging "Je naam" "Voor- en achternaam" model.naam NaamGewijzigd
        , verplichtVeld model.offertePoging "Je webshop (domeinnaam)" "bijv. uwshop.nl" model.webshopDomein WebshopDomeinGewijzigd
        , emailVeld model
        ]


{-| Zoals 'verplichtVeld', maar als e-mail-input die in de formulier-POST
meegaat. -}
emailVeld : Model -> Html Msg
emailVeld model =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Je e-mailadres (hierop ontvang je de offerte)" ]
        , input
            ([ Attr.type_ "email"
             , Attr.name "email"
             , Attr.value model.emailInvoer
             , Attr.placeholder "naam@voorbeeld.nl"
             , onInput EmailInvoerGewijzigd
             ]
                ++ foutRandKlasse model.offertePoging model.emailInvoer
            )
            []
        , foutMelding model.offertePoging model.emailInvoer
        ]


lockInNoot : Html Msg
lockInNoot =
    p [ Attr.class "calc-lockin" ]
        [ text "Dit is een richtprijs. Wil je tegen deze prijs verhuizen? Vraag nu een offerte aan." ]


{-| Geruststelling onder de offerte-knop: de aanvraag verplicht tot niets, de
bezoeker vraagt alleen een bevestiging van de getoonde prijs. -}
vrijblijvendNoot : Html Msg
vrijblijvendNoot =
    p [ Attr.class "calc-vrijblijvend" ]
        [ text "Vrijblijvend: met deze aanvraag zit je nergens aan vast. Je vraagt alleen een bevestiging van deze prijs, en beslist daarna rustig zelf." ]


formulierGeldig : Model -> Bool
formulierGeldig model =
    not (isLeeg model.naam)
        && not (isLeeg model.webshopDomein)
        && not (isLeeg model.emailInvoer)
        && String.contains "@" model.emailInvoer


-- Decision: de offerte-aanvraag POST als kaal HTML-formulier naar
-- /api/offerte in plaats van de oude mailto-link (besluit Jappie 4 sep
-- 2026). GA4 augustus: 2 offerte-mailto-kliks, nul ontvangen mails;
-- mailto faalt geluidloos bij bezoekers zonder gekoppeld
-- mailprogramma. De server logt elke aanvraag durabel en mailt Jappie,
-- en stuurt de browser met een 303 door naar /offerte-verzonden.html.


{-| Het formulier om de POST heen: de contactvelden, het volledige
bericht (dezelfde tekst die vroeger in de mailto stond) als verborgen
veld, en een honeypot-veld dat de server als spam-signaal leest. -}
offerteFormulier : Model -> Html Msg
offerteFormulier model =
    form
        [ Attr.action "/api/offerte", Attr.method "post", Attr.class "calc-offerte-formulier" ]
        [ contactVelden model
        , input [ Attr.type_ "hidden", Attr.name "bericht", Attr.value (offerteBody model) ] []
        , input [ Attr.type_ "hidden", Attr.name "shop", Attr.value model.webshopDomein ] []
        , input [ Attr.type_ "hidden", Attr.name "soort", Attr.value "rekenhulp" ] []
        , input [ Attr.type_ "hidden", Attr.name "website", Attr.value "" ] []
        , offerteKnop model
        ]


{-| Verstuurknop van het offerteformulier. Pas een echte submit als de
verplichte velden ingevuld zijn; daarvoor markeert een klik alleen de
ontbrekende velden (type button, dus geen verzending). -}
offerteKnop : Model -> Html Msg
offerteKnop model =
    if formulierGeldig model then
        button
            [ Attr.type_ "submit"
            , Attr.class "cta-button calc-offerte"
            , onClick OfferteVerzonden
            ]
            [ text "Vraag deze offerte aan" ]

    else
        button
            [ Attr.type_ "button"
            , Attr.class "cta-button calc-offerte"
            , onClick OfferteGepoogd
            ]
            [ text "Vraag deze offerte aan" ]


{-| De vooringevulde mailtekst: de shopgegevens plus de volledige prijsregels
(dezelfde als op het scherm) en het totaal, zodat we de indicatie na validatie
zo in de offerte kunnen overnemen. -}
offerteBody : Model -> String
offerteBody model =
    String.join "\n"
        ([ "Hallo,"
         , ""
         , "Ik wil graag een offerte voor het verhuizen van mijn webshop. Op basis van de rekenhulp heb ik dit ingevuld:"
         , ""
         , "Naam: " ++ model.naam
         , "Webshop: " ++ model.webshopDomein
         , "Aantal producten: " ++ String.fromInt (aantalProducten model)
         , "Aantal talen: " ++ String.fromInt (aantalTalen model)
         , "Huidig platform: " ++ bronOmschrijving model.bron
         , "Gewenst platform: " ++ doelOmschrijving model.doel
         , "Thema: " ++ themaOmschrijving model.thema
         , ""
         , "Prijsindicatie (excl. BTW):"
         ]
            ++ List.map prijsRegelTekst (prijsRegels model)
            ++ [ "Totaal: " ++ formatteerEuro (totaalCenten model) ]
            ++ pointOfSaleReiskostenRegel model
            ++ [ ""
               , "Kun je mij hiervoor een offerte sturen?"
               ]
        )


prijsRegelTekst : PrijsRegel -> String
prijsRegelTekst prijsregel =
    case prijsregel.niveau of
        Hoofdregel ->
            "- " ++ prijsregel.omschrijving ++ ": " ++ formatteerEuro prijsregel.centen

        Subregel ->
            "    \u{00B7} " ++ prijsregel.omschrijving ++ ": " ++ formatteerEuro prijsregel.centen


{-| Reiskosten-voorbehoud voor point-of-sale, alleen als die gekozen is; het
staat los van het getoonde totaal. -}
pointOfSaleReiskostenRegel : Model -> List String
pointOfSaleReiskostenRegel model =
    if model.pointOfSale then
        [ "(Kassa/point-of-sale: installatie op locatie en reiskosten komen hier los bij, op aanvraag.)" ]

    else
        []



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
