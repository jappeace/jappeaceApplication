port module PrijsCalculator exposing
    ( BronPlatform(..)
    , DoelPlatform(..)
    , Model
    , Msg(..)
    , ThemaKeuze(..)
    , initieelModel
    , isGroteCatalogus
    , main
    , totaalCenten
    , update
    )

{-| Interactieve prijsindicatie voor een webshop-migratie op webwinkelverhuis.nl.

De prijslogica is een 1-op-1 kopie van de standaard prijslijst (jappiesoft
strategy/standaard-prijslijst.org) en de tabel op /prijzen: basismigratie met
1.000 inbegrepen productvertalingen (producten maal talen tellen samen tegen
die ruimte), daarboven een degressieve staffel per duizend vertalingen
(€0,25, €0,20, €0,15, daarna €0,10), plus €250 configuratie per
extra taal, de losse modules (thema, klantaccounts, orderhistorie, nieuwsbrief,
voorraad) en de diensten domeinverhuizing en e-mail-setup. Alle bedragen worden
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
import Html exposing (Html, a, details, div, fieldset, h3, input, label, legend, li, option, p, select, span, strong, summary, text, ul)
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


basisMigratieCenten : Int
basisMigratieCenten =
    199900


inbegrepenProducten : Int
inbegrepenProducten =
    1000


-- Decision: de productstaffel is degressief (besluit Jappie 1 sep 2026,
-- na plotterenzo.nl en het 50.000-producten-anker van 15 aug): grote
-- catalogi kosten het migratieprogramma nauwelijks extra werk, dus een
-- vlak tarief prijst juist de goedkoopste meerschaal het hardst en
-- jaagt grote shops weg met bedragen die niets met de kostprijs te
-- maken hebben. Boven de inbegrepen 1.000 productvertalingen kost elke
-- volgende duizend een trede minder, tot een bodem van 10 cent.
-- Alternatief overwogen: een tweede maatwerkgrens op 5.000 euro
-- richtprijs (staat gebouwd op de geparkeerde branch
-- calculator-richtprijs-grens); afgewezen omdat de degressieve staffel
-- die bedragen gewoon eerlijk toonbaar maakt en de bestaande
-- grote-catalogus-grens de echt grote gevallen al naar het gesprek
-- stuurt.


staffelTredenCenten : List Int
staffelTredenCenten =
    [ 25, 20, 15 ]


staffelBodemCenten : Int
staffelBodemCenten =
    10


tredeGrootte : Int
tredeGrootte =
    1000


perTaalConfiguratieCenten : Int
perTaalConfiguratieCenten =
    25000


themaOverzettenCenten : Int
themaOverzettenCenten =
    74900


klantaccountsCenten : Int
klantaccountsCenten =
    25000


orderhistorieCenten : Int
orderhistorieCenten =
    25000


nieuwsbriefCenten : Int
nieuwsbriefCenten =
    25000


voorraadCenten : Int
voorraadCenten =
    25000


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
    , orderhistorie : Bool
    , nieuwsbrief : Bool
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
    , offertePoging : Bool
    , analyticsEngaged : Bool
    , groteCatalogusGemeld : Bool
    }


initieelModel : Model
initieelModel =
    { productenInvoer = "1000"
    , talenInvoer = "1"
    , bron = BronMijnwebwinkel
    , doel = DoelShopify
    , thema = ThemaStandaard
    , klantaccounts = False
    , orderhistorie = False
    , nieuwsbrief = False
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
    | OrderhistorieGewijzigd Bool
    | NieuwsbriefGewijzigd Bool
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


{-| Conversie-event met de richtprijs en de gekozen platforms. "value" en
"currency" zijn GA4's gereserveerde geldparameters, dus de waarde wordt native
herkend; "bron" en "doel" zijn custom parameters die in GA4 als custom dimension
geregistreerd moeten worden voordat je erop kunt uitsplitsen. -}
offerteAangevraagdEvent : Model -> Cmd Msg
offerteAangevraagdEvent model =
    gaEvent "offerte_aangevraagd"
        [ ( "value", Encode.int (totaalCenten model // 100) )
        , ( "currency", Encode.string "EUR" )
        , ( "bron", Encode.string (bronOmschrijving model.bron) )
        , ( "doel", Encode.string (doelOmschrijving model.doel) )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProductenGewijzigd waarde ->
            meldGroteCatalogus (markeerEngagement { model | productenInvoer = waarde })

        TalenGewijzigd waarde ->
            meldGroteCatalogus (markeerEngagement { model | talenInvoer = waarde })

        BronGewijzigd waarde ->
            markeerEngagement { model | bron = leesBron waarde }

        DoelGewijzigd waarde ->
            markeerEngagement { model | doel = leesDoel waarde }

        ThemaGewijzigd waarde ->
            markeerEngagement { model | thema = leesThema waarde }

        KlantaccountsGewijzigd aan ->
            markeerEngagement { model | klantaccounts = aan }

        OrderhistorieGewijzigd aan ->
            markeerEngagement { model | orderhistorie = aan }

        NieuwsbriefGewijzigd aan ->
            markeerEngagement { model | nieuwsbrief = aan }

        VoorraadGewijzigd aan ->
            markeerEngagement { model | voorraad = aan }

        ReviewsGewijzigd aan ->
            markeerEngagement { model | reviews = aan }

        CursusGewijzigd aan ->
            markeerEngagement { model | cursus = aan }

        DomeinGewijzigd aan ->
            markeerEngagement { model | domeinBijMijnwebwinkel = aan }

        EmailGewijzigd aan ->
            markeerEngagement { model | emailBijMijnwebwinkel = aan }

        VerzendkoppelingGewijzigd aan ->
            markeerEngagement { model | verzendkoppeling = aan }

        B2bKanaalGewijzigd aan ->
            markeerEngagement { model | b2bKanaal = aan }

        PointOfSaleGewijzigd aan ->
            markeerEngagement { model | pointOfSale = aan }

        NaamGewijzigd waarde ->
            markeerEngagement { model | naam = waarde }

        WebshopDomeinGewijzigd waarde ->
            markeerEngagement { model | webshopDomein = waarde }

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


-- Decision: productvertalingen tellen samen tegen de 1.000 inbegrepen
-- producten van de basismigratie (besluit Jappie 2026-08-08, n.a.v. de
-- bybjor-offerte). Elk product telt per taal één keer mee: 160 producten
-- in 3 talen zijn 480 productvertalingen en passen dus in de basisruimte,
-- terwijl het oude model elke extra taal over de hele catalogus liet
-- betalen zonder die ruimte. Voor catalogi vanaf 1.000 producten is de
-- uitkomst wiskundig gelijk aan het oude model; kleinere catalogi met
-- meerdere talen worden er goedkoper van. Alternatief was de oude
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
    staffelCenten (extraProductVertalingen model) staffelTredenCenten


{-| De degressieve som over de extra productvertalingen: de eerste
'tredeGrootte' vertalingen tegen de eerste trede, de volgende duizend
tegen de tweede, enzovoort; alles voorbij de laatste trede tegen de
bodemprijs. -}
staffelCenten : Int -> List Int -> Int
staffelCenten extra treden =
    if extra <= 0 then
        0

    else
        case treden of
            [] ->
                extra * staffelBodemCenten

            tarief :: rest ->
                let
                    inDezeTrede =
                        Basics.min extra tredeGrootte
                in
                inDezeTrede * tarief + staffelCenten (extra - inDezeTrede) rest


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
        + indienAan model.klantaccounts klantaccountsCenten
        + indienAan model.orderhistorie orderhistorieCenten
        + indienAan model.nieuwsbrief nieuwsbriefCenten
        + indienAan model.voorraad voorraadCenten
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


bronVeld : BronPlatform -> Html Msg
bronVeld bron =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Waar draait je webshop nu?" ]
        , select [ onInput BronGewijzigd ]
            [ keuzeOptie (bronNaarWaarde bron) "mijnwebwinkel" (bronOmschrijving BronMijnwebwinkel)
            , keuzeOptie (bronNaarWaarde bron) "ccv" (bronOmschrijving BronCcvShop)
            , keuzeOptie (bronNaarWaarde bron) "lightspeed" (bronOmschrijving BronLightspeed)
            , keuzeOptie (bronNaarWaarde bron) "woocommerce" (bronOmschrijving BronWoocommerce)
            , keuzeOptie (bronNaarWaarde bron) "anders" (bronOmschrijving BronAnders)
            ]
        ]


doelVeld : DoelPlatform -> Html Msg
doelVeld doel =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Waar wil je naartoe?" ]
        , select [ onInput DoelGewijzigd ]
            [ keuzeOptie (doelNaarWaarde doel) "shopify" (doelOmschrijving DoelShopify)
            , keuzeOptie (doelNaarWaarde doel) "woocommerce" (doelOmschrijving DoelWoocommerce)
            , keuzeOptie (doelNaarWaarde doel) "anders" (doelOmschrijving DoelAnders)
            , keuzeOptie (doelNaarWaarde doel) "weetniet" (doelOmschrijving DoelWeetNiet)
            ]
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


{-| Eén prijsregel: omschrijving plus bedrag in centen. -}
type alias PrijsRegel =
    { omschrijving : String
    , centen : Int
    }


optioneleRegel : Bool -> String -> Int -> List PrijsRegel
optioneleRegel toon omschrijving centen =
    if toon then
        [ PrijsRegel omschrijving centen ]

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


themaRegels : Model -> List PrijsRegel
themaRegels model =
    case model.thema of
        ThemaStandaard ->
            []

        ThemaOverzetten ->
            [ PrijsRegel "Uitstraling overzetten" themaOverzettenCenten ]

        ThemaNieuw ->
            []


{-| De volledige lijst prijsregels voor de huidige keuzes. Eén bron voor zowel
de uitsplitsing op het scherm als de vooringevulde offerte-mail, zodat die twee
nooit uit elkaar lopen. -}
prijsRegels : Model -> List PrijsRegel
prijsRegels model =
    [ PrijsRegel "Basismigratie (1.000 producten inbegrepen, over alle talen samen)" basisMigratieCenten ]
        ++ optioneleRegel
            (extraProductVertalingen model > 0)
            (aantalLabel (extraProductVertalingen model) "product boven de 1.000 inbegrepen (over alle talen, aflopende staffel)" "producten boven de 1.000 inbegrepen (over alle talen, aflopende staffel)")
            (extraProductVertalingenCenten model)
        ++ optioneleRegel
            (extraTalen model > 0)
            (aantalLabel (extraTalen model) "extra taal: configuratie \u{00D7} \u{20AC}250" "extra talen: configuratie \u{00D7} \u{20AC}250")
            (extraTaalConfiguratieCenten model)
        ++ themaRegels model
        ++ optioneleRegel model.klantaccounts "Klantaccounts meenemen" klantaccountsCenten
        ++ optioneleRegel model.orderhistorie "Bestelgeschiedenis meenemen" orderhistorieCenten
        ++ optioneleRegel model.nieuwsbrief "Nieuwsbrief-aanmeldingen meenemen" nieuwsbriefCenten
        ++ optioneleRegel model.voorraad "Voorraadaantallen live overzetten" voorraadCenten
        ++ optioneleRegel model.reviews "Reviews / beoordelingen overzetten" reviewsCenten
        ++ optioneleRegel (domeinGekozen model) "Domeinverhuizing" domeinverhuizingCenten
        ++ optioneleRegel (emailGekozen model) "E-mail-setup" emailSetupCenten
        ++ optioneleRegel model.verzendkoppeling "Verzendkoppeling (bijv. DHL)" verzendkoppelingCenten
        ++ optioneleRegel model.b2bKanaal "B2B-kanaal (zakelijke prijzen)" b2bKanaalCenten
        ++ optioneleRegel model.pointOfSale "Kassa / point-of-sale" pointOfSaleCenten
        ++ optioneleRegel model.cursus "Cursus Shopify (2 uur, 1-op-1)" cursusCenten


regelNaarHtml : PrijsRegel -> Html Msg
regelNaarHtml prijsregel =
    li [ Attr.class "calc-line" ]
        [ span [ Attr.class "calc-line-label" ] [ text prijsregel.omschrijving ]
        , span [ Attr.class "calc-line-price" ] [ text (formatteerEuro prijsregel.centen) ]
        ]


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
            , getalVeld "Hoeveel producten heeft je webshop ongeveer?" model.productenInvoer "1.000 zit in de basisprijs" ProductenGewijzigd
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
            , details [ Attr.class "calc-check-group" ]
                [ summary [ Attr.class "calc-label" ] [ text "Wat wil je meenemen naar de nieuwe shop?" ]
                , aanvinkVeld "Klantaccounts" "Je klanten houden hun eigen inlog" model.klantaccounts KlantaccountsGewijzigd
                , aanvinkVeld "Bestelgeschiedenis" "Alle eerdere bestellingen van je klanten" model.orderhistorie OrderhistorieGewijzigd
                , aanvinkVeld "Nieuwsbrief-aanmeldingen" "De adressenlijst van je nieuwsbrief" model.nieuwsbrief NieuwsbriefGewijzigd
                , aanvinkVeld "Voorraadaantallen" "De actuele voorraad per product" model.voorraad VoorraadGewijzigd
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
                    ++ [ lockInNoot, contactVelden model, offerteKnop model, vrijblijvendNoot ]
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
    , a
        [ Attr.href (groteCatalogusMailtoUrl model)
        , Attr.class "cta-button calc-offerte"
        , onClick GroteCatalogusContact
        ]
        [ text "Neem contact op" ]
    , vrijblijvendNoot
    ]


{-| Mailto voor de grote-catalogus-route, met de al ingevulde aantallen en
platforms in de mailtekst zodat het gesprek meteen ergens over gaat. -}
groteCatalogusMailtoUrl : Model -> String
groteCatalogusMailtoUrl model =
    "mailto:jappie@webwinkelverhuis.nl?subject="
        ++ Url.percentEncode "Migratie grote catalogus"
        ++ "&body="
        ++ Url.percentEncode
            (String.join "\n"
                [ "Hallo,"
                , ""
                , "Mijn webshop heeft ongeveer " ++ String.fromInt (aantalProducten model) ++ " producten in " ++ String.fromInt (aantalTalen model) ++ " taal/talen."
                , "Huidig platform: " ++ bronOmschrijving model.bron
                , "Gewenst platform: " ++ doelOmschrijving model.doel
                , ""
                , "Ik hoor graag wat een migratie voor mijn winkel zou kosten."
                ]
            )


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


{-| De twee verplichte contactvelden onder de richtprijs: naam en het
webshop-domein. Beide moeten ingevuld zijn voordat de offerte-knop verstuurt. -}
contactVelden : Model -> Html Msg
contactVelden model =
    div [ Attr.class "calc-contact" ]
        [ verplichtVeld model.offertePoging "Je naam" "Voor- en achternaam" model.naam NaamGewijzigd
        , verplichtVeld model.offertePoging "Je webshop (domeinnaam)" "bijv. uwshop.nl" model.webshopDomein WebshopDomeinGewijzigd
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
    not (isLeeg model.naam) && not (isLeeg model.webshopDomein)


{-| Knop die een offerte-mail opent met alle gekozen opties al ingevuld. Pas
klikbaar naar de mail als naam en webshop-domein ingevuld zijn; daarvoor markeert
een klik alleen de ontbrekende verplichte velden (geen href, dus geen navigatie).
-}
offerteKnop : Model -> Html Msg
offerteKnop model =
    if formulierGeldig model then
        a
            [ Attr.href (offerteMailtoUrl model)
            , Attr.class "cta-button calc-offerte"
            , onClick OfferteVerzonden
            ]
            [ text "Vraag deze offerte aan" ]

    else
        a
            [ Attr.class "cta-button calc-offerte"
            , onClick OfferteGepoogd
            ]
            [ text "Vraag deze offerte aan" ]


offerteMailtoUrl : Model -> String
offerteMailtoUrl model =
    "mailto:jappie@webwinkelverhuis.nl?subject="
        ++ Url.percentEncode "Offerte-aanvraag webshop-migratie"
        ++ "&body="
        ++ Url.percentEncode (offerteBody model)


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
    "- " ++ prijsregel.omschrijving ++ ": " ++ formatteerEuro prijsregel.centen


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
