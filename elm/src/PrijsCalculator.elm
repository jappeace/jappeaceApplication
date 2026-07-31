module PrijsCalculator exposing
    ( BronPlatform(..)
    , Model
    , ThemaKeuze(..)
    , initieelModel
    , main
    , totaalCenten
    )

{-| Interactieve prijsindicatie voor een webshop-migratie op webwinkelverhuis.nl.

De prijslogica is een 1-op-1 kopie van de standaard prijslijst (jappiesoft
strategy/standaard-prijslijst.org) en de tabel op /prijzen: basismigratie t/m
1.000 producten en 1 taal, daarboven een staffel per product en per extra taal,
plus de losse modules (thema, klantaccounts, orderhistorie, nieuwsbrief,
voorraad) en de diensten domeinverhuizing en e-mail-setup. Alle bedragen worden
intern in hele centen gerekend zodat er geen afrondingsfouten op de komma
ontstaan; pas bij het tonen zetten we centen om naar euro's.

De vragen zijn met opzet in gewone taal gesteld, voor een webshop-eigenaar
zonder technische kennis: geen "registrar" of "MX-records", maar "uw domeinnaam"
en "uw e-mailadressen".

Deze indicatie is bewust geen offerte: alleen een offerte legt de prijs vast.
Dat staat ook onder de uitkomst, zodat de bezoeker weet dat dit een richtprijs
is en niet een toezegging.
-}

import Browser
import Html exposing (Html, a, div, fieldset, h3, input, label, legend, li, option, p, select, span, strong, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onInput)
import Url



-- CONSTANTEN (centen), gelijk aan standaard-prijslijst.org en /prijzen


basisMigratieCenten : Int
basisMigratieCenten =
    199900


inbegrepenProducten : Int
inbegrepenProducten =
    1000


perProductCenten : Int
perProductCenten =
    25


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
offerte. Shopify is onze standaard; WooCommerce doen we ook. -}
type DoelPlatform
    = DoelShopify
    | DoelWoocommerce
    | DoelAnders


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
    , naam : String
    , webshopDomein : String
    , offertePoging : Bool
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
    , naam = ""
    , webshopDomein = ""
    , offertePoging = False
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
    | NaamGewijzigd String
    | WebshopDomeinGewijzigd String
    | OfferteGepoogd


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


doelOmschrijving : DoelPlatform -> String
doelOmschrijving doel =
    case doel of
        DoelShopify ->
            "Shopify"

        DoelWoocommerce ->
            "WooCommerce"

        DoelAnders ->
            "Een ander platform / weet ik nog niet"


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProductenGewijzigd waarde ->
            ( { model | productenInvoer = waarde }, Cmd.none )

        TalenGewijzigd waarde ->
            ( { model | talenInvoer = waarde }, Cmd.none )

        BronGewijzigd waarde ->
            ( { model | bron = leesBron waarde }, Cmd.none )

        DoelGewijzigd waarde ->
            ( { model | doel = leesDoel waarde }, Cmd.none )

        ThemaGewijzigd waarde ->
            ( { model | thema = leesThema waarde }, Cmd.none )

        KlantaccountsGewijzigd aan ->
            ( { model | klantaccounts = aan }, Cmd.none )

        OrderhistorieGewijzigd aan ->
            ( { model | orderhistorie = aan }, Cmd.none )

        NieuwsbriefGewijzigd aan ->
            ( { model | nieuwsbrief = aan }, Cmd.none )

        VoorraadGewijzigd aan ->
            ( { model | voorraad = aan }, Cmd.none )

        ReviewsGewijzigd aan ->
            ( { model | reviews = aan }, Cmd.none )

        DomeinGewijzigd aan ->
            ( { model | domeinBijMijnwebwinkel = aan }, Cmd.none )

        EmailGewijzigd aan ->
            ( { model | emailBijMijnwebwinkel = aan }, Cmd.none )

        VerzendkoppelingGewijzigd aan ->
            ( { model | verzendkoppeling = aan }, Cmd.none )

        B2bKanaalGewijzigd aan ->
            ( { model | b2bKanaal = aan }, Cmd.none )

        PointOfSaleGewijzigd aan ->
            ( { model | pointOfSale = aan }, Cmd.none )

        NaamGewijzigd waarde ->
            ( { model | naam = waarde }, Cmd.none )

        WebshopDomeinGewijzigd waarde ->
            ( { model | webshopDomein = waarde }, Cmd.none )

        OfferteGepoogd ->
            ( { model | offertePoging = True }, Cmd.none )



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


extraProducten : Model -> Int
extraProducten model =
    Basics.max 0 (aantalProducten model - inbegrepenProducten)


extraTalen : Model -> Int
extraTalen model =
    aantalTalen model - 1


extraProductenCenten : Model -> Int
extraProductenCenten model =
    extraProducten model * perProductCenten


extraTaalProductenCenten : Model -> Int
extraTaalProductenCenten model =
    extraTalen model * aantalProducten model * perProductCenten


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
        + extraProductenCenten model
        + extraTaalProductenCenten model
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
        [ span [ Attr.class "calc-label" ] [ text "Waar draait uw webshop nu?" ]
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
        [ span [ Attr.class "calc-label" ] [ text "Waar wilt u naartoe?" ]
        , select [ onInput DoelGewijzigd ]
            [ keuzeOptie (doelNaarWaarde doel) "shopify" (doelOmschrijving DoelShopify)
            , keuzeOptie (doelNaarWaarde doel) "woocommerce" (doelOmschrijving DoelWoocommerce)
            , keuzeOptie (doelNaarWaarde doel) "anders" (doelOmschrijving DoelAnders)
            ]
        ]


themaVeld : ThemaKeuze -> Html Msg
themaVeld thema =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Hoe moet uw nieuwe shop eruitzien?" ]
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
    [ PrijsRegel "Basismigratie (t/m 1.000 producten, 1 taal)" basisMigratieCenten ]
        ++ optioneleRegel
            (extraProducten model > 0)
            (aantalLabel (extraProducten model) "extra product \u{00D7} \u{20AC}0,25" "extra producten \u{00D7} \u{20AC}0,25")
            (extraProductenCenten model)
        ++ optioneleRegel
            (extraTalen model > 0)
            (aantalLabel (extraTalen model) "extra taal: vertaalwerk per product" "extra talen: vertaalwerk per product")
            (extraTaalProductenCenten model)
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


{-| Losse waarschuwingen voor keuzes die we niet kant-en-klaar kunnen prijzen:
een nieuw ontwerp en een onbekend bronplatform gaan altijd op aanvraag. -}
opAanvraagNoten : Model -> List (Html Msg)
opAanvraagNoten model =
    themaNoot model.thema ++ bronNoot model.bron ++ pointOfSaleNoot model.pointOfSale


{-| Bij point-of-sale komt de installatie op locatie: die en de reiskosten
rekenen we los, op aanvraag, dus ze zitten niet in het getoonde totaal. -}
pointOfSaleNoot : Bool -> List (Html Msg)
pointOfSaleNoot pointOfSale =
    if pointOfSale then
        [ p [ Attr.class "calc-note" ]
            [ text "Kassa/point-of-sale zetten we bij u op locatie op. Installatie en reiskosten rekenen we daar los bij, op aanvraag." ]
        ]

    else
        []



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "prijs-calculator" ]
        [ fieldset [ Attr.class "calc-inputs" ]
            [ legend [] [ text "Uw webshop" ]
            , bronVeld model.bron
            , doelVeld model.doel
            , getalVeld "Hoeveel producten heeft uw webshop ongeveer?" model.productenInvoer "t/m 1.000 zit in de basisprijs" ProductenGewijzigd
            , getalVeld "In hoeveel talen staat uw webshop?" model.talenInvoer "1 taal zit in de basisprijs" TalenGewijzigd
            , themaVeld model.thema
            , div [ Attr.class "calc-check-group" ]
                [ span [ Attr.class "calc-label" ] [ text "Wat wilt u meenemen naar de nieuwe shop?" ]
                , aanvinkVeld "Klantaccounts" "Uw klanten houden hun eigen inlog" model.klantaccounts KlantaccountsGewijzigd
                , aanvinkVeld "Bestelgeschiedenis" "Alle eerdere bestellingen van uw klanten" model.orderhistorie OrderhistorieGewijzigd
                , aanvinkVeld "Nieuwsbrief-aanmeldingen" "De adressenlijst van uw nieuwsbrief" model.nieuwsbrief NieuwsbriefGewijzigd
                , aanvinkVeld "Voorraadaantallen" "De actuele voorraad per product" model.voorraad VoorraadGewijzigd
                , aanvinkVeld "Reviews / beoordelingen" "Uw opgebouwde productbeoordelingen" model.reviews ReviewsGewijzigd
                ]
            , div [ Attr.class "calc-check-group" ] <|
                [ span [ Attr.class "calc-label" ] [ text "Extra diensten en koppelingen" ] ]
                    ++ domeinEmailVelden model
                    ++ [ aanvinkVeld "Verzendkoppeling (bijv. DHL)" "Pakketten en labels rechtstreeks vanuit uw shop" model.verzendkoppeling VerzendkoppelingGewijzigd
                       , aanvinkVeld "B2B-kanaal (zakelijke klanten)" "Aparte prijzen en inlog voor zakelijke klanten" model.b2bKanaal B2bKanaalGewijzigd
                       , aanvinkVeld "Kassa / point-of-sale voor mijn fysieke winkel" "Verkopen in de winkel \u{00E9}n online met \u{00E9}\u{00E9}n systeem (Shopify POS)" model.pointOfSale PointOfSaleGewijzigd
                       ]
            ]
        , div [ Attr.class "calc-result" ] <|
            [ h3 [] [ text "Uw richtprijs" ]
            , uitsplitsing model
            , p [ Attr.class "calc-total" ]
                [ span [] [ text "Totaal (excl. BTW)" ]
                , strong [] [ text (formatteerEuro (totaalCenten model)) ]
                ]
            ]
                ++ opAanvraagNoten model
                ++ [ lockInNoot, contactVelden model, offerteKnop model ]
        ]


{-| De domein- en e-mailvragen tonen we alleen voor bronplatforms die die zaken
zelf bundelen, met de platformnaam erin. Bij een zelf-gehost of onbekend
platform laten we ze weg. -}
domeinEmailVelden : Model -> List (Html Msg)
domeinEmailVelden model =
    if bundeltDomeinEnEmail model.bron then
        [ aanvinkVeld
            ("Mijn domeinnaam staat nog bij " ++ bronOmschrijving model.bron)
            "Het internetadres van uw shop (bijv. uwshop.nl). Weet u het niet zeker? Dan zoeken we het samen uit."
            model.domeinBijMijnwebwinkel
            DomeinGewijzigd
        , aanvinkVeld
            ("Mijn e-mailadressen horen bij " ++ bronOmschrijving model.bron)
            "Bijvoorbeeld info@uwshop.nl die u via dat platform gebruikt"
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
        [ verplichtVeld model.offertePoging "Uw naam" "Voor- en achternaam" model.naam NaamGewijzigd
        , verplichtVeld model.offertePoging "Uw webshop (domeinnaam)" "bijv. uwshop.nl" model.webshopDomein WebshopDomeinGewijzigd
        ]


lockInNoot : Html Msg
lockInNoot =
    p [ Attr.class "calc-lockin" ]
        [ text "Dit is een richtprijs. Wilt u tegen deze prijs verhuizen? Vraag nu een offerte aan." ]


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
    "mailto:hallo@jappiesoftware.com?subject="
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
               , "Kunt u mij hiervoor een offerte sturen?"
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
