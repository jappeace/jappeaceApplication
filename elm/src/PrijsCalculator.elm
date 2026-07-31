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
import Html.Events exposing (onCheck, onInput)
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
    | BronAnders


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
    }


initieelModel : Model
initieelModel =
    { productenInvoer = "1000"
    , talenInvoer = "1"
    , bron = BronMijnwebwinkel
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initieelModel, Cmd.none )



-- UPDATE


type Msg
    = ProductenGewijzigd String
    | TalenGewijzigd String
    | BronGewijzigd String
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


leesBron : String -> BronPlatform
leesBron waarde =
    if waarde == "ccv" then
        BronCcvShop

    else if waarde == "lightspeed" then
        BronLightspeed

    else if waarde == "anders" then
        BronAnders

    else
        BronMijnwebwinkel


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

        BronAnders ->
            "anders"


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
        + indienAan model.domeinBijMijnwebwinkel domeinverhuizingCenten
        + indienAan model.emailBijMijnwebwinkel emailSetupCenten
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
            , keuzeOptie (bronNaarWaarde bron) "anders" (bronOmschrijving BronAnders)
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


optioneleP : Bool -> String -> Int -> List PrijsRegel
optioneleP toon omschrijving centen =
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
        ++ optioneleP
            (extraProducten model > 0)
            (aantalLabel (extraProducten model) "extra product \u{00D7} \u{20AC}0,25" "extra producten \u{00D7} \u{20AC}0,25")
            (extraProductenCenten model)
        ++ optioneleP
            (extraTalen model > 0)
            (aantalLabel (extraTalen model) "extra taal: vertaalwerk per product" "extra talen: vertaalwerk per product")
            (extraTaalProductenCenten model)
        ++ optioneleP
            (extraTalen model > 0)
            (aantalLabel (extraTalen model) "extra taal: configuratie \u{00D7} \u{20AC}250" "extra talen: configuratie \u{00D7} \u{20AC}250")
            (extraTaalConfiguratieCenten model)
        ++ themaRegels model
        ++ optioneleP model.klantaccounts "Klantaccounts meenemen" klantaccountsCenten
        ++ optioneleP model.orderhistorie "Bestelgeschiedenis meenemen" orderhistorieCenten
        ++ optioneleP model.nieuwsbrief "Nieuwsbrief-aanmeldingen meenemen" nieuwsbriefCenten
        ++ optioneleP model.voorraad "Voorraadaantallen live overzetten" voorraadCenten
        ++ optioneleP model.reviews "Reviews / beoordelingen overzetten" reviewsCenten
        ++ optioneleP model.domeinBijMijnwebwinkel "Domeinverhuizing" domeinverhuizingCenten
        ++ optioneleP model.emailBijMijnwebwinkel "E-mail-setup" emailSetupCenten
        ++ optioneleP model.verzendkoppeling "Verzendkoppeling (bijv. DHL)" verzendkoppelingCenten
        ++ optioneleP model.b2bKanaal "B2B-kanaal (zakelijke prijzen)" b2bKanaalCenten
        ++ optioneleP model.pointOfSale "Kassa / point-of-sale" pointOfSaleCenten


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


{-| Losse waarschuwingen voor keuzes die we niet kant-en-klaar kunnen prijzen:
een nieuw ontwerp en een onbekend bronplatform gaan altijd op aanvraag. -}
opAanvraagNoten : Model -> List (Html Msg)
opAanvraagNoten model =
    themaNoot model.thema ++ bronNoot model.bron ++ posNoot model.pointOfSale


{-| Bij point-of-sale komt de installatie op locatie: die en de reiskosten
rekenen we los, op aanvraag, dus ze zitten niet in het getoonde totaal. -}
posNoot : Bool -> List (Html Msg)
posNoot pos =
    if pos then
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
            , getalVeld "Hoeveel producten heeft uw webshop ongeveer?" model.productenInvoer "t/m 1.000 zit in de basisprijs" ProductenGewijzigd
            , getalVeld "In hoeveel talen staat uw webshop?" model.talenInvoer "1 taal zit in de basisprijs" TalenGewijzigd
            , bronVeld model.bron
            , themaVeld model.thema
            , div [ Attr.class "calc-check-group" ]
                [ span [ Attr.class "calc-label" ] [ text "Wat wilt u meenemen naar de nieuwe shop?" ]
                , aanvinkVeld "Klantaccounts" "Uw klanten houden hun eigen inlog" model.klantaccounts KlantaccountsGewijzigd
                , aanvinkVeld "Bestelgeschiedenis" "Alle eerdere bestellingen van uw klanten" model.orderhistorie OrderhistorieGewijzigd
                , aanvinkVeld "Nieuwsbrief-aanmeldingen" "De adressenlijst van uw nieuwsbrief" model.nieuwsbrief NieuwsbriefGewijzigd
                , aanvinkVeld "Voorraadaantallen" "De actuele voorraad per product" model.voorraad VoorraadGewijzigd
                , aanvinkVeld "Reviews / beoordelingen" "Uw opgebouwde productbeoordelingen" model.reviews ReviewsGewijzigd
                ]
            , div [ Attr.class "calc-check-group" ]
                [ span [ Attr.class "calc-label" ] [ text "Extra diensten en koppelingen" ]
                , aanvinkVeld "Mijn domeinnaam staat nog bij MijnWebwinkel" "Het internetadres van uw shop (bijv. uwshop.nl). Weet u het niet zeker? Dan zoeken we het samen uit." model.domeinBijMijnwebwinkel DomeinGewijzigd
                , aanvinkVeld "Mijn e-mailadressen horen bij MijnWebwinkel" "Bijvoorbeeld info@uwshop.nl die u via MijnWebwinkel gebruikt" model.emailBijMijnwebwinkel EmailGewijzigd
                , aanvinkVeld "Verzendkoppeling (bijv. DHL)" "Pakketten en labels rechtstreeks vanuit uw shop" model.verzendkoppeling VerzendkoppelingGewijzigd
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
                ++ [ lockInNoot, offerteKnop model ]
        ]


lockInNoot : Html Msg
lockInNoot =
    p [ Attr.class "calc-lockin" ]
        [ text "Dit is een richtprijs, geen offerte. Alleen een offerte legt uw prijs vast. Wilt u tegen deze prijs verhuizen? Vraag nu een offerte aan." ]


{-| Knop die een offerte-mail opent met alle gekozen opties al ingevuld, zodat
de bezoeker alleen nog op verzenden hoeft te drukken. -}
offerteKnop : Model -> Html Msg
offerteKnop model =
    a
        [ Attr.href (offerteMailtoUrl model)
        , Attr.class "cta-button calc-offerte"
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
         , "Aantal producten: " ++ String.fromInt (aantalProducten model)
         , "Aantal talen: " ++ String.fromInt (aantalTalen model)
         , "Huidig platform: " ++ bronOmschrijving model.bron
         , "Thema: " ++ themaOmschrijving model.thema
         , ""
         , "Prijsindicatie (excl. BTW):"
         ]
            ++ List.map prijsRegelTekst (prijsRegels model)
            ++ [ "Totaal: " ++ formatteerEuro (totaalCenten model) ]
            ++ posReiskostenRegel model
            ++ [ ""
               , "Kunt u mij hiervoor een offerte sturen?"
               ]
        )


prijsRegelTekst : PrijsRegel -> String
prijsRegelTekst prijsregel =
    "- " ++ prijsregel.omschrijving ++ ": " ++ formatteerEuro prijsregel.centen


{-| Reiskosten-voorbehoud voor point-of-sale, alleen als die gekozen is; het
staat los van het getoonde totaal. -}
posReiskostenRegel : Model -> List String
posReiskostenRegel model =
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
