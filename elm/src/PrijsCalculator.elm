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
import Html exposing (Html, div, fieldset, h3, input, label, legend, li, option, p, select, span, strong, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onInput)



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


bronToeslagCenten : Int
bronToeslagCenten =
    25000


domeinverhuizingCenten : Int
domeinverhuizingCenten =
    25000


emailSetupCenten : Int
emailSetupCenten =
    15000



-- MODEL


{-| Waar draait de webshop nu? Bepaalt de bron-toeslag: MijnWebwinkel is de
basis (geen toeslag), CCV en Lightspeed kosten een vaste toeslag, en een ander
platform prijzen we op aanvraag omdat de import-laag dan per geval verschilt.
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
    , domeinBijMijnwebwinkel : Bool
    , emailBijMijnwebwinkel : Bool
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
    , domeinBijMijnwebwinkel = False
    , emailBijMijnwebwinkel = False
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
    | DomeinGewijzigd Bool
    | EmailGewijzigd Bool


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

        DomeinGewijzigd aan ->
            ( { model | domeinBijMijnwebwinkel = aan }, Cmd.none )

        EmailGewijzigd aan ->
            ( { model | emailBijMijnwebwinkel = aan }, Cmd.none )



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


bronToeslag : Model -> Int
bronToeslag model =
    case model.bron of
        BronMijnwebwinkel ->
            0

        BronCcvShop ->
            bronToeslagCenten

        BronLightspeed ->
            bronToeslagCenten

        BronAnders ->
            0


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
        + bronToeslag model
        + themaCenten model
        + indienAan model.klantaccounts klantaccountsCenten
        + indienAan model.orderhistorie orderhistorieCenten
        + indienAan model.nieuwsbrief nieuwsbriefCenten
        + indienAan model.voorraad voorraadCenten
        + indienAan model.domeinBijMijnwebwinkel domeinverhuizingCenten
        + indienAan model.emailBijMijnwebwinkel emailSetupCenten



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
            [ keuzeOptie (bronNaarWaarde bron) "mijnwebwinkel" "MijnWebwinkel"
            , keuzeOptie (bronNaarWaarde bron) "ccv" "CCV Shop"
            , keuzeOptie (bronNaarWaarde bron) "lightspeed" "Lightspeed"
            , keuzeOptie (bronNaarWaarde bron) "anders" "Een ander systeem / weet ik niet"
            ]
        ]


themaVeld : ThemaKeuze -> Html Msg
themaVeld thema =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Hoe moet uw nieuwe shop eruitzien?" ]
        , select [ onInput ThemaGewijzigd ]
            [ keuzeOptie (themaNaarWaarde thema) "standaard" "Standaard Shopify-thema, dat ik zelf inricht"
            , keuzeOptie (themaNaarWaarde thema) "overzetten" "Mijn huidige uitstraling, door ons 1-op-1 overgezet"
            , keuzeOptie (themaNaarWaarde thema) "nieuw" "Een volledig nieuw ontwerp door ons"
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


regel : String -> Int -> Html Msg
regel omschrijving centen =
    li [ Attr.class "calc-line" ]
        [ span [ Attr.class "calc-line-label" ] [ text omschrijving ]
        , span [ Attr.class "calc-line-price" ] [ text (formatteerEuro centen) ]
        ]


optioneleRegel : Bool -> String -> Int -> List (Html Msg)
optioneleRegel toon omschrijving centen =
    if toon then
        [ regel omschrijving centen ]

    else
        []


bronRegel : Model -> List (Html Msg)
bronRegel model =
    case model.bron of
        BronMijnwebwinkel ->
            []

        BronCcvShop ->
            [ regel "Bron-toeslag CCV Shop" bronToeslagCenten ]

        BronLightspeed ->
            [ regel "Bron-toeslag Lightspeed" bronToeslagCenten ]

        BronAnders ->
            []


themaRegel : Model -> List (Html Msg)
themaRegel model =
    case model.thema of
        ThemaStandaard ->
            []

        ThemaOverzetten ->
            [ regel "Huidige uitstraling 1-op-1 overzetten" themaOverzettenCenten ]

        ThemaNieuw ->
            []


uitsplitsing : Model -> Html Msg
uitsplitsing model =
    ul [ Attr.class "calc-lines" ] <|
        [ regel "Basismigratie (t/m 1.000 producten, 1 taal)" basisMigratieCenten ]
            ++ optioneleRegel
                (extraProducten model > 0)
                (String.fromInt (extraProducten model) ++ " extra producten \u{00D7} \u{20AC}0,25")
                (extraProductenCenten model)
            ++ optioneleRegel
                (extraTalen model > 0)
                (String.fromInt (extraTalen model) ++ " extra taal/talen: vertaalwerk per product")
                (extraTaalProductenCenten model)
            ++ optioneleRegel
                (extraTalen model > 0)
                (String.fromInt (extraTalen model) ++ " extra taal/talen: configuratie \u{00D7} \u{20AC}250")
                (extraTaalConfiguratieCenten model)
            ++ bronRegel model
            ++ themaRegel model
            ++ optioneleRegel model.klantaccounts "Klantaccounts meenemen" klantaccountsCenten
            ++ optioneleRegel model.orderhistorie "Bestelgeschiedenis meenemen" orderhistorieCenten
            ++ optioneleRegel model.nieuwsbrief "Nieuwsbrief-aanmeldingen meenemen" nieuwsbriefCenten
            ++ optioneleRegel model.voorraad "Voorraadaantallen live overzetten" voorraadCenten
            ++ optioneleRegel model.domeinBijMijnwebwinkel "Domeinverhuizing" domeinverhuizingCenten
            ++ optioneleRegel model.emailBijMijnwebwinkel "E-mail-setup" emailSetupCenten


{-| Toelichting bij de themakeuze. Het standaard-thema kost bij ons niets omdat
u het zelf (of via een ander) inricht; een nieuw ontwerp is los ontwerpwerk op
aanvraag. Bij 1-op-1 overzetten is geen extra uitleg nodig. -}
themaNoot : ThemaKeuze -> List (Html Msg)
themaNoot thema =
    case thema of
        ThemaStandaard ->
            [ p [ Attr.class "calc-note" ]
                [ text "Na de migratie staat uw shop op een standaard Shopify-thema dat u zelf inricht. Theming hoeft niet via ons: u kunt het zelf doen of een ontwerper naar keuze inhuren. Wij doen het ook, en zijn er inmiddels aardig goed in." ]
            ]

        ThemaNieuw ->
            [ p [ Attr.class "calc-note" ]
                [ text "Een volledig nieuw ontwerp is los ontwerpwerk. Dat prijzen we op aanvraag, dus het staat nog niet in het totaal." ]
            ]

        ThemaOverzetten ->
            []


{-| Waarschuwing bij een onbekend bronplatform: de prijs hangt af van hoe de
data eruit komt, dus het getoonde totaal is dan een ondergrens. -}
bronNoot : BronPlatform -> List (Html Msg)
bronNoot bron =
    case bron of
        BronAnders ->
            [ p [ Attr.class "calc-note" ]
                [ text "Bij een ander bronplatform hangt de prijs af van hoe uw data eruit komt. Dat bekijken we samen; het totaal hieronder is dan een ondergrens." ]
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
    themaNoot model.thema ++ bronNoot model.bron



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
                ]
            , aanvinkVeld "Mijn domeinnaam staat nog bij MijnWebwinkel" "Het internetadres van uw shop (bijv. uwshop.nl). Weet u het niet zeker? Dan zoeken we het samen uit." model.domeinBijMijnwebwinkel DomeinGewijzigd
            , aanvinkVeld "Mijn e-mailadressen horen bij MijnWebwinkel" "Bijvoorbeeld info@uwshop.nl die u via MijnWebwinkel gebruikt" model.emailBijMijnwebwinkel EmailGewijzigd
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
                ++ [ lockInNoot ]
        ]


lockInNoot : Html Msg
lockInNoot =
    p [ Attr.class "calc-lockin" ]
        [ text "Dit is een richtprijs, geen offerte. Alleen een offerte legt uw prijs vast. Wilt u tegen deze prijs verhuizen? Vraag nu een offerte aan." ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
