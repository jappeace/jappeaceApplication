module OfferteForm exposing
    ( Model
    , Msg(..)
    , initieelModel
    , main
    , samengesteldBericht
    , update
    , view
    )

{-| Het offerteformulier op webwinkelverhuis.nl/offerte.html: een
handvol intakevelden waaruit Elm het bericht samenstelt dat als
verborgen veld meereist in een kaal HTML-formulier naar POST
/api/offerte; de server logt en mailt (megavid
WebshopScanner.OfferteAanvraag) en stuurt door naar de bedankpagina.
De platform-dropdowns delen hun keuzelijsten met de rekenhulp
(PrijsCalculator.bronKeuzes/doelKeuzes), zodat er een lijst is.
-}

import Browser
import Html exposing (Html, button, form, input, label, select, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onInput)
import PrijsCalculator
    exposing
        ( BronPlatform(..)
        , DoelPlatform(..)
        , bronKeuzes
        , bronNaarWaarde
        , bronOmschrijving
        , doelKeuzes
        , doelNaarWaarde
        , doelOmschrijving
        , leesBron
        , leesDoel
        )


-- Decision: bewust maar vijf velden (besluit Jappie 4 sep 2026):
-- e-mailadres, domeinnaam, bron- en doel-dropdown en vrije
-- bijzonderheden. Alternatief was de volledige intake-vragenlijst uit
-- de oude mailto-body als velden (productaantal, talen, meenemen,
-- domein-bij-MijnWebwinkel); afgewezen omdat de domeinnaam genoeg is
-- om platform en catalogusomvang zelf uit te zoeken (we tellen het
-- exacte aantal bij de offerte toch na), meenemen in het gesprek aan
-- bod komt, en elke extra vraag conversie kost.
type alias Model =
    { emailInvoer : String
    , webshopDomein : String
    , bron : BronPlatform
    , doel : DoelPlatform
    , bijzonderheden : String
    }


initieelModel : Model
initieelModel =
    { emailInvoer = ""
    , webshopDomein = ""
    , bron = BronMijnwebwinkel
    , doel = DoelShopify
    , bijzonderheden = ""
    }


type Msg
    = EmailGewijzigd String
    | WebshopDomeinGewijzigd String
    | BronGewijzigd String
    | DoelGewijzigd String
    | BijzonderhedenGewijzigd String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EmailGewijzigd waarde ->
            { model | emailInvoer = waarde }

        WebshopDomeinGewijzigd waarde ->
            { model | webshopDomein = waarde }

        BronGewijzigd waarde ->
            { model | bron = leesBron waarde }

        DoelGewijzigd waarde ->
            { model | doel = leesDoel waarde }

        BijzonderhedenGewijzigd waarde ->
            { model | bijzonderheden = waarde }


{-| Het bericht dat meegaat in de aanvraag: de platformkeuzes met hun
leesbare labels plus de vrije bijzonderheden. De domeinnaam reist als
eigen shop-veld, het e-mailadres als eigen email-veld.
-}
samengesteldBericht : Model -> String
samengesteldBericht model =
    String.join "\n"
        [ "Huidig platform: " ++ bronOmschrijving model.bron
        , "Gewenst platform: " ++ doelOmschrijving model.doel
        , "Bijzonderheden: " ++ model.bijzonderheden
        ]


view : Model -> Html Msg
view model =
    form
        [ Attr.action "/api/offerte", Attr.method "post", Attr.class "offerte-formulier" ]
        [ emailVeld model
        , invoerVeld "Je webshop (domeinnaam)" "bijv. uwshop.nl" model.webshopDomein WebshopDomeinGewijzigd
        , keuzeVeld "Waar draait je webshop nu?" BronGewijzigd (bronNaarWaarde model.bron) bronKeuzes
        , keuzeVeld "Waar wil je naartoe?" DoelGewijzigd (doelNaarWaarde model.doel) doelKeuzes
        , bijzonderhedenVeld model
        , input [ Attr.type_ "hidden", Attr.name "bericht", Attr.value (samengesteldBericht model) ] []
        , input [ Attr.type_ "hidden", Attr.name "shop", Attr.value model.webshopDomein ] []
        , input [ Attr.type_ "hidden", Attr.name "website", Attr.value "" ] []
        , button [ Attr.type_ "submit", Attr.class "cta-button" ] [ text "Verstuur de aanvraag" ]
        ]


keuzeVeld : String -> (String -> Msg) -> String -> List ( String, String ) -> Html Msg
keuzeVeld veldLabel naarBericht huidigeWaarde keuzes =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text veldLabel ]
        , select [ onInput naarBericht ] (List.map (keuzeOptie huidigeWaarde) keuzes)
        ]


keuzeOptie : String -> ( String, String ) -> Html Msg
keuzeOptie huidigeWaarde ( waarde, omschrijving ) =
    Html.option
        [ Attr.value waarde
        , Attr.selected (huidigeWaarde == waarde)
        ]
        [ text omschrijving ]


emailVeld : Model -> Html Msg
emailVeld model =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Je e-mailadres (hierop ontvang je de offerte)" ]
        , input
            [ Attr.type_ "email"
            , Attr.name "email"
            , Attr.required True
            , Attr.value model.emailInvoer
            , Attr.placeholder "naam@voorbeeld.nl"
            , onInput EmailGewijzigd
            ]
            []
        ]


{-| Bewust zonder @Attr.name@: dit veld reist niet los in de POST mee
maar alleen via het samengestelde bericht (het verborgen bericht-veld
in 'view'). Alleen email, bericht, shop en de honeypot zijn echte
formuliervelden. -}
invoerVeld : String -> String -> String -> (String -> Msg) -> Html Msg
invoerVeld veldLabel plaatshouder waarde naarBericht =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text veldLabel ]
        , input
            [ Attr.type_ "text"
            , Attr.value waarde
            , Attr.placeholder plaatshouder
            , onInput naarBericht
            ]
            []
        ]


bijzonderhedenVeld : Model -> Html Msg
bijzonderhedenVeld model =
    label [ Attr.class "calc-field" ]
        [ span [ Attr.class "calc-label" ] [ text "Bijzonderheden (niet verplicht)" ]
        , textarea
            [ Attr.rows 3
            , Attr.placeholder "bijv. kassa in de winkel, zakelijke klanten, verzendkoppeling"
            , Attr.value model.bijzonderheden
            , onInput BijzonderhedenGewijzigd
            ]
            []
        ]


-- Decision: geen ports of Cmd's: het versturen is de native
-- browser-submit van het formulier, en GA4's enhanced measurement
-- meet form_start/form_submit vanzelf. Alternatief overwogen: een
-- eigen analytics-port zoals de rekenhulp (PrijsCalculator); afgewezen
-- omdat er hier geen rijkere parameters te melden zijn dan wat GA4 al
-- automatisch meet, en Browser.sandbox zonder ports de simpelste
-- correcte vorm is.
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initieelModel
        , update = update
        , view = view
        }
