module OfferteForm exposing
    ( Model
    , Msg(..)
    , initieelModel
    , main
    , samengesteldBericht
    , update
    , view
    )

{-| Het offerteformulier op webwinkelverhuis.nl/offerte.html.

De intakevragen die eerst als voorgevulde tekst in een textarea stonden
zijn losse invoervelden (besluit Jappie 4 sep 2026): losse velden vullen
makkelijker dan een tekstblok bewerken. Elm stelt er het bericht uit
samen, toont dat onder de velden zodat de bezoeker ziet wat er verstuurd
wordt, en stopt het als verborgen veld in een kaal HTML-formulier dat
native naar POST /api/offerte gaat; de server logt en mailt (megavid
WebshopScanner.OfferteAanvraag) en stuurt door naar de bedankpagina.
Geen ports of Cmd's nodig: het versturen is de browser-submit zelf, en
GA4's enhanced measurement meet form_start/form_submit vanzelf.

-}

import Browser
import Html exposing (Html, button, div, form, input, label, p, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onInput)


type alias Model =
    { emailInvoer : String
    , webshopDomein : String
    , huidigPlatform : String
    , gewenstPlatform : String
    , aantalProducten : String
    , aantalTalen : String
    , meenemen : String
    , domeinOfEmailBijMww : String
    , bijzonderheden : String
    }


initieelModel : Model
initieelModel =
    { emailInvoer = ""
    , webshopDomein = ""
    , huidigPlatform = ""
    , gewenstPlatform = ""
    , aantalProducten = ""
    , aantalTalen = ""
    , meenemen = ""
    , domeinOfEmailBijMww = ""
    , bijzonderheden = ""
    }


type Msg
    = EmailGewijzigd String
    | WebshopDomeinGewijzigd String
    | HuidigPlatformGewijzigd String
    | GewenstPlatformGewijzigd String
    | AantalProductenGewijzigd String
    | AantalTalenGewijzigd String
    | MeenemenGewijzigd String
    | DomeinOfEmailGewijzigd String
    | BijzonderhedenGewijzigd String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EmailGewijzigd waarde ->
            { model | emailInvoer = waarde }

        WebshopDomeinGewijzigd waarde ->
            { model | webshopDomein = waarde }

        HuidigPlatformGewijzigd waarde ->
            { model | huidigPlatform = waarde }

        GewenstPlatformGewijzigd waarde ->
            { model | gewenstPlatform = waarde }

        AantalProductenGewijzigd waarde ->
            { model | aantalProducten = waarde }

        AantalTalenGewijzigd waarde ->
            { model | aantalTalen = waarde }

        MeenemenGewijzigd waarde ->
            { model | meenemen = waarde }

        DomeinOfEmailGewijzigd waarde ->
            { model | domeinOfEmailBijMww = waarde }

        BijzonderhedenGewijzigd waarde ->
            { model | bijzonderheden = waarde }


{-| Het bericht dat meegaat in de aanvraag: dezelfde regels die vroeger
als invulsjabloon in de mailto-body stonden, nu met de veldwaarden
erachter. Lege velden reizen mee als lege regel; "niets ingevuld" is
zelf informatie voor de offerte.
-}
samengesteldBericht : Model -> String
samengesteldBericht model =
    String.join "\n"
        [ "- Huidig platform (bijv. MijnWebwinkel, CCV Shop): " ++ model.huidigPlatform
        , "- Gewenst platform (Shopify of WooCommerce): " ++ model.gewenstPlatform
        , "- Aantal producten (ongeveer): " ++ model.aantalProducten
        , "- Aantal talen: " ++ model.aantalTalen
        , "- Meenemen (klantaccounts, bestelgeschiedenis, nieuwsbrief, voorraad, reviews): " ++ model.meenemen
        , "- Domeinnaam of e-mail nog bij MijnWebwinkel?: " ++ model.domeinOfEmailBijMww
        , "- Bijzonderheden (kassa/point-of-sale, zakelijke klanten, verzendkoppeling): " ++ model.bijzonderheden
        ]


view : Model -> Html Msg
view model =
    form
        [ Attr.action "/api/offerte", Attr.method "post", Attr.class "offerte-formulier" ]
        [ emailVeld model
        , invoerVeld "Je webshop (domeinnaam)" "bijv. uwshop.nl" model.webshopDomein WebshopDomeinGewijzigd
        , invoerVeld "Huidig platform" "bijv. MijnWebwinkel, CCV Shop" model.huidigPlatform HuidigPlatformGewijzigd
        , invoerVeld "Gewenst platform" "Shopify of WooCommerce, of: weet ik nog niet" model.gewenstPlatform GewenstPlatformGewijzigd
        , invoerVeld "Aantal producten (ongeveer)" "bijv. 1.500" model.aantalProducten AantalProductenGewijzigd
        , invoerVeld "Aantal talen" "bijv. 1" model.aantalTalen AantalTalenGewijzigd
        , invoerVeld "Meenemen" "klantaccounts, bestelgeschiedenis, nieuwsbrief, voorraad, reviews" model.meenemen MeenemenGewijzigd
        , invoerVeld "Domeinnaam of e-mail nog bij MijnWebwinkel?" "ja / nee / weet ik niet" model.domeinOfEmailBijMww DomeinOfEmailGewijzigd
        , berichtVeld model
        , input [ Attr.type_ "hidden", Attr.name "bericht", Attr.value (samengesteldBericht model) ] []
        , input [ Attr.type_ "hidden", Attr.name "shop", Attr.value model.webshopDomein ] []
        , input [ Attr.type_ "hidden", Attr.name "website", Attr.value "" ] []
        , button [ Attr.type_ "submit", Attr.class "cta-button" ] [ text "Verstuur de aanvraag" ]
        ]


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


{-| Bewust zonder @Attr.name@: deze velden reizen niet los in de POST
mee maar alleen via het samengestelde bericht (het verborgen
bericht-veld in 'view'). Alleen email, bericht, shop en de honeypot
zijn echte formuliervelden. -}
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


{-| Bijzonderheden als vrij tekstveld, met daaronder het samengestelde
bericht zodat de bezoeker precies ziet wat er verstuurd wordt. -}
berichtVeld : Model -> Html Msg
berichtVeld model =
    div [ Attr.class "calc-field" ]
        [ label [ Attr.class "calc-field" ]
            [ span [ Attr.class "calc-label" ] [ text "Bijzonderheden" ]
            , textarea
                [ Attr.rows 3
                , Attr.placeholder "kassa/point-of-sale, zakelijke klanten, verzendkoppeling"
                , Attr.value model.bijzonderheden
                , onInput BijzonderhedenGewijzigd
                ]
                []
            ]
        , div [ Attr.class "offerte-bericht-voorbeeld" ]
            [ span [ Attr.class "calc-label" ] [ text "Dit bericht versturen we:" ]
            , p [ Attr.class "offerte-bericht-tekst" ] [ text (samengesteldBericht model) ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initieelModel
        , update = update
        , view = view
        }
