port module ScannerForm exposing
    ( Fase(..)
    , Kernmeting
    , Model
    , Msg(..)
    , Oplosbaarheid(..)
    , Rapport
    , RapportStand
    , ScanId(..)
    , ScanStatus(..)
    , Score
    , UitklapStand(..)
    , Verbeterpunt
    , WachtStand(..)
    , ingeklapteRapportStand
    , initieelModel
    , leesStartRespons
    , leesStatusRespons
    , main
    , normaliseerUrl
    , rapportDecoder
    , verbeterpuntDecoder
    , restVerbeterpunten
    , scanStatusDecoder
    , topVerbeterpunten
    , update
    , view
    )

{-| "Beoordeel mijn webshop" op webwinkelverhuis.nl.

De bezoeker vult het adres van zijn webshop in; de server draait een scan
(1 tot 2 minuten) en dit programma peilt elke drie seconden de status tot het
rapport klaar is. Het rapport toont het herkende platform, de scores, de
kernmetingen (standaard ingeklapt) en de verbeterpunten, met bij elk punt of
het oplosbaar is of vastligt in het huidige platform. Ligt er iets vast, dan
volgt alleen het migratie-aanbod met een knop naar de prijs-rekenhulp; ligt
er niets vast, dan alleen het losse-klussen-aanbod met een gesprek-knop.

API-contract (zelfde origin):

  - POST /api/scan met {"url": ...} geeft {"scanId": ...}, of 400/429 met
    {"fout": "<Nederlandse melding>"}.
  - GET /api/scan/<scanId> geeft {"status":"wachtrij","positie":n} |
    {"status":"bezig"} | {"status":"mislukt"} | {"status":"klaar","rapport":...};
    404 betekent onbekend scanId en behandelen we als mislukt.

-}

import Browser
import Html exposing (Html, a, button, dd, div, dl, dt, form, h3, input, label, p, small, span, strong, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task
import Url


-- Decision: analytics loopt via dezelfde uitgaande Elm-port als de
-- prijscalculator (PrijsCalculator.elm): JS geeft de events door aan gtag en
-- raakt de Elm-state niet aan. Events: scanner_started bij het aanvragen,
-- scanner_klaar zodra een rapport op het scherm staat, scanner_mislukt bij
-- elke mislukking, gesprek_knop_klik voor de gesprek-knop en
-- scanner_naar_calculator voor de rekenhulp-knop in het rapport (de statische
-- ctaTrackScript van de pagina hangt zijn listeners aan DOMContentLoaded en
-- ziet door Elm gerenderde links niet).


{-| Stuurt een analytics-event naar JavaScript, waar de pagina het aan Google
Analytics (gtag) doorgeeft. De waarde is een object {name, params}. Niet
"analyticsEvent" genoemd: portnamen zijn globaal per programma en elm-test
compileert dit programma samen met PrijsCalculator, dus dezelfde naam botst. -}
port scannerAnalyticsEvent : Encode.Value -> Cmd msg


{-| Bouw een gtag-event met een naam en losse parameters. -}
gaEvent : String -> List ( String, Encode.Value ) -> Cmd msg
gaEvent naam params =
    scannerAnalyticsEvent
        (Encode.object
            [ ( "name", Encode.string naam )
            , ( "params", Encode.object params )
            ]
        )



-- RAPPORT


{-| Eén Lighthouse-categorie met score 0-100; de server stuurt null wanneer
een categorie geen score kreeg. -}
type alias Score =
    { label : String
    , score : Maybe Int
    }


type alias Kernmeting =
    { label : String
    , waarde : String
    }


{-| Of een verbeterpunt op het huidige platform op te lossen is. De server
stuurt een boolean; wij vertalen die direct naar deze naam zodat de rest van
de code niet hoeft te raden wat true betekent. -}
type Oplosbaarheid
    = Oplosbaar
    | VastOpPlatform


type alias Verbeterpunt =
    { categorie : String
    , titel : String
    , meetwaarde : Maybe String
    , waarom : String
    , zelfDoen : Maybe String
    , oplosbaar : Oplosbaarheid
    }


type alias Rapport =
    { url : String
    , platform : String
    , platformHerkend : Bool
    , gemeten : String
    , lighthouseVersie : String
    , metingen : Int
    , scores : List Score
    , kernmetingen : List Kernmeting
    , verbeterpunten : List Verbeterpunt
    , vastOpPlatform : Int
    }


-- Decision: een eigen pipeline-hulpje (metVeld) in plaats van de
-- NoRedInk/elm-json-decode-pipeline dependency: Rapport heeft tien velden
-- (boven de map8-grens) en dit ene hulpje van twee regels is alles wat die
-- bibliotheek ons zou geven.


{-| Pas het volgende veld toe in een decoder-pipeline:
`Decode.succeed Bouw |> metVeld "a" ... |> metVeld "b" ...`. -}
metVeld : String -> Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
metVeld naam veldDecoder functieDecoder =
    Decode.map2 (\functie waarde -> functie waarde) functieDecoder (Decode.field naam veldDecoder)


scoreDecoder : Decode.Decoder Score
scoreDecoder =
    Decode.map2 Score
        (Decode.field "label" Decode.string)
        (Decode.field "score" (Decode.nullable Decode.int))


kernmetingDecoder : Decode.Decoder Kernmeting
kernmetingDecoder =
    Decode.map2 Kernmeting
        (Decode.field "label" Decode.string)
        (Decode.field "waarde" Decode.string)


oplosbaarheidDecoder : Decode.Decoder Oplosbaarheid
oplosbaarheidDecoder =
    Decode.map
        (\vlag ->
            if vlag then
                Oplosbaar

            else
                VastOpPlatform
        )
        Decode.bool


verbeterpuntDecoder : Decode.Decoder Verbeterpunt
verbeterpuntDecoder =
    Decode.map6 Verbeterpunt
        (Decode.field "categorie" Decode.string)
        (Decode.field "titel" Decode.string)
        (Decode.field "meetwaarde" (Decode.nullable Decode.string))
        (Decode.field "waarom" Decode.string)
        zelfDoenDecoder
        (Decode.field "oplosbaar" oplosbaarheidDecoder)


{-| Het optionele gratis-tip-veld. Tolerant voor zowel null als een
ontbrekend veld, zodat de frontend ook tegen een oudere backend zonder
zelfDoen blijft werken (uitrolvolgorde maakt dan niet uit). -}
zelfDoenDecoder : Decode.Decoder (Maybe String)
zelfDoenDecoder =
    Decode.oneOf
        [ Decode.field "zelfDoen" (Decode.nullable Decode.string)
        , Decode.succeed Nothing
        ]


rapportDecoder : Decode.Decoder Rapport
rapportDecoder =
    Decode.succeed Rapport
        |> metVeld "url" Decode.string
        |> metVeld "platform" Decode.string
        |> metVeld "platformHerkend" Decode.bool
        |> metVeld "gemeten" Decode.string
        |> metVeld "lighthouseVersie" Decode.string
        |> metVeld "metingen" Decode.int
        |> metVeld "scores" (Decode.list scoreDecoder)
        |> metVeld "kernmetingen" (Decode.list kernmetingDecoder)
        |> metVeld "verbeterpunten" (Decode.list verbeterpuntDecoder)
        |> metVeld "vastOpPlatform" Decode.int



-- SCANSTATUS


{-| Ondoorzichtig scan-kenmerk dat de server bij het aanvragen teruggeeft en
waarmee we de status peilen. -}
type ScanId
    = ScanId String


type ScanStatus
    = StatusWachtrij Int
    | StatusBezig
    | StatusMislukt
    | StatusTijdelijkGeweigerd
    | StatusKlaar Rapport


scanStatusDecoder : Decode.Decoder ScanStatus
scanStatusDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen statusInhoudDecoder


statusInhoudDecoder : String -> Decode.Decoder ScanStatus
statusInhoudDecoder status =
    if status == "wachtrij" then
        Decode.map StatusWachtrij (Decode.field "positie" Decode.int)

    else if status == "bezig" then
        Decode.succeed StatusBezig

    else if status == "mislukt" then
        Decode.succeed StatusMislukt

    else if status == "tijdelijk-geweigerd" then
        Decode.succeed StatusTijdelijkGeweigerd

    else if status == "klaar" then
        Decode.map StatusKlaar (Decode.field "rapport" rapportDecoder)

    else
        Decode.fail ("Onbekende scanstatus: " ++ status)



-- MODEL


{-| Waar de bezoeker zich in de flow bevindt. Invoeren draagt een eventuele
foutmelding over de ingevulde URL; Wachten draagt het scan-kenmerk waarmee we
peilen. -}
type Fase
    = Invoeren (Maybe String)
    | Aanvragen
    | Wachten ScanId WachtStand
    | Geslaagd Rapport RapportStand
    | Mislukt String


{-| Wat de server over een lopende scan meldt: in de wachtrij (met positie,
1 = als eerste aan de beurt) of daadwerkelijk aan het meten. -}
type WachtStand
    = InWachtrij Int
    | Bezig


{-| Of een inklapbaar rapportdeel open- of dichtgeklapt staat. -}
type UitklapStand
    = Ingeklapt
    | Uitgeklapt


{-| De uitklapstand van de twee inklapbare rapportdelen: de kernmetingen en
de verbeterpunten na de top 5. Beide beginnen ingeklapt, zodat het rapport
opent met de scores en de top 5. -}
type alias RapportStand =
    { puntenUitklap : UitklapStand
    , kernmetingenUitklap : UitklapStand
    }


ingeklapteRapportStand : RapportStand
ingeklapteRapportStand =
    { puntenUitklap = Ingeklapt
    , kernmetingenUitklap = Ingeklapt
    }


type alias Model =
    { invoer : String
    , fase : Fase
    }


initieelModel : Model
initieelModel =
    { invoer = ""
    , fase = Invoeren Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initieelModel, Cmd.none )



-- URL-INVOER


legeInvoerMelding : String
legeInvoerMelding =
    "Vul het adres van uw webshop in."


ongeldigeInvoerMelding : String
ongeldigeInvoerMelding =
    "Dat lijkt geen webadres. Vul bijvoorbeeld uwshop.nl in."


{-| Maak van de invoer een volledige URL: witruimte eraf, https:// ervoor als
de bezoeker een kaal domein typte, en duidelijk ongeldige invoer (leeg, met
spaties, zonder punt in de domeinnaam) wordt afgewezen met een Nederlandse
melding. -}
normaliseerUrl : String -> Result String String
normaliseerUrl invoer =
    if String.trim invoer == "" then
        Err legeInvoerMelding

    else if String.contains " " (String.trim invoer) then
        Err ongeldigeInvoerMelding

    else
        valideerUrl (metSchema (String.trim invoer))


{-| Zet https:// voor een kaal domein; een al aanwezig http(s)-schema blijft
staan. -}
metSchema : String -> String
metSchema geschoond =
    if String.startsWith "http://" geschoond || String.startsWith "https://" geschoond then
        geschoond

    else
        "https://" ++ geschoond


{-| Accepteer alleen invoer die als URL parseert en een punt in de hostnaam
heeft: een webshop leeft op een domeinnaam, niet op een los woord. -}
valideerUrl : String -> Result String String
valideerUrl kandidaat =
    case Url.fromString kandidaat of
        Nothing ->
            Err ongeldigeInvoerMelding

        Just url ->
            if String.contains "." url.host then
                Ok kandidaat

            else
                Err ongeldigeInvoerMelding



-- HTTP


netwerkMelding : String
netwerkMelding =
    "We konden de server niet bereiken. Controleer uw verbinding en probeer het opnieuw."


serverMelding : String
serverMelding =
    "Er ging iets mis aan onze kant. Probeer het later opnieuw."


scanMisluktMelding : String
scanMisluktMelding =
    "De scan is niet gelukt. Probeer het over een paar minuten opnieuw."


startScan : String -> Cmd Msg
startScan url =
    Http.post
        { url = "/api/scan"
        , body = Http.jsonBody (Encode.object [ ( "url", Encode.string url ) ])
        , expect = Http.expectStringResponse StartOntvangen leesStartRespons
        }


{-| Lees het antwoord op POST /api/scan. Bij 400 (ongeldige URL) en 429
(wachtrij vol) tonen we de Nederlandse melding uit het "fout"-veld van de
server. -}
leesStartRespons : Http.Response String -> Result String ScanId
leesStartRespons respons =
    case respons of
        Http.BadUrl_ _ ->
            Err serverMelding

        Http.Timeout_ ->
            Err netwerkMelding

        Http.NetworkError_ ->
            Err netwerkMelding

        Http.BadStatus_ _ lichaam ->
            Err (foutmeldingUitLichaam lichaam)

        Http.GoodStatus_ _ lichaam ->
            case Decode.decodeString (Decode.field "scanId" Decode.string) lichaam of
                Ok ruweId ->
                    Ok (ScanId ruweId)

                Err _ ->
                    Err serverMelding


{-| De Nederlandse foutmelding uit een {"fout": ...}-antwoord, met een
generieke melding als het lichaam onleesbaar is. -}
foutmeldingUitLichaam : String -> String
foutmeldingUitLichaam lichaam =
    case Decode.decodeString (Decode.field "fout" Decode.string) lichaam of
        Ok melding ->
            melding

        Err _ ->
            serverMelding


peilStatus : ScanId -> Cmd Msg
peilStatus (ScanId ruweId) =
    Http.get
        { url = "/api/scan/" ++ ruweId
        , expect = Http.expectStringResponse StatusOntvangen leesStatusRespons
        }


{-| Lees het antwoord op GET /api/scan/<scanId>. Een 404 betekent volgens het
contract een onbekend scanId en behandelen we als mislukte scan. -}
leesStatusRespons : Http.Response String -> Result String ScanStatus
leesStatusRespons respons =
    case respons of
        Http.BadUrl_ _ ->
            Err serverMelding

        Http.Timeout_ ->
            Err netwerkMelding

        Http.NetworkError_ ->
            Err netwerkMelding

        Http.BadStatus_ metadata _ ->
            if metadata.statusCode == 404 then
                Ok StatusMislukt

            else
                Err serverMelding

        Http.GoodStatus_ _ lichaam ->
            case Decode.decodeString scanStatusDecoder lichaam of
                Ok status ->
                    Ok status

                Err _ ->
                    Err serverMelding


-- Decision: peilen via een Process.sleep-keten (elk statusantwoord plant het
-- volgende peilmoment) in plaats van een Time.every-subscription. Dat spaart
-- de elm/time-dependency uit en stopt vanzelf: zodra de fase geen Wachten
-- meer is plant niets een volgend peilmoment.


peilOverDrieSeconden : Cmd Msg
peilOverDrieSeconden =
    Process.sleep 3000 |> Task.perform (\_ -> PeilMoment)



-- UPDATE


type Msg
    = InvoerGewijzigd String
    | ScanAangevraagd
    | StartOntvangen (Result String ScanId)
    | PeilMoment
    | StatusOntvangen (Result String ScanStatus)
    | UitklapGewisseld
    | KernmetingenGewisseld
    | OpnieuwGeprobeerd
    | GesprekGeklikt
    | CalculatorGeklikt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InvoerGewijzigd waarde ->
            ( { model | invoer = waarde, fase = Invoeren Nothing }, Cmd.none )

        ScanAangevraagd ->
            case normaliseerUrl model.invoer of
                Err melding ->
                    ( { model | fase = Invoeren (Just melding) }, Cmd.none )

                Ok url ->
                    ( { model | fase = Aanvragen }
                    , Cmd.batch [ gaEvent "scanner_started" [], startScan url ]
                    )

        StartOntvangen (Ok scanId) ->
            ( { model | fase = Wachten scanId Bezig }, peilStatus scanId )

        StartOntvangen (Err melding) ->
            ( { model | fase = Mislukt melding }, gaEvent "scanner_mislukt" [] )

        PeilMoment ->
            ( model, peilCommando model.fase )

        StatusOntvangen resultaat ->
            verwerkStatus resultaat model

        UitklapGewisseld ->
            ( { model | fase = metRapportStand wisselPuntenUitklap model.fase }, Cmd.none )

        -- Bewust zonder analytics-event: de kernmetingen-knop is bladeren
        -- binnen het rapport, geen funnel-stap.
        KernmetingenGewisseld ->
            ( { model | fase = metRapportStand wisselKernmetingenUitklap model.fase }, Cmd.none )

        OpnieuwGeprobeerd ->
            ( { model | fase = Invoeren Nothing }, Cmd.none )

        GesprekGeklikt ->
            ( model, gaEvent "gesprek_knop_klik" [] )

        CalculatorGeklikt ->
            ( model, gaEvent "scanner_naar_calculator" [] )


{-| Peil alleen wanneer we nog op de scan wachten; in elke andere fase is het
peilmoment achterhaald (de bezoeker is bijvoorbeeld opnieuw begonnen). -}
peilCommando : Fase -> Cmd Msg
peilCommando fase =
    case fase of
        Wachten scanId _ ->
            peilStatus scanId

        Invoeren _ ->
            Cmd.none

        Aanvragen ->
            Cmd.none

        Geslaagd _ _ ->
            Cmd.none

        Mislukt _ ->
            Cmd.none


{-| Verwerk een statusantwoord. Antwoorden die binnenkomen terwijl we niet
meer wachten (de bezoeker begon opnieuw) zijn achterhaald en veranderen niets. -}
verwerkStatus : Result String ScanStatus -> Model -> ( Model, Cmd Msg )
verwerkStatus resultaat model =
    case model.fase of
        Wachten scanId _ ->
            case resultaat of
                Ok (StatusWachtrij positie) ->
                    ( { model | fase = Wachten scanId (InWachtrij positie) }, peilOverDrieSeconden )

                Ok StatusBezig ->
                    ( { model | fase = Wachten scanId Bezig }, peilOverDrieSeconden )

                Ok StatusMislukt ->
                    ( { model | fase = Mislukt scanMisluktMelding }, gaEvent "scanner_mislukt" [] )

                Ok StatusTijdelijkGeweigerd ->
                    -- De shop zelf weigerde de meting met een 429 (zie
                    -- megavid #145); eerlijk melden en later terugkomen
                    -- werkt beter dan een generiek "mislukt".
                    ( { model | fase = Mislukt tijdelijkGeweigerdMelding }
                    , gaEvent "scanner_geweigerd" []
                    )

                Ok (StatusKlaar rapport) ->
                    ( { model | fase = Geslaagd rapport ingeklapteRapportStand }
                    , gaEvent "scanner_klaar" [ ( "platform", Encode.string rapport.platform ) ]
                    )

                Err melding ->
                    ( { model | fase = Mislukt melding }, gaEvent "scanner_mislukt" [] )

        Invoeren _ ->
            ( model, Cmd.none )

        Aanvragen ->
            ( model, Cmd.none )

        Geslaagd _ _ ->
            ( model, Cmd.none )

        Mislukt _ ->
            ( model, Cmd.none )


{-| Pas de uitklapstand van het rapport aan; buiten de rapportfase is er
niets uit te klappen en verandert er niets. -}
metRapportStand : (RapportStand -> RapportStand) -> Fase -> Fase
metRapportStand pasAan fase =
    case fase of
        Geslaagd rapport stand ->
            Geslaagd rapport (pasAan stand)

        Invoeren mFout ->
            Invoeren mFout

        Aanvragen ->
            Aanvragen

        Wachten scanId stand ->
            Wachten scanId stand

        Mislukt melding ->
            Mislukt melding


wisselStand : UitklapStand -> UitklapStand
wisselStand stand =
    case stand of
        Ingeklapt ->
            Uitgeklapt

        Uitgeklapt ->
            Ingeklapt


wisselPuntenUitklap : RapportStand -> RapportStand
wisselPuntenUitklap stand =
    { stand | puntenUitklap = wisselStand stand.puntenUitklap }


wisselKernmetingenUitklap : RapportStand -> RapportStand
wisselKernmetingenUitklap stand =
    { stand | kernmetingenUitklap = wisselStand stand.kernmetingenUitklap }



-- TOP 5 EN UITKLAPPEN


{-| De vijf zwaarstwegende verbeterpunten; de server levert de lijst al
gesorteerd op impact aan. -}
topVerbeterpunten : List Verbeterpunt -> List Verbeterpunt
topVerbeterpunten punten =
    List.take 5 punten


{-| De punten na de top 5, zichtbaar na uitklappen. -}
restVerbeterpunten : List Verbeterpunt -> List Verbeterpunt
restVerbeterpunten punten =
    List.drop 5 punten



-- VIEW


meetUrl : String
meetUrl =
    "https://meet.jappiesoftware.com"


{-| De prijs-rekenhulp op de eigen prijzenpagina, dezelfde bestemming als de
rekenhulp-knoppen elders op de site (zie WebwinkelTemplates.hs). -}
rekenhulpUrl : String
rekenhulpUrl =
    "/prijzen.html#rekenhulp"


view : Model -> Html Msg
view model =
    div [ Attr.class "webshop-scanner" ] (faseWeergave model)


faseWeergave : Model -> List (Html Msg)
faseWeergave model =
    case model.fase of
        Invoeren mFout ->
            invoerWeergave model.invoer mFout

        Aanvragen ->
            laadWeergave "We starten de scan."

        Wachten _ stand ->
            laadWeergave (wachtTekst stand)

        Geslaagd rapport stand ->
            if rapport.platformHerkend then
                rapportWeergave rapport stand

            else
                nietHerkendWeergave rapport model.invoer

        Mislukt melding ->
            misluktWeergave melding


invoerWeergave : String -> Maybe String -> List (Html Msg)
invoerWeergave invoer mFout =
    [ form [ Attr.class "scanner-form", onSubmit ScanAangevraagd ]
        [ label [ Attr.class "calc-field" ]
            [ span [ Attr.class "calc-label" ] [ text "Het adres van uw webshop" ]
            , input
                ([ Attr.type_ "text"
                 , Attr.value invoer
                 , Attr.placeholder "uwshop.nl"
                 , onInput InvoerGewijzigd
                 ]
                    ++ invoerFoutKlasse mFout
                )
                []
            , invoerFoutMelding mFout
            ]
        , button [ Attr.type_ "submit", Attr.class "cta-button" ] [ text "Beoordeel mijn webshop" ]
        , p [ Attr.class "calc-hint" ] [ text "De meting duurt ongeveer een minuut." ]
        ]
    ]


invoerFoutKlasse : Maybe String -> List (Html.Attribute msg)
invoerFoutKlasse mFout =
    case mFout of
        Just _ ->
            [ Attr.class "calc-veld-fout" ]

        Nothing ->
            []


invoerFoutMelding : Maybe String -> Html msg
invoerFoutMelding mFout =
    case mFout of
        Just melding ->
            span [ Attr.class "calc-fout" ] [ text melding ]

        Nothing ->
            text ""


laadWeergave : String -> List (Html Msg)
laadWeergave statusRegel =
    [ div [ Attr.class "scanner-laden" ]
        [ div [ Attr.class "scanner-spinner" ] []
        , p [ Attr.class "scanner-status" ] [ text statusRegel ]
        , p [ Attr.class "calc-hint" ] [ text "De meting duurt ongeveer een minuut. Laat deze pagina open." ]
        ]
    ]


wachtTekst : WachtStand -> String
wachtTekst stand =
    case stand of
        InWachtrij positie ->
            if positie == 1 then
                "U bent als eerste aan de beurt."

            else
                "U staat op plek " ++ String.fromInt positie ++ " in de wachtrij."

        Bezig ->
            "We meten uw webshop."


{-| Melding wanneer de gescande shop onze meting tijdelijk weigert. -}
tijdelijkGeweigerdMelding : String
tijdelijkGeweigerdMelding =
    "Deze shop beperkt tijdelijk onze metingen (te veel verzoeken kort na elkaar). Probeer het over een uur opnieuw."


misluktWeergave : String -> List (Html Msg)
misluktWeergave melding =
    [ div [ Attr.class "scanner-mislukt" ]
        [ p [ Attr.class "calc-fout" ] [ text melding ]
        , button [ Attr.class "cta-button-secondary", onClick OpnieuwGeprobeerd ] [ text "Probeer opnieuw" ]
        ]
    ]



-- RAPPORTWEERGAVE


rapportWeergave : Rapport -> RapportStand -> List (Html Msg)
rapportWeergave rapport stand =
    [ h3 [ Attr.class "scanner-rapport-kop" ] [ text ("Rapport voor " ++ rapport.url) ]
    , p [ Attr.class "scanner-platform" ] [ text (platformRegel rapport) ]
    , div [ Attr.class "scanner-scores" ] (List.map scoreWeergave rapport.scores)
    ]
        ++ kernmetingenDeel rapport stand.kernmetingenUitklap
        ++ [ h3 [] [ text "Verbeterpunten" ] ]
        ++ List.map (puntWeergave rapport.platform) (topVerbeterpunten rapport.verbeterpunten)
        ++ uitklapDeel rapport stand.puntenUitklap
        ++ upsellBlok rapport


-- Decision: een niet-herkend platform krijgt geen rapport (geen scores,
-- verbeterpunten of aanbod), alleen "Platform niet herkend." en de
-- zoekbox opnieuw. Gekozen door Jappie (2 aug 2026) boven het alternatief
-- (het volledige Lighthouse-rapport tonen met "platform: niet herkend"
-- erboven, zoals het eerst deed): wij fixeren op de ondersteunde
-- webshopplatformen en willen geen generiek Lighthouse-loket zijn, en
-- zonder herkend platform kunnen we oplosbaar-versus-vast toch niet
-- duiden, dus het rapport zou leuren zonder advies te dragen.


{-| Vervangt het volledige rapport wanneer de scanner het platform niet
herkent: alleen de melding en een nieuwe zoekbox. Het formulier is hetzelfde
als op de startfase ('invoerWeergave'), met het gescande adres nog
ingevuld zodat een tikfout snel hersteld is; 'ScanAangevraagd' werkt
vanuit elke fase, dus opnieuw scannen werkt hiervandaan direct. -}
nietHerkendWeergave : Rapport -> String -> List (Html Msg)
nietHerkendWeergave rapport invoer =
    [ h3 [ Attr.class "scanner-rapport-kop" ] [ text ("Rapport voor " ++ rapport.url) ]
    , p [ Attr.class "scanner-platform" ] [ strong [] [ text "Platform niet herkend." ] ]
    , p []
        [ text "Wij beoordelen webshops op MijnWebwinkel, CCV Shop, Lightspeed, WooCommerce en Shopify. Draait uw shop ergens anders, dan kunnen wij er weinig zinnigs over zeggen. Controleer het adres of probeer een andere shop:" ]
    ]
        ++ invoerWeergave invoer Nothing
        ++ [ p [ Attr.class "scanner-platform-verzoek" ]
                [ text "Draait uw shop op een platform dat wij nog niet kennen? "
                , a [ Attr.href (platformVerzoekMailto rapport.url) ]
                    [ text "Mail ons welk platform u gebruikt" ]
                , text "; bij genoeg vraag voegen we het toe."
                ]
           ]


{-| Mailto voor een platform-ondersteuningsverzoek, met het gescande
adres voor-ingevuld zodat wij meteen kunnen fingerprinten wat er draait. -}
platformVerzoekMailto : String -> String
platformVerzoekMailto gescandeUrl =
    "mailto:jappie@webwinkelverhuis.nl?subject="
        ++ Url.percentEncode "Scanner: mijn platform wordt niet herkend"
        ++ "&body="
        ++ Url.percentEncode
            ("Hallo,\n\nIk scande "
                ++ gescandeUrl
                ++ " maar het platform werd niet herkend.\nMijn shop draait op: \n\nGroet,"
            )


{-| De kernmetingen, standaard ingeklapt: de meeste bezoekers hebben genoeg
aan de scores en de verbeterpunten, en de ruwe metingen blijven een klik weg.
De labels komen van de server en tonen we onvertaald. -}
kernmetingenDeel : Rapport -> UitklapStand -> List (Html Msg)
kernmetingenDeel rapport uitklap =
    case uitklap of
        Ingeklapt ->
            [ h3 [] [ text "Kernmetingen" ]
            , uitklapKnop KernmetingenGewisseld "Toon de kernmetingen"
            ]

        Uitgeklapt ->
            [ h3 [] [ text "Kernmetingen" ]
            , dl [ Attr.class "scanner-kernmetingen" ] (List.concatMap kernmetingWeergave rapport.kernmetingen)
            , uitklapKnop KernmetingenGewisseld "Verberg de kernmetingen"
            ]


platformRegel : Rapport -> String
platformRegel rapport =
    if rapport.platformHerkend then
        "Platform: " ++ rapport.platform

    else
        "Platform: niet herkend"


scoreWeergave : Score -> Html Msg
scoreWeergave score =
    div [ Attr.class ("scanner-score " ++ scoreKlasse score.score) ]
        [ span [ Attr.class "scanner-score-getal" ] [ text (scoreTekst score.score) ]
        , span [ Attr.class "scanner-score-label" ] [ text score.label ]
        ]


scoreTekst : Maybe Int -> String
scoreTekst mScore =
    case mScore of
        Just getal ->
            String.fromInt getal

        Nothing ->
            "\u{2013}"


-- Decision: de kleurgrenzen volgen Lighthouse zelf (0-49 rood, 50-89 oranje,
-- 90-100 groen), zodat ons rapport dezelfde kleur geeft als wat de bezoeker
-- in andere Lighthouse-rapporten ziet.
scoreKlasse : Maybe Int -> String
scoreKlasse mScore =
    case mScore of
        Nothing ->
            "scanner-score-onbekend"

        Just getal ->
            if getal >= 90 then
                "scanner-score-goed"

            else if getal >= 50 then
                "scanner-score-matig"

            else
                "scanner-score-slecht"


kernmetingWeergave : Kernmeting -> List (Html Msg)
kernmetingWeergave kernmeting =
    [ dt [] [ text kernmeting.label ]
    , dd [] [ text kernmeting.waarde ]
    ]


puntWeergave : String -> Verbeterpunt -> Html Msg
puntWeergave platformNaam punt =
    div [ Attr.class "scanner-punt" ]
        [ div [ Attr.class "scanner-punt-kop" ]
            [ strong [] [ text punt.titel ]
            , oplosbaarBadge platformNaam punt.oplosbaar
            ]
        , small [ Attr.class "scanner-punt-categorie" ] [ text punt.categorie ]
        , meetwaardeWeergave punt.meetwaarde
        , p [ Attr.class "scanner-punt-waarom" ] [ text punt.waarom ]
        , zelfDoenWeergave punt.zelfDoen
        ]


{-| De zelf-doen-tip, alleen aanwezig wanneer de server zeker is
van een suggestie (besluit Jappie, 3 aug 2026: het waarom leggen we
altijd uit, een suggestie alleen als hij zeker klopt). -}
zelfDoenWeergave : Maybe String -> Html Msg
zelfDoenWeergave mTip =
    case mTip of
        Just tip ->
            p [ Attr.class "scanner-punt-tip" ]
                [ strong [] [ text "Tip: " ], text tip ]

        Nothing ->
            text ""


meetwaardeWeergave : Maybe String -> Html Msg
meetwaardeWeergave mWaarde =
    case mWaarde of
        Just waarde ->
            p [ Attr.class "scanner-punt-meetwaarde" ] [ text waarde ]

        Nothing ->
            text ""


oplosbaarBadge : String -> Oplosbaarheid -> Html Msg
oplosbaarBadge platformNaam oplosbaarheid =
    case oplosbaarheid of
        Oplosbaar ->
            span [ Attr.class "scanner-badge scanner-badge-oplosbaar" ] [ text "Oplosbaar" ]

        VastOpPlatform ->
            span [ Attr.class "scanner-badge scanner-badge-vast" ] [ text ("Ligt vast in " ++ platformNaam) ]


uitklapDeel : Rapport -> UitklapStand -> List (Html Msg)
uitklapDeel rapport uitklap =
    if List.isEmpty (restVerbeterpunten rapport.verbeterpunten) then
        []

    else
        case uitklap of
            Ingeklapt ->
                [ uitklapKnop UitklapGewisseld ("Toon alle " ++ String.fromInt (List.length rapport.verbeterpunten) ++ " punten") ]

            Uitgeklapt ->
                List.map (puntWeergave rapport.platform) (restVerbeterpunten rapport.verbeterpunten)
                    ++ [ uitklapKnop UitklapGewisseld "Verberg de extra punten" ]


uitklapKnop : Msg -> String -> Html Msg
uitklapKnop bericht tekst =
    button [ Attr.class "cta-button-secondary scanner-uitklap", onClick bericht ] [ text tekst ]



-- UPSELLS


-- Decision: de upsell-voorwaarden en het badge-label komen uit de
-- verbeterpuntenlijst zelf (telling van oplosbaar/vast), niet uit het losse
-- vastOpPlatform-veld van de server, zodat badges en blokken nooit kunnen
-- afwijken van wat er op het scherm staat.

-- Decision: precies een upsell-blok, nooit twee (twee keer een aanbod onder
-- een rapport leest als leuren). Ligt er iets vast in het platform, dan is
-- verhuizen het antwoord en gaat de knop naar de prijs-rekenhulp: de
-- bezoeker wil op dat moment een prijs, geen agenda. Ligt er niets vast, dan
-- is het aanbod de losse klussen en blijft de gesprek-knop.


aantalVast : Rapport -> Int
aantalVast rapport =
    List.length (List.filter (\punt -> punt.oplosbaar == VastOpPlatform) rapport.verbeterpunten)


heeftOplosbarePunten : Rapport -> Bool
heeftOplosbarePunten rapport =
    List.any (\punt -> punt.oplosbaar == Oplosbaar) rapport.verbeterpunten


{-| Het ene upsell-blok onder het rapport: migratie zodra er punten
vastliggen, anders losse klussen. -}
upsellBlok : Rapport -> List (Html Msg)
upsellBlok rapport =
    if aantalVast rapport > 0 then
        migratieBlok rapport

    else
        oplosBlok rapport


{-| Migratie-aanbod met een knop naar de prijs-rekenhulp op /prijzen. -}
migratieBlok : Rapport -> List (Html Msg)
migratieBlok rapport =
    [ div [ Attr.class "scanner-upsell scanner-upsell-migratie" ]
        [ h3 [] [ text "Vast aan uw platform" ]
        , p [] [ text (vastZin (aantalVast rapport) rapport.platform) ]
        , a [ Attr.href rekenhulpUrl, Attr.class "cta-button", onClick CalculatorGeklikt ]
            [ text "Bereken uw verhuisprijs" ]
        ]
    ]


vastZin : Int -> String -> String
vastZin aantal platformNaam =
    if aantal == 1 then
        "1 punt ligt vast in " ++ platformNaam ++ ". Verhuizen lost het op."

    else
        String.fromInt aantal ++ " punten liggen vast in " ++ platformNaam ++ ". Verhuizen lost ze op."


{-| Losse-klussen-aanbod voor de punten die op het huidige platform
oplosbaar zijn; alleen getoond als er niets vastligt. -}
oplosBlok : Rapport -> List (Html Msg)
oplosBlok rapport =
    if heeftOplosbarePunten rapport then
        [ div [ Attr.class "scanner-upsell" ]
            [ h3 [] [ text "Liever laten doen?" ]
            , p [] [ text "De oplosbare punten pakken wij voor u op, tegen een vaste prijs." ]
            , a [ Attr.href meetUrl, Attr.class "cta-button", onClick GesprekGeklikt ]
                [ text "Plan een gesprek" ]
            ]
        ]

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
