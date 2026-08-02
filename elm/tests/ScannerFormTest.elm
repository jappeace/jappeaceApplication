module ScannerFormTest exposing (suite)

{-| Test het gedrag van de webshop-scanner via de echte functies uit
ScannerForm: de JSON-decoders tegen een fixture die het API-contract volgt,
de URL-normalisatie, de top-5/uitklap-selectie en de fase-overgangen via
update. De Cmd's (HTTP en analytics) zijn in elm-test niet te inspecteren; de
fases die ze aansturen wel.
-}

import Dict
import Expect
import Http
import Json.Decode as Decode
import ScannerForm
    exposing
        ( Fase(..)
        , Msg(..)
        , Oplosbaarheid(..)
        , Rapport
        , ScanId(..)
        , ScanStatus(..)
        , UitklapStand(..)
        , Verbeterpunt
        , WachtStand(..)
        , initieelModel
        , leesStartRespons
        , leesStatusRespons
        , normaliseerUrl
        , rapportDecoder
        , restVerbeterpunten
        , scanStatusDecoder
        , topVerbeterpunten
        , update
        )
import Test exposing (Test, describe, test)


{-| Fixture die het API-contract volgt, inclusief een score zonder waarde
(null) en een verbeterpunt zonder meetwaarde (null). -}
rapportJson : String
rapportJson =
    """
    {"url":"https://shop.example","platform":"Mijnwebwinkel","platformHerkend":true,
     "gemeten":"2026-08-02T09:15:00Z","lighthouseVersie":"13.0.3","metingen":1,
     "scores":[{"label":"Prestaties","score":54},{"label":"PWA","score":null}],
     "kernmetingen":[{"label":"Largest Contentful Paint","waarde":"8,4 s"}],
     "verbeterpunten":[
       {"categorie":"Prestaties","titel":"Trage serverreactie","meetwaarde":"Hoofddocument duurde 1.860 ms","waarom":"een trage eerste serverreactie remt alles","oplosbaar":false},
       {"categorie":"SEO","titel":"Meta description ontbreekt","meetwaarde":null,"waarom":"zoekmachines tonen dan willekeurige tekst","oplosbaar":true}
     ],
     "vastOpPlatform":1}
    """


verwachtRapport : Rapport
verwachtRapport =
    { url = "https://shop.example"
    , platform = "Mijnwebwinkel"
    , platformHerkend = True
    , gemeten = "2026-08-02T09:15:00Z"
    , lighthouseVersie = "13.0.3"
    , metingen = 1
    , scores =
        [ { label = "Prestaties", score = Just 54 }
        , { label = "PWA", score = Nothing }
        ]
    , kernmetingen = [ { label = "Largest Contentful Paint", waarde = "8,4 s" } ]
    , verbeterpunten =
        [ { categorie = "Prestaties"
          , titel = "Trage serverreactie"
          , meetwaarde = Just "Hoofddocument duurde 1.860 ms"
          , waarom = "een trage eerste serverreactie remt alles"
          , oplosbaar = VastOpPlatform
          }
        , { categorie = "SEO"
          , titel = "Meta description ontbreekt"
          , meetwaarde = Nothing
          , waarom = "zoekmachines tonen dan willekeurige tekst"
          , oplosbaar = Oplosbaar
          }
        ]
    , vastOpPlatform = 1
    }


{-| Genummerd verbeterpunt om de top-5-selectie mee te testen. -}
punt : Int -> Verbeterpunt
punt nummer =
    { categorie = "Prestaties"
    , titel = "Punt " ++ String.fromInt nummer
    , meetwaarde = Nothing
    , waarom = "omdat"
    , oplosbaar = Oplosbaar
    }


{-| HTTP-metadata voor een respons met de gegeven statuscode. -}
metadataMetStatus : Int -> Http.Metadata
metadataMetStatus statusCode =
    { url = "/api/scan"
    , statusCode = statusCode
    , statusText = ""
    , headers = Dict.empty
    }


decoderTests : Test
decoderTests =
    describe "JSON-decoders volgen het API-contract"
        [ test "rapportDecoder leest de fixture, incl. null score en null meetwaarde" <|
            \_ ->
                Expect.equal (Ok verwachtRapport)
                    (Decode.decodeString rapportDecoder rapportJson)
        , test "status wachtrij draagt de positie" <|
            \_ ->
                Expect.equal (Ok (StatusWachtrij 3))
                    (Decode.decodeString scanStatusDecoder """{"status":"wachtrij","positie":3}""")
        , test "status bezig" <|
            \_ ->
                Expect.equal (Ok StatusBezig)
                    (Decode.decodeString scanStatusDecoder """{"status":"bezig"}""")
        , test "status mislukt" <|
            \_ ->
                Expect.equal (Ok StatusMislukt)
                    (Decode.decodeString scanStatusDecoder """{"status":"mislukt"}""")
        , test "status klaar draagt het rapport" <|
            \_ ->
                Expect.equal (Ok (StatusKlaar verwachtRapport))
                    (Decode.decodeString scanStatusDecoder
                        ("""{"status":"klaar","rapport":""" ++ rapportJson ++ "}")
                    )
        , test "een onbekende status faalt in plaats van stil door te gaan" <|
            \_ ->
                Expect.err
                    (Decode.decodeString scanStatusDecoder """{"status":"verzonnen"}""")
        ]


responsTests : Test
responsTests =
    describe "HTTP-responsafhandeling"
        [ test "400 met fout-veld toont de servermelding" <|
            \_ ->
                Expect.equal (Err "Dat is geen geldige URL.")
                    (leesStartRespons
                        (Http.BadStatus_ (metadataMetStatus 400) """{"fout":"Dat is geen geldige URL."}""")
                    )
        , test "429 met fout-veld toont de servermelding" <|
            \_ ->
                Expect.equal (Err "De wachtrij is vol, probeer het later.")
                    (leesStartRespons
                        (Http.BadStatus_ (metadataMetStatus 429) """{"fout":"De wachtrij is vol, probeer het later."}""")
                    )
        , test "geslaagde start levert het scanId" <|
            \_ ->
                Expect.equal (Ok (ScanId "abc123"))
                    (leesStartRespons (Http.GoodStatus_ (metadataMetStatus 200) """{"scanId":"abc123"}"""))
        , test "404 bij het peilen geldt als mislukte scan" <|
            \_ ->
                Expect.equal (Ok StatusMislukt)
                    (leesStatusRespons (Http.BadStatus_ (metadataMetStatus 404) ""))
        ]


urlTests : Test
urlTests =
    describe "normaliseerUrl"
        [ test "kaal domein krijgt https:// ervoor" <|
            \_ ->
                Expect.equal (Ok "https://uwshop.nl") (normaliseerUrl "uwshop.nl")
        , test "witruimte wordt weggeknipt" <|
            \_ ->
                Expect.equal (Ok "https://uwshop.nl") (normaliseerUrl "  uwshop.nl  ")
        , test "bestaand http-schema blijft staan" <|
            \_ ->
                Expect.equal (Ok "http://uwshop.nl") (normaliseerUrl "http://uwshop.nl")
        , test "lege invoer wordt afgewezen" <|
            \_ ->
                Expect.err (normaliseerUrl "   ")
        , test "invoer met spaties wordt afgewezen" <|
            \_ ->
                Expect.err (normaliseerUrl "mijn winkel")
        , test "los woord zonder punt is geen webadres" <|
            \_ ->
                Expect.err (normaliseerUrl "uwshop")
        ]


topVijfTests : Test
topVijfTests =
    describe "top-5 en uitklaplijst"
        [ test "top toont maximaal vijf punten" <|
            \_ ->
                Expect.equal (List.map punt [ 1, 2, 3, 4, 5 ])
                    (topVerbeterpunten (List.map punt [ 1, 2, 3, 4, 5, 6, 7 ]))
        , test "de rest bevat precies de punten na de top 5" <|
            \_ ->
                Expect.equal (List.map punt [ 6, 7 ])
                    (restVerbeterpunten (List.map punt [ 1, 2, 3, 4, 5, 6, 7 ]))
        , test "bij vijf of minder punten is er niets uit te klappen" <|
            \_ ->
                Expect.equal []
                    (restVerbeterpunten (List.map punt [ 1, 2, 3 ]))
        ]


{-| Doorloop update tot en met een klaar-status en geef de eindfase terug. -}
faseNaKlaar : Fase
faseNaKlaar =
    let
        naStart =
            Tuple.first
                (update (StartOntvangen (Ok (ScanId "abc123")))
                    { initieelModel | invoer = "uwshop.nl", fase = Aanvragen }
                )
    in
    (Tuple.first (update (StatusOntvangen (Ok (StatusKlaar verwachtRapport))) naStart)).fase


updateTests : Test
updateTests =
    describe "fase-overgangen via update"
        [ test "geldige invoer start de aanvraag" <|
            \_ ->
                Expect.equal Aanvragen
                    (Tuple.first (update ScanAangevraagd { initieelModel | invoer = "uwshop.nl" })).fase
        , test "ongeldige invoer blijft in de invoerfase met een melding" <|
            \_ ->
                Expect.equal (Invoeren (Just "Vul het adres van uw webshop in."))
                    (Tuple.first (update ScanAangevraagd initieelModel)).fase
        , test "een wachtrij-status toont de positie" <|
            \_ ->
                Expect.equal (Wachten (ScanId "abc123") (InWachtrij 2))
                    (Tuple.first
                        (update (StatusOntvangen (Ok (StatusWachtrij 2)))
                            { initieelModel | fase = Wachten (ScanId "abc123") Bezig }
                        )
                    ).fase
        , test "een klaar-status rendert het rapport ingeklapt" <|
            \_ ->
                Expect.equal (Geslaagd verwachtRapport Ingeklapt) faseNaKlaar
        , test "uitklappen wisselt naar Uitgeklapt" <|
            \_ ->
                Expect.equal (Geslaagd verwachtRapport Uitgeklapt)
                    (Tuple.first
                        (update UitklapGewisseld
                            { initieelModel | fase = Geslaagd verwachtRapport Ingeklapt }
                        )
                    ).fase
        , test "een mislukt-status eindigt in de misluktfase" <|
            \_ ->
                Expect.equal True
                    (isMislukt
                        (Tuple.first
                            (update (StatusOntvangen (Ok StatusMislukt))
                                { initieelModel | fase = Wachten (ScanId "abc123") Bezig }
                            )
                        ).fase
                    )
        , test "opnieuw proberen keert terug naar de invoer" <|
            \_ ->
                Expect.equal (Invoeren Nothing)
                    (Tuple.first
                        (update OpnieuwGeprobeerd { initieelModel | fase = Mislukt "melding" })
                    ).fase
        ]


isMislukt : Fase -> Bool
isMislukt fase =
    case fase of
        Mislukt _ ->
            True

        Invoeren _ ->
            False

        Aanvragen ->
            False

        Wachten _ _ ->
            False

        Geslaagd _ _ ->
            False


suite : Test
suite =
    describe "ScannerForm"
        [ decoderTests
        , responsTests
        , urlTests
        , topVijfTests
        , updateTests
        ]
