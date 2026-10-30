module PricingTest exposing (groteCatalogusSuite, staffelSegmentenSuite, suite, weergaveSuite, zelfImportSuite)

{-| Test dat de prijsberekening van de calculator gelijk blijft aan de tabel op
/prijzen (en dus aan standaard-prijslijst.org). Deze test faalt zodra de
staffel of een moduleprijs afwijkt van de gepubliceerde bedragen; hij toetst
logica, geen tekst. De verwachte totalen zijn de rekenvoorbeelden van de
prijzenpagina, in centen.
-}

import Expect
import PrijsCalculator
    exposing
        ( BronPlatform(..)
        , DoelPlatform(..)
        , Model
        , Msg(..)
        , ThemaKeuze(..)
        , Weergave(..)
        , getoondeKeuzes
        , heeftProductExport
        , initieelModel
        , isGroteCatalogus
        , leesQuery
        , itemStaffelSegmenten
        , meerPuntenDanAccounts
        , Regelniveau(..)
        , orderhistorieStaffel
        , prijsRegels
        , productStaffelSegmenten
        , spaarpuntenStaffel
        , totaalCenten
        , update
        , zelfImportActief
        )
import Test exposing (Test, describe, test)


metProducten : Int -> Int -> Model -> Model
metProducten producten talen model =
    { model
        | productenInvoer = String.fromInt producten
        , talenInvoer = String.fromInt talen
    }


groteCatalogusSuite : Test
groteCatalogusSuite =
    describe "PrijsCalculator.isGroteCatalogus (grens 100.000 productvertalingen, vangnet tegen absurde invoer)"
        [ test "99.999 producten in 1 taal toont gewoon een prijs" <|
            \_ ->
                Expect.equal False (isGroteCatalogus (metProducten 99999 1 initieelModel))
        , test "100.000 producten in 1 taal raakt het vangnet" <|
            \_ ->
                Expect.equal True (isGroteCatalogus (metProducten 100000 1 initieelModel))
        , test "producten maal talen telt mee: 40.000 producten in 3 talen raakt het vangnet" <|
            \_ ->
                Expect.equal True (isGroteCatalogus (metProducten 40000 3 initieelModel))
        , test "het oude 50.000-anker toont nu gewoon zijn prijs: 6.099" <|
            \_ ->
                Expect.all
                    [ \model -> Expect.equal False (isGroteCatalogus model)
                    , \model -> Expect.equal 609900 (totaalCenten model)
                    ]
                    (metProducten 50000 1 initieelModel)
        ]


staffelSegmentenSuite : Test
staffelSegmentenSuite =
    describe "staffelsegmenten: de stappen die de bezoeker te zien krijgt"
        [ test "5.000 producten: drie stappen, gelijke buurtreden samengevoegd (1-500 om 20, 501-1.500 om 15, 1.501-5.000 om 10)" <|
            \_ ->
                Expect.equal
                    [ { van = 1, tot = 500, tariefCenten = 20 }
                    , { van = 501, tot = 1500, tariefCenten = 15 }
                    , { van = 1501, tot = 5000, tariefCenten = 10 }
                    ]
                    (productStaffelSegmenten 5000)
        , test "1.170 producten eindigt midden in een trede: 1-500, 501-1.170" <|
            \_ ->
                Expect.equal
                    [ { van = 1, tot = 500, tariefCenten = 20 }
                    , { van = 501, tot = 1170, tariefCenten = 15 }
                    ]
                    (productStaffelSegmenten 1170)
        , test "40 producten: één stap" <|
            \_ ->
                Expect.equal [ { van = 1, tot = 40, tariefCenten = 20 } ] (productStaffelSegmenten 40)
        , test "0 producten: geen stappen" <|
            \_ ->
                Expect.equal [] (productStaffelSegmenten 0)
        , test "de som van de stappen is het staffelbedrag: 5.000 producten = 600" <|
            \_ ->
                Expect.equal 60000
                    (List.sum (List.map (\seg -> (seg.tot - seg.van + 1) * seg.tariefCenten) (productStaffelSegmenten 5000)))
        , test "bestelgeschiedenis 11.504: boven de inbegrepen 1.000 eerst 10.000 om 8ct, dan 504 om 4ct" <|
            \_ ->
                Expect.equal
                    [ { van = 1001, tot = 11000, tariefCenten = 8 }
                    , { van = 11001, tot = 11504, tariefCenten = 4 }
                    ]
                    (itemStaffelSegmenten orderhistorieStaffel 11504)
        , test "spaarpunten 20.000: één vlakke stap om 5ct boven de inbegrepen 1.000, geen lagere trede" <|
            \_ ->
                Expect.equal
                    [ { van = 1001, tot = 20000, tariefCenten = 5 } ]
                    (itemStaffelSegmenten spaarpuntenStaffel 20000)
        , test "uitsplitsing spaarpunten 3.000 bij 4.850 accounts: hoofdregel 100 met de eerste 1.000 op nul en één stap eronder" <|
            \_ ->
                let
                    regels =
                        List.drop 6 (prijsRegels { initieelModel | klantaccounts = True, klantaccountsInvoer = "4850", spaarpunten = True, spaarpuntenInvoer = "3000" })
                in
                Expect.equal
                    [ ( Hoofdregel, 10000 ), ( Subregel, 0 ), ( Subregel, 10000 ) ]
                    (List.map (\r -> ( r.niveau, r.centen )) regels)
        , test "bestelgeschiedenis 115.000: 8ct tot 11.000, 4ct tot 20.000, 2ct tot 40.000, daarboven 1ct" <|
            \_ ->
                Expect.equal
                    [ { van = 1001, tot = 11000, tariefCenten = 8 }
                    , { van = 11001, tot = 20000, tariefCenten = 4 }
                    , { van = 20001, tot = 40000, tariefCenten = 2 }
                    , { van = 40001, tot = 115000, tariefCenten = 1 }
                    ]
                    (itemStaffelSegmenten orderhistorieStaffel 115000)
        , test "bestelgeschiedenis 800: niets boven de inbegrepen items, geen stappen" <|
            \_ ->
                Expect.equal [] (itemStaffelSegmenten orderhistorieStaffel 800)
        , test "uitsplitsing 5.000 producten: na de basis een hoofdregel met het staffeltotaal en drie subregels die er precies op optellen" <|
            \_ ->
                let
                    regels =
                        prijsRegels (metProducten 5000 1 initieelModel)

                    niveaus =
                        List.map .niveau regels

                    subCenten =
                        List.sum (List.map .centen (List.filter (\r -> r.niveau == Subregel) regels))

                    hoofdCenten =
                        List.map .centen (List.filter (\r -> r.niveau == Hoofdregel) regels)
                in
                Expect.all
                    [ \_ -> Expect.equal [ Hoofdregel, Hoofdregel, Subregel, Subregel, Subregel ] niveaus
                    , \_ -> Expect.equal [ 99900, 60000 ] hoofdCenten
                    , \_ -> Expect.equal 60000 subCenten
                    ]
                    ()
        , test "uitsplitsing bestelgeschiedenis 11.504: hoofdregel 920,16 met vaste deel en twee stappen eronder, som gelijk" <|
            \_ ->
                let
                    regels =
                        List.drop 3 (prijsRegels { initieelModel | orderhistorie = True, bestellingenInvoer = "11504" })
                in
                Expect.equal
                    [ ( Hoofdregel, 92016 ), ( Subregel, 10000 ), ( Subregel, 80000 ), ( Subregel, 2016 ) ]
                    (List.map (\r -> ( r.niveau, r.centen )) regels)
        , test "het totaal is de som van de hoofdregels alleen (subregels tellen niet dubbel)" <|
            \_ ->
                let
                    model =
                        { initieelModel | productenInvoer = "5000", orderhistorie = True, bestellingenInvoer = "11504", klantaccounts = True, klantaccountsInvoer = "4850", pointOfSale = True }
                in
                Expect.equal (totaalCenten model)
                    (List.sum (List.map .centen (List.filter (\r -> r.niveau == Hoofdregel) (prijsRegels model))))
        ]


suite : Test
suite =
    describe "PrijsCalculator.totaalCenten"
        [ test "basis: 500 producten, 1 taal, geen modules = 1.099 (999 + 500 om 20ct)" <|
            \_ ->
                Expect.equal 109900 (totaalCenten initieelModel)
        , test "40 producten, 1 taal = 1.007 (elk product telt vanaf de eerste)" <|
            \_ ->
                Expect.equal 100700
                    (totaalCenten (metProducten 40 1 initieelModel))
        , test "0 producten = precies de basis van 999" <|
            \_ ->
                Expect.equal 99900
                    (totaalCenten (metProducten 0 1 initieelModel))
        , test "1.000 producten, 1 taal = 1.174 (500 om 20ct, 500 om 15ct)" <|
            \_ ->
                Expect.equal 117400
                    (totaalCenten (metProducten 1000 1 initieelModel))
        , test "1.170 producten, 1 taal = 1.200 (Kruidje-rekenvoorbeeld, 170 in de derde trede om 15ct)" <|
            \_ ->
                Expect.equal 119950
                    (totaalCenten (metProducten 1170 1 initieelModel))
        , test "2.000 producten, 1 taal = 1.299 (vier volle treden: 100 + 75 + 75 + 50)" <|
            \_ ->
                Expect.equal 129900
                    (totaalCenten (metProducten 2000 1 initieelModel))
        , test "3.000 producten, 1 taal = 1.399 (1.000 boven de treden om de bodem van 10ct)" <|
            \_ ->
                Expect.equal 139900
                    (totaalCenten (metProducten 3000 1 initieelModel))
        , test "5.000 producten, 1 taal = 1.599" <|
            \_ ->
                Expect.equal 159900
                    (totaalCenten (metProducten 5000 1 initieelModel))
        , test "10.000 producten, 1 taal = 2.099" <|
            \_ ->
                Expect.equal 209900
                    (totaalCenten (metProducten 10000 1 initieelModel))
        , test "2.400 producten, 3 talen = 2.319 (Panzer-rekenvoorbeeld: 7.200 vertalingen plus 2 x 250 taalconfig)" <|
            \_ ->
                Expect.equal 231900
                    (totaalCenten (metProducten 2400 3 initieelModel))
        , test "160 producten, 3 talen: 480 vertalingen om 20ct plus 2 x 250 configuratie = 1.595 (bybjor-regel)" <|
            \_ ->
                Expect.equal 159500
                    (totaalCenten (metProducten 160 3 initieelModel))
        , test "700 producten, 2 talen: 1.400 vertalingen (100 + 75 + 60) plus 1 x 250 = 1.484" <|
            \_ ->
                Expect.equal 148400
                    (totaalCenten (metProducten 700 2 initieelModel))
        , test "Panzer + thema overzetten + domeinverhuizing = 3.318" <|
            \_ ->
                let
                    model =
                        metProducten 2400 3 initieelModel
                in
                Expect.equal 331800
                    (totaalCenten
                        { model
                            | thema = ThemaOverzetten
                            , domeinBijMijnwebwinkel = True
                        }
                    )
        , test "nieuw ontwerp telt het uitwerken van 1.499 mee (het ontwerp zelf is de aparte offerte van de ontwerper)" <|
            \_ ->
                Expect.equal (109900 + 149900)
                    (totaalCenten { initieelModel | thema = ThemaNieuw })
        , test "onbekend bronplatform telt geen toeslag (op aanvraag)" <|
            \_ ->
                Expect.equal 109900
                    (totaalCenten { initieelModel | bron = BronAnders })
        , test "CCV-bron rekent geen toeslag (alleen eerste import is werk)" <|
            \_ ->
                Expect.equal 109900
                    (totaalCenten { initieelModel | bron = BronCcvShop })
        , test "reviews overzetten voegt 150 toe" <|
            \_ ->
                Expect.equal 124900
                    (totaalCenten { initieelModel | reviews = True })
        , test "verzendkoppeling voegt 150 toe" <|
            \_ ->
                Expect.equal 124900
                    (totaalCenten { initieelModel | verzendkoppeling = True })
        , test "B2B-kanaal voegt 750 toe" <|
            \_ ->
                Expect.equal 184900
                    (totaalCenten { initieelModel | b2bKanaal = True })
        , test "WooCommerce-bron rekent geen domeinverhuizing (zelf-gehost)" <|
            \_ ->
                Expect.equal 109900
                    (totaalCenten
                        { initieelModel
                            | bron = BronWoocommerce
                            , domeinBijMijnwebwinkel = True
                            , emailBijMijnwebwinkel = True
                        }
                    )
        , test "MijnWebwinkel-bron rekent domeinverhuizing wel (250)" <|
            \_ ->
                Expect.equal 134900
                    (totaalCenten { initieelModel | domeinBijMijnwebwinkel = True })
        , test "point-of-sale voegt 500 toe (reiskosten buiten 30 minuten van Kampen apart)" <|
            \_ ->
                Expect.equal 159900
                    (totaalCenten { initieelModel | pointOfSale = True })
        , test "cursus Shopify voegt 300 toe" <|
            \_ ->
                Expect.equal 139900
                    (totaalCenten { initieelModel | cursus = True })
        , test "alle vier de meegroeiende modules zonder opgave tellen 4 x 100 op (het vaste deel, tot 1.000 items)" <|
            \_ ->
                Expect.equal 149900
                    (totaalCenten
                        { initieelModel
                            | klantaccounts = True
                            , orderhistorie = True
                            , nieuwsbrief = True
                            , voorraad = True
                        }
                    )
        , test "klantaccounts met 4.850 accounts = 677,50 (100 + 3.850 om 15ct; Panzer)" <|
            \_ ->
                Expect.equal (109900 + 67750)
                    (totaalCenten { initieelModel | klantaccounts = True, klantaccountsInvoer = "4850" })
        , test "klantaccounts met 8.000 accounts: na 5.000 boven de inbegrepen zakt het naar 8ct = 1.010" <|
            \_ ->
                Expect.equal (109900 + 101000)
                    (totaalCenten { initieelModel | klantaccounts = True, klantaccountsInvoer = "8000" })
        , test "spaarpunten delen de 100 van de klantaccounts: 900 accounts met punten kosten niets extra" <|
            \_ ->
                Expect.equal (109900 + 10000)
                    (totaalCenten { initieelModel | klantaccounts = True, klantaccountsInvoer = "900", spaarpunten = True, spaarpuntenInvoer = "900" })
        , test "4.850 accounts waarvan 3.000 met punten = 677,50 + 2.000 om 5ct = 777,50" <|
            \_ ->
                Expect.equal (109900 + 67750 + 10000)
                    (totaalCenten { initieelModel | klantaccounts = True, klantaccountsInvoer = "4850", spaarpunten = True, spaarpuntenInvoer = "3000" })
        , test "spaarpunten zonder klantaccounts tellen niet mee (de punten hangen aan de accounts)" <|
            \_ ->
                Expect.equal 109900
                    (totaalCenten { initieelModel | spaarpunten = True, spaarpuntenInvoer = "3000" })
        , test "meer accounts met punten dan klantaccounts wordt gemeld: 5.000 met punten op 500 accounts" <|
            \_ ->
                Expect.equal True
                    (meerPuntenDanAccounts { initieelModel | klantaccounts = True, klantaccountsInvoer = "500", spaarpunten = True, spaarpuntenInvoer = "5000" })
        , test "3.000 met punten op 4.850 accounts is geen melding" <|
            \_ ->
                Expect.equal False
                    (meerPuntenDanAccounts { initieelModel | klantaccounts = True, klantaccountsInvoer = "4850", spaarpunten = True, spaarpuntenInvoer = "3000" })
        , test "binnen de 1.000 die de 100 dekt geen melding, ook zonder opgave van de klantaccounts" <|
            \_ ->
                Expect.equal False
                    (meerPuntenDanAccounts { initieelModel | klantaccounts = True, spaarpunten = True, spaarpuntenInvoer = "800" })
        , test "het aantal accounts met punten loopt via update in het model en de prijs" <|
            \_ ->
                Expect.equal (109900 + 10000 + 5000)
                    (totaalCenten (Tuple.first (update (SpaarpuntenAantalGewijzigd "2000") { initieelModel | klantaccounts = True, spaarpunten = True })))
        , test "bestelgeschiedenis met 11.504 bestellingen = 920,16 (100 + 10.000 om 8ct + 504 om 4ct; Panzer)" <|
            \_ ->
                Expect.equal (109900 + 92016)
                    (totaalCenten { initieelModel | orderhistorie = True, bestellingenInvoer = "11504" })
        , test "bestelgeschiedenis met 115.000 bestellingen = 2.410 (100 + 800 + 360 + 400 + 750; Kralenstulpje)" <|
            \_ ->
                Expect.equal (109900 + 241000)
                    (totaalCenten { initieelModel | orderhistorie = True, bestellingenInvoer = "115000" })
        , test "bestelgeschiedenis met 800 bestellingen blijft op het vaste deel van 100" <|
            \_ ->
                Expect.equal (109900 + 10000)
                    (totaalCenten { initieelModel | orderhistorie = True, bestellingenInvoer = "800" })
        , test "nieuwsbrief met 1.500 adressen = 125 (100 + 500 om 5ct)" <|
            \_ ->
                Expect.equal (109900 + 12500)
                    (totaalCenten { initieelModel | nieuwsbrief = True, abonneesInvoer = "1500" })
        , test "voorraad gaat per product, niet per vertaling: 2.400 producten in 3 talen = 170 (100 + 1.400 om 5ct)" <|
            \_ ->
                let
                    panzer =
                        metProducten 2400 3 initieelModel
                in
                Expect.equal (231900 + 17000)
                    (totaalCenten { panzer | voorraad = True })
        , test "een aantal telt niet mee zolang de module uit staat" <|
            \_ ->
                Expect.equal 109900
                    (totaalCenten { initieelModel | bestellingenInvoer = "11504" })
        , test "het aantal van een module loopt via update in het model" <|
            \_ ->
                Expect.equal "2500"
                    (Tuple.first (update (BestellingenGewijzigd "2500") initieelModel)).bestellingenInvoer
        , test "doelkeuze 'weetniet' in de dropdown wordt DoelWeetNiet" <|
            \_ ->
                Expect.equal DoelWeetNiet
                    (Tuple.first (update (DoelGewijzigd "weetniet") initieelModel)).doel
        , test "doelplatform beinvloedt de prijs niet, ook 'weet ik nog niet' niet" <|
            \_ ->
                Expect.equal (List.repeat 4 109900)
                    (List.map
                        (\doel -> totaalCenten { initieelModel | doel = doel })
                        [ DoelShopify, DoelWoocommerce, DoelAnders, DoelWeetNiet ]
                    )
        ]


{-| Iemand die de producten al zelf uit CCV Shop in Shopify zette: de
staffel vervalt, de basis wordt drie afrondingsregels die er precies op
optellen, en elk vinkje haalt zijn regel uit het totaal. -}
ccvZelf : Model
ccvZelf =
    Tuple.first (update (ZelfGeimporteerdGewijzigd True) { initieelModel | bron = BronCcvShop, productenInvoer = "5000" })


zelfImportSuite : Test
zelfImportSuite =
    describe "PrijsCalculator: producten al zelf in Shopify (afrondingsroute)"
        [ test "CCV met 5.000 producten, alles aangevinkt: 999, gelijk aan de basis, geen staffel" <|
            \_ ->
                Expect.equal 99900 (totaalCenten ccvZelf)
        , test "de regels: producten door jou gedaan op 0, dan collecties 499, redirects 350, pagina's 150" <|
            \_ ->
                Expect.equal [ 0, 49900, 35000, 15000 ]
                    (List.map .centen (List.filter (\r -> r.niveau == Hoofdregel) (prijsRegels ccvZelf)))
        , test "pagina's uitvinken haalt 150 van het totaal: 849" <|
            \_ ->
                Expect.equal 84900 (totaalCenten (Tuple.first (update (AfrondingPaginasGewijzigd False) ccvZelf)))
        , test "alleen collecties: 499" <|
            \_ ->
                let
                    alleenCollecties =
                        { ccvZelf | afrondingRedirects = False, afrondingPaginas = False }
                in
                Expect.equal 49900 (totaalCenten alleenCollecties)
        , test "productopties en tweede domein tellen erbij: 999 + 250 + 250" <|
            \_ ->
                Expect.equal 149900 (totaalCenten { ccvZelf | productoptiesHerstellen = True, tweedeDomeinRedirects = True })
        , test "de meegroeiende modules en B2B blijven gewoon werken: B2B erbij is 1.749" <|
            \_ ->
                Expect.equal 174900 (totaalCenten (Tuple.first (update (B2bKanaalGewijzigd True) ccvZelf)))
        , test "aanvinken zet het thema terug op standaard, want dat heeft hij al" <|
            \_ ->
                Expect.equal ThemaStandaard
                    (Tuple.first (update (ZelfGeimporteerdGewijzigd True) { initieelModel | bron = BronCcvShop, thema = ThemaOverzetten })).thema
        , test "bij MijnWebwinkel telt het vinkje niet: gewone prijs, geen afrondingsroute" <|
            \_ ->
                let
                    mww =
                        { initieelModel | zelfGeimporteerd = True }
                in
                Expect.all
                    [ \m -> Expect.equal False (zelfImportActief m)
                    , \m -> Expect.equal (totaalCenten initieelModel) (totaalCenten m)
                    ]
                    mww
        , test "Lightspeed heeft de vraag ook, MijnWebwinkel en WooCommerce niet" <|
            \_ ->
                Expect.equal [ True, True, False, False, False ]
                    (List.map heeftProductExport [ BronCcvShop, BronLightspeed, BronMijnwebwinkel, BronWoocommerce, BronAnders ])
        , test "de link vanaf de landingspagina vult voor: ?bron=ccv&zelf=ja" <|
            \_ ->
                let
                    model =
                        leesQuery "?bron=ccv&zelf=ja" initieelModel
                in
                Expect.all
                    [ \m -> Expect.equal BronCcvShop m.bron
                    , \m -> Expect.equal True (zelfImportActief m)
                    ]
                    model
        , test "een onbekende query verandert niets" <|
            \_ ->
                Expect.equal initieelModel (leesQuery "?utm_source=x&zelf=nee" initieelModel)
        ]


{-| Een bezoeker die in Uitgebreid klantaccounts en het overzetten van zijn
uitstraling aanvinkte en daarna naar Eenvoudig terugschakelde. -}
terugNaarEenvoudig : Model
terugNaarEenvoudig =
    Tuple.first
        (update (WeergaveGewijzigd Eenvoudig)
            { initieelModel | weergave = Uitgebreid, klantaccounts = True, thema = ThemaOverzetten }
        )


weergaveSuite : Test
weergaveSuite =
    describe "PrijsCalculator: Eenvoudig toont alleen basis en producten, Uitgebreid alles"
        [ test "in Eenvoudig tellen keuzes uit Uitgebreid niet mee: 500 producten is 1.099" <|
            \_ ->
                Expect.equal 109900 (totaalCenten (getoondeKeuzes terugNaarEenvoudig))
        , test "terug naar Uitgebreid brengt de keuzes terug: 1.099 + 100 accounts + 749 uitstraling" <|
            \_ ->
                Expect.equal 194800
                    (totaalCenten (getoondeKeuzes (Tuple.first (update (WeergaveGewijzigd Uitgebreid) terugNaarEenvoudig))))
        , test "de eigen import telt in Eenvoudig mee, met alle afrondingsregels: 999" <|
            \_ ->
                Expect.equal 99900
                    (totaalCenten (getoondeKeuzes { ccvZelf | afrondingPaginas = False }))
        , test "in Uitgebreid telt een uitgevinkte afrondingsregel wel: 849" <|
            \_ ->
                Expect.equal 84900
                    (totaalCenten (getoondeKeuzes { ccvZelf | weergave = Uitgebreid, afrondingPaginas = False }))
        , test "een link met weergave=uitgebreid opent de volledige rekenhulp" <|
            \_ ->
                Expect.equal Uitgebreid (leesQuery "?bron=ccv&weergave=uitgebreid" initieelModel).weergave
        ]

