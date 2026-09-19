module PricingTest exposing (groteCatalogusSuite, suite)

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
        , initieelModel
        , isGroteCatalogus
        , totaalCenten
        , update
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
        , test "nieuw thema telt niet mee in het totaal (op aanvraag)" <|
            \_ ->
                Expect.equal 109900
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
        , test "point-of-sale voegt 750 toe (excl. reiskosten op aanvraag)" <|
            \_ ->
                Expect.equal 184900
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
        , test "bestelgeschiedenis met 11.504 bestellingen = 920,16 (100 + 10.000 om 8ct + 504 om 4ct; Panzer)" <|
            \_ ->
                Expect.equal (109900 + 92016)
                    (totaalCenten { initieelModel | orderhistorie = True, bestellingenInvoer = "11504" })
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
