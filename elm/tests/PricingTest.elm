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
        , test "het oude 50.000-anker toont nu gewoon zijn prijs: 7.199" <|
            \_ ->
                Expect.all
                    [ \model -> Expect.equal False (isGroteCatalogus model)
                    , \model -> Expect.equal 719900 (totaalCenten model)
                    ]
                    (metProducten 50000 1 initieelModel)
        ]


suite : Test
suite =
    describe "PrijsCalculator.totaalCenten"
        [ test "basis: 1.000 producten, 1 taal, geen modules = 1.999" <|
            \_ ->
                Expect.equal 199900 (totaalCenten initieelModel)
        , test "3.000 producten, 1 taal = 2.449 (degressief: 1.000 om 25ct en 1.000 om 20ct)" <|
            \_ ->
                Expect.equal 244900
                    (totaalCenten (metProducten 3000 1 initieelModel))
        , test "2.000 producten, 1 taal = 2.249 (hele eerste trede om 25ct)" <|
            \_ ->
                Expect.equal 224900
                    (totaalCenten (metProducten 2000 1 initieelModel))
        , test "3.500 producten, 1 taal = 2.524 (halverwege de derde trede)" <|
            \_ ->
                Expect.equal 252400
                    (totaalCenten (metProducten 3500 1 initieelModel))
        , test "5.000 producten, 1 taal = 2.699 (bodemtrede van 10ct bereikt)" <|
            \_ ->
                Expect.equal 269900
                    (totaalCenten (metProducten 5000 1 initieelModel))
        , test "10.000 producten, 1 taal = 3.199" <|
            \_ ->
                Expect.equal 319900
                    (totaalCenten (metProducten 10000 1 initieelModel))
        , test "2.400 producten, 3 talen = 3.419 (Panzer-rekenvoorbeeld, degressief over 6.200 extra vertalingen)" <|
            \_ ->
                Expect.equal 341900
                    (totaalCenten (metProducten 2400 3 initieelModel))
        , test "160 producten, 3 talen: vertalingen passen in de basisruimte, alleen 2 x 250 configuratie = 2.499 (bybjor-regel)" <|
            \_ ->
                Expect.equal 249900
                    (totaalCenten (metProducten 160 3 initieelModel))
        , test "700 producten, 2 talen: alleen de 400 vertalingen boven de 1.000 tellen = 2.349" <|
            \_ ->
                Expect.equal 234900
                    (totaalCenten (metProducten 700 2 initieelModel))
        , test "Panzer + thema overzetten + domeinverhuizing = 4.418" <|
            \_ ->
                let
                    model =
                        metProducten 2400 3 initieelModel
                in
                Expect.equal 441800
                    (totaalCenten
                        { model
                            | thema = ThemaOverzetten
                            , domeinBijMijnwebwinkel = True
                        }
                    )
        , test "nieuw thema telt niet mee in het totaal (op aanvraag)" <|
            \_ ->
                Expect.equal 199900
                    (totaalCenten { initieelModel | thema = ThemaNieuw })
        , test "onbekend bronplatform telt geen toeslag (op aanvraag)" <|
            \_ ->
                Expect.equal 199900
                    (totaalCenten { initieelModel | bron = BronAnders })
        , test "CCV-bron rekent geen toeslag (alleen eerste import is werk)" <|
            \_ ->
                Expect.equal 199900
                    (totaalCenten { initieelModel | bron = BronCcvShop })
        , test "reviews overzetten voegt 150 toe" <|
            \_ ->
                Expect.equal 214900
                    (totaalCenten { initieelModel | reviews = True })
        , test "verzendkoppeling voegt 150 toe" <|
            \_ ->
                Expect.equal 214900
                    (totaalCenten { initieelModel | verzendkoppeling = True })
        , test "B2B-kanaal voegt 750 toe" <|
            \_ ->
                Expect.equal 274900
                    (totaalCenten { initieelModel | b2bKanaal = True })
        , test "WooCommerce-bron rekent geen domeinverhuizing (zelf-gehost)" <|
            \_ ->
                Expect.equal 199900
                    (totaalCenten
                        { initieelModel
                            | bron = BronWoocommerce
                            , domeinBijMijnwebwinkel = True
                            , emailBijMijnwebwinkel = True
                        }
                    )
        , test "MijnWebwinkel-bron rekent domeinverhuizing wel (250)" <|
            \_ ->
                Expect.equal 224900
                    (totaalCenten { initieelModel | domeinBijMijnwebwinkel = True })
        , test "point-of-sale voegt 750 toe (excl. reiskosten op aanvraag)" <|
            \_ ->
                Expect.equal 274900
                    (totaalCenten { initieelModel | pointOfSale = True })
        , test "cursus Shopify voegt 300 toe" <|
            \_ ->
                Expect.equal 229900
                    (totaalCenten { initieelModel | cursus = True })
        , test "alle overzet-modules samen tellen 4 x 250 op" <|
            \_ ->
                Expect.equal 299900
                    (totaalCenten
                        { initieelModel
                            | klantaccounts = True
                            , orderhistorie = True
                            , nieuwsbrief = True
                            , voorraad = True
                        }
                    )
        , test "doelkeuze 'weetniet' in de dropdown wordt DoelWeetNiet" <|
            \_ ->
                Expect.equal DoelWeetNiet
                    (Tuple.first (update (DoelGewijzigd "weetniet") initieelModel)).doel
        , test "doelplatform beinvloedt de prijs niet, ook 'weet ik nog niet' niet" <|
            \_ ->
                Expect.equal (List.repeat 4 199900)
                    (List.map
                        (\doel -> totaalCenten { initieelModel | doel = doel })
                        [ DoelShopify, DoelWoocommerce, DoelAnders, DoelWeetNiet ]
                    )
        ]
