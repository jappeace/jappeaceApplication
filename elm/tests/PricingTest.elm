module PricingTest exposing (suite)

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
        , Model
        , ThemaKeuze(..)
        , initieelModel
        , totaalCenten
        )
import Test exposing (Test, describe, test)


metProducten : Int -> Int -> Model -> Model
metProducten producten talen model =
    { model
        | productenInvoer = String.fromInt producten
        , talenInvoer = String.fromInt talen
    }


suite : Test
suite =
    describe "PrijsCalculator.totaalCenten"
        [ test "basis: 1.000 producten, 1 taal, geen modules = 1.999" <|
            \_ ->
                Expect.equal 199900 (totaalCenten initieelModel)
        , test "3.000 producten, 1 taal = 2.499 (rekenvoorbeeld)" <|
            \_ ->
                Expect.equal 249900
                    (totaalCenten (metProducten 3000 1 initieelModel))
        , test "2.400 producten, 3 talen = 4.049 (Panzer-rekenvoorbeeld)" <|
            \_ ->
                Expect.equal 404900
                    (totaalCenten (metProducten 2400 3 initieelModel))
        , test "Panzer + thema overzetten + domeinverhuizing = 5.048" <|
            \_ ->
                let
                    model =
                        metProducten 2400 3 initieelModel
                in
                Expect.equal 504800
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
        ]
