module EngagementTest exposing (suite)

{-| Test het gedrag rond de analytics-events via de echte update-functie. De
Cmd's zelf (de gtag-events) zijn in elm-test niet te inspecteren, maar de
model-vlaggen die ze aansturen zijn dat wel: analyticsEngaged bepaalt of het
"calculator_engaged"-event één keer vuurt, en offertePoging hoort na een
geblokkeerde verzendpoging op True te staan.
-}

import Expect
import PrijsCalculator exposing (Msg(..), formulierGeldig, initieelModel, update)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "analytics-engagement via update"
        [ test "start is niet engaged" <|
            \_ ->
                Expect.equal False initieelModel.analyticsEngaged
        , test "eerste interactie markeert engagement (event vuurt)" <|
            \_ ->
                Expect.equal True
                    (Tuple.first (update (BronGewijzigd "ccv") initieelModel)).analyticsEngaged
        , test "een al-engaged model blijft engaged (geen tweede event)" <|
            \_ ->
                Expect.equal True
                    (Tuple.first
                        (update (ProductenGewijzigd "50")
                            { initieelModel | analyticsEngaged = True }
                        )
                    ).analyticsEngaged
        , test "geblokkeerde verzendpoging zet offertePoging op True" <|
            \_ ->
                Expect.equal True
                    (Tuple.first (update OfferteGepoogd initieelModel)).offertePoging
        , test "zonder e-mailadres is het offerteformulier ongeldig (naam en domein zijn niet genoeg)" <|
            \_ ->
                Expect.equal False
                    (formulierGeldig { initieelModel | naam = "Jan", webshopDomein = "uwshop.nl" })
        , test "een e-mailadres zonder apenstaartje blijft ongeldig" <|
            \_ ->
                Expect.equal False
                    (formulierGeldig { initieelModel | naam = "Jan", webshopDomein = "uwshop.nl", emailInvoer = "geen-adres" })
        , test "met naam, domein en e-mailadres is het formulier geldig" <|
            \_ ->
                Expect.equal True
                    (formulierGeldig { initieelModel | naam = "Jan", webshopDomein = "uwshop.nl", emailInvoer = "jan@uwshop.nl" })
        , test "start is niet als grote catalogus gemeld" <|
            \_ ->
                Expect.equal False initieelModel.groteCatalogusGemeld
        , test "150.000 producten invoeren markeert grote catalogus (event vuurt)" <|
            \_ ->
                Expect.equal True
                    (Tuple.first (update (ProductenGewijzigd "150000") initieelModel)).groteCatalogusGemeld
        , test "50.000 producten blijft sinds de verruimde grens onder de melding (prijs wordt gewoon getoond)" <|
            \_ ->
                Expect.equal False
                    (Tuple.first (update (ProductenGewijzigd "50000") initieelModel)).groteCatalogusGemeld
        , test "onder de grens blijft de melding uit" <|
            \_ ->
                Expect.equal False
                    (Tuple.first (update (ProductenGewijzigd "500") initieelModel)).groteCatalogusGemeld
        , test "een al gemeld model meldt niet opnieuw (vlag blijft staan)" <|
            \_ ->
                Expect.equal True
                    (Tuple.first
                        (update (ProductenGewijzigd "60000")
                            { initieelModel | groteCatalogusGemeld = True }
                        )
                    ).groteCatalogusGemeld
        ]
