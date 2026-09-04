module OfferteFormTest exposing (suite)

{-| Test dat het samengestelde bericht van het offerteformulier de
ingevulde velden op de juiste regels draagt: dit is de tekst die de
server logt en doormailt, dus een velddat op de verkeerde regel landt
geeft een verwarrende offerte-intake.
-}

import Expect
import OfferteForm exposing (Msg(..), initieelModel, samengesteldBericht, update)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "OfferteForm.samengesteldBericht"
        [ test "ingevulde velden landen op hun eigen regel" <|
            \_ ->
                let
                    model =
                        initieelModel
                            |> update (HuidigPlatformGewijzigd "MijnWebwinkel")
                            |> update (AantalProductenGewijzigd "1.500")
                            |> update (BijzonderhedenGewijzigd "kassa in de winkel")
                in
                Expect.all
                    [ \bericht -> Expect.equal True (String.contains "- Huidig platform (bijv. MijnWebwinkel, CCV Shop): MijnWebwinkel" bericht)
                    , \bericht -> Expect.equal True (String.contains "- Aantal producten (ongeveer): 1.500" bericht)
                    , \bericht -> Expect.equal True (String.contains "- Bijzonderheden (kassa/point-of-sale, zakelijke klanten, verzendkoppeling): kassa in de winkel" bericht)
                    ]
                    (samengesteldBericht model)
        , test "een leeg formulier stuurt de zeven sjabloonregels leeg mee" <|
            \_ ->
                Expect.equal 7
                    (List.length (String.lines (samengesteldBericht initieelModel)))
        , test "het e-mailadres hoort niet in het bericht (dat reist als eigen veld)" <|
            \_ ->
                Expect.equal False
                    (String.contains "voorbeeld.nl"
                        (samengesteldBericht (update (EmailGewijzigd "jan@voorbeeld.nl") initieelModel))
                    )
        ]
