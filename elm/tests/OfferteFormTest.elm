module OfferteFormTest exposing (suite)

{-| Test dat het samengestelde bericht van het offerteformulier de
gekozen platforms met hun leesbare labels draagt: dit is de tekst die
de server logt en doormailt, dus een keuze die verkeerd vertaalt geeft
een verwarrende offerte-intake.
-}

import Expect
import OfferteForm exposing (Msg(..), initieelModel, samengesteldBericht, update)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "OfferteForm.samengesteldBericht"
        [ test "de standaardkeuzes vertalen naar hun leesbare labels" <|
            \_ ->
                Expect.all
                    [ \bericht -> Expect.equal True (String.contains "Huidig platform: MijnWebwinkel" bericht)
                    , \bericht -> Expect.equal True (String.contains "Gewenst platform: Shopify" bericht)
                    ]
                    (samengesteldBericht initieelModel)
        , test "een gewijzigde platformkeuze landt met rekenhulp-label in het bericht" <|
            \_ ->
                let
                    model =
                        initieelModel
                            |> update (BronGewijzigd "ccv")
                            |> update (DoelGewijzigd "weetniet")
                in
                Expect.all
                    [ \bericht -> Expect.equal True (String.contains "Huidig platform: CCV Shop" bericht)
                    , \bericht -> Expect.equal True (String.contains "Gewenst platform: Weet ik nog niet / ik wil advies" bericht)
                    ]
                    (samengesteldBericht model)
        , test "bijzonderheden landen op hun eigen regel" <|
            \_ ->
                Expect.equal True
                    (String.contains "Bijzonderheden: kassa in de winkel"
                        (samengesteldBericht (update (BijzonderhedenGewijzigd "kassa in de winkel") initieelModel))
                    )
        , test "het e-mailadres hoort niet in het bericht (dat reist als eigen veld)" <|
            \_ ->
                Expect.equal False
                    (String.contains "voorbeeld.nl"
                        (samengesteldBericht (update (EmailGewijzigd "jan@voorbeeld.nl") initieelModel))
                    )
        ]
