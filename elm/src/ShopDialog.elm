module ShopDialog exposing
    ( ClickPoint
    , ShopFold(..)
    , clickPointDecoder
    , viewPurchaseDialog
    , viewShopGroupHeading
    , viewShopItem
    , viewShopToggle
    )

{-| The shared in-game shop chrome: the fold-away toggle, the item rows,
and the confirm/cancel purchase dialog that renders its confirm button
under the click that opened it. Message-generic so both game engines
(CoinFlipGame and MultiCoinGame) use one implementation.
-}
-- Decision: one message-generic module (Html msg, callers pass their
-- own Msg constructors) instead of the alternatives tried first:
-- duplicating the shop chrome per engine (how it started; the two
-- copies immediately drifted when level 3 gained dialogs level 2
-- lacked) or letting one engine render the other's Html Msg (needs
-- Html.map noise and still couples the Msg types). Prices arrive as
-- pre-formatted strings rather than cents so this module does not
-- import CoinFlipGame's formatCents, which would create an import
-- cycle: CoinFlipGame imports ShopDialog for the views.

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode


{-| The shop folds away behind its header. It starts collapsed so the
game front is the coin, not the catalogue; the labelled toggle is a
click away.
-}
type ShopFold
    = ShopCollapsed
    | ShopExpanded


{-| Where in the viewport a shop item was clicked. The dialog is
positioned so its confirm button renders right under this point: the
pointer cannot be moved by a web page, but the button can be brought to
the pointer.
-}
type alias ClickPoint =
    { x : Float
    , y : Float
    }


clickPointDecoder : Decode.Decoder ClickPoint
clickPointDecoder =
    Decode.map2 ClickPoint
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


viewShopToggle : msg -> ShopFold -> Html msg
viewShopToggle onToggle fold =
    Html.button
        [ Html.Attributes.class "shop-toggle", Html.Events.onClick onToggle ]
        [ Html.text
            (case fold of
                ShopCollapsed ->
                    "\u{1F6D2} Shop \u{25B8}"

                ShopExpanded ->
                    "\u{1F6D2} Shop \u{25BE}"
            )
        ]


{-| A group heading inside the shop, for sections like "Upgrades" and
"Intel". Plain text, always visible; the items below it get indented by
CSS.
-}
viewShopGroupHeading : String -> Html msg
viewShopGroupHeading label =
    Html.div [ Html.Attributes.class "shop-group-heading" ] [ Html.text label ]


{-| One row in the shop: item name left, price right. The click carries
its viewport coordinates so the dialog can open under the pointer, and
stops propagating so it never reaches a document-level dismiss listener.
-}
viewShopItem : (ClickPoint -> msg) -> String -> String -> Html msg
viewShopItem onBuyAt itemName priceText =
    Html.button
        [ Html.Attributes.class "shop-item"
        , Html.Events.stopPropagationOn "click"
            (Decode.map (\clickPoint -> ( onBuyAt clickPoint, True )) clickPointDecoder)
        ]
        [ Html.span [] [ Html.text itemName ]
        , Html.span [] [ Html.text priceText ]
        ]


{-| The confirm/cancel dialog a considered purchase opens, explaining
what the item does before any money moves. Fixed-positioned so the
confirm button (a known 120x42 box, first in the dialog, inside the
0.6em padding and 1px border) renders centered under the click that
opened it: buying again is a click without moving the mouse. Clicks
inside stop propagating (as `onInsideClick`) so only genuinely-outside
clicks reach the caller's document-level dismiss listener.
-}
viewPurchaseDialog :
    { clickPoint : ClickPoint
    , question : String
    , explanation : String
    , onConfirm : msg
    , onCancel : msg
    , onInsideClick : msg
    }
    -> Html msg
viewPurchaseDialog dialog =
    Html.div
        [ Html.Attributes.class "purchase-dialog"
        , Html.Attributes.style "left" (pixels (max 8 (dialog.clickPoint.x - 71)))
        , Html.Attributes.style "top" (pixels (max 8 (dialog.clickPoint.y - 32)))
        , Html.Events.stopPropagationOn "click" (Decode.succeed ( dialog.onInsideClick, True ))
        ]
        [ Html.div [ Html.Attributes.class "dialog-actions" ]
            [ Html.button
                [ Html.Events.onClick dialog.onConfirm
                , Html.Attributes.style "width" "120px"
                , Html.Attributes.style "height" "42px"
                ]
                [ Html.text "Confirm" ]
            , Html.button [ Html.Events.onClick dialog.onCancel ] [ Html.text "Cancel" ]
            ]
        , Html.div [] [ Html.strong [] [ Html.text dialog.question ] ]
        , Html.div [] [ Html.text dialog.explanation ]
        ]


pixels : Float -> String
pixels amount =
    String.fromFloat amount ++ "px"
