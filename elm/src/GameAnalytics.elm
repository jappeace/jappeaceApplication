port module GameAnalytics exposing (AnalyticsEvent, send)

{-| The analytics bridge for the coin-flip games, following the
webwinkelverhuis price calculator: the engines stay pure and describe
events as data, one outgoing port hands them to page JavaScript, and
the page forwards them to Google Analytics (gtag). The engines compute
their events by diffing the model before and after an update, so the
event logic is plain data in, data out, and the test suite can assert
it without touching the port.
-}
-- Decision: one shared port module instead of a port per game engine.
-- Both engines (CoinFlipGame, MultiCoinGame) import this, so every
-- compiled level bundle exposes the same port and the posts share one
-- JS subscription snippet. A port per engine was rejected: two names
-- for the same wire means the posts' boot scripts drift apart. The
-- port is `gameAnalyticsEvent`, not `analyticsEvent`: elm-test links
-- every port module into one program, and PrijsCalculator already owns
-- that name. Events-as-data over Cmd-in-the-logic keeps the diff
-- functions testable in elm-test, where Cmds are opaque.

import Json.Encode as Encode


{-| One analytics event: a gtag event name and its parameters.
-}
type alias AnalyticsEvent =
    { name : String
    , params : List ( String, Encode.Value )
    }


{-| Sends an analytics event to JavaScript, where the page passes it to
Google Analytics (gtag). The value is an object {name, params}.
-}
port gameAnalyticsEvent : Encode.Value -> Cmd msg


send : AnalyticsEvent -> Cmd msg
send event =
    gameAnalyticsEvent
        (Encode.object
            [ ( "name", Encode.string event.name )
            , ( "params", Encode.object event.params )
            ]
        )
