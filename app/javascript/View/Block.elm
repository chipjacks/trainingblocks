module View.Block exposing (view)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Mouse
import Msg exposing (Msg(..))
import Date.Extra as Date
import Activity
import BlockEvent


-- VIEW


view : Block.Model -> Svg Msg
view model =
    case ( model.data, model.view ) of
        ( Activity activity, _ ) ->
            g
                [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ]
                , Mouse.onOver (\me -> UpdateBlockEvent (BlockEvent.View me model))
                , Mouse.onOut (\me -> UpdateBlockEvent BlockEvent.EscapeView)
                , Mouse.onDown (\me -> UpdateBlockEvent (BlockEvent.Edit me model))
                ]
                [ viewBlock model
                ]

        ( _, Normal ) ->
            g
                [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ]
                , Mouse.onOver (\me -> UpdateBlockEvent (BlockEvent.View me model))
                , Mouse.onOut (\_ -> UpdateBlockEvent BlockEvent.EscapeView)
                , Mouse.onDown (\me -> UpdateBlockEvent (BlockEvent.Edit me model))
                ]
                [ viewBlock model
                ]

        ( Blocks blocks, Children ) ->
            g [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ] ] (List.map view blocks)

        ( _, _ ) ->
            g [] []



-- INTERNAL


viewBlock : Block.Model -> Svg Msg
viewBlock model =
    rect
        [ width <| toString <| model.w
        , height <| toString <| model.h
        , Svg.Attributes.fill model.color
        , stroke "white"
        ]
        []
