module View.Block exposing (view)

import Activity
import Block exposing (..)
import Date
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Msg exposing (Msg(..))
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, transform, width, x, y)



-- VIEW


view : Block.Model -> Svg Msg
view model =
    case ( model.data, model.view ) of
        ( Activity activity, _ ) ->
            g
                [ transform <| String.join " " [ "translate(", String.fromInt model.x, " ", String.fromInt model.y, ")" ]
                ]
                [ viewBlock model
                ]

        ( _, Normal ) ->
            g
                [ transform <| String.join " " [ "translate(", String.fromInt model.x, " ", String.fromInt model.y, ")" ]
                ]
                [ viewBlock model
                ]

        ( Blocks blocks, Children ) ->
            g [ transform <| String.join " " [ "translate(", String.fromInt model.x, " ", String.fromInt model.y, ")" ] ] (List.map view blocks)

        ( _, _ ) ->
            g [] []


viewBlock : Block.Model -> Svg Msg
viewBlock model =
    rect
        [ width <| String.fromInt <| model.w
        , height <| String.fromInt <| model.h
        , Svg.Attributes.fill model.color
        , stroke "white"
        ]
        []
