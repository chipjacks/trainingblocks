module View.Block exposing (view, viewEvent)

import Activity
import Block exposing (..)
import Date.Extra as Date
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Mouse exposing (Position)
import Msg exposing (Msg(..))
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, transform, width, x, y)
import Svg.Events exposing (onMouseOut, onMouseOver)



-- VIEW


view : Block.Model -> Svg Msg
view model =
    case ( model.data, model.view ) of
        ( Activity activity, _ ) ->
            g
                [ transform <| String.join " " [ "translate(", toString model.x, " ", toString model.y, ")" ]
                , onMouseOver (BlockEvent (Just ( MouseOver, model )))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( _, Normal ) ->
            g
                [ transform <| String.join " " [ "translate(", toString model.x, " ", toString model.y, ")" ]
                , onMouseOver (BlockEvent (Just ( MouseOver, model )))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( Blocks blocks, Children ) ->
            g [ transform <| String.join " " [ "translate(", toString model.x, " ", toString model.y, ")" ] ] (List.map view blocks)

        ( _, _ ) ->
            g [] []


viewEvent : Position -> Maybe ( Block.Event, Block.Model ) -> Html Msg
viewEvent mousePos event =
    case event of
        Nothing ->
            div [] []

        Just ( event, eventModel ) ->
            div
                [ class "block-tooltip"
                , Html.Attributes.style "top" (toString (mousePos.y + 5) ++ "px")
                , Html.Attributes.style "left" (toString (mousePos.x + 5) ++ "px")
                ]
                [ viewTooltip eventModel.data ]


viewBlock : Block.Model -> Svg Msg
viewBlock model =
    rect
        [ width <| toString <| model.w
        , height <| toString <| model.h
        , Svg.Attributes.fill model.color
        , stroke "white"
        ]
        []


viewTooltip : Block.Data -> Html Msg
viewTooltip data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , div [ class "ui list" ]
                        [ div [ class "item" ] [ Html.text <| toString activity.type_ ]
                        , div [ class "item" ] [ Html.text <| Date.toFormattedString "h:mm a" activity.startDate ]
                        , div [ class "item" ] [ Html.text <| (toString (activity.movingTime // 60) ++ " minutes") ]
                        , div [ class "item" ] [ Html.text <| Activity.miles activity ]
                        , div [ class "item" ] [ Html.text <| Activity.pace activity ]
                        , div [ class "item" ] [ Html.text <| ((toString <| round <| activity.totalElevationGain) ++ " feet elevation gain") ]
                        ]
                    ]
                ]

        Blocks blocks ->
            div [] []
