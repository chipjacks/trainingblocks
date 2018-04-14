module View.Block exposing (view, viewEvent)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Mouse
import Msg exposing (Msg(..))
import Date.Extra as Date
import Activity


-- VIEW


view : Block.Model -> Svg Msg
view model =
    case ( model.data, model.view ) of
        ( Activity activity, _ ) ->
            g
                [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ]
                , Mouse.onOver (\me -> BlockEvent (Just ( me, View, model )))
                , Mouse.onOut (\me -> BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( _, Normal ) ->
            g
                [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ]
                , Mouse.onOver (\mouseEvent -> BlockEvent (Just ( mouseEvent, View, model )))
                , Mouse.onOut (\_ -> BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( Blocks blocks, Children ) ->
            g [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ] ] (List.map view blocks)

        ( _, _ ) ->
            g [] []


viewEvent : Maybe ( Mouse.Event, Block.Event, Block.Model ) -> Html Msg
viewEvent event =
    case event of
        Nothing ->
            div [] []

        Just ( mouseEvent, blockEvent, blockModel ) ->
            div
                [ class "block-tooltip"
                , Html.Attributes.style (positionTooltip mouseEvent.pagePos mouseEvent.offsetPos blockModel.w)
                ]
                [ viewTooltip blockModel.data ]



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


positionTooltip : ( Float, Float ) -> ( Float, Float ) -> Int -> List ( String, String )
positionTooltip pagePos offsetPos blockWidth =
    [ ( "top", toString ((Tuple.second pagePos) - (Tuple.second offsetPos)) ++ "px" )
    , ( "left", toString ((Tuple.first pagePos) - (Tuple.first offsetPos) + (toFloat blockWidth) + 5) ++ "px" )
    ]


viewTooltip : Block.Data -> Html Msg
viewTooltip data =
    case data of
        Activity activity ->
            div [ class "ui card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , div [ class "ui list" ]
                        [ div [ class "item" ] [ Html.text <| (toString activity.type_) ]
                        , div [ class "item" ] [ Html.text <| (Date.toFormattedString "h:mm a" activity.startDate) ]
                        , div [ class "item" ] [ Html.text <| ((toString (activity.duration // 60)) ++ " minutes") ]
                        , div [ class "item" ] [ Html.text <| Activity.miles activity ]
                        , div [ class "item" ] [ Html.text <| Activity.pace activity ]
                        ]
                    ]
                ]

        Blocks blocks ->
            div [] []
