module View.Block exposing (view, viewEvent)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import SvgMouseEvents exposing (MouseEvent, onMouseOver, onMouseOut)
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
                , onMouseOver (\me -> BlockEvent (Just ( me, View, model )))
                , onMouseOut (\me -> BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( _, Normal ) ->
            g
                [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ]
                , onMouseOver (\mouseEvent -> BlockEvent (Just ( mouseEvent, View, model )))
                , onMouseOut (\_ -> BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        ( Blocks blocks, Children ) ->
            g [ transform <| String.join " " [ "translate(", (toString model.x), " ", (toString model.y), ")" ] ] (List.map view blocks)

        ( _, _ ) ->
            g [] []


viewEvent : Maybe ( MouseEvent, Block.Event, Block.Model ) -> Html Msg
viewEvent event =
    case event of
        Nothing ->
            div [] []

        Just ( mouseEvent, blockEvent, blockModel ) ->
            div
                [ class "block-tooltip"
                , Html.Attributes.style
                    [ ( "top", toString ({- mouseEvent.targetPos.y + -} 5) ++ "px" )
                    , ( "left", toString ({- mouseEvent.targetPos.x + -} 5) ++ "px" )
                    ]
                ]
                [ viewTooltip blockModel.data ]


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
