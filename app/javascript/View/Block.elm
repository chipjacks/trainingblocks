module View.Block exposing (view, viewEvent)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Svg.Events exposing (onMouseOver, onMouseOut)
import Msg exposing (Msg(..))
import Mouse exposing (Position)

-- VIEW

view : Block.Model -> Svg Msg
view model =
    case (model.data, model.view) of
        (Activity activity, _) ->
            g 
                [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"]
                , onMouseOver (BlockEvent (Just (MouseOver, model)))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock model
                ]

        (_, Normal) ->
            g 
                [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"]
                , onMouseOver (BlockEvent (Just (MouseOver, model)))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock model
                ]


        (Blocks blocks, Children) ->
            g [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"] ] (List.map view blocks)


        (_, _) ->
            g [] []


viewEvent : Position -> Maybe (Block.Event, Block.Model) -> Html Msg
viewEvent mousePos event =
    case event of
        Nothing ->
            div [] [ ]
    
        Just (event, eventModel) ->
            div [ class "block-tooltip", Html.Attributes.style [("top", toString (mousePos.y + 5) ++ "px"), ("left", toString (mousePos.x + 5) ++ "px")] ] [ viewTooltip eventModel.data ]


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
            div [ class "ui card block-tooltip" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ Html.text activity.name ]
                    , div [ class "ui list" ]
                        [ div [ class "item" ] [ Html.text <| (toString activity.startDate) ]
                        , div [ class "item" ] [ Html.text <| ((toString (activity.movingTime // 60)) ++ " minutes") ]
                        ]
                    ]
                ]
            
    
        Blocks blocks ->
            div [] []
            