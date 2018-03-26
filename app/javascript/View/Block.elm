module View.Block exposing (view)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)
import Html exposing (Html, div)
import Svg.Events exposing (onMouseOver, onMouseOut)
import Msg exposing (Msg(..))

-- VIEW

view : Maybe (Block.Event, Block.Model) -> Block.Model -> Svg Msg
view event model =
    case (model.data, model.view) of
        (Activity activity, _) ->
            g 
                [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"]
                , onMouseOver (BlockEvent (Just (MouseOver, model)))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock event model
                , viewEvent event model
                ]

        (_, Normal) ->
            g 
                [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"]
                , onMouseOver (BlockEvent (Just (MouseOver, model)))
                , onMouseOut (BlockEvent Nothing)
                ]
                [ viewBlock event model
                , viewEvent event model
                ]


        (Blocks blocks, Children) ->
            g [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"] ] (List.map (view event) blocks)


        (_, _) ->
            g [] []


viewEvent : Maybe (Block.Event, Block.Model) -> Block.Model -> Svg Msg
viewEvent event model =
    case event of
        Nothing ->
            Svg.text_ [] [ ]
    
        Just (event, eventModel) ->
            if (model == eventModel) then
                Svg.foreignObject [ width "100", height "100", y "-40" ] [ viewTooltip model.data ]
            else
                Svg.text_ [] [ ]


viewBlock : Maybe (Block.Event, Block.Model) -> Block.Model -> Svg Msg
viewBlock event model =
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
            div [ ]
                [ Html.text "type:"
                , Html.text <| toString <| activity.type_
                , Html.br [] []
                , Html.text "duration:"
                , Html.text <| toString <| activity.movingTime
                ]
            
    
        Blocks blocks ->
            div [] []
            