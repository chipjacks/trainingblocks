module View.Block exposing (view)

import Block exposing (..)
import Svg exposing (Svg, rect, svg, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, transform)

-- VIEW

view : Block.Model -> Svg msg
view model =
    case (model.data, model.view) of
        (_, Normal) ->
            rect
                [ x <| toString <| model.x
                , y <| toString <| model.y
                , width <| toString <| model.w --(toString (((activity.durationMinutes |> toFloat) / (splitDuration + 20)) * 100)) ++ "%"
                , height <| toString <| model.h --(activity.intensity * 10 |> toString)
                , Svg.Attributes.fill model.color
                , stroke "white"
                ]
                []

        (Blocks blocks, Children) ->
            g [ transform <| String.join " " ["translate(", (toString model.x), " ", (toString model.y), ")"] ] (List.map view blocks)

        (Activity activity, Split) ->
            rect
                [ x <| toString <| model.x
                , y <| toString <| model.y
                , width <| toString <| model.w --(toString (((activity.durationMinutes |> toFloat) / (splitDuration + 20)) * 100)) ++ "%"
                , height <| toString <| model.h --(activity.intensity * 10 |> toString)
                , Svg.Attributes.fill "skyblue"
                , stroke "white"
                ]
                []
        
        (_, _) ->
            g [] []