module ActivityShape exposing (view, viewDefault)

import Activity exposing (Activity, Pace(..))
import Emoji
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Skeleton exposing (column, row, styleIf)


type Shape
    = Block Color Bool { width : Float, height : Float }
    | Circle Color Bool (Maybe Char)
    | Emoji String


type Color
    = Green
    | Orange
    | Red
    | Gray


view : Activity -> Html msg
view activity =
    case activity.data of
        Activity.Run mins pace completed ->
            Block Green completed { width = toWidth pace, height = toHeight mins }
                |> viewShape

        Activity.Interval secs pace completed ->
            Block Orange completed { width = toWidth pace, height = toIntervalHeight secs }
                |> viewShape

        Activity.Race mins dist completed ->
            Block Red completed { width = toWidth (Maybe.withDefault Activity.Lactate Nothing), height = toHeight mins }
                |> viewShape

        Activity.Other mins completed ->
            Circle Gray completed (String.toList activity.description |> List.head)
                |> viewShape

        Activity.Note emoji ->
            Emoji emoji
                |> viewShape

        Activity.Session activities ->
            div [] (List.map view activities)


viewDefault : Bool -> Activity.ActivityData -> Html msg
viewDefault completed activityData =
    case activityData of
        Activity.Run _ _ _ ->
            Block Green completed { width = 3, height = 1 }
                |> viewShape

        Activity.Interval _ _ _ ->
            Block Orange completed { width = 3, height = 1 }
                |> viewShape

        Activity.Race _ _ _ ->
            Block Red completed { width = 3, height = 1 }
                |> viewShape

        Activity.Other _ _ ->
            Circle Gray completed Nothing
                |> viewShape

        Activity.Note _ ->
            Emoji Emoji.default.name
                |> viewShape

        Activity.Session _ ->
            Block Orange completed { width = 3, height = 1 }
                |> viewShape


viewShape : Shape -> Html msg
viewShape shape =
    case shape of
        Block color completed { width, height } ->
            div
                [ style "width" <| String.fromFloat (width * 0.3) ++ "rem"
                , style "height" <| String.fromFloat height ++ "rem"
                , style "border" ("2px solid " ++ colorString color)
                , style "border-radius" "2px"
                , class "block"
                , if completed then
                    style "background-color" (colorString color)

                  else
                    style "background-color" "white"
                ]
                []

        Circle color completed charM ->
            let
                ( backgroundColor, textColor ) =
                    if completed then
                        ( colorString color, "white" )

                    else
                        ( "white", colorString color )
            in
            div
                [ style "width" "1rem"
                , style "height" "1rem"
                , style "border-radius" "50%"
                , style "border" ("2px solid " ++ colorString color)
                , style "text-align" "center"
                , style "font-size" "0.8rem"
                , style "background-color" backgroundColor
                , style "color" textColor
                ]
                [ Html.text (charM |> Maybe.map Char.toUpper |> Maybe.map String.fromChar |> Maybe.withDefault "") ]

        Emoji name ->
            Emoji.view (Emoji.find name)


colorString : Color -> String
colorString color =
    case color of
        Green ->
            "var(--activity-green)"

        Orange ->
            "var(--activity-orange)"

        Red ->
            "red"

        Gray ->
            "var(--activity-gray)"


toHeight : Activity.Minutes -> Float
toHeight duration =
    toFloat duration / 10


toIntervalHeight : Activity.Seconds -> Float
toIntervalHeight duration =
    toFloat duration / 600


toWidth : Activity.Pace -> Float
toWidth pace =
    case pace of
        Easy ->
            1

        Moderate ->
            2

        Steady ->
            3

        Brisk ->
            4

        Aerobic ->
            5

        Lactate ->
            6

        Groove ->
            7

        VO2 ->
            8

        Fast ->
            9
