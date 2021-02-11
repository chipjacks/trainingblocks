module ActivityShape exposing (Color(..), colorString, view)

import Activity exposing (ActivityData)
import Emoji
import EmojiData exposing (EmojiData)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Pace exposing (TrainingPace(..))
import Skeleton exposing (column, row, styleIf, viewMaybe)


type Shape
    = Block { width : Float, height : Float } Color Bool (Maybe EmojiData)
    | Circle Color Bool (Maybe EmojiData)
    | Emoji (Maybe EmojiData)


type Color
    = Green
    | Orange
    | Red
    | Gray


view : Maybe Int -> ActivityData -> Html msg
view levelM data =
    let
        width paceM =
            Maybe.map2 Pace.secondsToTrainingPace levelM paceM
                |> Maybe.map toWidth
                |> Maybe.withDefault 1

        height =
            Maybe.map toHeight data.duration |> Maybe.withDefault 1

        color =
            case data.effort of
                Nothing ->
                    Gray

                Just Activity.Easy ->
                    Green

                Just Activity.Moderate ->
                    Orange

                Just Activity.Hard ->
                    Red

        emoji =
            Maybe.map Emoji.find data.emoji

        shape =
            case data.activityType of
                Activity.Run ->
                    Block { width = width data.pace, height = height } color data.completed emoji

                Activity.Cross ->
                    Circle color data.completed emoji

                Activity.Note ->
                    Emoji emoji
    in
    viewShape shape


viewShape : Shape -> Html msg
viewShape shape =
    case shape of
        Block { width, height } color completed emojiM ->
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
                [ viewMaybe emojiM Emoji.view ]

        Circle color completed emojiM ->
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
                [ viewMaybe emojiM (\e -> div [ style "margin-left" "0.5rem", style "margin-top" "-3px" ] [ Emoji.view e ]) ]

        Emoji emojiM ->
            Emoji.view (Maybe.withDefault Emoji.default emojiM)


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


toHeight : Activity.Seconds -> Float
toHeight duration =
    toFloat duration / 600


toWidth : TrainingPace -> Float
toWidth pace =
    case pace of
        Slow ->
            0.5

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
