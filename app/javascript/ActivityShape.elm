module ActivityShape exposing (Color(..), colorString, view)

import Activity
import Activity.Types exposing (ActivityData, ActivityType(..), Completion(..), Effort(..), Seconds)
import Emoji exposing (EmojiDict)
import EmojiData exposing (EmojiData)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Msg exposing (ActivityConfigs)
import Pace exposing (TrainingPace)
import Skeleton exposing (column, row, styleIf, viewMaybe)


type Shape
    = Block { width : Float, height : Float } Color Completion (Maybe EmojiData)
    | Circle Color Completion (Maybe EmojiData)
    | Emoji (Maybe EmojiData)


type Color
    = Green
    | Orange
    | Red
    | Gray


view : ActivityConfigs -> ActivityData -> Html msg
view { paces, emojis } data =
    let
        width paceM =
            Maybe.map2 Pace.secondsToTrainingPace paces paceM
                |> Maybe.withDefault Pace.Easy
                |> toWidth

        height =
            Maybe.withDefault (30 * 60) data.duration |> toHeight

        color =
            case data.effort of
                Nothing ->
                    Gray

                Just Easy ->
                    Green

                Just Moderate ->
                    Orange

                Just Hard ->
                    Red

        emoji =
            Emoji.get emojis (data.emoji |> Maybe.withDefault "")

        shape =
            case data.activityType of
                Run ->
                    Block { width = width data.pace, height = height } color data.completed emoji

                Other ->
                    if data.emoji /= Nothing && data.effort == Nothing && data.duration == Nothing then
                        Emoji emoji

                    else
                        Circle color data.completed emoji
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
                , case completed of
                    Completed ->
                        style "background-color" (colorString color)

                    Planned ->
                        style "background-color" "white"
                ]
                [ viewMaybe emojiM Emoji.view ]

        Circle color completed emojiM ->
            let
                ( backgroundColor, textColor ) =
                    case completed of
                        Completed ->
                            ( colorString color, "white" )

                        Planned ->
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
            "var(--yellow-300)"

        Orange ->
            "var(--orange-300)"

        Red ->
            "var(--red-300)"

        Gray ->
            "var(--grey-900)"


toHeight : Seconds -> Float
toHeight duration =
    toFloat duration / 600


toWidth : TrainingPace -> Float
toWidth pace =
    case pace of
        Pace.VeryEasy ->
            0.5

        Pace.Easy ->
            1

        Pace.Moderate ->
            2

        Pace.Steady ->
            3

        Pace.Brisk ->
            4

        Pace.Aerobic ->
            5

        Pace.Lactate ->
            6

        Pace.Groove ->
            7

        Pace.VO2 ->
            8

        Pace.Fast ->
            9
