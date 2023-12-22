module ActivityShape exposing (Color(..), colorString, view)

import Activity.Types exposing (ActivityData, ActivityType(..), Completion(..), Effort(..), Seconds)
import Emoji
import EmojiData exposing (EmojiData)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Msg exposing (ActivityConfigs)
import Pace exposing (StandardPace)
import UI.Layout exposing (compactColumn)
import UI.Util exposing (styleIf, viewIf, viewMaybe)


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
            case paceM |> Maybe.andThen (Pace.secondsToStandardPace paces) of
                Just sp ->
                    toWidth sp

                _ ->
                    0.5

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
        Block size color completed emojiM ->
            let
                maxBlockHeight =
                    20
            in
            if size.height > maxBlockHeight then
                let
                    offsets =
                        List.range 0 (round size.height // maxBlockHeight)

                    offsetWidth =
                        3
                in
                compactColumn
                    [ style "position" "relative"
                    , style "height" <| String.fromFloat (maxBlockHeight + ((List.length offsets |> toFloat) * 0.1)) ++ "rem"
                    , style "width" <| String.fromFloat (size.width * 0.3 + ((List.length offsets |> toFloat) * 0.2)) ++ "rem"
                    ]
                    (offsets
                        |> List.map
                            (\i ->
                                if i == List.length offsets - 1 then
                                    viewBlock { size | height = remainderBy (maxBlockHeight * 600) (round (size.height * 600)) |> toFloat |> (\h -> h / 600) } color completed emojiM (i * offsetWidth)

                                else
                                    viewBlock { size | height = maxBlockHeight } color completed emojiM (i * offsetWidth)
                            )
                    )

            else
                viewBlock size color completed emojiM 0

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
                , style "border" ("1px solid " ++ colorString color)
                , style "text-align" "center"
                , style "font-size" "0.8rem"
                , style "background-color" backgroundColor
                , style "color" textColor
                ]
                [ viewMaybe emojiM (\e -> div [ style "margin-left" "0.5rem", style "margin-top" "-3px" ] [ Emoji.view e ]) ]

        Emoji emojiM ->
            Emoji.view (Maybe.withDefault Emoji.default emojiM)


viewBlock : { width : Float, height : Float } -> Color -> Completion -> Maybe EmojiData -> Int -> Html msg
viewBlock { width, height } color completed emojiM offset =
    div
        [ style "width" <| String.fromFloat (width * 0.3) ++ "rem"
        , style "height" <| String.fromFloat height ++ "rem"
        , style "border-radius" "2px"
        , style "margin-top" <| String.fromInt offset ++ "px"
        , style "margin-left" <| String.fromInt offset ++ "px"
        , styleIf (offset /= 0) "position" "absolute"
        , class "block"
        , case completed of
            Completed ->
                style "background-color" (colorString color)

            Planned ->
                style "background-color" "white"
        , case completed of
            Completed ->
                style "border" "1px solid white"

            Planned ->
                style "border" ("1px solid " ++ colorString color)
        ]
        [ viewIf (offset == 0)
            (viewMaybe emojiM
                (\e ->
                    div [ style "position" "absolute", style "z-index" "3", style "margin-left" "0.2rem", style "margin-top" "3px" ]
                        [ Emoji.view e ]
                )
            )
        ]


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


toWidth : StandardPace -> Float
toWidth pace =
    case pace of
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
