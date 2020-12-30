module Duration exposing (formatSeconds, fromString, parser, stripTimeStr, timeStrToHrsMinsSecs, timeStrToSeconds, timeToSeconds, toString, toStringWithUnits)

import Parser exposing ((|.), (|=), Parser)


toString : Int -> String
toString seconds =
    let
        ( hrs, mins, secs ) =
            toHrsMinsSecs seconds
    in
    String.join ""
        [ if hrs > 0 then
            String.fromInt hrs ++ ":"

          else
            ""
        , if hrs > 0 then
            formatSeconds mins

          else
            String.fromInt mins
        , if secs > 0 then
            ":" ++ formatSeconds secs

          else
            ""
        ]


toStringWithUnits : Int -> String
toStringWithUnits seconds =
    let
        ( hrs, mins, secs ) =
            toHrsMinsSecs seconds
    in
    String.join " "
        [ if hrs > 0 then
            String.fromInt hrs ++ "h"

          else
            ""
        , if mins > 0 then
            String.fromInt mins ++ "m"

          else
            ""
        , if secs > 0 then
            String.fromInt secs ++ "s"

          else
            ""
        ]


toHrsMinsSecs : Int -> ( Int, Int, Int )
toHrsMinsSecs seconds =
    let
        hrs =
            (seconds // 60) // 60

        mins =
            (seconds // 60) - (60 * hrs)

        secs =
            remainderBy 60 seconds
    in
    ( hrs, mins, secs )


fromString : String -> Maybe Int
fromString str =
    case Parser.run parser str of
        Ok [ 0, mins, secs ] ->
            Just (mins * 60 + secs)

        Ok [ hrs, mins, secs ] ->
            Just (hrs * 60 * 60 + mins * 60 + secs)

        Ok [ hrs, mins ] ->
            Just (hrs * 60 * 60 + mins * 60)

        Ok [ mins ] ->
            Just (mins * 60)

        _ ->
            Nothing


timeToSeconds : Int -> Int -> Int -> Int
timeToSeconds hours minutes seconds =
    (hours * 60 * 60)
        + (minutes * 60)
        + seconds


timeStrToSeconds : String -> Result String Int
timeStrToSeconds str =
    let
        times =
            timeStrToHrsMinsSecs str
    in
    case times of
        Ok [ hours, minutes, seconds ] ->
            Ok (timeToSeconds hours minutes seconds)

        _ ->
            Err ("Invalid time: " ++ str)


leadingField : Parser Int
leadingField =
    Parser.succeed ()
        |. Parser.chompWhile Char.isDigit
        |> Parser.mapChompedString
            (\s a -> String.toInt s)
        |> Parser.andThen
            (\maybeInt ->
                case maybeInt of
                    Just i ->
                        Parser.succeed i

                    Nothing ->
                        Parser.problem "Failed to parse int"
            )


trailingField : Parser Int
trailingField =
    Parser.succeed identity
        |. Parser.symbol ":"
        |= (Parser.getChompedString <| Parser.chompWhile Char.isDigit)
        |> Parser.andThen
            (\f ->
                case String.length f of
                    2 ->
                        Parser.succeed f

                    _ ->
                        Parser.problem "Field incorrect length"
            )
        |> Parser.andThen
            (\f ->
                case String.toInt f of
                    Just n ->
                        Parser.succeed n

                    Nothing ->
                        Parser.problem "Invalid number"
            )
        |> Parser.andThen
            (\n ->
                if n > 59 then
                    Parser.problem "Invalid time"

                else
                    Parser.succeed n
            )


parser : Parser (List Int)
parser =
    Parser.succeed (\a b -> a :: b)
        |= leadingField
        |= (Parser.loop [] <|
                \fields ->
                    Parser.oneOf
                        [ Parser.succeed (\field -> Parser.Loop (field :: fields)) |= trailingField
                        , Parser.map (\_ -> Parser.Done (List.reverse fields)) (Parser.succeed () |. Parser.end)
                        ]
           )


timeStrToHrsMinsSecs : String -> Result (List Parser.DeadEnd) (List Int)
timeStrToHrsMinsSecs str =
    Parser.run parser str


stripTimeStr : String -> String
stripTimeStr str =
    case timeStrToHrsMinsSecs str of
        Ok [ 0, min, sec ] ->
            String.fromInt min
                ++ ":"
                ++ formatSeconds sec

        _ ->
            str


formatSeconds : Int -> String
formatSeconds sec =
    if sec < 10 then
        "0" ++ String.fromInt sec

    else
        String.fromInt sec
