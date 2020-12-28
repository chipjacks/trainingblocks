module Duration exposing (formatSeconds, stripTimeStr, timeStrToHrsMinsSecs, timeStrToSeconds, timeToSeconds)

import Parser exposing ((|.), (|=), Parser)


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
    Parser.mapChompedString
        (\s a -> String.toInt s |> Maybe.withDefault 0)
    <|
        Parser.succeed ()
            |. Parser.chompWhile Char.isDigit


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


durationParser : Parser (List Int)
durationParser =
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
    Parser.run durationParser str


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
