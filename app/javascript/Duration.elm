module Duration exposing (formatSeconds, stripTimeStr, timeStrToHrsMinsSecs, timeStrToSeconds, timeToSeconds)


type alias Minutes =
    Int


type alias Seconds =
    Int


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
        [ hours, minutes, seconds ] ->
            Ok (timeToSeconds hours minutes seconds)

        _ ->
            Err ("Invalid time: " ++ str)


timeStrToHrsMinsSecs : String -> List Int
timeStrToHrsMinsSecs str =
    String.split ":" str
        |> List.map (String.toInt >> Maybe.withDefault 0)


stripTimeStr : String -> String
stripTimeStr str =
    case timeStrToHrsMinsSecs str of
        [ 0, min, sec ] ->
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
