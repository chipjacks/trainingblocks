module Zoom exposing (Model, initModel, range, zoomIn, newer, older, jump, string)

import Date exposing (Date)
import Date.Extra as Date exposing (Interval(..))


type alias Model =
    { level : Date.Interval
    , start : Date
    , end : Date
    }


initModel : Interval -> Date -> Model
initModel level endDate =
    let
        ( start, end ) =
            dateLimits level endDate
    in
        Model level start end


range : Model -> List Model
range zoom =
    Date.range (zoomIn zoom.level) 1 zoom.start zoom.end
        |> List.map (\d -> Model (zoomIn zoom.level) d (Date.add (zoomIn zoom.level) 1 d))


newer : Model -> Model
newer model =
    initModel model.level (Date.add (increment model.level) 1 model.end)


older : Model -> Model
older model =
    initModel model.level (Date.add (increment model.level) -1 model.end)


jump : Int -> Model -> Model
jump distance model =
    initModel model.level (Date.add model.level distance (Date.floor model.level model.end))


string : Model -> String
string model =
    Date.toFormattedString "MMMM ddd, y" model.start ++ " - " ++ Date.toFormattedString "MMMM ddd, y" model.end



-- INTERNAL


dateLimits : Interval -> Date -> ( Date, Date )
dateLimits level date =
    case level of
        Year ->
            ( Date.add Quarter -4 (Date.ceiling Quarter date)
            , Date.ceiling Quarter date
            )

        Month ->
            ( Date.add Week -5 (Date.ceiling Week date)
            , Date.ceiling Week date
            )

        Week ->
            ( Date.add Day -7 (Date.ceiling Day date)
            , Date.ceiling Day date
            )

        _ ->
            Debug.crash "Invalid interval"


zoomIn : Interval -> Interval
zoomIn level =
    case level of
        Year ->
            Month

        Month ->
            Week

        Week ->
            Day

        _ ->
            Debug.crash "Invalid interval"


increment : Interval -> Interval
increment level =
    case level of
        Year ->
            Quarter

        Month ->
            Week

        Week ->
            Day

        _ ->
            Debug.crash "Invalid interval"
