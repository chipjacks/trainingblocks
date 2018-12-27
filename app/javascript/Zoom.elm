module Zoom exposing (Model, initModel, jump, newer, older, range, string, zoomIn)

import Date exposing (Date, Interval(..), Unit(..))


type alias Model =
    { level : Date.Unit
    , start : Date
    , end : Date
    }


initModel : Unit -> Date -> Model
initModel level endDate =
    let
        ( start, end ) =
            dateLimits level endDate
    in
    Model level start end


range : Model -> List Model
range zoom =
    Date.range (zoomIn zoom.level |> toInterval) 1 zoom.start zoom.end
        |> List.map (\d -> Model (zoomIn zoom.level) d (Date.add (zoomIn zoom.level) 1 d))


newer : Model -> Model
newer model =
    initModel model.level (Date.add (increment model.level) 1 model.end)


older : Model -> Model
older model =
    initModel model.level (Date.add (increment model.level) -1 model.end)


jump : Int -> Model -> Model
jump distance model =
    initModel model.level (Date.add model.level distance (Date.floor (toInterval model.level) model.end))


string : Model -> String
string model =
    Date.format "MMMM ddd, y" model.start ++ " - " ++ Date.format "MMMM ddd, y" model.end



-- INTERNAL


toInterval : Unit -> Interval
toInterval level =
    case level of
        Years ->
            Year

        Months ->
            Month

        Weeks ->
            Week

        Days ->
            Day


dateLimits : Unit -> Date -> ( Date, Date )
dateLimits level date =
    case level of
        Years ->
            ( Date.add Months -12 (Date.ceiling Quarter date)
            , Date.ceiling Quarter date
            )

        Months ->
            ( Date.add Weeks -5 (Date.ceiling Week date)
            , Date.ceiling Week date
            )

        Weeks ->
            ( Date.add Days -7 (Date.ceiling Day date)
            , Date.ceiling Day date
            )

        _ ->
            Debug.todo "Invalid interval"


zoomIn : Unit -> Unit
zoomIn level =
    case level of
        Years ->
            Months

        Months ->
            Weeks

        Weeks ->
            Days

        _ ->
            Debug.todo "Invalid interval"


increment : Unit -> Unit
increment level =
    case level of
        Years ->
            Months

        Months ->
            Weeks

        Weeks ->
            Days

        _ ->
            Debug.todo "Invalid interval"
