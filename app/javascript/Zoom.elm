module Zoom exposing (Model, initModel, range, zoomIn)

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



-- INTERNAL


dateLimits : Interval -> Date -> ( Date, Date )
dateLimits level date =
    case level of
        Month ->
            ( Date.add Week -4 (Date.floor Week date)
            , Date.ceiling Week date
            )

        Week ->
            ( Date.add Week -1 (Date.ceiling Week date)
            , Date.ceiling Week date
            )

        Year ->
            ( Date.add Quarter -3 (Date.floor Quarter date)
            , Date.ceiling Quarter date
            )

        _ ->
            ( Date.add level -1 (Date.floor (zoomIn level) date)
            , Date.ceiling (zoomIn level) date
            )


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
            Day
