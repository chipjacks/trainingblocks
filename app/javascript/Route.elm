module Route exposing (Route(..), parseLocation, toString)

import Date exposing (Date)
import Date.Extra exposing (Interval(..), fromRataDie, toRataDie)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, custom, int, oneOf, parseHash, s)
import Zoom


type Route
    = Zoom Zoom.Model
    | NotFound


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ Url.map Zoom <| Url.map Zoom.initModel (zoomLevel </> zoomDate)
        ]


zoomLevel : Parser (Interval -> a) a
zoomLevel =
    custom "LEVEL" <|
        \segment ->
            case segment of
                "year" ->
                    Ok Year

                "month" ->
                    Ok Month

                "week" ->
                    Ok Week

                _ ->
                    Err "Invalid zoom level"


zoomDate : Parser (Date -> a) a
zoomDate =
    custom "DATE" <|
        \segment ->
            String.toInt segment |> Result.map fromRataDie


toString : Route -> String
toString route =
    let
        pieces =
            case route of
                Zoom model ->
                    [ model.level |> Basics.toString |> String.toLower, model.end |> toRataDie |> Basics.toString ]

                NotFound ->
                    [ "notfound" ]
    in
    "#/" ++ String.join "/" pieces


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFound
