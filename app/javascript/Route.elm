module Route exposing (Route(..), ZoomLevel(..), parseLocation, toString)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, int, custom)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Date exposing (Date)
import Date.Extra exposing (fromRataDie, toRataDie)


type Route
    = Zoom ZoomLevel Date
    | NotFound


type ZoomLevel
    = Year
    | Month
    | Week


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ Url.map Zoom (zoomLevel </> zoomDate)
        ]


zoomLevel : Parser (ZoomLevel -> a) a
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
                Zoom Year date ->
                    [ "year", date |> toRataDie |> Basics.toString ]

                Zoom Month date ->
                    [ "month", date |> toRataDie |> Basics.toString ]

                Zoom Week date ->
                    [ "week", date |> toRataDie |> Basics.toString ]

                NotFound ->
                    [ "notfound" ]
    in
        "#/" ++ String.join "/" pieces


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFound
