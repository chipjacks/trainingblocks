module Route exposing (Route(..), fromUrl, toString)

import Date exposing (Date, Unit(..), fromRataDie, toRataDie)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, custom, int, oneOf, s, map)
import Zoom


type Route
    = Zoom Zoom.Model
    | NotFound


toString : Route -> String
toString route =
    let
        pieces =
            case route of
                Zoom model ->
                    [ model.level |> Debug.toString |> String.toLower, model.end |> toRataDie |> String.fromInt ]

                NotFound ->
                    [ "notfound" ]
    in
    "#/" ++ String.join "/" pieces


fromUrl : Url -> Route
fromUrl url =
    case Url.Parser.parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFound


-- INTERNAL

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Zoom <| map Zoom.initModel (zoomLevel </> zoomDate)
        ]


zoomLevel : Parser (Unit -> a) a
zoomLevel =
    custom "LEVEL" <|
        \segment ->
            case segment of
                "year" ->
                    Just Years

                "month" ->
                    Just Months

                "week" ->
                    Just Weeks

                _ ->
                    Nothing


zoomDate : Parser (Date -> a) a
zoomDate =
    custom "DATE" <|
        \segment ->
            String.toInt segment |> Maybe.map fromRataDie

