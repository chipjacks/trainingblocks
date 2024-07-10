module UI.Toast exposing (Position(..), bottom, top, view, withAttributes, withStackHeight)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (attributeMaybe, borderStyle)


type Position
    = Top
    | Bottom


type alias Config msg =
    { position : Position
    , stackHeight : Maybe Int
    , attrs : List (Html.Attribute msg)
    }


top : Config msg
top =
    { position = Top
    , stackHeight = Nothing
    , attrs = []
    }


bottom : Config msg
bottom =
    { position = Bottom
    , stackHeight = Nothing
    , attrs = []
    }


withStackHeight : Int -> Config msg -> Config msg
withStackHeight height config =
    { config | stackHeight = Just height }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = config.attrs ++ attrs }


view : Html msg -> Config msg -> Html msg
view body { position, stackHeight, attrs } =
    let
        boxShadow =
            [ "white -3px -3px 0 -1px, var(--grey-500) -3px -3px"
            , "white -6px -6px 0 -1px, var(--grey-500) -6px -6px"
            , "white -9px -9px 0 -1px, var(--grey-500) -9px -9px"
            ]

        positionStyles =
            case position of
                Top ->
                    [ class "toast--top"
                    ]

                Bottom ->
                    [ class "toast--bottom"
                    ]
    in
    row
        ([ class "toast" ]
            ++ positionStyles
            ++ attrs
        )
        [ row
            [ case position of
                Top ->
                    style "justify-content" "flex-start"

                Bottom ->
                    style "justify-content" "flex-end"
            , style "margin" "10px"
            , style "max-width" "95%"
            ]
            [ row
                [ class "toast__content"
                , attributeMaybe stackHeight (\height -> style "box-shadow" (boxShadow |> List.take (height - 1) |> String.join ","))
                ]
                [ MonoIcons.icon (MonoIcons.circleInformation "var(--blue-500)")
                , compactColumn [ style "width" "5px" ] []
                , body
                ]
            ]
        ]
