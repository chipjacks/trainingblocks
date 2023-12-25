module UI.Label exposing (field, input, view, withOnClear)

import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import MonoIcons
import UI.Layout exposing (compactColumn, row)
import UI.Util exposing (viewMaybe)


type Size
    = Field
    | Input


type alias Config msg =
    { name : String
    , onClear : Maybe msg
    , size : Size
    }


field : String -> Config msg
field name =
    { name = name
    , onClear = Nothing
    , size = Field
    }


input : String -> Config msg
input name =
    { name = name
    , onClear = Nothing
    , size = Input
    }


withOnClear : msg -> Config msg -> Config msg
withOnClear msg config =
    { config | onClear = Just msg }


view : Config msg -> Html msg
view config =
    let
        sizeClass =
            case config.size of
                Field ->
                    "label--field"

                Input ->
                    "label--input"
    in
    row []
        [ Html.label
            [ class sizeClass
            , Html.Attributes.attribute "for" (config.name |> String.toLower)
            ]
            [ text config.name ]
        , viewMaybe config.onClear
            (\onClear ->
                compactColumn
                    [ style "margin-left" "0.2rem"
                    , style "cursor" "pointer"
                    , style "margin-top" "-2px"
                    , style "margin-bottom" "-2px"
                    , onClick onClear
                    ]
                    [ MonoIcons.icon (MonoIcons.close "var(--grey-900)")
                    ]
            )
        ]
