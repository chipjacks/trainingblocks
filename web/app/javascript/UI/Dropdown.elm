module UI.Dropdown exposing (default, view, withAttributes)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Config msg =
    { header : Html msg
    , items : List ( String, msg )
    , attrs : List (Html.Attribute msg)
    }


default : Html msg -> List ( String, msg ) -> Config msg
default header items =
    { header = header
    , items = items
    , attrs = []
    }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = config.attrs ++ attrs }


view : Config msg -> Html msg
view config =
    div (class "dropdown" :: config.attrs)
        [ config.header
        , div [ class "dropdown-content" ]
            (List.map viewDropdownItem config.items)
        ]


viewDropdownItem : ( String, msg ) -> Html msg
viewDropdownItem ( label, msg ) =
    a [ onClick msg ] [ text label ]
