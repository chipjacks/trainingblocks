module UI.Skeleton exposing (default, view, withAttributes, withBody, withNavbar)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import UI.Layout exposing (column, expandingRow)
import UI.Navbar as Navbar


type alias Config msg =
    { navbar : Html msg
    , body : Html msg
    , attrs : List (Html.Attribute msg)
    }


default : Config msg
default =
    { navbar = Navbar.default |> Navbar.view
    , attrs = []
    , body = Html.text "Content"
    }


withNavbar : Html msg -> Config msg -> Config msg
withNavbar navbar config =
    { config | navbar = navbar }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = attrs }


withBody : Html msg -> Config msg -> Config msg
withBody body config =
    { config | body = body }


view : Config msg -> Html msg
view { navbar, attrs, body } =
    column attrs
        [ navbar
        , div [ class "container" ] [ body ]
        ]
