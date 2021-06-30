module UI.Skeleton exposing (default, view, withBody, withContainer, withNavbar)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import UI.Layout exposing (column, expandingRow)
import UI.Navbar as Navbar


type alias Config msg =
    { navbar : Html msg
    , container : Html msg -> Html msg
    , body : Html msg
    }


default : Config msg
default =
    { navbar = Navbar.default |> Navbar.view
    , container = defaultContainer
    , body = Html.text "Content"
    }


defaultContainer : Html msg -> Html msg
defaultContainer =
    \body ->
        expandingRow [ style "overflow-y" "scroll" ]
            [ column [ class "container" ]
                [ body ]
            ]


withNavbar : Html msg -> Config msg -> Config msg
withNavbar navbar config =
    { config | navbar = navbar }


withContainer : (Html msg -> Html msg) -> Config msg -> Config msg
withContainer container config =
    { config | container = container }


withBody : Html msg -> Config msg -> Config msg
withBody body config =
    { config | body = body }


view : Config msg -> Html msg
view { navbar, container, body } =
    column
        [ style "height" "100vh", style "width" "100vw", style "position" "absolute" ]
        [ navbar
        , container body
        ]
