module UI.Skeleton exposing (default, view, withAttributes, withBody, withNavbar)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, style)
import MonoIcons
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Util exposing (borderStyle)


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
        , row []
            [ viewSidebar
            , div [ class "container" ] [ body ]
            ]
        ]


viewSidebar : Html msg
viewSidebar =
    compactColumn [ class "sidebar", borderStyle "border-right" ]
        [ viewSidebarLink MonoIcons.calendar "Calendar" "/calendar" []
        , viewSidebarLink MonoIcons.barChartAlt "Trends" "/trends" []
        , viewSidebarLink MonoIcons.settings "Settings" "/settings" []
        , viewSidebarLink MonoIcons.logOut "Log out" "/users/sign_out" [ Html.Attributes.attribute "data-method" "delete" ]
        ]


viewSidebarLink : (String -> Html msg) -> String -> String -> List (Html.Attribute msg) -> Html msg
viewSidebarLink icon name dest attrs =
    a
        ([ class "button button--basic row"
         , style "padding-top" "1rem"
         , style "padding-bottom" "1rem"
         , style "align-items" "center"
         , href dest
         ]
            ++ attrs
        )
        [ MonoIcons.icon (icon "#3d3d3d")
        , compactColumn [ style "width" "10px" ] []
        , compactColumn [ class "sidebar__text" ] [ text name ]
        ]
