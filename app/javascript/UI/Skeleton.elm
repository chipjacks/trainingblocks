module UI.Skeleton exposing (default, view, withAttributes, withBody, withNavbar)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, style)
import MonoIcons
import UI
import UI.Layout exposing (column, compactColumn, expandingRow, row)
import UI.Navbar as Navbar
import UI.Util exposing (attributeIf, borderStyle, styleIf, viewIf)


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
    column (style "min-height" "100%" :: attrs)
        [ navbar
        , expandingRow []
            [ viewSidebar
            , div [ class "container" ] [ body ]
            ]
        ]


viewSidebar : Html msg
viewSidebar =
    compactColumn [ class "sidebar", borderStyle "border-right" ]
        [ div [ class "sidebar__content" ]
            [ viewSidebarLink MonoIcons.calendar "Calendar" "/calendar" False []
            , viewSidebarLink MonoIcons.barChartAlt "Trends" "/trends" True []
            , viewSidebarLink MonoIcons.settings "Settings" "/settings" False []
            , viewSidebarLink MonoIcons.logOut "Log out" "/users/sign_out" False [ Html.Attributes.attribute "data-method" "delete" ]
            ]
        ]


viewSidebarLink : (String -> Html msg) -> String -> String -> Bool -> List (Html.Attribute msg) -> Html msg
viewSidebarLink icon name dest selected attrs =
    let
        color =
            if selected then
                "var(--orange-700)"

            else
                "#3d3d3d"
    in
    a
        ([ class "button button--basic row"
         , style "padding-top" "1rem"
         , style "padding-bottom" "1rem"
         , style "align-items" "center"
         , class "sidebar__item"
         , attributeIf selected (class "sidebar__item--selected")
         , style "color" color
         , href dest
         ]
            ++ attrs
        )
        [ MonoIcons.icon (icon color)
        , compactColumn [ class "sidebar__text", style "margin-left" "10px" ] [ text name ]
        ]
