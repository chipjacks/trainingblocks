module UI exposing (iconButton, logo, spinner)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import UI.Layout exposing (..)
import UI.Util exposing (styleIf)


logo : Html msg
logo =
    Html.img [ class "navbar__logo", Html.Attributes.src "icon.svg" ] []


spinner : String -> Html msg
spinner fontSize =
    div [ class "spinner", style "width" fontSize, style "height" fontSize ] []


iconButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
iconButton attributes children =
    Html.button
        (attributes
            ++ [ class "button"
               , style "font-size" "0.8rem"
               , style "padding" "0.05rem 0.25rem"
               ]
        )
        children
