module UI exposing (dropdown, iconButton, logo, spinner)

import Html exposing (Html, div, i, img, text)
import Html.Attributes exposing (class, src, style)
import UI.Layout exposing (..)
import UI.Util exposing (styleIf)


logo : Html msg
logo =
    Html.img [ style "height" "2rem", Html.Attributes.src "icon.svg" ] []


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


dropdown : Bool -> Html msg -> List (Html msg) -> Html msg
dropdown rightAlign header content =
    div [ class "dropdown" ]
        [ header
        , div [ class "dropdown-content", styleIf rightAlign "right" "0" ]
            content
        ]
