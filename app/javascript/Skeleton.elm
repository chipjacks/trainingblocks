module Skeleton exposing (dropdown, iconButton, layout, logo, spinner)

import Html exposing (Html, div, i, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events
import Json.Decode as Decode
import MonoIcons
import UI.Layout exposing (..)
import UI.Util exposing (styleIf)


layout : List (Html msg) -> Html msg -> Html msg
layout navbar page =
    column []
        [ Html.header [ class "row compact no-select navbar" ]
            [ column [ class "container" ]
                navbar
            ]
        , expandingRow []
            [ column [ class "container" ]
                [ page ]
            ]
        ]


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
