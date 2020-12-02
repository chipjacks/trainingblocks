module Skeleton exposing (attributeIf, borderStyle, column, compactColumn, expandingRow, layout, logo, row, spinner, styleIf, viewIf, viewMaybe)

import Html exposing (Html, div, i, img, text)
import Html.Attributes exposing (class, src, style)


layout : Html msg -> Html msg -> Html msg
layout navbar page =
    column [ class "container-y" ]
        [ row [ class "navbar no-select" ]
            [ column [ class "container-x" ]
                [ navbar ]
            ]
        , expandingRow []
            [ column [ class "container-x" ]
                [ page ]
            ]
        ]


logo : Html msg
logo =
    Html.img [ style "height" "2rem", Html.Attributes.src "icon.svg" ] []


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attributes children =
    div
        (class "row compact" :: attributes)
        children


expandingRow : List (Html.Attribute msg) -> List (Html msg) -> Html msg
expandingRow attributes children =
    div
        (class "row expand" :: attributes)
        children


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attributes children =
    div
        (class "column expand" :: attributes)
        children


compactColumn : List (Html.Attribute msg) -> List (Html msg) -> Html msg
compactColumn attributes children =
    div
        (class "column compact" :: attributes)
        children


spinner : String -> Html msg
spinner fontSize =
    div [ class "spinner", style "width" fontSize, style "height" fontSize ] []


styleIf : Bool -> String -> String -> Html.Attribute msg
styleIf bool name value =
    if bool then
        style name value

    else
        style "" ""


viewIf : Bool -> Html msg -> Html msg
viewIf bool html =
    if bool then
        html

    else
        Html.text ""


viewMaybe : Maybe a -> (a -> Html msg) -> Html msg
viewMaybe attrM viewF =
    case attrM of
        Just attr ->
            viewF attr

        Nothing ->
            Html.text ""


attributeIf : Bool -> Html.Attribute msg -> Html.Attribute msg
attributeIf bool attr =
    if bool then
        attr

    else
        style "" ""


borderStyle : String -> Html.Attribute msg
borderStyle position =
    style position "1px solid var(--border-gray)"
