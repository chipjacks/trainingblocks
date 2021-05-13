module Skeleton exposing (attributeIf, borderStyle, column, compactColumn, dropdown, expandingRow, iconButton, layout, logo, row, spinner, stopPropagationOnClick, styleIf, toast, viewIf, viewMaybe)

import Html exposing (Html, div, i, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events
import Json.Decode as Decode
import MonoIcons


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


toast : Bool -> Int -> Html msg -> Html msg
toast positionTop stackHeight body =
    let
        boxShadow =
            [ "white -3px -3px 0 -1px, var(--grey-500) -3px -3px"
            , "white -6px -6px 0 -1px, var(--grey-500) -6px -6px"
            , "white -9px -9px 0 -1px, var(--grey-500) -9px -9px"
            ]
    in
    row
        [ style "position" "fixed"
        , if positionTop then
            style "top" "70px"

          else
            style "bottom" "0"
        , if positionTop then
            style "animation" "slidein-top 0.5s"

          else
            style "animation" "slidein-bottom 0.5s"
        , style "height" "fit-content"
        , style "width" "100%"
        , style "z-index" "35"
        ]
        [ column [ class "container" ]
            [ row
                [ if positionTop then
                    style "justify-content" "flex-start"

                  else
                    style "justify-content" "flex-end"
                , style "margin" "10px"
                ]
                [ row
                    [ style "padding" "10px"
                    , style "background-color" "white"
                    , style "border-radius" "5px"
                    , style "align-items" "middle"
                    , borderStyle "border"
                    , style "box-shadow" (boxShadow |> List.take (stackHeight - 1) |> String.join ",")
                    ]
                    [ MonoIcons.icon (MonoIcons.circleInformation "var(--blue-500)")
                    , compactColumn [ style "width" "5px" ] []
                    , body
                    ]
                ]
            ]
        ]


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
    style position "1px solid var(--grey-500)"


stopPropagationOnClick : Decode.Decoder msg -> Html.Attribute msg
stopPropagationOnClick decoder =
    Html.Events.stopPropagationOn "pointerdown"
        (decoder
            |> Decode.map (\m -> ( m, True ))
        )
