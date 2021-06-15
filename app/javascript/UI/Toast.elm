module UI.Toast exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI.Layout exposing (column, compactColumn, row)
import UI.Util exposing (borderStyle)


view : Bool -> Int -> Html msg -> Html msg
view positionTop stackHeight body =
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
