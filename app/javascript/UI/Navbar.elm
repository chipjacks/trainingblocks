module UI.Navbar exposing (default, view, withItems, withLeftItem, withRightItem, withSecondRow)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI exposing (spinner)
import UI.Layout exposing (..)
import UI.Util exposing (borderStyle, viewMaybe)


default : Config msg
default =
    { leftItem = Nothing
    , items = []
    , rightItem = Nothing
    , secondRow = Nothing
    }


withLeftItem : Html msg -> Config msg -> Config msg
withLeftItem item config =
    { config | leftItem = Just item }


withItems : List (Html msg) -> Config msg -> Config msg
withItems items config =
    { config | items = items }


withRightItem : Html msg -> Config msg -> Config msg
withRightItem item config =
    { config | rightItem = Just item }


withSecondRow : Html msg -> Config msg -> Config msg
withSecondRow row config =
    { config | secondRow = Just row }


type alias Config msg =
    { leftItem : Maybe (Html msg)
    , items : List (Html msg)
    , rightItem : Maybe (Html msg)
    , secondRow : Maybe (Html msg)
    }


view : Config msg -> Html msg
view { leftItem, items, rightItem, secondRow } =
    let
        height =
            case secondRow of
                Nothing ->
                    "3.2rem"

                Just _ ->
                    "4.4rem"

        container body =
            Html.header
                [ class "row compact no-select"
                , borderStyle "border-bottom"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "right" "0"
                , style "z-index" "4"
                , style "background-color" "white"
                , style "height" height
                ]
                [ compactColumn [ class "sidebar", style "justify-content" "center", style "padding-left" "10px" ] [ UI.logo ]
                , Html.div [ class "container" ]
                    body
                , Html.node "style" [] [ text (":root { --navbar-height: " ++ height ++ " }") ]
                ]
    in
    container
        [ row
            [ style "padding" "0.5rem" ]
            [ compactColumn [ style "justify-content" "center", style "min-width" "50px", style "align-items" "flex-start" ] [ viewMaybe leftItem identity ]
            , column [] [ row [ style "justify-content" "center" ] items ]
            , compactColumn [ style "justify-content" "center", style "min-width" "50px", style "align-items" "flex-end" ] [ viewMaybe rightItem identity ]
            ]
        , secondRow |> Maybe.withDefault (Html.text "")
        ]
