module UI.Navbar exposing (default, view, withBackButton, withItems, withLoading, withSecondRow)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import MonoIcons
import UI exposing (spinner)
import UI.Layout exposing (..)
import UI.Util exposing (borderStyle)


default : Config msg
default =
    { loading = False
    , items = []
    , backButton = Nothing
    , secondRow = Nothing
    }


withLoading : Bool -> Config msg -> Config msg
withLoading loading config =
    { config | loading = loading }


withItems : List (Html msg) -> Config msg -> Config msg
withItems items config =
    { config | items = items }


withBackButton : Html msg -> Config msg -> Config msg
withBackButton override config =
    { config | backButton = Just override }


withSecondRow : Html msg -> Config msg -> Config msg
withSecondRow row config =
    { config | secondRow = Just row }


type alias Config msg =
    { loading : Bool
    , items : List (Html msg)
    , backButton : Maybe (Html msg)
    , secondRow : Maybe (Html msg)
    }


view : Config msg -> Html msg
view { loading, items, backButton, secondRow } =
    let
        container body =
            Html.header
                [ class "row compact no-select"
                , borderStyle "border-bottom"
                , style "position" "sticky"
                , style "top" "0"
                , style "z-index" "4"
                , style "background-color" "white"
                ]
                [ Html.div [ class "container" ]
                    body
                ]
    in
    container
        [ row
            [ style "padding" "0.5rem", style "height" "2.2rem" ]
            [ compactColumn [ style "justify-content" "center" ] [ backButton |> Maybe.withDefault UI.logo ]
            , column [] [ row [ style "justify-content" "center" ] items ]
            , if loading then
                spinner "1.5rem"

              else
                text ""
            ]
        , secondRow |> Maybe.withDefault (Html.text "")
        ]
