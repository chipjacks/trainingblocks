module UI.Navbar exposing (default, view, withItems, withLoading, withLogoOverride, withSecondRow)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import MonoIcons
import Skeleton exposing (borderStyle, column, compactColumn, row, spinner)


default : Config msg
default =
    { loading = False
    , items = []
    , logoOverride = Nothing
    , secondRow = Nothing
    }


withLoading : Bool -> Config msg -> Config msg
withLoading loading config =
    { config | loading = loading }


withItems : List (Html msg) -> Config msg -> Config msg
withItems items config =
    { config | items = items }


withLogoOverride : Html msg -> Config msg -> Config msg
withLogoOverride override config =
    { config | logoOverride = Just override }


withSecondRow : Html msg -> Config msg -> Config msg
withSecondRow row config =
    { config | secondRow = Just row }


type alias Config msg =
    { loading : Bool
    , items : List (Html msg)
    , logoOverride : Maybe (Html msg)
    , secondRow : Maybe (Html msg)
    }


view : Config msg -> Html msg
view { loading, items, logoOverride, secondRow } =
    let
        container body =
            Html.header
                [ class "row compact no-select"
                , borderStyle "border-bottom"
                ]
                [ column [ class "container" ]
                    body
                ]

        dropdown =
            compactColumn [ style "min-width" "1.5rem", style "justify-content" "center" ]
                [ Skeleton.dropdown True
                    (if loading then
                        spinner "1.5rem"

                     else
                        div [ style "font-size" "1.4rem", style "padding-top" "2px" ] [ MonoIcons.icon (MonoIcons.optionsVertical "var(--grey-900)") ]
                    )
                    [ a [ Html.Attributes.href " /users/sign_out", Html.Attributes.attribute "data-method" "delete" ] [ text "Logout" ] ]
                ]
    in
    container
        [ row
            [ style "padding" "0.5rem", style "height" "2.2rem" ]
            [ compactColumn [ style "justify-content" "center" ] [ logoOverride |> Maybe.withDefault Skeleton.logo ]
            , column [] [ row [ style "justify-content" "center" ]  items ]
            , dropdown
            ]
        , secondRow |> Maybe.withDefault (Html.text "")
        ]
