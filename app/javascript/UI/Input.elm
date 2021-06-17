module UI.Input exposing (pace)

import Html exposing (Html, input)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)


pace : String -> (String -> msg) -> Result error String -> Html msg
pace value_ msg result =
    input
        [ onInput msg
        , class "input"
        , style "width" "3rem"
        , value value_
        , case ( value_, result ) of
            ( "", Ok paceStr ) ->
                Html.Attributes.placeholder paceStr

            _ ->
                Html.Attributes.placeholder "mm:ss"
        , case result of
            Err _ ->
                class "error"

            Ok _ ->
                class ""
        ]
        []
