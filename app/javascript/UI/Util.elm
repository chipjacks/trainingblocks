module UI.Util exposing (attributeIf, borderStyle, stopPropagationOnClick, styleIf, viewIf, viewMaybe, attributeMaybe)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode


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
viewMaybe viewM viewF =
    case viewM of
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


attributeMaybe : Maybe a -> (a -> Html.Attribute msg) -> Html.Attribute msg
attributeMaybe attrM attrF =
    case attrM of
        Just attr ->
            attrF attr

        Nothing ->
            style "" ""



stopPropagationOnClick : Decode.Decoder msg -> Html.Attribute msg
stopPropagationOnClick decoder =
    Html.Events.stopPropagationOn "pointerdown"
        (decoder
            |> Decode.map (\m -> ( m, True ))
        )


borderStyle : String -> Html.Attribute msg
borderStyle position =
    style position "1px solid var(--grey-500)"
