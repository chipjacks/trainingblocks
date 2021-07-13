module UI.Input exposing (Size(..), number, pace, text, view, withAppearance, withAttributes, withError, withLabel, withPlaceholder, withResultError)

import Html exposing (Html, input)
import Html.Attributes exposing (attribute, class, name, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import UI.Util exposing (attributeMaybe)
import Validate exposing (FieldError)


type Size
    = Tiny
    | Medium


type alias Config msg =
    { type_ : String
    , toMsg : String -> msg
    , error : Maybe FieldError
    , placeholder : Maybe String
    , attrs : List (Html.Attribute msg)
    , size : Size
    , label : Maybe String
    }


text : (String -> msg) -> Config msg
text toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Nothing
    , attrs = []
    , size = Medium
    , label = Nothing
    }


number : (String -> msg) -> Int -> Config msg
number toMsg max =
    { type_ = "number"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Nothing
    , attrs =
        [ Html.Attributes.min "0"
        , Html.Attributes.max (String.fromInt max)
        , Html.Attributes.step "1"
        , Html.Attributes.maxlength (String.length (String.fromInt max))
        , Html.Attributes.autocomplete False
        ]
    , size = Medium
    , label = Nothing
    }


pace : (String -> msg) -> Config msg
pace toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Just "mm:ss"
    , attrs = [ style "width" "3rem" ]
    , size = Medium
    , label = Nothing
    }


withAppearance : Size -> Config msg -> Config msg
withAppearance size config =
    { config | size = size }


withLabel : String -> Config msg -> Config msg
withLabel label config =
    { config | label = Just label }


withAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
withAttributes attrs config =
    { config | attrs = config.attrs ++ attrs }


withError : FieldError -> Config msg -> Config msg
withError error config =
    { config | error = Just error }


withResultError : Result FieldError String -> Config msg -> Config msg
withResultError result config =
    case result of
        Err error ->
            { config | error = Just error }

        Ok _ ->
            config


withPlaceholder : String -> Config msg -> Config msg
withPlaceholder placeholder config =
    { config | placeholder = Just placeholder }


view : String -> Config msg -> Html msg
view currentValue config =
    let
        sizeClass =
            case config.size of
                Tiny ->
                    "input--tiny"

                Medium ->
                    ""
    in
    input
        ([ onInput config.toMsg
         , type_ config.type_
         , class "input"
         , class sizeClass
         , value currentValue
         , attributeMaybe config.placeholder placeholder
         , attributeMaybe config.error (\_ -> class "input--error")
         , attributeMaybe config.label (\label -> attribute "aria-label" label)
         , attributeMaybe config.label (\label -> name label)
         ]
            ++ config.attrs
        )
        []
