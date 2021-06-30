module UI.Input exposing (Size(..), pace, text, view, withAppearance, withAttributes, withError, withPlaceholder, withResultError)

import Html exposing (Html, input)
import Html.Attributes exposing (class, placeholder, style, type_, value)
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
    }


text : (String -> msg) -> Config msg
text toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Nothing
    , attrs = []
    , size = Medium
    }


pace : (String -> msg) -> Config msg
pace toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Just "mm:ss"
    , attrs = [ style "width" "3rem" ]
    , size = Medium
    }


withAppearance : Size -> Config msg -> Config msg
withAppearance size config =
    { config | size = size }


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

        Ok str ->
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
         ]
            ++ config.attrs
        )
        []
