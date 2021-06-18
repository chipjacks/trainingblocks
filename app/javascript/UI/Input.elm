module UI.Input exposing (pace, text, view, withError, withPlaceholder, withResultError)

import Html exposing (Html, input)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import UI.Util exposing (attributeMaybe)
import Validate exposing (FieldError)


type alias Config msg =
    { type_ : String
    , toMsg : String -> msg
    , error : Maybe FieldError
    , placeholder : Maybe String
    , attrs : List (Html.Attribute msg)
    }


text : (String -> msg) -> Config msg
text toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Nothing
    , attrs = []
    }


pace : (String -> msg) -> Config msg
pace toMsg =
    { type_ = "text"
    , toMsg = toMsg
    , error = Nothing
    , placeholder = Just "mm:ss"
    , attrs = [ style "width" "3rem" ]
    }


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
    input
        ([ onInput config.toMsg
         , type_ config.type_
         , class "input"
         , value currentValue
         , attributeMaybe config.placeholder placeholder
         , attributeMaybe config.error (\_ -> class "error")
         ]
            ++ config.attrs
        )
        []
