module Validate exposing (Field, FieldError(..), init, parsePace, update, updateFallback, parseDuration)

import Pace


type FieldError
    = MissingError
    | ParseError
    | ValueError


type alias Field a b =
    { value : a
    , result : Result FieldError b
    , fallback : b
    , parse : a -> Result FieldError b
    }


init : (a -> Result FieldError b) -> b -> a -> Field a b
init parse fallback value =
    { value = value
    , result = parse value
    , fallback = fallback
    , parse = parse
    }


updateFallback : Field a b -> Field a b
updateFallback field =
    case field.result of
        Ok res ->
            { field | fallback = res }

        _ ->
            field


update : a -> Field a b -> Field a b
update value field =
    { field | value = value, result = field.parse value }


parsePace : String -> Result FieldError Int
parsePace str =
    case str of
        "" ->
            Err MissingError

        _ ->
            Pace.paceFromString str |> Result.fromMaybe ParseError


parseDuration : ( String, String, String ) -> Result FieldError Int
parseDuration ( hrs, mins, secs ) =
    let
        toIntResult str =
            if str == "" then
                Ok 0

            else
                String.toInt str |> Result.fromMaybe ParseError

        handleZeroTime result =
            case result of
                Ok 0 ->
                    Err ValueError

                _ ->
                    result
    in
    Result.map3
        (\h m s -> h * 60 * 60 + m * 60 + s)
        (toIntResult hrs)
        (toIntResult mins)
        (toIntResult secs)
        |> handleZeroTime
