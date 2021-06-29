module Validate exposing (Field, FieldError(..), init, parsePace, update, updateFallback)

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
