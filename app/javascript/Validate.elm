module Validate exposing (Field, FieldError(..), init, parsePace, update)

import Pace


type FieldError
    = MissingError
    | ParseError
    | ValueError


type alias Field a b =
    { initial : a
    , value : a
    , result : Result FieldError b
    , parse : a -> Result FieldError b
    }


init : (a -> Result FieldError b) -> a -> Field a b
init parse value =
    { initial = value
    , value = value
    , result = parse value
    , parse = parse
    }


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
