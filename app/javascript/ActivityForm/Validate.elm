module ActivityForm.Validate exposing (init, validate)

import ActivityForm.Types exposing (ActivityForm, FieldError(..), ValidatedFields)
import Date exposing (Date)
import Pace


init : ValidatedFields
init =
    { date = Err MissingError
    , repeats = Err MissingError
    , duration = Err MissingError
    , pace = Err MissingError
    , distance = Err MissingError
    }


validate : ActivityForm -> ValidatedFields
validate model =
    { date = Result.fromMaybe MissingError model.date
    , repeats = parseRepeats model.repeats
    , duration = parseDuration model.duration
    , pace = parsePace model.pace
    , distance = parseDistance model.distance
    }


parseRepeats : Maybe String -> Result FieldError Int
parseRepeats strM =
    case strM of
        Nothing ->
            Err MissingError

        Just "" ->
            Err MissingError

        Just str ->
            case String.toInt str of
                Nothing ->
                    Err ParseError

                Just 0 ->
                    Err ValueError

                Just num ->
                    Ok num


parseDuration : ( String, String, String ) -> Result FieldError Int
parseDuration ( hrs, mins, secs ) =
    let
        toIntResult str =
            if str == "" then
                Ok 0

            else
                String.toInt str |> Result.fromMaybe ParseError
    in
    Result.map3
        (\h m s -> h * 60 * 60 + m * 60 + s)
        (toIntResult hrs)
        (toIntResult mins)
        (toIntResult secs)


parsePace : String -> Result FieldError Int
parsePace str =
    Pace.paceFromString str |> Result.fromMaybe ParseError


parseDistance : String -> Result FieldError Int
parseDistance str =
    case str of
        "" ->
            Err ValueError

        _ ->
            case String.toInt str of
                Nothing ->
                    Err ParseError

                Just 0 ->
                    Err ValueError

                Just num ->
                    Ok num
