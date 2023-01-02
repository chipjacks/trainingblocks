module ActivityForm.Validate exposing (init, validate)

import Activity.Types exposing (DistanceUnits(..))
import ActivityForm.Types exposing (ActivityForm, ValidatedFields)
import Distance
import Duration
import Pace
import Validate exposing (FieldError(..), parseDuration, parsePace)


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
        |> calculatePace model.distanceUnits
        |> calculateDistance model.distanceUnits
        |> calculateDuration model.distanceUnits


calculatePace : DistanceUnits -> ValidatedFields -> ValidatedFields
calculatePace units validated =
    let
        paceM =
            Maybe.map2
                Pace.calculate
                (Result.toMaybe validated.duration)
                (Result.toMaybe validated.distance |> Maybe.map (Distance.toMeters units))
    in
    case ( validated.pace, paceM ) of
        ( Err MissingError, Just pace ) ->
            { validated | pace = Ok pace }

        _ ->
            validated


calculateDistance : DistanceUnits -> ValidatedFields -> ValidatedFields
calculateDistance units validated =
    let
        distanceM =
            Maybe.map2
                (Distance.calculate units)
                (Result.toMaybe validated.duration)
                (Result.toMaybe validated.pace)
    in
    case ( validated.distance, distanceM ) of
        ( Err MissingError, Just distance ) ->
            { validated | distance = Ok distance }

        _ ->
            validated


calculateDuration : DistanceUnits -> ValidatedFields -> ValidatedFields
calculateDuration units validated =
    let
        durationM =
            Maybe.map2
                (Duration.calculate)
                (Result.toMaybe validated.distance |> Maybe.map (Distance.toMeters units))
                (Result.toMaybe validated.pace)
    in
    case ( validated.duration, durationM ) of
        ( Err MissingError, Just duration ) ->
            { validated | duration = Ok duration }

        _ ->
            validated


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


parseDistance : String -> Result FieldError Float
parseDistance str =
    case str of
        "" ->
            Err MissingError

        _ ->
            case String.toFloat str of
                Nothing ->
                    Err ParseError

                Just num ->
                    Ok num
