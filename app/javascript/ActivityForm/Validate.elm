module ActivityForm.Validate exposing (init, validate)

import ActivityForm.Types exposing (ActivityForm, FieldError(..), ValidatedFields)
import Date exposing (Date)
import Emoji
import Pace


init : ValidatedFields
init =
    { date = Err MissingError
    , repeats = Err MissingError
    , duration = Err MissingError
    , pace = Err MissingError
    , emoji = Err MissingError
    }


validate : ActivityForm -> ValidatedFields
validate model =
    { date = Result.fromMaybe MissingError model.date
    , repeats = Maybe.withDefault "" model.repeats |> String.toInt |> Result.fromMaybe ParseError
    , duration = parseDuration model.duration
    , pace = parsePace model.pace
    , emoji = validateEmojiName model.emoji
    }


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


validateEmojiName : String -> Result FieldError String
validateEmojiName name =
    Emoji.get name |> Result.fromMaybe ParseError |> Result.map (\a -> name)
