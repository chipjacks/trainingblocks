module ActivityForm.Validate exposing (validate, init)

import ActivityForm.Types exposing (ActivityForm, FieldError(..), ValidatedFields)
import Date exposing (Date)
import Emoji
import Pace


init : ValidatedFields
init =
    ValidatedFields (Err MissingError) (Err MissingError) (Err MissingError) (Err MissingError) (Err MissingError)

validate : ActivityForm -> ValidatedFields
validate model =
    ValidatedFields
        (Result.fromMaybe MissingError model.date)
        (Ok 2)
        (parseDuration model.duration)
        (parsePace model.pace)
        (validateEmojiName model.emoji)


parseDuration : ( String, String, String ) -> Result FieldError Int
parseDuration ( hrs, mins, secs ) =
    Maybe.map3
        (\h m s -> h * 60 * 60 + m * 60 + s)
        (String.toInt hrs)
        (String.toInt mins)
        (String.toInt secs)
        |> Result.fromMaybe ParseError


parsePace : String -> Result FieldError Int
parsePace str =
    Pace.paceFromString str |> Result.fromMaybe ParseError


validateEmojiName : String -> Result FieldError String
validateEmojiName name =
    Emoji.get name |> Result.fromMaybe ParseError |> Result.map (\a -> name)
