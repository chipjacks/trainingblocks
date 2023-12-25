module Emoji exposing (EmojiDict, default, filter, get, recommended, toDict, toList, view)

import Dict exposing (Dict)
import EmojiData exposing (EmojiData)
import EmojiData.Category
import EmojiData.List
import EmojiData.View
import Html exposing (Html)


type alias EmojiDict =
    Dict String EmojiData


default : EmojiData
default =
    { category = EmojiData.Category.SmileysAndEmotion, char = "😃", keywords = [], name = "smiley", sprite = ( 30, 35 ) }


recommended : EmojiDict -> List EmojiData
recommended emojis =
    [ get emojis "slightly_smiling_face"
    , get emojis "neutral_face"
    , get emojis "disappointed"
    , get emojis "dizzy_face"
    , get emojis "face_with_thermometer"
    , get emojis "face_with_head_bandage"
    ]
        |> List.map (Maybe.withDefault default)


filter : List EmojiData -> String -> List EmojiData
filter emojis name =
    EmojiData.search emojis name
        |> List.sortBy (\e -> String.length e.name)


toDict : List EmojiData -> EmojiDict
toDict _ =
    List.map (\e -> ( e.name, e )) EmojiData.List.emojis
        |> Dict.fromList


toList : EmojiDict -> List EmojiData
toList emojiDict =
    Dict.toList emojiDict
        |> List.map Tuple.second


get : EmojiDict -> String -> Maybe EmojiData
get emojis name =
    Dict.get name emojis


view : EmojiData -> Html msg
view emoji =
    EmojiData.View.emoji EmojiData.View.Twitter 22 emoji.sprite
