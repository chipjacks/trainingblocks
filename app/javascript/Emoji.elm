module Emoji exposing (default, filter, find, view)

import EmojiData exposing (EmojiData)
import EmojiData.Category
import EmojiData.List exposing (emojis)
import EmojiData.View
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


default : EmojiData
default =
    { category = EmojiData.Category.SmileysAndEmotion, char = "😃", keywords = [], name = "smiley", sprite = ( 30, 35 ) }


recommended : List EmojiData
recommended =
    [ find "smiley"
    , find "sweat smile"
    , find "relieved"
    , find "disappointed"
    , find "worried"
    , find "tired face"
    , find "grimacing"
    , find "dizzy face"
    , find "mask"
    , find "face with head bandage"
    ]


filter : String -> List EmojiData
filter name =
    if String.isEmpty name then
        recommended

    else
        EmojiData.search emojis name
            |> List.sortBy (\e -> String.length e.name)


find : String -> EmojiData
find name =
    filter name |> List.head |> Maybe.withDefault default


view : EmojiData -> Html msg
view emoji =
    EmojiData.View.emoji EmojiData.View.Twitter 22 emoji.sprite
