module Emoji exposing (default, filter, get, recommended, view)

import Dict
import EmojiData exposing (EmojiData)
import EmojiData.Category
import EmojiData.List
import EmojiData.View
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


default : EmojiData
default =
    { category = EmojiData.Category.SmileysAndEmotion, char = "😃", keywords = [], name = "smiley", sprite = ( 30, 35 ) }


recommended : List EmojiData
recommended =
    [ get "smiley"
    , get "slightly_smiling_face"
    , get "neutral_face"
    , get "disappointed"
    , get "tired_face"
    ]
        |> List.map (Maybe.withDefault default)


filter : List EmojiData -> String -> List EmojiData
filter emojis name =
    EmojiData.search emojis name
        |> List.sortBy (\e -> String.length e.name)


get : String -> Maybe EmojiData
get name =
    List.map (\e -> ( e.name, e )) EmojiData.List.emojis
        |> Dict.fromList
        |> Dict.get name


view : EmojiData -> Html msg
view emoji =
    EmojiData.View.emoji EmojiData.View.Twitter 22 emoji.sprite
