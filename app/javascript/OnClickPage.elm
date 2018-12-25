module OnClickPage exposing (onClickPage)

import Html exposing (Attribute)
import Html.Attributes exposing (href, style)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode exposing (Decoder)
import Msg exposing (Msg(..))
import Route exposing (Route)


onClickPage : Route -> List (Attribute Msg)
onClickPage page =
    [ style "pointer" "cursor"
    , href (Route.toString page)
    , onPreventDefaultClick (NewPage page)
    ]


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Decoder Bool
preventDefault2 =
    Json.Decode.map2
        invertedOr
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)
