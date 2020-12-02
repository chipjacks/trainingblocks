module Enum exposing (Enum, create)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode



-- SOURCE: https://discourse.elm-lang.org/t/enum-helper-package/3426


type alias Enum a =
    { toString : a -> String
    , fromString : String -> Maybe a
    , encode : a -> Value
    , decoder : Decoder a
    , dict : Dict String a
    , list : List ( String, a )
    }


create : List a -> (a -> String) -> Enum a
create list toStr =
    let
        list2 =
            list |> List.map (\a -> ( toStr a, a ))

        dict =
            Dict.fromList list2
    in
    { toString = toStr
    , fromString = \string -> Dict.get string dict
    , encode = toStr >> Encode.string
    , decoder =
        Decode.string
            |> Decode.andThen
                (\string ->
                    case Dict.get string dict of
                        Just a ->
                            Decode.succeed a

                        Nothing ->
                            Decode.fail ("Missing enum: " ++ string)
                )
    , dict = dict
    , list = list2
    }
