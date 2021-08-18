module User exposing (User, decoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias User =
    { email : String
    , provider : Maybe String
    }


decoder : Decode.Decoder User
decoder =
    Decode.succeed User
        |> required "email" Decode.string
        |> optional "provider" (Decode.map Just Decode.string) Nothing
