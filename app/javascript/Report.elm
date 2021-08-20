module Report exposing (Reporter, error, send, withField)

import App exposing (Env)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Rollbar exposing (Rollbar)
import Task exposing (Task)
import Uuid exposing (Uuid)


type alias Reporter =
    { send : String -> Dict String Encode.Value -> Task Http.Error Uuid
    , data : Dict String Encode.Value
    }


error : Env -> Reporter
error env =
    { send =
        (Rollbar.scoped
            (Rollbar.token env.rollbarAccessToken)
            (Rollbar.codeVersion "")
            (Rollbar.environment env.environment)
            env.title
        ).error
    , data = Dict.fromList [ ( "user_id", Encode.int env.user.id ) ]
    }


withField : String -> Encode.Value -> Reporter -> Reporter
withField str val reporter =
    { reporter | data = Dict.insert str val reporter.data }


send : String -> Reporter -> Task Http.Error Uuid
send message reporter =
    reporter.send message reporter.data
