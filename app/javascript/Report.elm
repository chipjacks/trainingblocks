module Report exposing (error, data, withField)

import Rollbar exposing (Rollbar)
import Dict exposing (Dict)
import Json.Encode as Encode


rollbar : Rollbar
rollbar =
    Rollbar.scoped
        (Rollbar.token "6d4dd0ca4af74f349cef2b6d153330f7")
        (Rollbar.codeVersion "a09ed197479018d180f8f554e378367783435a2d")
        (Rollbar.environment "development")
        "Calendar"


error =
    rollbar.error


data : Dict String Encode.Value
data =
    Dict.empty


withField : String -> Encode.Value -> Dict String Encode.Value -> Dict String Encode.Value
withField str val dict = 
    Dict.insert str val dict
