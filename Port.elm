port module Port exposing (..)

import Json.Decode

port fileChange : Json.Decode.Value -> Cmd msg
port fileRead : ((String, Int, String) -> msg) -> Sub msg
port notify : String -> Cmd msg
