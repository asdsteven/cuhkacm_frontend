port module Ports exposing (..)

import Json.Decode

port readFile : Json.Decode.Value -> Cmd msg
port notify : (String, String, String) -> Cmd msg
port fileRead : (String -> msg) -> Sub msg
port oversize : (Int -> msg) -> Sub msg
port contest : (String -> msg) -> Sub msg
