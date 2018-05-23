port module Ports.WindowResize exposing (..)

import Model exposing (..)

port getTela : String -> Cmd msg
port scrollOrResize : (Tela -> msg) -> Sub msg