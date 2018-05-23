port module Ports.Store exposing (..)

import ModelWE
-- Ports : Outgoing

port setUsuario : ModelWE.Usuario -> Cmd msg


-- Ports: Incoming
port observeStore : (String -> msg) -> Sub msg
