port module Ports.Uuid exposing (..)

-- Ports : Outgoing

port uuid : String -> Cmd msg


-- Ports: Incoming
port observe_uuid : (String -> msg) -> Sub msg
