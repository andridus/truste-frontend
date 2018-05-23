port module Ports.Sound exposing (..)

-- Ports : Outgoing

port play_sound : String -> Cmd msg

port playlist_sound : String -> Cmd msg
port playlist_toggle_stop : String -> Cmd msg

-- Ports: Incoming
port receive_sound : (String -> msg) -> Sub msg
