module Subscriptions exposing (..)

import MainModel exposing (..)
import Msg exposing(..)

import Ports.Uuid as Uuid
import Ports.WindowResize as WindowResize

import State.Jogo.Subscription as JogoSub
import Phoenix.Socket


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    WindowResize.scrollOrResize OnResize
  , Uuid.observe_uuid DefineNovoJogo
  , Sub.map JogoMsg <| JogoSub.subscriptions model
  , case model.phxSocket of
      Nothing -> 
        Sub.none
      Just phxSocket -> 
        Phoenix.Socket.listen phxSocket PhoenixMsg
  ]
  