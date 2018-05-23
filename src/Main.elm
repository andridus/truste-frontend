module Main exposing (..)

import MainModel exposing (..)
import Model exposing (..)
import ModelWE
import Msg exposing (..)
import Update exposing (..)
import View exposing (..)
import Subscriptions exposing (..)

import Task exposing (Task)

import Time

import Dict exposing (Dict)
import Html exposing (..)


import Debug


entrarNoWebsocket : Cmd Msg
entrarNoWebsocket =
  Task.perform EntrarNoWebsocket Time.now

initialWindowSizeCmd : Cmd Msg
initialWindowSizeCmd =
  Task.perform OnResizeGet Time.now


init : ModelWE.Usuario -> ( Model, Cmd Msg )
init user =
    let
      d = (Debug.log "flags" user)
    in  
      ( { me = (Model.novo_jogador1 user.uid Nothing user.nome user.avatar) 
        , jogos = []
        , meu_jogo = Nothing
        , dialogos = []
        , tela = { scrollTop = 0
                  , pageHeight = 0
                  , viewportHeight = 0
                  , viewportWidth = 0
                  }
        , zoom = 1.0
        , phxSocket = Nothing
        , jogoSocket = Nothing
        , phxPresences = Dict.empty
        , novaMensagem = ""
        , mensagens = []
        , usuarios = [user]
        , opts = novo_opts
        }, Cmd.batch 
        [
          initialWindowSizeCmd
        , entrarNoWebsocket
        ] )


---- PROGRAM ----


main : Program ModelWE.Usuario Model Msg
main  =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
