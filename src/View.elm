module View exposing (..)

import MainModel exposing (..)
import Msg exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


import Plugs.Dialogo.View as DialogoView

import State.Inicial as Inicial
import Ports.Uuid as Uuid
---- VIEW ----


view : Model -> Html Msg
view model =
  div []
  [
    Html.map DialogoMsg <| DialogoView.view model
  , jogo model
  ]
  

jogo : Model -> Html Msg
jogo model =
    case model.me.online of
      False ->
        div []
        [ h3 [] [text ""]
        , Inicial.error model
        ]
      True ->
        div []
        [
         div [ class "buttons-game"] 
         [ 
            div [ class "button-audio", onClick ToggleAudio]
              [ if model.opts.audio then
                  img [src "/icos/295-volume-high.svg", style [("width","25px")]] []
                else
                  img [src "/icos/299-volume-mute2.svg", style [("width","25px")]] []
              ]
         ]
        ,case model.meu_jogo of
          Nothing ->
            Inicial.render model
          Just jogo ->
            case jogo.fim_do_jogo of
              True ->
                Inicial.fim_do_jogo model jogo
              False ->
                Inicial.meu_jogo model jogo
        ]
        


