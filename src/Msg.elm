module Msg exposing (..)

import Time


import Plugs.Dialogo.Msg as DialogoMsg
import State.Jogo.Msg as JogoMsg

import Json.Encode as JE
import Http
import Phoenix.Socket
type alias Tela1 =
  {   scrollTop: Int
    , pageHeight: Int
    , viewportHeight: Int
    , viewportWidth: Int
  }



type Msg
    = NoOp
    | OnResizeGet Time.Time
    | EntrarNoWebsocket Time.Time
    
    | OnResize Tela1
    | DialogoMsg DialogoMsg.Msg
    | JogoMsg JogoMsg.Msg
    | IniciandoJogo
    | AbreNovoJogo
    | DefineNovoJogo String
    | AlteraNomeDoJogador String
    | AtualizaNomeJogo String
    | EstouPronto
    | AbreDialogoSelecionaImagemUsuario
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | EnviaMensagem
    | DefineNovaMensagem String
    | SairDoJogo
    | RecebeSairDoJogo JE.Value
    | RecebeRetiraTodosDoJogo JE.Value
    | ToggleAudio



    ---------------------------FUNCOES DE WEBSOCKET----------------
    --FUNCOES GERAIS
    | EnviaTurnoAtualizado
    | EnviaTurno
    | RecebePagaBonus JE.Value
    | RecebeVendaAcaoFusao JE.Value
    | RecebeUid JE.Value
    | RecebeJogoParaExibir JE.Value
    | RetiraJogoParaExibir JE.Value
    | RecebeJogo JE.Value
    | RecebeJogadores JE.Value
    | RecebeBoard JE.Value
    | RecebeDialogos JE.Value
    | RecebeJogoParaJogadores JE.Value
    | RecebeUsuario JE.Value
    | EntrouNoJogo JE.Value
    | RecebeTurno JE.Value
    | RecebeTurnoAtualizado JE.Value
    | RecebeCancelaDialogo JE.Value
    | EntreNoJogo JE.Value


    ----FUNCOES PARA QUANDO O JOGO ESTIVER ACONTECENDO
    | RecebeJogador JE.Value



    | EntrarNoJogo String String

    | JoinChannel
    | LeaveChannel
    | ShowJoinedMessage String
    | ShowLeftMessage String
    | HandlePresenceState JE.Value
    | HandlePresenceDiff JE.Value
