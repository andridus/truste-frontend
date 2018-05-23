module Update exposing (..)

import Msg exposing (..)
import Model exposing (..)
import MainModel exposing (..)
import Plugs.Dialogo.Msg as DialogoMsg
import Plugs.Dialogo.Update as DialogoUpdate
import State.Jogo.Msg as JogoMsg
import State.Jogo.Update as JogoUpdate

import Ports.Uuid as Uuid
import Ports.Store as Store
import Ports.Sound as Sound
import Ports.WindowResize as WindowResize
import Utils.Jogador as Jogador
import Utils.Fn as Fn
import Utils.Push as Push
import Lista
import List.Extra as ListX
import String

import Phoenix.Socket
import Phoenix.Channel
import Dict exposing (Dict)
-- UPDATE
import Phoenix.Presence exposing (PresenceState, syncState, syncDiff, presenceStateDecoder, presenceDiffDecoder)
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Cmd.Extra as CmdX
--IMPORT DECODERS
import Utils.DE as DE



decodeUid : JD.Decoder String
decodeUid =
  JD.at ["uid"] JD.string

userParams : String -> JE.Value
userParams uid =
    JE.object [ ( "uid", JE.string uid ) ]


userPresenceDecoder : JD.Decoder UserPresence
userPresenceDecoder =
    JD.map2 UserPresence
        (JD.field "online_at" JD.string)
        (JD.field "phx_ref" JD.string)


socket_server : String -> String
--socket_server uid = "ws://truste.econnecta.com.br/socket/websocket?vsn=1.0.0&uid="++uid
socket_server uid = "ws://truste.econnecta.com.br/socket/websocket?vsn=1.0.0&uid="++uid

initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket uid =
  Phoenix.Socket.init (socket_server uid)
    --|> Phoenix.Socket.withoutHeartbeat
    |> Phoenix.Socket.on "recebe_jogo_para_exibir" "base" RecebeJogoParaExibir
    |> Phoenix.Socket.on "retira_jogo_para_exibir" "base" RetiraJogoParaExibir
    |> Phoenix.Socket.on ("entrou_no_jogo:"++uid) "base" EntrouNoJogo
    |> Phoenix.Socket.on ("entre_no_jogo:"++uid) "base" EntreNoJogo
    |> Phoenix.Socket.on "recebe_usuario" "base" RecebeUsuario
    |> Phoenix.Socket.on "presence_state" "base" HandlePresenceState
    |> Phoenix.Socket.on "presence_diff" "base" HandlePresenceDiff

initJogoPhxSocket : Phoenix.Socket.Socket Msg -> String -> String -> Phoenix.Socket.Socket Msg
initJogoPhxSocket phx uid jid=
    phx
    --|> Phoenix.Socket.withoutHeartbeat
    |> Phoenix.Socket.on ("recebe_jogo") ("jogo:" ++ jid) RecebeJogo
    |> Phoenix.Socket.on "recebe_jogo_para_jogadores" ("jogo:" ++ jid) RecebeJogoParaJogadores
    |> Phoenix.Socket.on "recebe_cancela_dialogo" ("jogo:" ++ jid) RecebeCancelaDialogo
    |> Phoenix.Socket.on "recebe_dialogos" ("jogo:" ++ jid) RecebeDialogos
    |> Phoenix.Socket.on "recebe_jogadores" ("jogo:" ++ jid) RecebeJogadores
    |> Phoenix.Socket.on "recebe_board" ("jogo:" ++ jid) RecebeBoard
    |> Phoenix.Socket.on "recebe_turno" ("jogo:" ++ jid) RecebeTurno
    |> Phoenix.Socket.on "recebe_turno_atualizado" ("jogo:" ++ jid) RecebeTurnoAtualizado
    |> Phoenix.Socket.on "recebe_paga_bonus" ("jogo:" ++ jid) RecebePagaBonus
    |> Phoenix.Socket.on "recebe_venda_acoes_fusao" ("jogo:" ++ jid) RecebeVendaAcaoFusao
    |> Phoenix.Socket.on ("sair_do_jogo:"++uid) ("jogo:" ++ jid) RecebeSairDoJogo
    |> Phoenix.Socket.on ("retira_todos_do_jogo:"++uid) ("jogo:" ++ jid) RecebeRetiraTodosDoJogo

leaveJogoPhxSocket : Phoenix.Socket.Socket Msg -> String -> String -> Phoenix.Socket.Socket Msg
leaveJogoPhxSocket phx uid jid=
    phx
    --|> Phoenix.Socket.withoutHeartbeat
    |> Phoenix.Socket.off ("recebe_jogo") ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_jogo_para_jogadores" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_cancela_dialogo" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_dialogos" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_jogadores" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_board" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_turno" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_turno_atualizado" ("jogo:" ++ jid)
    |> Phoenix.Socket.off "recebe_paga_bonus" ("jogo:" ++ jid) 
    |> Phoenix.Socket.off "recebe_venda_acoes_fusao" ("jogo:" ++ jid) 
    |> Phoenix.Socket.off ("sair_do_jogo:"++uid) ("jogo:" ++ jid)
    |> Phoenix.Socket.off ("retira_todos_do_jogo:"++uid) ("jogo:" ++ jid)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      ToggleAudio ->
        let
          opts = model.opts
          opts1 = {opts | audio = (not opts.audio)}
        in
        ({model | opts = opts1}, Sound.playlist_toggle_stop "")
      SairDoJogo ->
        let
          dialogos = model.dialogos
          dialogo = 
            novo_dialogo 
              "ENCERRAR_JOGO" 
              "Alerta" 
              "Você tem certeza de que deseja sair desse jogo?" ""
          dialogos2 = dialogo :: dialogos
        in
          ({ model | dialogos = dialogos2},Sound.play_sound("ERROR") )
      EntrarNoJogo jid criador_id->
        case model.phxSocket of
          Nothing -> model ! []
          Just phxSocket ->
            let
              dialogo = novo_dialogo "PROCESSO" "MENSAGEM" "Entrando no jogo selecionado. Aguarde..." ""
              dialogo1 = {dialogo | did = "entrando_no_jogo_selecionado"}
              dialogos = dialogo1 :: model.dialogos
              (phxSocket1, phxCmd) = Push.entra_no_jogo phxSocket model.me.uid jid criador_id model.me.nome model.me.avatar
              channel =
                Phoenix.Channel.init ("jogo:" ++ jid)
                  |> Phoenix.Channel.withPayload (userParams model.me.uid)
                  |> Phoenix.Channel.onJoin (always ( ShowJoinedMessage ("jogo:" ++ jid)))
                  |> Phoenix.Channel.onClose (always ( ShowLeftMessage ("jogo:" ++ jid)))
              jogoPhxSocket = initJogoPhxSocket phxSocket1 model.me.uid jid
              (phxSocket2, jogoCmd ) = Phoenix.Socket.join channel jogoPhxSocket
            in
              ({ model | dialogos = dialogos, phxSocket = Just phxSocket2}, Cmd.batch [
                                                                                        Cmd.map PhoenixMsg phxCmd
                                                                                      , Cmd.map PhoenixMsg jogoCmd 
                                                                                      ])
      
      EntreNoJogo raw ->
        case  JD.decodeValue DE.jid_criador raw of
          Ok jid_c ->
            case model.phxSocket of
              Nothing -> model ! []
              Just phxSocket ->
                let
                  dialogo = novo_dialogo "PROCESSO" "MENSAGEM" "Entrando no jogo. Aguarde..." ""
                  dialogo1 = {dialogo | did = "entrando_no_jogo_selecionado"}
                  dialogos = dialogo1 :: model.dialogos

                  (phxSocket1, phxCmd) = Push.entra_no_jogo phxSocket model.me.uid jid_c.jid jid_c.criador model.me.nome model.me.avatar
                  channel =
                    Phoenix.Channel.init ("jogo:" ++ jid_c.jid)
                      |> Phoenix.Channel.withPayload (userParams model.me.uid)
                      |> Phoenix.Channel.onJoin (always ( ShowJoinedMessage ("jogo:" ++ jid_c.jid)))
                      |> Phoenix.Channel.onClose (always ( ShowLeftMessage ("jogo:" ++ jid_c.jid)))
                  jogoPhxSocket = initJogoPhxSocket phxSocket1 model.me.uid jid_c.jid
                  (phxSocket2, jogoCmd ) = Phoenix.Socket.join channel jogoPhxSocket
                in
                  ({ model | dialogos = dialogos, phxSocket = Just phxSocket2}, Cmd.batch [
                                                                                            Cmd.map PhoenixMsg phxCmd
                                                                                          , Cmd.map PhoenixMsg jogoCmd
                                                                                          ])
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []

      EntrouNoJogo raw ->
        case  JD.decodeValue DE.jogador_jid raw of
          Ok jogador_jid ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo -> 
                let
                   _ = Debug.log "teste" jogador_jid
                    
                in
                  if jogo.criador_id == model.me.uid  then
                    case model.phxSocket of
                      Nothing -> model ! []
                      Just phxSocket ->
                        case ListX.find(\x -> x.uid == jogador_jid.uid) jogo.jogadores of
                          Nothing ->
                            if jogo.inicializando then
                              let
                                dialogo = novo_dialogo "ERROR" "Alerta" "Você não pode se conectar a esse jogo ele já começou" jogador_jid.uid
                                (phxSocket1, phxCmd) = Push.envia_dialogos phxSocket [dialogo] jogo.jid
                              in
                                ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                            else
                              let
                                novo_jogador2 = novo_jogador1 jogador_jid.uid (Just jogo.jid) jogador_jid.nome jogador_jid.avatar
                                jogadores = jogo.jogadores
                                jogadores2 = novo_jogador2 :: jogadores
                                jogadores1 = ListX.uniqueBy .uid jogadores2
                                jogo1 = {jogo | jogadores = jogadores1}
                                (phxSocket1, phxCmd) = Push.envia_jogo phxSocket jogador_jid.uid model.me.uid jogo1
                                (phxSocket2, phxCmd2) = Push.envia_jogadores phxSocket1 jogador_jid.uid jogo.jid jogo1.jogadores
                                cmds = Cmd.batch [Cmd.map PhoenixMsg phxCmd, Cmd.map PhoenixMsg phxCmd2]
                              in
                                ({ model | meu_jogo = Just jogo1, phxSocket = Just phxSocket1}, cmds)
                          Just j ->
                            let
                              (phxSocket1, phxCmd) = Push.envia_jogo phxSocket jogador_jid.uid model.me.uid jogo
                              (phxSocket2, phxCmd2) = Push.envia_jogadores phxSocket1 jogador_jid.uid jogo.jid jogo.jogadores
                              cmds = Cmd.batch [Cmd.map PhoenixMsg phxCmd, Cmd.map PhoenixMsg phxCmd2]
                            in
                              ({ model | meu_jogo = Just jogo, phxSocket = Just phxSocket1}, cmds)
                  else
                    model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []


      DialogoMsg subMsg->
        case subMsg of
          DialogoMsg.PerformSairDoJogo ->
            case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  let
                    (model1, cmd) = 
                      case model.meu_jogo of
                        Nothing -> model ! []
                        Just jogo ->
                          case model.me.uid == jogo.criador_id of
                            True -> 
                              let 
                                (phxSocket1, phxCmd) = Push.retira_todos_do_jogo phxSocket jogo.criador_id jogo.jid 
                                (phxSocket2, phxCmd1) = Push.retira_jogo_para_exibir phxSocket1 jogo.jid 
                              in
                                ({ model | phxSocket = Just phxSocket2}, Cmd.batch [Cmd.map PhoenixMsg phxCmd, Cmd.map PhoenixMsg phxCmd1])
                            False ->
                              let 
                                (phxSocket1, phxCmd) = Push.sair_do_jogo phxSocket model.me.uid jogo.criador_id jogo.jid
                                (phxSocket2, phxCmd2) = Push.sair_do_jogo phxSocket1 model.me.uid model.me.uid jogo.jid

                                cmd = Cmd.batch [ (Cmd.map PhoenixMsg phxCmd), (Cmd.map PhoenixMsg phxCmd2)]
                              in
                                ({model | phxSocket = Just phxSocket2}, cmd)
                  in
                    (model1, Cmd.batch[ cmd, Sound.play_sound("BUTTON-CANCEL")])
          DialogoMsg.EnviaMe ->
            let
              me1 = model.me
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  let
                    (phxSocket1, phxCmd) = Push.envia_me phxSocket me1
                  in
                    ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
              usuario = {uid = me1.uid, nome = me1.nome, avatar = me1.avatar, pronto = me1.pronto}
            in
              (model1, Cmd.batch [  cmd
                                  , (Store.setUsuario usuario)
                                  ])
          DialogoMsg.EnviaCancelaWebSocketDiag dialogo ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        me1 = model.me
                        (phxSocket1, phxCmd) = Push.envia_cancela_dialogo phxSocket dialogo.did jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                          
            in
              (model1, cmd)
          DialogoMsg.EnviaTurno ->
           -- (model, CmdX.perform EnviaTurno)
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      case model.me.uid == jogo.criador_id of
                        False ->
                          let
                            me1 = model.me
                            jogadores = List.map(\x -> {x | vez = False } ) jogo.jogadores
                            jogo1 = { jogo  | jogadores = jogadores}


                           -- jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                            --jogadores1 = List.map(\x -> x.uid) jogadores0
                            --(phxSocket1, phxCmd) = Push.envia_jogo_para_jogadores phxSocket jogadores1 jogo1

                            
                            (phxSocket1, phxCmd) = Push.envia_turno phxSocket jogo.jid jogo.turno

                          in
                            ({ model | meu_jogo = Just jogo1, phxSocket = Just phxSocket1}, Cmd.batch [ Cmd.map PhoenixMsg phxCmd])
                        True ->
                          let
                            jogadores = List.map(\x -> x.uid) jogo.jogadores

                            (phxSocket1, phxCmd) = Push.envia_jogo_para_jogadores phxSocket jogadores model.me.uid jogo
                          in
                            ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                          
            in
              (model1, cmd)
          DialogoMsg.EnviaTurnoAtualizado ->
            (model, CmdX.perform EnviaTurnoAtualizado)

          DialogoMsg.EnviaPagaBonus ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                        jogadores1 = List.map(\x -> x.uid) jogadores0
                        me1 = Jogador.me model.me jogo.jogadores

                        (phxSocket1, phxCmd) = Push.paga_bonus phxSocket jogadores1 me1 jogo.turno jogo.board jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.batch [Cmd.map PhoenixMsg phxCmd])
            in
              (model1, cmd)

          DialogoMsg.EnviaVendaAcaoFusao ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                        jogadores1 = List.map(\x -> x.uid) jogadores0
                        me1 = Jogador.me model.me jogo.jogadores

                        (phxSocket1, phxCmd) = Push.paga_bonus phxSocket jogadores1 me1 jogo.turno jogo.board jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.batch [Cmd.map PhoenixMsg phxCmd])
            in
              (model1, cmd)

          DialogoMsg.EnviaDialogoFusao ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        (phxSocket1, phxCmd) = Push.envia_dialogos phxSocket model.dialogos jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                          
            in
              (model, cmd)
          DialogoMsg.EnviaDialogoInformacao ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        (phxSocket1, phxCmd) = Push.envia_dialogos phxSocket model.dialogos jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                          
            in
              (model, cmd)
          _ ->
            let
              (model1, subCmd) = DialogoUpdate.update subMsg model
            in
              (model1, Cmd.map DialogoMsg subCmd)

      JogoMsg subMsg->
        case subMsg of

          JogoMsg.EnviaBoard ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just j ->
                let
                  (model1, cmd) = case model.phxSocket of
                    Nothing -> model ! []
                    Just phxSocket ->
                      let
                        board = j.board
                        (phxSocket1, phxCmd) = Push.envia_board phxSocket j.jid board
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd {-CmdX.perform EnviaTurnoAtualizado -})
                in
                  (model1, cmd)
          JogoMsg.EnviaTurnoAtualizado ->
            (model, CmdX.perform EnviaTurnoAtualizado)
          JogoMsg.EnviaDialogoInformacao ->
            let
              (model1, cmd) = case model.phxSocket of
                Nothing -> model ! []
                Just phxSocket ->
                  case model.meu_jogo of
                    Nothing -> model! []
                    Just jogo ->
                      let
                        (phxSocket1, phxCmd) = Push.envia_dialogos phxSocket model.dialogos jogo.jid
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
            in
              (model, cmd)
          _ ->
            let
              (model1, subCmd) = JogoUpdate.update subMsg model
            in
              (model1, Cmd.map JogoMsg subCmd)
      EnviaTurnoAtualizado ->
      {-
          DADOS A SEREM ENVIADOS
            - JOGADORES (EU JOGADOR)
            - TURNO
            - BOARD
        -}
      let
          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              case model.meu_jogo of
                Nothing -> model! []
                Just jogo ->
                  let
                    jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                    jogadores1 = List.map(\x -> x.uid) jogadores0
                    me1 = Jogador.me model.me jogo.jogadores

                    (phxSocket1, phxCmd) = Push.atualiza_turno phxSocket jogadores1 me1 jogo.turno jogo.board jogo.jid
                  in
                    ({ model | phxSocket = Just phxSocket1}, Cmd.batch [Cmd.map PhoenixMsg phxCmd])
        {-let
          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              case model.meu_jogo of
                Nothing -> model! []
                Just jogo ->
                  case jogo.turno.jogador == model.me.uid of
                    True ->
                      let
                        jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                        jogadores1 = List.map(\x -> x.uid) jogadores0
                        me1 = model.me

                        (phxSocket1, phxCmd) = Push.envia_jogo_para_jogadores phxSocket jogadores1 model.me.uid jogo
                        (phxSocket2, phxCmd1) = Push.atualiza_turno phxSocket1 jogo.jid jogo.turno
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.batch [Cmd.map PhoenixMsg phxCmd, Cmd.map PhoenixMsg phxCmd1 ])
                    False ->
                      let
                        (phxSocket1, phxCmd) = Push.envia_jogo phxSocket jogo.turno.jogador model.me.uid jogo
                        (phxSocket2, phxCmd1) = Push.atualiza_turno phxSocket1 jogo.jid jogo.turno
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)-}
                      
        in
          (model1, cmd)
      EnviaTurno ->
        let
          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              case model.meu_jogo of
                Nothing -> model! []
                Just jogo ->
                  case jogo.turno.jogador == model.me.uid of
                    True ->
                      let
                        jogadores0 = List.filter(\x -> x.uid /= model.me.uid) jogo.jogadores
                        jogadores1 = List.map(\x -> x.uid) jogadores0
                        me1 = model.me
                        jogadores2 = List.map(\x -> {x | vez = False } ) jogo.jogadores
                        jogo1 = { jogo  | jogadores = jogadores2}

                        (phxSocket1, phxCmd) = Push.envia_jogo_para_jogadores phxSocket jogadores1 model.me.uid jogo1
                        (phxSocket2, phxCmd1) = Push.envia_turno phxSocket1 jogo.jid jogo.turno
                      in
                        ({ model | phxSocket = Just phxSocket1, meu_jogo = Just jogo1}, Cmd.batch [Cmd.map PhoenixMsg phxCmd, Cmd.map PhoenixMsg phxCmd1 ])
                    False ->
                      let
                        (phxSocket1, phxCmd) = Push.envia_jogo phxSocket jogo.turno.jogador model.me.uid jogo
                      in
                        ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                      
        in
          (model1, cmd)
      EntrarNoWebsocket tempo ->
        let
          modelPhxSocket = initPhxSocket model.me.uid
          channel =
            Phoenix.Channel.init "base"
              |> Phoenix.Channel.withPayload (userParams model.me.uid)
              |> Phoenix.Channel.onJoin (always ( ShowJoinedMessage "base"))
              |> Phoenix.Channel.onClose (always ( ShowLeftMessage "base"))
          (phxSocket, phxCmd ) =
              Phoenix.Socket.join channel modelPhxSocket
          usuario = {uid = model.me.uid, nome = model.me.nome, avatar = model.me.avatar, pronto = model.me.pronto}
        in
          ({model | phxSocket = Just phxSocket}, Cmd.batch [
                                                             Cmd.map PhoenixMsg phxCmd
                                                            , (Store.setUsuario usuario)
                                                            , Sound.playlist_sound "OUT"
                                                            ]
                                                            )

      PhoenixMsg msg1 ->
        case model.phxSocket of
          Nothing -> model ! []
          Just modelPhxSocket ->
            let
              (phxSocket, phxCmd) = Phoenix.Socket.update msg1 modelPhxSocket
            in
              ({model | phxSocket = Just phxSocket}, Cmd.map PhoenixMsg phxCmd)


      EnviaMensagem ->
        (model, Cmd.none)

      DefineNovaMensagem mensagem ->
        (model, Cmd.none)

      RecebeSairDoJogo raw ->
        case  JD.decodeValue DE.uid raw of
          Ok u ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                if model.me.uid == jogo.criador_id && model.me.uid /= u.uid then
                  let
                    _ = Debug.log "asas" u
                    jogadores1 = List.filter(\x -> x.uid /= u.uid) jogo.jogadores
                    jogo1 = { jogo | jogadores = jogadores1}
                    model1 = { model | meu_jogo = Just jogo1}
                  in
                    (model1, Cmd.none)
                else
                 case model.phxSocket of
                    Nothing -> model ! []
                    Just phxSocket ->
                      let
                        phxSocket1 = leaveJogoPhxSocket phxSocket model.me.uid jogo.jid
                        (phxSocket2, cmd1) = Phoenix.Socket.leave ("jogo:" ++ jogo.jid) phxSocket1
                        cmd = (Cmd.map PhoenixMsg cmd1)
                      in
                        ({model | phxSocket = Just phxSocket2, meu_jogo = Nothing}, cmd)
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []

      RecebeRetiraTodosDoJogo raw ->
        case  JD.decodeValue DE.jid raw of
          Ok j ->
            let
              _ = Debug.log "h" j
                
            in
              case model.meu_jogo of
                Nothing -> model ! []
                Just jogo ->
                  if model.me.uid == jogo.criador_id && jogo.jid == j.jid then
                    case model.phxSocket of
                      Nothing -> model ! []
                      Just phxSocket ->
                        let
                          cmds = List.map(\x -> 
                            let 
                              (phxSocket1, phxCmd) = Push.sair_do_jogo phxSocket x.uid x.uid jogo.jid
                            in
                              (Cmd.map PhoenixMsg phxCmd)
                            ) jogo.jogadores
                          (phxSocket1, _) = Push.sair_do_jogo phxSocket model.me.uid model.me.uid jogo.jid

                          cmd = Cmd.batch cmds
                        in
                          ({model | phxSocket = Just phxSocket1 }, cmd)
                  else
                      model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []

      RecebeJogo raw ->
        case  JD.decodeValue DE.jogo raw of
          Ok jogo_ ->
            if model.me.uid == jogo_.jogador then
              let
                dialogos = List.filter (\x -> 
                  x.did /= "entrando_no_jogo_selecionado"
                ) model.dialogos
                me = model.me
                me1 = 
                  if jogo_.jogo.turno.jogador == model.me.uid then
                    {me | vez = True, acoes = me.acoes}
                  else
                    me
                sound_money = 
                    if model.me.din < me1.din then
                      Sound.play_sound("MONEY")
                    else
                      Cmd.none
                jogadores1 = List.map(\x -> if x.uid == jogo_.jogo.turno.jogador then {x | vez = True} else x) jogo_.jogo.jogadores
                jogo = jogo_.jogo
                turno0 = jogo.turno
                avc1 = 
                  case model.meu_jogo of
                    Nothing -> []
                    Just j -> j.turno.acoes_vendidas_fusao
                avc = turno0.acoes_vendidas_fusao --avc1 ++ 
                turno1 = { turno0 | acoes_vendidas_fusao =  avc }
                board1 = List.map (\x -> {x| sobre = False, na_peca = False}) jogo.board
                jogo1 = { jogo | board = board1, jogadores = jogadores1, turno = turno1}
                model1 = { model | dialogos = dialogos, me = me1, meu_jogo = Just jogo1}
              in
                (model1, sound_money)
            else
              model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []

      RecebeJogadores raw ->
        case  JD.decodeValue DE.jogador_jogadores raw of
          Ok jogo_ ->
            if model.me.uid /= jogo_.jogador then
              let
                jogo = case model.meu_jogo of
                  Nothing -> Nothing 
                  Just j ->
                    let
                      jogadores1 = jogo_.jogadores
                      jogo1 = { j | jogadores = jogadores1}
                    in
                      Just jogo1
              in
                ({ model | meu_jogo = jogo}, Cmd.none)
            else
              model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []
      RecebePagaBonus raw ->
        case  JD.decodeValue DE.recebe_turno_atualizado raw of
          Ok jogo_ ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                let
                  uids = String.split "|" jogo_.jogadores
                in
                  if (List.member model.me.uid uids) then
                    let


                      jogadores0 = List.map(\x -> 
                          if x.uid == jogo_.from.uid then
                            jogo_.from
                          else
                            x
                        ) jogo.jogadores

                      jogadores = Jogador.atualizar_bonus jogo_.turno.bonus jogadores0
                      me = Jogador.me model.me jogadores

                      jogo1 = { jogo |  jogadores = jogadores}
                      model1 = { model | me = me, meu_jogo = Just jogo1}
                    in
                      (model1, Cmd.none)
                else
                  model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []
      RecebeVendaAcaoFusao raw ->
        case  JD.decodeValue DE.recebe_turno_atualizado raw of
          Ok jogo_ ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                let
                  uids = String.split "|" jogo_.jogadores
                in
                  if (List.member model.me.uid uids) then
                    let


                      jogadores = List.map(\x -> 
                          if x.uid == jogo_.from.uid then
                            jogo_.from
                          else
                            x
                        ) jogo.jogadores

                      turno1 = 
                        if model.me.uid == jogo_.turno.jogador then
                          let
                            turno0 = jogo.turno
                            turno1 = jogo_.turno
                            avc = (turno0.acoes_vendidas_fusao ++ turno1.acoes_vendidas_fusao)
                          in
                            { turno0 | acoes_vendidas_fusao =  avc }
                        else
                          let
                            turno0 = jogo.turno
                            turno1 = jogo_.turno
                            avc = (turno0.acoes_vendidas_fusao ++ turno1.acoes_vendidas_fusao)
                          in
                            { turno1 | acoes_vendidas_fusao =  avc }

                      me = Jogador.me model.me jogadores

                      jogo1 = { jogo |  jogadores = jogadores, turno = turno1}
                      model1 = { model | me = me, meu_jogo = Just jogo1}
                    in
                      (model1, Cmd.none)
                else
                  model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []
      RecebeTurnoAtualizado raw ->
         {-
            jogadores
            from
            turno
            board
            empresas
          -}
        case  JD.decodeValue DE.recebe_turno_atualizado raw of
          Ok jogo_ ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                let
                  uids = String.split "|" jogo_.jogadores
                in
                  if (List.member model.me.uid uids) then
                    let


                      jogadores = List.map(\x -> 
                          if x.uid == jogo_.from.uid then
                            jogo_.from
                          else
                            x
                        ) jogo.jogadores


                      me = Jogador.me model.me jogadores

                      empresas1 = case jogo_.turno.atualizar_pecas of
                        Nothing -> jogo.empresas
                        Just ap ->
                          let
                            empresas1 = case ap.empresa of
                              Nothing -> jogo.empresas
                              Just emp ->
                                List.map (\x -> 
                                  if x.eid == emp.eid then
                                    {x | disponivel = False}
                                  else
                                    x
                                  ) jogo.empresas
                            eids = List.map (\x -> x.eid ) ap.falencias
                            empresas2 = List.map (\x ->
                              if (List.member x.eid eids) then
                                {x | disponivel = True}
                              else
                                x
                              ) empresas1
                          in
                            empresas2

                      _ = Debug.log "asdsadasd asd" jogo_.turno.atualizar_pecas
                      _ = Debug.log "EMPRESAS " empresas1
                              

                      

                      turno1 = jogo_.turno

                      me1 = {me | online = True, acoes = me.acoes}
                     
                      board1 = List.map (\x -> {x| sobre = False, na_peca = False}) jogo_.board
                      
                      jogo1 = { jogo | board = board1, turno = turno1, empresas = empresas1, jogadores = jogadores}



                      (dialogos, sound_vez) =
                        if me1.vez then
                          let
                            dialogo = novo_dialogo "INFO" "ALERTA" "TURNO ATUALIZADO" ""
                          in
                            (dialogo :: model.dialogos, Sound.play_sound("VEZ"))
                        else
                          (model.dialogos, Cmd.none)

                      sound_money = 
                        if model.me.din < me1.din then
                          Sound.play_sound("MONEY")
                        else
                          Cmd.none
                      model1 = { model | dialogos = dialogos, me = me1, meu_jogo = Just jogo1}
                    in
                      (model1, Cmd.batch[Cmd.none, sound_vez, sound_money])
                else
                  model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []
      RecebeTurno raw ->
        case  JD.decodeValue DE.recebe_turno raw of
          Ok jogo_ ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                if jogo.jid == jogo_.jogo && jogo.criador_id == model.me.uid then
                  let
                    turno0 = jogo.turno
                    turno1 = jogo_.turno
                    avc = (turno0.acoes_vendidas_fusao ++ turno1.acoes_vendidas_fusao)
                    turno2 = { turno1 | acoes_vendidas_fusao =  avc }
                    empresas1 = case jogo_.turno.atualizar_pecas of
                        Nothing -> jogo.empresas
                        Just ap ->
                          let
                            empresas1 = case ap.empresa of
                              Nothing -> jogo.empresas
                              Just emp ->
                                List.map (\x -> 
                                  if x.eid == emp.eid then
                                    {x | disponivel = False}
                                  else
                                    x
                                  ) jogo.empresas
                            eids = List.map (\x -> x.eid ) ap.falencias
                            empresas2 = List.map (\x ->
                              if (List.member x.eid eids) then
                                {x | disponivel = True}
                              else
                                x
                              ) empresas1
                          in
                            empresas2

                    jogo1 = {jogo | turno = turno2, empresas = empresas1} 
                    
                    model1 = { model |  meu_jogo = Just jogo1}
                    
                    model2 = Fn.finaliza_jogada model1 jogo_.turno.jogador jogo1

                    jogadores = List.map(\x -> x.uid) jogo1.jogadores

                  in
                    case model2.meu_jogo of
                      Nothing -> model2! []
                      Just jogo2 ->
                        let 
                          (model4, cmd) = 
                            case model.phxSocket of
                              Nothing -> model2! []
                              Just phxSocket ->
                                let
                                  jogadores = List.map(\x -> x.uid) jogo2.jogadores
                                  me1 = Jogador.me model.me jogo2.jogadores
                                  (dialogos, sound_vez) =
                                        if me1.vez && me1.uid /= jogo2.criador_id then
                                          let
                                            dialogo = novo_dialogo "INFO" "ALERTA" "É a sua vez de jogar!" ""
                                          in
                                            (dialogo :: model2.dialogos, Sound.play_sound("VEZ"))
                                        else
                                          (model2.dialogos, Cmd.none)
                                  (phxSocket1, phxCmd) = Push.envia_jogo_para_jogadores phxSocket jogadores model.me.uid jogo2
                                in
                                  ({ model2 | dialogos = dialogos, me = me1, phxSocket = Just phxSocket1}, Cmd.batch[sound_vez, Cmd.map PhoenixMsg phxCmd])
                        in
                          (model4, cmd)
                else
                  model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []


      RecebeDialogos raw ->
        case  JD.decodeValue DE.recebe_dialogos raw of
          Ok d ->
            case model.meu_jogo of
              Nothing -> model ![]
              Just jogo ->
                if jogo.turno.jogador /= model.me.uid then
                  let
                    dialogos1 = List.filter(\x -> x.jogador == model.me.uid || x.jogador == "") d.dialogos
                    dialogos_model  = model.dialogos
                    dialogos2 =  dialogos1 ++ dialogos_model
                  in
                  {model| dialogos = dialogos2} ! []
                else
                  model ![]
          Err error ->
              model ! []

      RecebeCancelaDialogo raw ->
        case  JD.decodeValue DE.recebe_cancela_dialogo raw of
          Ok d ->
            case model.meu_jogo of
              Nothing -> model ![]
              Just jogo ->
                if jogo.turno.jogador == model.me.uid then
                  let
                    dialogos1 = List.filter(\x -> x.did /= d.dialogo) model.dialogos
                   
                  in
                  {model| dialogos = dialogos1} ! []
                else
                  model ![]
          Err error ->
              let
                dialogo = novo_dialogo "ERROR" "ALERTA" error ""
                dialogos = dialogo :: model.dialogos
              in
                {model| dialogos = dialogos} ! []

      RecebeBoard raw ->
        case  JD.decodeValue DE.recebe_board raw of
          Ok jogo_ ->
            case model.meu_jogo of
              Nothing -> model ! []
              Just jogo ->
                if jogo.jid == jogo_.jogo then
                  let
                    board1 = List.map (\x -> {x| sobre = False, na_peca = False}) jogo_.board
                    meu_jogo = { jogo | board = board1}
                    model1 = { model | meu_jogo = Just meu_jogo}
                  in
                    (model1, Cmd.none)
                else
                  model ! []
          Err error ->
              model ! []

      RecebeJogoParaJogadores raw ->
        case  JD.decodeValue DE.jogo_jogadores raw of
          Ok jogo_ ->
            let
              uids = String.split "|" jogo_.jogadores
            in
              case model.meu_jogo of
                Nothing -> model ! []
                Just jogo ->
                    if (List.member model.me.uid uids) then
                      let
                        me0 = if model.me.vez then
                            Jogador.me model.me jogo.jogadores
                          else
                            Jogador.me model.me jogo_.jogo.jogadores
                        me2 = {me0 | online = True, acoes = me0.acoes}
                        (dialogos, sound_vez) =
                          if me2.vez then
                            let
                              dialogo = novo_dialogo "INFO" "ALERTA" "É a sua vez de jogar!" ""
                            in
                              (dialogo :: model.dialogos, Sound.play_sound("VEZ"))
                          else
                            (model.dialogos, Cmd.none)
                        board1 = List.map (\x -> {x| sobre = False, na_peca = False}) jogo_.jogo.board
                        
                        jogo0 = jogo_.jogo
                        turno0 = jogo.turno
                        turno1 = jogo0.turno
                        avc1 =  turno0.acoes_vendidas_fusao
                        avc = turno0.acoes_vendidas_fusao --avc1 ++ turno1.acoes_vendidas_fusao
                        turno2 = { turno1 | acoes_vendidas_fusao =  avc }

                        sound_money = 
                          if model.me.din < me2.din then
                            Sound.play_sound("MONEY")
                          else
                            Cmd.none
                        jogadores1 = case (List.filter(\x -> x.uid == x.uid) jogo0.jogadores)|> List.head of
                          Nothing ->
                             List.map(\x -> if x.uid == me2.uid then me2 else x ) jogo.jogadores
                          Just j ->
                             List.map(\x -> if x.uid == me2.uid then me2 else x ) jogo.jogadores
                             |>  List.map(\x -> if x.uid == j.uid then j else x )

                        jogo2 = { jogo0 | board = board1, jogadores = jogadores1, turno = turno2}
                        model1 = { model | dialogos = dialogos, me = me2, meu_jogo = Just jogo2}
                      in
                        (model1, Cmd.batch[Cmd.none, sound_vez, sound_money])
                  else
                    model ! []
          Err error ->
              let
                dialogo = novo_dialogo "ERROR" "ALERTA" error ""
                dialogos = dialogo :: model.dialogos
              in
                {model| dialogos = dialogos} ! []

      RecebeJogoParaExibir raw ->
        case  JD.decodeValue DE.jogo_para_exibir raw of
          Ok jogo_para_exibir ->
            if model.me.uid /= jogo_para_exibir.criador_id then
              let
                jogos = model.jogos
                jogos1 = 
                  case (List.filter(\x -> x.jid == jogo_para_exibir.jid) jogos) |> List.head of
                    Nothing -> 
                      jogo_para_exibir :: jogos
                    Just j ->
                      List.map (\x -> 
                        if x.jid == j.jid then
                          jogo_para_exibir
                        else
                          x
                      ) jogos
                meu_jogo1 = 
                  case model.meu_jogo of
                    Nothing -> Nothing
                    Just c -> Just { c | nome = jogo_para_exibir.nome}
              in
              ({ model | meu_jogo = meu_jogo1, jogos = jogos1}, Cmd.none)
            else
              model ! []
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []

      RetiraJogoParaExibir raw ->
        case  JD.decodeValue DE.jid raw of
          Ok j ->
            let
              jogos = model.jogos
              jogos1 = (List.filter(\x -> x.jid /= j.jid) jogos)
            in
            ({ model | jogos = jogos1}, Cmd.none)
          Err error ->
            let
              dialogo = novo_dialogo "ERROR" "ALERTA" error ""
              dialogos = dialogo :: model.dialogos
            in
              {model| dialogos = dialogos} ! []



      RecebeUsuario raw ->
        case  JD.decodeValue DE.jogador raw of
          Ok jogador ->
            if model.me.uid /= jogador.uid then
              let
                usuarios = List.map(\x -> 
                  if x.uid == jogador.uid then
                    {x | nome = jogador.nome, avatar = jogador.avatar, pronto = jogador.pronto}
                  else
                    x
                ) model.usuarios
                meu_jogo =
                 case model.meu_jogo of
                  Nothing -> Nothing
                  Just c ->
                    case (List.filter (\x -> x.uid == jogador.uid ) c.jogadores) |> List.head of
                      Nothing -> Just c
                      Just j -> 
                        let
                          jogadores1 = List.map(\x ->
                              if x.uid == jogador.uid then
                                {x | nome = jogador.nome, avatar = jogador.avatar, pronto = jogador.pronto}
                              else
                                x

                            ) c.jogadores
                        in
                          Just {c | jogadores = jogadores1}


              in
              ({ model | meu_jogo = meu_jogo, usuarios = usuarios}, Cmd.none)
            else
              model ! []
          Err error ->
            model ! []


      JoinChannel ->
        (model, Cmd.none)

      LeaveChannel ->
        case model.phxSocket of
          Nothing -> model ! []
          Just modelPhxSocket ->
            let
              (phxSocket, phxCmd ) = Phoenix.Socket.leave "jogo:1" modelPhxSocket
            in
              ({model | phxSocket = Just phxSocket}, Cmd.map PhoenixMsg phxCmd)

      ShowJoinedMessage mensagem ->
        
          (model, Cmd.none)
      ShowLeftMessage mensagem ->
        (model, Cmd.none)
      HandlePresenceState raw ->
            case JD.decodeValue (presenceStateDecoder userPresenceDecoder) raw of
                Ok presenceState ->
                    let
                        newPresenceState =
                            model.phxPresences |> syncState presenceState

                        usuarios =
                          Dict.toList presenceState
                          |> List.map (\(uid,x) -> {uid = uid, nome = "", avatar = "/imgs/players/no-image.png", pronto = False})
                        me = model.me
                        me1 = case Dict.get model.me.uid presenceState of
                                  Nothing -> { me | online = False }
                                  Just u -> {me | online = True}
                    in
                        { model | me = me1, usuarios = usuarios, phxPresences = newPresenceState } ! []

                Err error ->
                  let
                    dialogo = novo_dialogo "ERROR" "ALERTA" error ""
                    dialogos = dialogo :: model.dialogos
                  in
                    {model| dialogos = dialogos} ! []

      HandlePresenceDiff raw ->
          case JD.decodeValue (presenceDiffDecoder userPresenceDecoder) raw of
              Ok presenceDiff ->
                  let
                    newPresenceState =
                        model.phxPresences |> syncDiff presenceDiff

                    entrou = case (Dict.toList presenceDiff.joins) |> List.map (\(x,y) -> x ) |> List.head of
                            Nothing -> ""
                            Just c -> c

                    deixou = case (Dict.toList presenceDiff.leaves) |> List.map (\(x,y) -> x ) |> List.head of
                            Nothing -> ""
                            Just c -> c

                    usuarios =
                        Dict.toList newPresenceState
                        |> List.map (\(uid,x) -> 
                          {uid = uid, nome = "", avatar = "/imgs/players/no-image.png", pronto = False}
                        )
                    me = model.me
                    me1 = case Dict.get me.uid newPresenceState of
                                Nothing -> { me | online = False }
                                Just u -> {me | online = True}


                    jogos = List.filter(\x -> x.criador_id /= deixou ) model.jogos


                    model1 = { model | jogos = jogos}
                    retorno =case model.phxSocket of
                      Nothing -> { model | me = me1, usuarios = usuarios, phxPresences = newPresenceState } ! []
                      Just phxSocket ->
                        let
                          (model2, cmd) = 
                            case model.meu_jogo of
                              Nothing -> model1![]
                              Just jogo ->
                                if jogo.criador_id == model.me.uid then
                                  let
                                    jogo1 = { jid = jogo.jid
                                            , nome = jogo.nome
                                            , jogadores = List.length jogo.jogadores
                                            , timestamp = jogo.timestamp
                                            , criador_id = jogo.criador_id
                                            , max_users = jogo.max_users
                                            , iniciado = jogo.inicializando
                                            }
                                    (phxSocket1, phxCmd) = Push.envia_jogo_para_exibir phxSocket jogo1
                                  in
                                    ({ model1 | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                                else
                                  model1![]
                        in
                          ({ model1 | me = me1, usuarios = usuarios,  phxPresences = newPresenceState}, Cmd.batch [cmd, Sound.play_sound("JOIN")])

                    retorno1: Jogador -> (Model, Cmd Msg)
                    retorno1 j=case model.phxSocket of
                      Nothing -> { model1 | me = me1, usuarios = usuarios, phxPresences = newPresenceState } ! []
                      Just phxSocket ->
                        let
                          (model2, cmd) = 
                            case model.meu_jogo of
                              Nothing -> model1![]
                              Just jogo ->
                                if jogo.criador_id == model.me.uid then
                                  let
                                    (phxSocket1, phxCmd) = Push.entre_no_jogo phxSocket jogo.jid jogo.criador_id j.uid
                                  in
                                    ({ model1 | meu_jogo = Just jogo, phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
                                else
                                  model1![]
                        in
                          ({ model2 | me = me1, usuarios = usuarios,  phxPresences = newPresenceState}, Cmd.batch [cmd, Sound.play_sound("JOIN")])

                    retorno2 = case model.meu_jogo of
                      Nothing -> retorno
                      Just jogo ->
                        let
                          jogador = (List.filter(\x -> x.uid == entrou) jogo.jogadores) |> List.head
                        in
                          case jogador of
                            Nothing -> retorno
                            Just j -> retorno1 j
                  in
                    retorno2

              Err error ->
                let
                  dialogo = novo_dialogo "ERROR" "ALERTA" error ""
                  dialogos = dialogo :: model.dialogos
                in
                  {model| dialogos = dialogos} ! []


      OnResizeGet time ->
        
            
        (model, WindowResize.getTela "")

      OnResize tela ->
        ({model | tela = tela}, Cmd.none)
      

      

      AbreDialogoSelecionaImagemUsuario ->
        let
          dialogo = novo_dialogo_imagem_usuario 
          dialogos = dialogo :: model.dialogos
        in
        ({model | dialogos = dialogos}, Cmd.none)


      AlteraNomeDoJogador nome->
        let
          meu_jogo1 = case model.meu_jogo of
                        Nothing -> Nothing
                        Just jogo -> Just (Jogador.atualiza_nome jogo model.me nome)
          me = model.me
          me1 = { me | nome = nome}

          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              let
                (phxSocket1, phxCmd) = Push.envia_me phxSocket me1
              in
                ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
          usuario = {uid = me1.uid, nome = me1.nome, avatar = me1.avatar, pronto = me1.pronto}
        in
          ({model1 | me = me1, meu_jogo = meu_jogo1}, Cmd.batch [
                                                             cmd
                                                            , (Store.setUsuario usuario)
                                                            ])

      EstouPronto ->
        let
          me = model.me
          me1 = {me | pronto = (not me.pronto)}
          meu_jogo1 = case model.meu_jogo of
                        Nothing -> Nothing
                        Just jogo -> Just (Jogador.estou_pronto jogo me1)

          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              let
                (phxSocket1, phxCmd) = Push.envia_me phxSocket me1
              in
                ({ model | phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
          usuario = {uid = me1.uid, nome = me1.nome, avatar = me1.avatar, pronto = me1.pronto}

        in
         ({model1 | meu_jogo = meu_jogo1, me = me1}, Cmd.batch  [  cmd
                                  , (Store.setUsuario usuario)
                                  , Sound.play_sound("CLICK")])

      IniciandoJogo ->
        let
          (me1, meu_jogo) = case model.meu_jogo of
              Nothing -> (model.me, Nothing)
              Just jogo -> 
                let
                  (linhas, colunas) = Fn.gera_tamanho_board jogo
                  jogo1 = {jogo | colunas = colunas, linhas = linhas}
                  board = Fn.gera_board jogo1
                  pecas_disponiveis = Lista.shuffle board



                  (jogadores1, pecas_disponiveis1) = 
                    (Jogador.pega_pecas_de_jogadores [] pecas_disponiveis jogo.jogadores)
                  
                  jogadores2 = Jogador.organiza_vez jogadores1
                  
                  jogadores3 = (List.map (\p ->
                        if p.ordem == 1 then
                          {p | vez = True }
                        else
                          p
                    ) jogadores2)
                 
                  

                  me2 = Jogador.me model.me jogadores3
                    
                  me_id = 
                    case (List.filter (\x -> x.ordem ==1 ) jogadores3) |> List.head of
                      Nothing -> me2.uid
                      Just u -> u.uid

                  turno = jogo.turno
                  turno1 = {turno | jogador = me_id}
                  meu_jogo1 = { jogo | inicializando = True
                                     , board = board
                                     , linhas = linhas
                                     , colunas = colunas
                                     , jogadores = jogadores3
                                     , turno = turno1
                                     , pecas_disponiveis = pecas_disponiveis1
                                   }
                in
                  (me2, Just meu_jogo1)

          (model1, cmd) = case model.phxSocket of
            Nothing -> model ! []
            Just phxSocket ->
              case meu_jogo of
              Nothing -> (model, Cmd.none)
              Just jogo -> 
                let
                  jogadores = List.map(\x -> x.uid) jogo.jogadores
                  (phxSocket1, cmd) = 
                    case (List.filter(\x -> x.jid == jogo.jid ) model.jogos) |> List.head of
                      Nothing ->  
                        let
                          (phxSocket1, phxCmd1) = Push.envia_jogo_para_jogadores phxSocket jogadores model.me.uid jogo
                        in
                          (phxSocket1, Cmd.map PhoenixMsg phxCmd1)
                      Just j ->
                          let
                            jogo_ex = { j | iniciado =  True }
                            (phxSocket1, phxCmd1) = Push.envia_jogo_para_jogadores phxSocket jogadores model.me.uid jogo
                            (phxSocket2, phxCmd2) = Push.envia_jogo_para_exibir phxSocket1 jogo_ex
                          in
                            (phxSocket2, Cmd.batch [Cmd.map PhoenixMsg phxCmd1,Cmd.map PhoenixMsg phxCmd2])
                  
                  
                in
                  ({ model | phxSocket = Just phxSocket1}, cmd)
        in
          ({model1 | me = me1, meu_jogo = meu_jogo}, Cmd.batch [Sound.play_sound("START"), cmd])

      AtualizaNomeJogo nome ->
        case model.meu_jogo of
          Nothing -> model ! []
          Just jogo ->
            let
              jogo1 = { jogo| nome = nome}
              jogos = List.map(\j -> 
                if j.jid == jogo.jid then
                  {j| nome = nome}
                else 
                  j
                ) model.jogos
              retorno = 
                case (List.filter(\j -> j.jid == jogo.jid) jogos) |> List.head of
                  Nothing -> ({ model | jogos = jogos, meu_jogo = Just jogo1}, Cmd.none)
                  Just jo ->
                    case model.phxSocket of
                      Nothing -> model ! []
                      Just phxSocket ->
                        let
                          (phxSocket1, phxCmd) = Push.envia_jogo_para_exibir phxSocket jo
                        in
                          ({ model | jogos = jogos, meu_jogo = Just jogo1, phxSocket = Just phxSocket1}, Cmd.map PhoenixMsg phxCmd)
              
            in
              retorno


      AbreNovoJogo ->
        (model, Cmd.batch [Uuid.uuid "", Sound.play_sound("CLICK")])

      DefineNovoJogo jid->
        case model.phxSocket of
          Nothing -> model ! []
          Just phxSocket ->
            let
              me = model.me
              me1 = { me | jogo_id = Just jid}
              meu_jogo = Model.novo_jogo model.me.uid jid
              jogo_para_exibir = Model.novo_jogo_para_exibir model.me.uid jid
              jogadores = model.me :: meu_jogo.jogadores
              meu_jogo1 = {meu_jogo | jogadores = jogadores}
              jogo_para_exibir1 = { jogo_para_exibir | jogadores = (List.length jogadores)}
              jogos = jogo_para_exibir1 :: model.jogos
              (phxSocket1, phxCmd) = Push.envia_jogo_para_exibir phxSocket jogo_para_exibir1


              jogoPhxSocket = initJogoPhxSocket phxSocket1 model.me.uid jid
              channel =
                Phoenix.Channel.init ("jogo:" ++ jid)
                  |> Phoenix.Channel.withPayload (userParams model.me.uid)
                  |> Phoenix.Channel.onJoin (always ( ShowJoinedMessage ("jogo:" ++ jid)))
                  |> Phoenix.Channel.onClose (always ( ShowLeftMessage ("jogo:" ++ jid)))
              (phxSocket2, jogoCmd ) =
                Phoenix.Socket.join channel jogoPhxSocket

            in
            ( { model | me = me1, jogos = jogos, meu_jogo = Just meu_jogo1, phxSocket = Just phxSocket2}, Cmd.batch [
                                                                                                                      Cmd.map PhoenixMsg phxCmd
                                                                                                                    , Cmd.map PhoenixMsg jogoCmd
                                                                                                                    ])
      _ ->
        model ! []

