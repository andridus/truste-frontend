module State.Jogo.Msg exposing (..)

import Model exposing (..)

type Msg
     = NoOp
     | SobreAPeca Jogo Peca
     | SobreAPecaSai
     | InserePeca Jogo Peca
     | FinalizaJogada Jogo
     | EfetivaFinalizaJogada Jogo
     | ComprarAcao Jogo Empresa
     | StatusEmpresa Jogo Empresa
     | MaisZoom 
     | MenosZoom 
     | SobreEncerrarJogo
     | ToggleGraficos

     --ṔARA WEBSOCKET
     | EnviaBoard
     | EnviaDialogoInformacao
     | EnviaTurnoAtualizado

