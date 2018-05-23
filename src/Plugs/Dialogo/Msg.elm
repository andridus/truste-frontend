module Plugs.Dialogo.Msg exposing (..)

import Model exposing (..)
import Phoenix.Socket
type Msg
    = NoOp
    | FinalizarJogada Dialogo
    | EncerrarJogo Dialogo
    | CompraAcao Empresa Jogo
    | VendeAcao Empresa Jogo
    | VendeAcaoFusao Empresa Jogador Jogo
    | CompraAcaoFusao Empresa Jogador Jogo
    | SelecionaAvatar Dialogo String
    | SelecionaNovaEmpresa Dialogo Empresa 
    | SelecionaFusaoEmpresa Dialogo Empresa (List Empresa) (List BonusAcionista) (List Empresa)
    | SelecionaEmpresaParaFusao Dialogo Empresa Jogo (List Empresa)
    | MinimizarDiag Dialogo
    | MaximizarDiag Dialogo
    | OkDiag Dialogo
    | CancelarDiag Dialogo
    | CancelarWebsocketDiag Dialogo
    | ConfirmaVendaAcaoFusao Dialogo
    | PerformSairDoJogo


    |EnviaMe
    |EnviaTurno
    |EnviaTurnoAtualizado
    |EnviaTurnoAtualizado1
    |EnviaPagaBonus
    |EnviaVendaAcaoFusao
    |EnviaDialogoFusao
    |EnviaDialogoInformacao
    |EnviaCancelaWebSocketDiag Dialogo