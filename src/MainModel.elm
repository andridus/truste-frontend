module MainModel exposing (..)

import Phoenix.Socket
import Phoenix.Presence exposing (PresenceState, syncState, syncDiff, presenceStateDecoder, presenceDiffDecoder)
import Msg exposing (..)
import Plugs.Dialogo.Msg

import Model exposing (..)
import ModelWE exposing (..)




type alias UserPresence =
    { online_at : String
    , device : String
    }


type alias Model =
  { me: Jogador
  , jogos : List JogoParaExibir
  , meu_jogo: Maybe Jogo
  , dialogos : List Dialogo
  , tela : Tela 
  , zoom : Float
  , phxSocket : Maybe (Phoenix.Socket.Socket Msg)
  , jogoSocket : Maybe (Phoenix.Socket.Socket Msg)
  , phxPresences : PresenceState UserPresence
  , mensagens: List String
  , novaMensagem: String
  , usuarios : List Usuario
  , opts: {
      audio: Bool
    , high_graphs : Bool  
    }
}

novo_opts =
  {
    audio = True
  , high_graphs = False
  }
