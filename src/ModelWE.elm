module ModelWE exposing (..)

import Model exposing (..)


type alias Texto = 
  { texto: String }

type alias Usuario =
    { uid : String
    , nome : String
    , avatar : String
    , pronto : Bool
    }
type alias JogadorJid =
  { uid: String
  , jid: String
  , nome : String
  , avatar : String
  }

type alias JogadorJogo =
  { jogador: String
  , jogo : Jogo
  }