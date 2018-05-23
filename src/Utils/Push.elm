module Utils.Push exposing (..)

import Model exposing  (..)
import Msg exposing (..)
import Phoenix.Socket 
import Phoenix.Push
import Json.Encode as JE
import String

acaoCoder : Acao -> JE.Value
acaoCoder acao =
  JE.object [
    ("empresa", JE.string acao.empresa)
  , ("acoes", JE.int acao.acoes)
  ]

pecaCoder: Peca -> JE.Value
pecaCoder peca=
  JE.object [
    ("pid", JE.string peca.pid)
  , ("coluna", JE.int peca.coluna)
  , ("linha", JE.string peca.linha)
  , ("sobre", JE.bool peca.sobre)
  , ("na_peca", JE.bool peca.na_peca)
  , ("jogador", 
    case peca.jogador of 
        Nothing -> JE.null 
        Just j -> JE.string j
    )
  , ("empresa", 
    case peca.empresa of 
        Nothing -> JE.null 
        Just e -> JE.string e
    )
  , ("solo", JE.int peca.solo)
  , ("sede", JE.bool peca.sede)
  ]

acaoTurnoCoder : AcaoTurno -> JE.Value
acaoTurnoCoder acao_turno=
  JE.object [
    ("empresa", JE.string acao_turno.empresa)
  , ("acoes", JE.int acao_turno.acoes)
  , ("valor", JE.int acao_turno.valor)
  , ("jogador", JE.string acao_turno.jogador)
  ]

acaoBonusTurnoCoder : AcaoBonusTurno -> JE.Value
acaoBonusTurnoCoder acao_turno=
  JE.object [
    ("empresa", JE.string acao_turno.empresa)
  , ("acoes", JE.int acao_turno.acoes)
  , ("jogador", JE.string acao_turno.jogador)
  ]
empresaCoder : Empresa -> JE.Value
empresaCoder empresa=
  JE.object [
    ("eid", JE.string empresa.eid)
  , ("nome", JE.string empresa.nome)
  , ("preco", JE.int empresa.preco)
  , ("pecas", JE.int empresa.pecas)
  , ("limite", JE.int empresa.limite)
  , ("icon", JE.string empresa.icon)
  , ("peso", JE.int empresa.peso)
  , ("cor", JE.string empresa.cor)
  , ("disponivel", JE.bool empresa.disponivel)
  , ("construcao", JE.string empresa.construcao)
  ]

jogadaCoder :  Maybe Jogada -> JE.Value
jogadaCoder jogada =
  case jogada of
    Nothing -> JE.null
    Just j ->
      JE.object [
        ("empresa", 
          case j.empresa of 
            Nothing -> JE.null 
            Just e -> empresaCoder e)
      , ("falencias", JE.list (List.map (\x -> empresaCoder x) j.falencias) )
      , ("pecas", JE.list (List.map (\x -> pecaCoder x) j.pecas) )
      ]
bonusCoder : BonusTurno -> JE.Value
bonusCoder bonus=
  JE.object [
    ("info", JE.string bonus.info)
  , ("valor", JE.int bonus.valor)
  , ("empresa", JE.string bonus.empresa)
  , ("jogador", JE.string bonus.jogador)
  ]

turnoCoder: Turno -> JE.Value
turnoCoder turno =
  JE.object [
    ("jogador", JE.string turno.jogador )
  , ("peca", 
      case turno.peca of 
        Nothing -> JE.null 
        Just p -> pecaCoder p
    )
  , ("nova_peca", 
    case turno.nova_peca of 
        Nothing -> JE.null 
        Just p -> pecaCoder p
    )
  , ("acoes_vendidas", JE.list (List.map (\x -> acaoTurnoCoder x) turno.acoes_vendidas) )
  , ("acoes_compradas", JE.list (List.map (\x -> acaoTurnoCoder x) turno.acoes_compradas) )
  , ("acoes_vendidas_fusao", JE.list (List.map (\x -> acaoTurnoCoder x) turno.acoes_vendidas_fusao) )
  , ("acoes_extras", JE.list (List.map (\x -> acaoBonusTurnoCoder x) turno.acoes_extras) )
  , ("atualizar_pecas", if turno.atualizar_pecas == Nothing then JE.null else (jogadaCoder turno.atualizar_pecas))
  , ("bonus", JE.list (List.map (\x -> bonusCoder x) turno.bonus) )
  , ("timestamp_entrada", JE.int turno.timestamp_entrada )
  , ("timestamp_saida", JE.int turno.timestamp_saida )
  , ("pronto_pra_finalizar", JE.bool turno.pronto_pra_finalizar )
  ]

jogadorCoder : Jogador -> JE.Value
jogadorCoder jogador =
  JE.object [
    ("uid", JE.string jogador.uid )
  , ("online", JE.bool jogador.online )
  , ("nome", JE.string jogador.nome )
  , ("avatar", JE.string jogador.avatar )
  , ("pecas", JE.list (List.map (\x -> pecaCoder x) jogador.pecas) )
  , ("acoes", JE.list (List.map (\x -> acaoCoder x) jogador.acoes) )
  , ("jogo_id", 
    case jogador.jogo_id of 
      Nothing -> JE.null 
      Just j -> JE.string j
    )
  , ("din", JE.int jogador.din )
  , ("pronto", JE.bool jogador.pronto )
  , ("vez", JE.bool jogador.vez )
  , ("ordem", JE.int jogador.ordem )
  ]


jogoCoder: Jogo -> JE.Value
jogoCoder jogo =
  JE.object [
    ("jid", JE.string jogo.jid)
  , ("nome", JE.string jogo.nome)
  , ("inicializando", JE.bool jogo.inicializando)
  , ("historico", JE.list (List.map turnoCoder jogo.historico))
  , ("cores", JE.list (List.map (\x -> JE.string x) jogo.cores))
  , ("linhas", JE.list (List.map (\x -> JE.string x) jogo.linhas))
  , ("colunas", JE.list (List.map (\x -> JE.int x) jogo.colunas))
  , ("board", JE.list (List.map pecaCoder jogo.board))
  , ("pecas_disponiveis", JE.list (List.map pecaCoder jogo.pecas_disponiveis))
  , ("jogadores", JE.list (List.map jogadorCoder jogo.jogadores))
  , ("empresas", JE.list (List.map empresaCoder jogo.empresas))
  , ("timestamp", JE.int jogo.timestamp)
  , ("criador_id", JE.string jogo.criador_id)
  , ("max_users", JE.int jogo.max_users)
  , ("bonus", JE.bool jogo.bonus)
  , ("fim_do_jogo", JE.bool jogo.fim_do_jogo)
  , ("tamanho", JE.string jogo.tamanho)
  , ("turno", turnoCoder jogo.turno)



  ]
dialogoCoder: Dialogo -> JE.Value
dialogoCoder dialogo =
  JE.object [
    ("did", JE.string dialogo.did)
  , ("titulo", JE.string dialogo.titulo)
  , ("conteudo", JE.string dialogo.conteudo)
  , ("minimizado", JE.bool dialogo.minimizado)
  , ("tipo", JE.string dialogo.tipo)
  , ("empresa", 
      case dialogo.empresa of 
          Nothing -> JE.null 
          Just e -> empresaCoder e
    )
  , ("jogador", JE.string dialogo.jogador)
  ]   
retira_todos_do_jogo : (Phoenix.Socket.Socket Msg) ->  String -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
retira_todos_do_jogo phoenix criador jid =
  let
    payload = 
      (JE.object [
          ("jid",JE.string jid)
        , ("criador",JE.string criador)
        ])

    push_ =
      Phoenix.Push.init "retira_todos_do_jogo" ("jogo:"++jid)
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

sair_do_jogo : (Phoenix.Socket.Socket Msg) ->  String -> String -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
sair_do_jogo phoenix uid criador jid=
  let
    payload = 
      (JE.object [
          ("uid",JE.string uid)
        , ("criador", JE.string criador)
        ])

    push_ =
      Phoenix.Push.init "sair_do_jogo" ("jogo:"++jid)
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

entre_no_jogo : (Phoenix.Socket.Socket Msg) ->  String -> String -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
entre_no_jogo phoenix jid criador_id jogador  =
  let
    payload = 
      (JE.object [
          ("criador", JE.string criador_id)
        , ("jid", JE.string jid )
        , ("jogador", JE.string jogador)
        ])

    push_ =
      Phoenix.Push.init "entre_no_jogo" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

entra_no_jogo : (Phoenix.Socket.Socket Msg) ->  String -> String ->  String -> String -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
entra_no_jogo phoenix uid jid criador_id nome avatar =
  let
    payload = 
      (JE.object [
          ("uid",JE.string uid)
        , ("criador", JE.string criador_id)
        , ("jid", JE.string jid )
        , ("nome", JE.string nome)
        , ("avatar", JE.string avatar)
        ])

    push_ =
      Phoenix.Push.init "entra_no_jogo" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_jogadores : (Phoenix.Socket.Socket Msg) -> String -> String -> List Jogador -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_jogadores phoenix uid jid jogadores =
  let
    payload = 
      JE.object [
        ("jogadores", JE.list (List.map jogadorCoder jogadores))
      , ("jogador", JE.string uid)
      ]
   

    push_ =
        Phoenix.Push.init "envia_jogadores" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)
type alias JogadorJogo = {jogador: String, jogo : Jogo}


envia_jogo_para_jogadores : (Phoenix.Socket.Socket Msg) -> List String -> String -> Jogo -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_jogo_para_jogadores phoenix uids from jogo =
  let
    payload = 
      JE.object [
        ("jogadores", JE.string (String.join "|" uids))
      , ("from", JE.string from)
      , ("jogo", jogoCoder jogo)
      ]
   

    push_ =
        Phoenix.Push.init "envia_jogo_para_jogadores" ("jogo:"++jogo.jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)
{-atualiza_turno : (Phoenix.Socket.Socket Msg) -> String -> Turno -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
atualiza_turno phoenix jid turno =
  let
    payload = 
      JE.object [
        ("jogo", JE.string jid)
      , ("turno", turnoCoder turno)

      ]
    push_ =
        Phoenix.Push.init "atualiza_turno" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)-}

paga_bonus : (Phoenix.Socket.Socket Msg) -> List String -> Jogador -> Turno -> List Peca -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
paga_bonus phoenix uids from turno board jid =
  let
    payload = 
      JE.object [
        ("jogadores", JE.string (String.join "|" uids))
      , ("from", jogadorCoder from)
      , ("turno", turnoCoder turno)
      , ("board", JE.list (List.map (\x -> pecaCoder x) board) )
      ]
   

    push_ =
        Phoenix.Push.init "paga_bonus" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

venda_acao_fusao : (Phoenix.Socket.Socket Msg) -> List String -> Jogador -> Turno -> List Peca -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
venda_acao_fusao phoenix uids from turno board jid =
  let
    payload = 
      JE.object [
        ("jogadores", JE.string (String.join "|" uids))
      , ("from", jogadorCoder from)
      , ("turno", turnoCoder turno)
      , ("board", JE.list (List.map (\x -> pecaCoder x) board) )
      ]
   

    push_ =
        Phoenix.Push.init "venda_acao_fusao" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

atualiza_turno : (Phoenix.Socket.Socket Msg) -> List String -> Jogador -> Turno -> List Peca -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
atualiza_turno phoenix uids from turno board jid =
  let
    payload = 
      JE.object [
        ("jogadores", JE.string (String.join "|" uids))
      , ("from", jogadorCoder from)
      , ("turno", turnoCoder turno)
      , ("board", JE.list (List.map (\x -> pecaCoder x) board) )
      ]
   

    push_ =
        Phoenix.Push.init "atualiza_turno" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)


envia_turno : (Phoenix.Socket.Socket Msg) -> String -> Turno -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_turno phoenix jid turno =
  let
    payload = 
      JE.object [
        ("jogo", JE.string jid)
      , ("turno", turnoCoder turno)

      ]
    push_ =
        Phoenix.Push.init "envia_turno" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_board : (Phoenix.Socket.Socket Msg) -> String -> List Peca -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_board phoenix jogo board =
  let
    board1 = List.map(\x -> {x | sobre = False, na_peca = False}) board
    payload = 
      JE.object [
        ("jogo", JE.string jogo)
      , ("board", JE.list (List.map (\x -> pecaCoder x) board1))

      ]
   

    push_ =
        Phoenix.Push.init "envia_board" ("jogo:"++jogo)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_cancela_dialogo : (Phoenix.Socket.Socket Msg) -> String -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_cancela_dialogo phoenix did jid=
  let
    payload = 
      JE.object [
        ("dialogo", JE.string did)
      ]
   

    push_ =
        Phoenix.Push.init "envia_cancela_dialogo" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_dialogos : (Phoenix.Socket.Socket Msg) -> List Dialogo -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_dialogos phoenix dialogos jid =
  let
    payload = 
      JE.object [
        ("dialogos", JE.list (List.map dialogoCoder dialogos))
      ]
   

    push_ =
        Phoenix.Push.init "envia_dialogos" ("jogo:"++jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_jogo : (Phoenix.Socket.Socket Msg) -> String -> String -> Jogo -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_jogo phoenix  jogador from jogo =
  let
    payload = 
      JE.object [
        ("jogador", JE.string jogador)
      , ("from" , JE.string from)
      , ("jogo", jogoCoder jogo)
      ]
   

    push_ =
        Phoenix.Push.init "envia_jogo" ("jogo:"++jogo.jid)
          |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)


envia_me : (Phoenix.Socket.Socket Msg) -> Jogador -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_me phoenix me =
  let
    payload = 
      (JE.object [
          ("uid", JE.string me.uid )
        , ("nome", JE.string me.nome )
        , ("avatar", JE.string me.avatar)
        , ("pronto", JE.bool me.pronto)
        ])

    push_ =
      Phoenix.Push.init "envia_me" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

retira_jogo_para_exibir : (Phoenix.Socket.Socket Msg) -> String -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
retira_jogo_para_exibir phoenix jogo =
  let
    payload = 
      (JE.object [
          ("jid", JE.string jogo )
        ])

    push_ =
      Phoenix.Push.init "retira_jogo_para_exibir" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

envia_jogo_para_exibir : (Phoenix.Socket.Socket Msg) -> JogoParaExibir -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_jogo_para_exibir phoenix jogo =
  let
    payload = 
      (JE.object [
          ("jid", JE.string jogo.jid )
        , ("nome", JE.string jogo.nome )
        , ("jogadores", JE.int jogo.jogadores)
        , ("timestamp", JE.int jogo.timestamp)
        , ("criador_id", JE.string jogo.criador_id)
        , ("max_users", JE.int jogo.max_users)
        , ("iniciado", JE.bool jogo.iniciado)
        ])

    push_ =
      Phoenix.Push.init "envia_jogo_para_exibir" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)

{-
envia_jogos : (Phoenix.Socket.Socket Msg) -> Jogador -> JogoParaExibir -> ((Phoenix.Socket.Socket Msg), Cmd (Phoenix.Socket.Msg Msg))
envia_jogos phoenix me j =
  let
    jogo1 =
        JE.object [
            ("jid", JE.string j.jid )
          , ("nome", JE.string j.nome )
          , ("jogadores", JE.int j.jogadores)
          , ("timestamp", JE.int j.timestamp)
          , ("criador_id", JE.string j.criador_id)
          , ("max_users", JE.int j.max_users)
          ]
    payload = 
      (JE.object [
          ("uid", JE.string me.uid )
        , ("jogo", jogo1 )
        ])
    push_ =
      Phoenix.Push.init "envia_jogos" "base"
        |> Phoenix.Push.withPayload payload
    (phxSocket, phxCmd) =
      Phoenix.Socket.push push_ phoenix
  in
    (phxSocket, phxCmd)
    -}