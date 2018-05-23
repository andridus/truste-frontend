module Utils.DE exposing (..)


import Model exposing (..)
import ModelWE exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias WJogo =
  {jogador: String, from : String , jogo: Jogo}

type alias WJogoJogadores =
  {jogadores: String, from: String , jogo: Jogo}

acao_decoder : Decoder Acao
acao_decoder =
  decode Acao
    |> required "empresa" string
    |> required "acoes" int

peca_decoder: Decoder Peca
peca_decoder =
  decode Peca 
    |> required "pid" string
    |> required "coluna" int
    |> required "linha" string
    |> required "sobre" bool
    |> required "na_peca" bool
    |> required "jogador" (nullable string)
    |> required "empresa" (nullable string)
    |> required "solo" int
    |> required "sede" bool

acao_turno_decoder : Decoder AcaoTurno
acao_turno_decoder =
  decode AcaoTurno 
    |> required "empresa" string
    |> required "acoes" int
    |> required "valor" int
    |> required "jogador" string

acao_bonus_turno_decoder : Decoder AcaoBonusTurno
acao_bonus_turno_decoder =
  decode AcaoBonusTurno 
    |> required "empresa" string
    |> required "acoes" int
    |> required "jogador" string
empresa_decoder : Decoder Empresa
empresa_decoder =
  decode Empresa 
    |> required "eid" string
    |> required "nome" string
    |> required "preco" int
    |> required "pecas" int
    |> required "limite" int
    |> required "icon" string
    |> required "peso" int
    |> required "cor" string
    |> required "disponivel" bool
    |> required "construcao" string

 
dialogo_decoder : Decoder Dialogo
dialogo_decoder =
  decode Dialogo 
    |> required "did" string
    |> required "titulo" string
    |> required "conteudo" string
    |> required "minimizado" bool
    |> required "tipo" string
    |> required "empresa" (nullable empresa_decoder)
    |> required "jogador" string

jogada_decoder : Decoder Jogada
jogada_decoder =
  decode Jogada 
    |> required "empresa" (nullable empresa_decoder)
    |> required "falencias" (list empresa_decoder)
    |> required "pecas" (list peca_decoder)
   
bonus_decoder : Decoder BonusTurno
bonus_decoder =
  decode BonusTurno
    |> required "info" string
    |> required "valor" int
    |> required "empresa" string
    |> required "jogador" string

turno_decoder : Decoder Turno
turno_decoder =
  decode Turno
    |> required "jogador" string
    |> required "peca" (nullable peca_decoder)
    |> required "nova_peca" (nullable peca_decoder)
    |> required "acoes_vendidas" (list acao_turno_decoder)
    |> required "acoes_compradas" (list acao_turno_decoder)
    |> required "acoes_vendidas_fusao" (list acao_turno_decoder)
    |> required "acoes_extras" (list acao_bonus_turno_decoder)
    |> required "atualizar_pecas" (nullable jogada_decoder)
    |> required "bonus" (list bonus_decoder)
    |> required "timestamp_entrada" int
    |> required "timestamp_saida" int
    |> required "pronto_pra_finalizar" bool


jogador_decoder : Decoder Jogador
jogador_decoder  =
  decode Jogador
    |> required "uid" string
    |> required "online" bool
    |> required "nome" string
    |> required "avatar" string
    |> required "pecas" (list peca_decoder)
    |> required "acoes" (list acao_decoder)
    |> required "jogo_id" (nullable string)
    |> required "din" int
    |> required "pronto" bool
    |> required "vez" bool
    |> required "ordem" int

jogo_decoder : Decoder Jogo
jogo_decoder =
  decode Jogo
    |> required "jid" string
    |> required "nome" string
    |> required "inicializando" bool
    |> required "historico" (list turno_decoder)
    |> required "cores" (list string)
    |> required "linhas" (list string)
    |> required "colunas" (list int)
    |> required "board" (list peca_decoder)
    |> required "pecas_disponiveis" (list peca_decoder)
    |> required "jogadores" (list jogador_decoder)
    |> required "empresas" (list empresa_decoder)
    |> required "timestamp" int
    |> required "criador_id" string
    |> required "max_users" int
    |> required "bonus" bool
    |> required "fim_do_jogo" bool
    |> required "turno" turno_decoder
    |> required "tamanho" string
    
jogo : Decoder WJogo
jogo =
  decode WJogo
    |> required "jogador" string
    |> required "from" string
    |> required "jogo" jogo_decoder


jogo_jogadores : Decoder WJogoJogadores
jogo_jogadores =
  decode WJogoJogadores
    |> required "jogadores" string
    |> required "from" string
    |> required "jogo" jogo_decoder

jogador_jid : Decoder JogadorJid
jogador_jid =
  map4 JogadorJid
    (field "uid" string)
    (field "jid" string)
    (field "nome" string)
    (field "avatar" string)

type alias WJJogadorJogadores =
  {jogador: String
  ,jogadores: List Jogador}

jogador_jogadores : Decoder WJJogadorJogadores
jogador_jogadores =
  decode WJJogadorJogadores
    |> required "jogador" string
    |> required "jogadores" (list jogador_decoder)

type alias WJJidCriador =
  {jid: String
  ,criador: String}

jid_criador : Decoder WJJidCriador
jid_criador =
  decode WJJidCriador
    |> required "jid" string
    |> required "criador" string


type alias WJJid =
  {jid: String}

jid : Decoder WJJid
jid =
  map WJJid 
    (field "jid" string)

type alias WJUid =
  {uid: String}

uid : Decoder WJUid
uid =
  map WJUid 
    (field "uid" string)

texto : Decoder Texto
texto =
  map Texto 
    (field "texto" string)

type alias WJTurnoA =
  {jogadores: String, turno: Turno, board: List Peca, from: Jogador}

recebe_turno_atualizado : Decoder WJTurnoA
recebe_turno_atualizado =
  decode WJTurnoA
    |> required "jogadores" string
    |> required "turno" turno_decoder
    |> required "board" (list peca_decoder)
    |> required "from" (jogador_decoder)

type alias WJTurno =
  {jogo: String, turno: Turno}

recebe_turno : Decoder WJTurno
recebe_turno =
  decode WJTurno
    |> required "jogo" string
    |> required "turno" turno_decoder


type alias WJDialogo =
  {dialogo: String}

recebe_cancela_dialogo : Decoder WJDialogo
recebe_cancela_dialogo =
  decode WJDialogo
    |> required "dialogo" string

type alias WJDialogos =
  {dialogos: List Dialogo}

recebe_dialogos : Decoder WJDialogos
recebe_dialogos =
  decode WJDialogos
    |> required "dialogos" (list dialogo_decoder)

type alias WJBoard =
  {jogo: String, board: List Peca}

recebe_board : Decoder WJBoard
recebe_board =
  decode WJBoard
    |> required "jogo" string
    |> required "board" (list peca_decoder)

jogador : Decoder Usuario
jogador =
  map4 Usuario 
    (field "uid" string)
    (field "nome" string)
    (field "avatar" string)
    (field "pronto" bool)

jogo_para_exibir : Decoder JogoParaExibir
jogo_para_exibir =
  map7 JogoParaExibir
      (field "jid" string)
      (field "nome" string)
      (field "jogadores" int)
      (field "timestamp" int)
      (field "criador_id" string)
      (field "max_users" int)
      (field "iniciado" bool)

type alias WJogosParaExibir =
  {uid: String, jogo: JogoParaExibir}

jogos_para_exibir : Decoder WJogosParaExibir
jogos_para_exibir =
  map2 WJogosParaExibir
      (field "uid" string)
      (field "jogo" (jogo_para_exibir))
      
