module Model exposing (..)

import Uuid  as Uuid

type alias Peca = 
  { pid: String
  , coluna: Int
  , linha: String
  , sobre: Bool
  , na_peca: Bool
  , jogador: Maybe String
  , empresa: Maybe String
  , solo : Int
  , sede: Bool
  }
type alias Acao =
  { empresa: String
  , acoes: Int
  }
type alias Jogador =
  { uid: String
  , online: Bool
  , nome: String
  , avatar: String
  , pecas: List Peca
  , acoes: List Acao
  , jogo_id: Maybe String
  , din : Int
  , pronto: Bool
  , vez: Bool
  , ordem: Int
  }
type alias Empresa =
  { eid: String
  , nome: String
  , preco: Int
  , pecas: Int
  , limite: Int
  , icon: String
  , peso: Int
  , cor: String
  , disponivel: Bool
  , construcao: String

  }

type alias JogoParaExibir = 
  { jid: String
  , nome: String
  , jogadores: Int
  , timestamp: Int
  , criador_id: String
  , max_users: Int
  , iniciado: Bool
  }
type alias BonusAcionistaJogador =
  {
    jogador: String
  , nome: String
  , acoes: Int
  , empresa: String  
  }
type alias BonusAcionistaE =
    {
      jogadores: List BonusAcionistaJogador
    , valor: Int
    }
type alias BonusAcionista =
  { empresa: String
  , primeiro: Maybe BonusAcionistaE
  , segundo: Maybe BonusAcionistaE
  , os_dois: Maybe BonusAcionistaE
  }
type alias AcaoTurno =
  { empresa: String
  , acoes: Int
  , valor: Int
  , jogador: String
  }
type alias AcaoBonusTurno =
  { empresa: String
  , acoes: Int
  , jogador: String
  }
type alias BonusTurno =
  { info: String
  , valor: Int
  , empresa: String
  , jogador: String
  }
type alias Jogada =
  { empresa: Maybe Empresa
  , falencias : List Empresa
  , pecas : List Peca
  }
type alias Turno =
  { jogador: String
  , peca: Maybe Peca
  , nova_peca: Maybe Peca
  , acoes_vendidas : List AcaoTurno
  , acoes_compradas : List AcaoTurno
  , acoes_vendidas_fusao : List AcaoTurno
  , acoes_extras : List AcaoBonusTurno
  , atualizar_pecas : Maybe Jogada
  , bonus : List BonusTurno
  , timestamp_entrada : Int
  , timestamp_saida : Int
  , pronto_pra_finalizar: Bool
  }
type alias Jogo =
  { jid: String
  , nome: String
  , inicializando: Bool
  , historico: List Turno
  , cores: List String
  , linhas: List String
  , colunas: List Int
  , board: List Peca
  , pecas_disponiveis: List Peca
  , jogadores: List Jogador
  , empresas: List Empresa
  , timestamp: Int
  , criador_id: String
  , max_users: Int
  , bonus: Bool
  , fim_do_jogo: Bool
  , turno: Turno
  , tamanho: String
  }

type alias Dialogo =
  {
    did : String 
  , titulo : String
  , conteudo : String
  , minimizado: Bool
  , tipo : String
  , empresa : Maybe Empresa
  , jogador : String
  }
type alias Tela =
  {   scrollTop: Int
    , pageHeight: Int
    , viewportHeight: Int
    , viewportWidth: Int
  }
novo_jogo_para_exibir : String -> String -> JogoParaExibir 
novo_jogo_para_exibir uid jid= 
    { jid = jid
    , nome = ""
    , jogadores = 0
    , timestamp = 0
    , criador_id = uid
    , max_users = 6
    , iniciado = False
    }

nova_peca : String -> Int -> Peca
nova_peca linha coluna =
  { pid = Uuid.gen 25
  , coluna = coluna
  , linha = linha
  , sobre = False
  , na_peca = False
  , jogador = Nothing
  , empresa = Nothing
  , solo = Uuid.random 5
  , sede = False
  }


novo_jogo : String -> String -> Jogo 
novo_jogo uid jid= 
    { jid = jid
    , inicializando = False
    , nome = ""
    , historico = []
    , cores = []
    , linhas = []
    , colunas = []
    , board = []
    , pecas_disponiveis = []
    , jogadores = []
    , empresas = [
          nova_empresa 1 "Hotal" 1 "/imgs/enterprise/empresa1.png" "#F0E165" "1"
        , nova_empresa 2 "DeBurgue" 1 "/imgs/enterprise/empresa2.png" "#D64E4E" "2"
        , nova_empresa 3 "EcoTur" 2 "/imgs/enterprise/empresa3.png" "#22E651" "3"
        , nova_empresa 4 "CitLuz" 2 "/imgs/enterprise/empresa4.png" "#404040" "4"
        , nova_empresa 5 "Mercado" 2 "/imgs/enterprise/empresa5.png" "#974A0D" "5"
        , nova_empresa 6 "Telenet" 3 "/imgs/enterprise/empresa6.png" "#2A71E2" "6"
        , nova_empresa 7 "DaFonte" 3 "/imgs/enterprise/empresa7.png" "#FFAB1A" "7"
      ]
    , timestamp = 0
    , criador_id = uid
    , max_users = 6
    , bonus = True
    , fim_do_jogo = False
    , turno = (novo_turno "")
    , tamanho = "medio"

    }

nova_empresa : Int -> String -> Int -> String -> String -> String -> Empresa
nova_empresa tm nome peso icon cor construcao = 
  { eid = Uuid.gen tm
  , nome = nome
  , preco = 0
  , pecas = 0
  , limite = 25
  , icon = icon
  , peso = peso
  , disponivel = True
  , cor = cor
  , construcao = construcao

  }
novo_turno : String -> Turno
novo_turno  uid =
  { jogador = uid
  , peca = Nothing
  , nova_peca = Nothing
  , acoes_vendidas = []
  , acoes_vendidas_fusao = []
  , acoes_compradas = []
  , acoes_extras = []
  , atualizar_pecas = Just nova_jogada
  , bonus = []
  , timestamp_entrada = 0
  , timestamp_saida = 0
  , pronto_pra_finalizar = False
  }

nova_jogada : Jogada
nova_jogada =
  { empresa = Nothing
  , falencias = []
  , pecas = []
  }
novo_jogador : String -> Maybe String-> Jogador
novo_jogador  uid jid=
    { uid = uid
    , nome = ""
    , avatar = "/imgs/players/no-image.png"
    , online = False
    , pecas = []
    , acoes = []
    , din = 600
    , jogo_id = jid
    , pronto = False
    , vez = False
    , ordem = 0
    }

novo_jogador1 : String -> Maybe String -> String -> String -> Jogador
novo_jogador1  uid jid nome avatar =
    { uid = uid
    , nome = nome
    , avatar = avatar
    , online = False
    , pecas = []
    , acoes = []
    , din = 600
    , jogo_id = jid
    , pronto = False
    , vez = False
    , ordem = 0
    }
novo_dialogo_imagem_usuario :  Dialogo
novo_dialogo_imagem_usuario =
  novo_dialogo "SELECIONA_IMAGEM_USUARIO" "" "" ""

novo_dialogo_funde_empresas :  Dialogo
novo_dialogo_funde_empresas =
  novo_dialogo "FUNDE_EMPRESA" "" "" ""


novo_dialogo_nova_empresa :  Dialogo
novo_dialogo_nova_empresa =
  novo_dialogo "NOVA_EMPRESA" "" "" ""

novo_dialogo_status_empresa : String ->  Dialogo
novo_dialogo_status_empresa empresa =
  novo_dialogo "STATUS_EMPRESA" empresa "" ""

novo_dialogo_vender_acoes_empresa_fundida : String -> String ->  Dialogo
novo_dialogo_vender_acoes_empresa_fundida empresa jogador =
  novo_dialogo "VENDER_ACOES_EMPRESA_FUNDIDA" empresa jogador jogador

novo_dialogo : String -> String -> String -> String -> Dialogo
novo_dialogo tipo titulo conteudo jogador=
  {
    did = Uuid.gen 55
  , titulo= titulo
  , conteudo= conteudo
  , minimizado = False
  , tipo= tipo
  , empresa = Nothing
  , jogador = jogador
  }