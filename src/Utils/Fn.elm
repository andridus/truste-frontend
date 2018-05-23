module Utils.Fn exposing (..)

import Model exposing (..)
import MainModel exposing (..)

import List.Extra as ListX
import Debug
import Utils.Jogador as Jogador


gera_tamanho_board : Jogo -> (List String, List Int)
gera_tamanho_board jogo =
  case jogo.tamanho of
    "pequeno" -> 
      ([],[])
    "medio" -> 
      (["A","B","C","D","E","F","G","H","I"],[1,2,3,4,5,6,7,8,9,10,11,12])
    "grande" ->
      ([],[])
    _ ->
      ([],[])


gera_board : Jogo -> List Peca
gera_board jogo =
  let
    pecas = (List.map (\linha ->
          List.map (\coluna ->
            Model.nova_peca linha coluna
          ) jogo.colunas

      ) jogo.linhas)
      |> List.concat

  in
    pecas

converte_linha_para_numero : String -> Int
converte_linha_para_numero linha =
  case linha of
    "A" -> 1
    "B" -> 2
    "C" -> 3
    "D" -> 4
    "E" -> 5
    "F" -> 6
    "G" -> 7
    "H" -> 8
    "I" -> 9
    "J" -> 10
    "K" -> 11
    "L" -> 12
    "M" -> 13
    "N" -> 14
    "O" -> 15
    "P" -> 16
    "Q" -> 17
    "R" -> 18
    "S" -> 19
    "T" -> 20
    _ -> 21

pega_peca : Jogo -> Int -> Int -> List Peca
pega_peca jogo linha coluna =
  case ((List.filter (\x -> 
        case (x.coluna == coluna && (converte_linha_para_numero x.linha) == linha) of
            True ->
              if x.jogador /= Nothing then
                True
              else
                False
            False ->
              False
      ) jogo.board)
  |> List.head) of
    Nothing ->  []
    Just peca -> [peca]


vizinhanca_da_peca1: List Peca -> Jogo -> List Peca
vizinhanca_da_peca1 pecas jogo =
  case List.head pecas of
    Nothing -> []
    Just peca ->
      let
        top = pega_peca jogo ((converte_linha_para_numero peca.linha)-1) peca.coluna
        left = pega_peca jogo ((converte_linha_para_numero peca.linha)) (peca.coluna-1)
        bottom = pega_peca jogo ((converte_linha_para_numero peca.linha+1)) (peca.coluna)
        right = pega_peca jogo ((converte_linha_para_numero peca.linha)) (peca.coluna+1)
        pecas1 = List.concat [top, left, right, bottom]
      in
        pecas1

vizinhanca_da_peca: Peca -> Jogo -> List Peca
vizinhanca_da_peca peca jogo =
  let
    top = pega_peca jogo ((converte_linha_para_numero peca.linha)-1) peca.coluna
    left = pega_peca jogo ((converte_linha_para_numero peca.linha)) (peca.coluna-1)
    bottom = pega_peca jogo ((converte_linha_para_numero peca.linha+1)) (peca.coluna)
    right = pega_peca jogo ((converte_linha_para_numero peca.linha)) (peca.coluna+1)
    pecas = List.concat [ top, left, right, bottom
                        , vizinhanca_da_peca1 top jogo
                        , vizinhanca_da_peca1 bottom jogo
                        , vizinhanca_da_peca1 left jogo
                        , vizinhanca_da_peca1 right jogo
                        ]
  in
    pecas

analiza_jogada : List Peca -> Jogo -> (String, Jogada)
analiza_jogada pecas jogo =
  if (List.length pecas) == 0 then
    let
      jogada = nova_jogada
    in
      ("NULO", { jogada | pecas = pecas, empresa = Nothing})
  else
    let
      jogada = nova_jogada
      com_empresas = List.filter(\x ->
          x.empresa /= Nothing
      ) pecas

      --PEGADO A EMPRESA ATRAVES DA EMPRESA_ID DA PEÇA
      empresas = (List.map (\x->
          let

            empresa = case x.empresa of
                  Nothing -> Nothing
                  Just emp ->
                    (List.filter(\y ->
                     y.eid == emp
                    ) jogo.empresas) |> List.head
          in  
            case empresa of
              Nothing -> []
              Just emp -> [emp]

        ) com_empresas) |> List.concat
      empresas1 = ListX.uniqueBy (\x -> x.eid) empresas
      d = (Debug.log "empresas" (empresas1))
      (tipo, empresa) = 
        case (List.length empresas1) of
          0 -> 
            ("NOVA_EMPRESA",Nothing)
          1 ->
            let
              empresa = case List.head empresas1 of
                Nothing -> Debug.crash "ERRO AO SELECIONAR EMPRESA MDOULE FN"
                Just empresa -> empresa
              
             in
              ("AMPLIA_EMPRESA", Just empresa)
          _ ->
            let
              empresa = case List.head empresas1 of
                Nothing -> Debug.crash "ERRO AO SELECIONAR EMPRESA MDOULE FN"
                Just empresa -> empresa
             in
              ("FUNDE_EMPRESA", Nothing)

    in
      (tipo, { jogada | pecas = pecas, empresa = empresa})

verifica_jogada: Jogo -> Peca -> (String, Jogada)
verifica_jogada jogo peca =
  let
    vizinhanca = vizinhanca_da_peca peca jogo
    d = (Debug.log "Vizinhanca" (List.length vizinhanca))
    


    jogada_feita = analiza_jogada vizinhanca jogo
    d1 = Debug.log "jogada feita#1" jogada_feita

  in
    jogada_feita



atualiza_board : List Peca -> List Peca -> List Peca
atualiza_board board pecas =
  case List.head pecas of
    Nothing -> board
    Just pec ->
      case List.tail pecas of
        Nothing ->
          atualiza_board1 board pec
        Just tail ->
          let
            board1 = atualiza_board1 board pec
          in
          atualiza_board board1 tail

atualiza_board1 : List Peca -> Peca -> List Peca
atualiza_board1 board peca =
  let
    board1 = List.map(\b ->
        if b.pid == peca.pid then
          peca
        else
          b
      ) board

  in
    board1

reduz_uma_acao_da_empresa : Empresa -> List Empresa -> List Empresa 
reduz_uma_acao_da_empresa empresa empresas =
  let
    empresas1 = List.map(\e ->
        case e.eid == empresa.eid of
          True ->
            let
              limite = e.limite - 1
            in
              {e | limite = limite}
          False -> 
            e
      ) empresas 
  in
    empresas1

aumenta_uma_acao_da_empresa : Empresa -> List Empresa -> List Empresa 
aumenta_uma_acao_da_empresa empresa empresas =
  let
    empresas1 = List.map(\e ->
        case e.eid == empresa.eid of
          True ->
            let
              limite = e.limite + 1
            in
              {e | limite = limite}
          False -> 
            e
      ) empresas 
  in
    empresas1
    
conta_pecas_empresas : List Peca -> List Empresa -> List Empresa
conta_pecas_empresas board empresas =
  List.map (\x ->
    let
      contagem = (List.filter (\y -> y.empresa == Just x.eid) board) |> List.length
    in
      { x | pecas = contagem, preco = (valor_empresa1 x contagem)  }
  ) empresas

empresas_majo : List Empresa -> (List Empresa, List Empresa)
empresas_majo empresas =
  let
    empr1 = ListX.maximumBy (\x -> x.pecas ) empresas
    iguais = case empr1 of
      Nothing -> []
      Just emp ->
        List.filter (\x -> x.pecas == emp.pecas) empresas
    falencia = case empr1 of
      Nothing -> []
      Just emp ->
        List.filter (\x -> x.pecas /= emp.pecas) empresas
  in
    (iguais, falencia)

valor_empresa : Empresa -> Int
valor_empresa emp =
  valor_empresa1 emp emp.pecas

valor_empresa1 : Empresa -> Int -> Int
valor_empresa1 emp pecas=
    if pecas == 0 then
        0
    else if pecas == 2 then
        case emp.peso of
          1 -> 20
          2 -> 30
          3 -> 40
          _ -> 0
    else if pecas == 3 then
        case emp.peso of
          1 -> 30
          2 -> 40
          3 -> 50
          _ -> 0
    else if pecas == 4 then
        case emp.peso of
          1 -> 40
          2 -> 50
          3 -> 60
          _ -> 0
    else if pecas == 5 then
        case emp.peso of
          1 -> 50
          2 -> 60
          3 -> 70
          _ -> 0
    else if pecas >= 6 && pecas <= 10 then
        case emp.peso of
          1 -> 60
          2 -> 70
          3 -> 80
          _ -> 0
    else if pecas >= 11 && pecas <= 20 then
        case emp.peso of
          1 -> 70
          2 -> 80
          3 -> 90
          _ -> 0
    else if pecas >= 21 && pecas <= 30 then
        case emp.peso of
          1 -> 80
          2 -> 90
          3 -> 100
          _ -> 0
    else if pecas >= 31 && pecas <= 40 then
        case emp.peso of
          1 -> 90
          2 -> 100
          3 -> 110
          _ -> 0
    else if pecas >= 41 then
        case emp.peso of
          1 -> 100
          2 -> 110
          3 -> 120
          _ -> 0
    else
      0

bonus_empresa : Empresa -> {primeiro: Int, segundo: Int}
bonus_empresa emp =
  let
    primeiro = (valor_empresa emp)*10
    segundo =  (valor_empresa emp)*5 
  in
    {primeiro = primeiro, segundo = segundo}

pega_bonus_do_jogador_da_empresa : Empresa -> Jogo -> BonusAcionista
pega_bonus_do_jogador_da_empresa empresa jogo =
  let
    empresas = conta_pecas_empresas jogo.board jogo.empresas
    empresa1 = 
      case ListX.find (\y -> y.eid == empresa.eid ) empresas of 
        Nothing -> empresa
        Just emp -> emp

    valor_bonus = bonus_empresa empresa1

    acionistas0 = List.filterMap (\x -> 
        case ListX.find (\y-> y.empresa == empresa1.eid) x.acoes of
          Nothing -> Nothing
          Just a ->
            let
              nome = case x.nome of
                "" -> "Sem Nome"
                _ -> x.nome
            in
              Just {jogador = x.uid, nome = nome, acoes = a.acoes, empresa = a.empresa}
      ) jogo.jogadores
    acionistas = List.filter (\x -> x.acoes /= 0 ) acionistas0
    maior_acionista = ListX.maximumBy (\x -> x.acoes ) acionistas
    iguais = case maior_acionista of
      Nothing -> []
      Just ac ->
        List.filter (\x -> x.acoes == ac.acoes) acionistas
    (primeiro, segundo, os_dois) = 

      case maior_acionista of
        Nothing -> (Nothing, Nothing, Nothing)
        Just ac ->
          case List.length iguais > 1 of
            True ->
              (Nothing, Nothing, Just {jogadores= iguais, valor=(valor_bonus.primeiro+valor_bonus.segundo)})
            False ->

              let
                acionistas_tail = ListX.remove ac acionistas
                (primeiro, segundo, os_dois) =  
                  case List.length acionistas_tail > 0 of
                    False -> 
                      (Nothing, Nothing, Just {jogadores = iguais, valor = (valor_bonus.primeiro+valor_bonus.segundo)})
                    True ->
                      let
                        segundo_maior_acionista = ListX.maximumBy (\x -> x.acoes ) acionistas_tail
                        segundo_iguais = case segundo_maior_acionista of
                          Nothing -> []
                          Just ac1 ->
                            List.filter (\x -> x.acoes == ac1.acoes) acionistas_tail
                      in
                        (Just {jogadores = iguais, valor = valor_bonus.primeiro}, Just {jogadores = segundo_iguais, valor = valor_bonus.segundo}, Nothing)


              in
                (primeiro, segundo, os_dois)

    bonus = { empresa = empresa.eid
            , primeiro = primeiro
            , segundo = segundo
            , os_dois = os_dois
          }
  in
      
    bonus


finaliza_jogada : Model -> String -> Jogo -> Model
finaliza_jogada model uid jogo =
  let 
    jogador = (List.filter(\x -> x.uid == uid ) jogo.jogadores) |> List.head
  in
    case jogador of
      Nothing -> model
      Just me ->  
        let
          ordem = if (me.ordem+1)<=(List.length jogo.jogadores) then
                    me.ordem + 1
                  else
                    1
          turno = jogo.turno
          
          historico = List.reverse jogo.historico
          

          
          (nova_peca, pecas_disponiveis1) = (Jogador.pega_peca 1 jogo.pecas_disponiveis)
                  

          jogadores1 = case jogo.turno.peca of
            Nothing -> jogo.jogadores
            Just p ->
              let 
                minhas_pecas1 = (List.filter(\b -> 
                    if p.linha == b.linha && p.coluna == b.coluna then
                      False
                    else
                      True
                  ) me.pecas)
                minhas_pecas2 = List.concat [nova_peca,minhas_pecas1]
                me1 = {me | pecas = minhas_pecas2}
              in 
                (Jogador.atualiza_minhas_pecas me1 jogo.jogadores)

          jogadores2 = (List.map (\p ->
                        if p.ordem == ordem then
                          {p | vez = True }
                        else
                          {p | vez = False}
                    ) jogadores1)

          turno1 = case List.head nova_peca of
            Nothing -> turno
            Just p1 ->
              { turno | nova_peca = Just p1}
          
          historico1 = turno1 :: historico
          historico2 = List.reverse historico1

          me1 = Jogador.me me jogadores2
          prox = Jogador.proximo_jogador jogadores2

          model1 = 
            case prox of
              Nothing ->
                model
              Just prox1 ->
                let
                  novo_turno1 = novo_turno prox1.uid
                  jogo1 = { jogo | historico = historico2, jogadores = jogadores2, turno = novo_turno1, pecas_disponiveis = pecas_disponiveis1}
                in
                  {model | me = me1, meu_jogo = ( Just jogo1 )}

          {-dialogos = model1.dialogos
          dialogo = novo_dialogo "JOGADA_ENCERRADA" "Informação" "Sua jogada foi finalizada."
          dialogos2 = dialogo :: dialogos-}

          --model2 = {model1 | dialogos = dialogos2}

          model3 = verifica_final_do_jogo model1
      in
        model3


verifica_final_do_jogo : Model -> Model
verifica_final_do_jogo model =
  case model.meu_jogo of
    Nothing -> model
    Just jogo ->
      let
        empresas = conta_pecas_empresas jogo.board jogo.empresas
        empresas_com_limite_41_atingido = List.filter(\x -> x.pecas >= 41) empresas
        empresa_com_limite_12_atingido = List.filter(\x -> x.pecas >= 12) empresas
        empresa_no_tabuleiro = List.filter(\x -> x.disponivel == False) empresas
        fim_do_jogo = 
          case (List.length empresa_com_limite_12_atingido) == 1 && ((List.length empresa_no_tabuleiro) == 1)   of
            True -> True
            False ->
              if (List.length empresas_com_limite_41_atingido) > 0 then
                True
              else
                if (List.length jogo.pecas_disponiveis) == 0 then
                  True
                else
                  False
        model1 = 
          if fim_do_jogo then 
            let
              jogo1 = { jogo | fim_do_jogo = True, empresas = empresas}
              jogo2 = calcula_fim_do_jogo jogo1
            in
              {model | meu_jogo = Just jogo2 } 
          else
            model
      in
        model1


calcula_fim_do_jogo : Jogo -> Jogo
calcula_fim_do_jogo jogo =
  --- 1 - DAR BONUS A TODOS OS QUE FOREM DONOS DE EMPRESA
  --- 2 - VENDE TODAS AS AÇÕES
  --- 3 - ORDENA JOGADORES POR VALOR
  let
    -----1
    todos_os_bonus = List.map (\e ->
      pega_bonus_do_jogador_da_empresa e jogo
      ) jogo.empresas

    bonus2 =
      (List.map(\bonus ->
          case bonus.os_dois of
            Just bd ->
              let
                valor1 = bd.valor// (List.length bd.jogadores)
                os_dois = (List.map (\x ->
                    { info = "OS_DOIS" , valor = valor1 , jogador =  x.jogador, empresa = bonus.empresa}
                  ) bd.jogadores)
              in
                List.concat([[], os_dois])
            Nothing ->
              case bonus.primeiro of
                Nothing ->
                  []
                Just bp ->
                    let
                      valor1 = bp.valor// (List.length bp.jogadores)
                      primeiro = (List.map (\x ->
                          { info = "PRIMEIRO" , valor = valor1 , jogador =  x.jogador, empresa =  bonus.empresa}
                        ) bp.jogadores)
                      segundo = 
                        case bonus.segundo of
                          Nothing ->
                            []
                          Just bs ->
                            let
                              valor1 = bs.valor// (List.length bs.jogadores)
                            in
                              (List.map (\x ->
                                    { info = "SEGUNDO" , valor = valor1 , jogador =  x.jogador, empresa =  bonus.empresa}
                                  ) bs.jogadores)
                    in
                      List.concat([primeiro,segundo, []])
        ) todos_os_bonus) |> List.concat


    jogadores1 = Jogador.atualizar_bonus bonus2 jogo.jogadores


    

    ----- 2
    jogadores2 = List.map(\j -> 
        let
          din_de_acoes = (List.map(\a -> 
              let
                emp = ListX.find (\e -> a.empresa == e.eid) jogo.empresas
                a1 = case emp of
                      Nothing -> 0
                      Just e ->
                        let
                          acs = a.acoes*e.preco
                        in
                          acs

              in
                a1
            ) j.acoes) |> List.sum
          din = j.din
          din1 = din + din_de_acoes
        in
          {j | acoes = [], din = din1}

      ) jogadores1

    ---- 3
    jogadores3 = List.sortBy (\j -> j.din ) jogadores2
    turno = jogo.turno
    turno1 = {turno | bonus = bonus2}

  in
    {jogo | jogadores = jogadores3, turno = turno1 }