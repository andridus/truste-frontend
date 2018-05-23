module Utils.Jogador exposing (..)

import Model exposing (..)

import Lista
import List.Extra as ListX
import Debug

atualiza_acoes : Jogo -> Jogador -> Jogo
atualiza_acoes jogo jogador= 
  let
    jogo1 = let
              jogadores1 = List.map(\y -> 
                            case y.uid == jogador.uid of
                              True ->
                                {y | acoes = jogador.acoes}
                              False ->
                                y
                          ) jogo.jogadores
              x1 = {jogo | jogadores = jogadores1}
            in
              x1
  in
    jogo1

atualiza_nome : Jogo -> Jogador -> String -> Jogo
atualiza_nome jogo jogador nome= 
  let
    jogo1 = let
              jogadores1 = List.map(\y -> 
                            case y.uid == jogador.uid of
                              True ->
                                {y | nome = nome}
                              False ->
                                y
                          ) jogo.jogadores
              x1 = {jogo | jogadores = jogadores1}
            in
              x1
  in
    jogo1


atualiza_avatar : Jogo -> Jogador -> String -> Jogo
atualiza_avatar jogo jogador avatar= 
  let
    jogo1 = let
              jogadores1 = List.map(\y -> 
                            case y.uid == jogador.uid of
                              True ->
                                {y | avatar = avatar}
                              False ->
                                y
                          ) jogo.jogadores
              x1 = {jogo | jogadores = jogadores1}
            in
              x1
  in
    jogo1


estou_pronto : Jogo -> Jogador -> Jogo
estou_pronto jogo jogador= 
  let
    jogo1 = let
                jogadores1 = List.map(\y -> 
                              case y.uid == jogador.uid of
                                True ->
                                  {y | pronto = jogador.pronto}
                                False ->
                                  y
                            ) jogo.jogadores
                x1 = {jogo | jogadores = jogadores1}
              in
                x1
  in
    jogo1

todos_pronto : Jogo -> Maybe String -> Bool
todos_pronto jogo jid= 
  let
    jogadores1 =  let
                    jogadores = List.filterMap(\y -> 
                                  case y.pronto of
                                    True -> Nothing
                                    False -> Just False
                                ) jogo.jogadores
                  in
                   jogadores
  in
    case jogadores1 of
      [] -> True
      _ -> False


me : Jogador -> List Jogador -> Jogador
me me jogadores =
  let
    me1 = (List.filter (\j -> j.uid == me.uid ) jogadores)
          |> List.head 
  in
    case me1 of
      Nothing ->
        Debug.crash "Jogadro Não encontrado!"
      Just me2 -> 
        me2

pega_acoes: Jogador -> Empresa -> Int
pega_acoes me empresa =
  let
    acoes = List.filter (\x -> x.empresa == empresa.eid) me.acoes
            |> List.head
  in
    case acoes of
      Nothing -> 0
      Just a -> a.acoes

vender_acao: Jogador -> Empresa -> Jogador
vender_acao me empresa =
  let
    acoes = List.filter (\x -> x.empresa == empresa.eid) me.acoes
            |> List.head
    qtd_acao = case acoes of
              Nothing -> 0
              Just a -> a.acoes - 1
    acoes1 = List.map(\x -> 
        case x.empresa == empresa.eid of
          True -> 
            { x | acoes = qtd_acao}
          False ->
            x 
      ) me.acoes
    din = me.din + empresa.preco
    me1 = { me | acoes = acoes1, din = din}
  in
    me1



comprar_acao: Jogador -> Empresa -> Jogador
comprar_acao me empresa =
  let
    acoes = List.filter (\x -> x.empresa == empresa.eid) me.acoes
            |> List.head
    acoes1 = case acoes of
      Nothing ->
        { empresa= empresa.eid, acoes= 1} :: me.acoes
      Just ac ->
        List.map(\x -> 
        case x.empresa == ac.empresa of
            True -> 
              { x | acoes = ac.acoes +1}
            False ->
              x 
        ) me.acoes
    din = me.din - empresa.preco
    me1 = { me | acoes = acoes1, din = din}
  in
    me1


proximo_jogador : List Jogador -> Maybe Jogador
proximo_jogador jogadores =
  let
    prox1 = (List.filter (\j -> j.vez == True ) jogadores)
          |> List.head 
  in
    prox1
organiza_vez : List Jogador -> List Jogador
organiza_vez jogadores =
  let 
    jogadores1 = Lista.shuffle jogadores
    jogadores2 = List.indexedMap (\i p ->
        { p | ordem = (i+1)}
      ) jogadores1

  in
    jogadores2

atualiza_me : Jogador -> List Jogador -> List Jogador
atualiza_me me jogadores=
  let
    jogadores1 = List.map(\y -> 
                      case y.uid == me.uid of
                        True ->
                          me
                        False ->
                          y
                    ) jogadores
    
  in
    jogadores1


atualizar_bonus : List BonusTurno -> List Jogador -> List Jogador
atualizar_bonus bonus jogadores=
  let
    jogadores1 = List.map(\y -> 
                    let 
                      f = ListX.find (\j -> j.jogador == y.uid) bonus
                    in
                      case f of
                        Just f1 ->
                          let
                            din1 = y.din + f1.valor
                          in
                            {y | din = din1}
                        Nothing ->
                          y
                    ) jogadores
    
  in
    jogadores1

atualiza_minhas_pecas : Jogador -> List Jogador -> List Jogador
atualiza_minhas_pecas me jogadores=
  let
    jogadores1 = List.map(\y -> 
                      case y.uid == me.uid of
                        True ->
                          {y | pecas = me.pecas}
                        False ->
                          y
                    ) jogadores
    
  in
    jogadores1
    


pega_peca : Int -> List Peca -> ( List Peca, List Peca)
pega_peca num pecas_disponiveis =
  let
    (pecas, restante) = Lista.getElements num pecas_disponiveis

  in
    (pecas, restante)


pega_pecas_de_jogadores : List Jogador -> List Peca -> List Jogador -> ( List Jogador , List Peca)
pega_pecas_de_jogadores jogadores1 pecas_disponiveis jogadores=
  case List.head jogadores of
    Nothing -> (jogadores1, pecas_disponiveis)
    Just j ->  
      let
        (pecas, pecas_restante) = pega_peca 6 pecas_disponiveis
        j1 = { j | pecas = pecas }
        jogadores2 = j1 :: jogadores1
        tail = case (List.tail jogadores) of
                  Nothing -> []
                  Just t -> t
      in
        pega_pecas_de_jogadores jogadores2 pecas_restante tail
