module Plugs.Dialogo.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


import Model exposing (..)
import MainModel exposing (..)
import Plugs.Dialogo.Msg exposing (..)
import Debug
import Utils.Fn as Fn
import Utils.Jogador as Jogador
import List.Extra as ListX
--TIPOs
-- SELECIONA_IMAGEM_USUARIO

view: Model -> Html Msg
view model =
  let 
    dialogos = List.filter (\x -> x.jogador == model.me.uid || x.jogador == "" ) model.dialogos
  in
    div []
        (List.map (render model) dialogos)


render: Model -> Dialogo -> Html Msg
render model dialogo =
  let
    error1 = case dialogo.tipo of
            "ERROR" -> style [("z-index","15002")]
            "INFO" -> style [("z-index","15001")]
            _ -> style []
  in
      
    div 
      [ class "dialogo-bg-all", error1]
      [
        if dialogo.minimizado then
          minimizado model dialogo
        else
          
              case dialogo.tipo of
                "ERROR" ->
                  error model dialogo
                "INFO" ->
                  info model dialogo
                "PROCESSO" ->
                  processo model dialogo
                "JOGADA_ENCERRADA" ->
                  jogada_encerrada model dialogo
                "QUESTAO" ->
                  questao model dialogo
                "ENCERRAR_JOGO" ->
                  encerrar_jogo model dialogo
                "SEM_VEZ" ->
                  sem_vez model dialogo
                "SELECIONA_IMAGEM_USUARIO" ->
                  seleciona_imagem_usuario model dialogo
                "STATUS_EMPRESA" ->
                  status_empresa model dialogo
                "NOVA_EMPRESA" ->
                  nova_empresa model dialogo
                "FUNDE_EMPRESA" ->
                  funde_empresa model dialogo
                "VENDER_ACOES_EMPRESA_FUNDIDA" ->
                  vender_acoes_empresa_fundida model dialogo
                _ ->
                  div 
                    [class "dialogo"]
                    [
                      div 
                        [ class "dialogo-titulo"]
                        [ text dialogo.titulo]
                    , div
                        [ class "dialogo-conteudo"]
                        [ text dialogo.conteudo]
                    
                    ]
      ]



minimizado : Model -> Dialogo -> Html Msg
minimizado model dialogo =
  div 
    [class "dialogo-minimizado", onClick (MaximizarDiag dialogo)]
    [
      div 
        [ class "dialogo-titulo"]
        [ (case dialogo.tipo of
            "NOVA_EMPRESA" -> "Nova Empresa"
            "FUNDE_EMPRESA" -> "Fusão de Empresas"
            "STATUS_EMPRESA" -> "Ver Empresa"
            "VENDER_ACOES_EMPRESA_FUNDIDA" -> "Vender Ações"
            _ -> "Diálogo"

          )|> text

        ]
    
    ]
vender_acoes_empresa_fundida : Model -> Dialogo -> Html Msg
vender_acoes_empresa_fundida model dialogo =
   case model.meu_jogo of
          Nothing ->
            error model dialogo
          Just jogo ->
            let
              windowHeight = (toString (model.tela.viewportHeight-50)) ++ "px"
              windowWidth = case model.tela.viewportWidth < 400 of
                              True ->
                                 "100%"
                              False ->
                                "420px"

              empresa = List.filter(\e ->
                  e.eid == dialogo.titulo
                ) jogo.empresas
            in
              case List.head empresa of
                Nothing ->
                  error model dialogo
                Just emp ->
                  let
                    me1 = Jogador.me model.me jogo.jogadores
                    acoes = Jogador.pega_acoes me1 emp
                    acoes_vendidas = List.filter(\x ->
                       x.empresa == emp.eid && x.jogador == model.me.uid
                      ) jogo.turno.acoes_vendidas_fusao
                  in
                    div 
                      [ class "dialogo" 
                      , style [ ("height", windowHeight)
                              , ("width", windowWidth)
                              , ("margin-top","50px")

                              ]
                      ]
                      [
                        div
                          [ class "dialogo-con", style [("width", windowWidth)]]
                          [ div 
                              [ class "dialogo-titulo"]
                              [ text ("Vender ações da  "++emp.nome++" ")
                              , a [onClick (MinimizarDiag dialogo)] [text "minimizar"]
                              ]
                          , hr [] []
                          , div
                              [ class "dialogo-conteudo"
                              , style [
                                        ("text-align","center")
                                      , ("height",(toString (model.tela.viewportHeight-140)) ++ "px")]
                              
                              ]
                              [
                                p [] [ text "Essa empresa entrou em falência. Aqui você pode vender as ações que você ainda tem dessa empresa. "]
                              , p [] [ text "Lembre-se que na próxima rodada é possível que essa empresa não tenha mais valor."]
                              , img [src emp.icon, style [("width","80px")] ] []
                              , div []
                                    [b [] [text emp.nome] ]
                              , div [] 
                                    [ small [] [text "Suas ações: "]
                                    , br [] []
                                    , text (toString acoes) 
                                    ]
                              , div [] 
                                    [ small [] [text "Preço atual das ações: "]
                                    , br [] []
                                    , text ("R$ "++(toString emp.preco) ++ ",00")
                                    ]
                              , div [] 
                                    [ small [] [text "Preço obtido pela venda das ações: "]
                                    , br [] []
                                    , text ("R$ "++(toString (emp.preco*(List.length acoes_vendidas ))) ++ ",00")
                                    ]
                              , div
                                [ class "buttons"]
                                [
                                  if acoes > 0 then
                                    button 
                                      [class "button is-dark", onClick (VendeAcaoFusao emp me1 jogo)]
                                      [ text "Vender 1 Ação"]
                                  else
                                    button 
                                      [class "button is-dark is-disabled"]
                                      [ text "Vender 1 Ação"]
                                    
                                , div 
                                    [class "button is-warning", onClick (ConfirmaVendaAcaoFusao dialogo)]
                                    [ text "Aplicar"]

                                ]
                              , div []
                                  (List.map (\x ->
                                    div [ class "acoezinhas1", style [("background", emp.cor)], onClick (CompraAcaoFusao emp me1 jogo)]
                                      [ img [ src emp.icon] [] ]
                                    ) acoes_vendidas)
                              ]
                          
                               
                          ]
                      , div 
                          [ class "dialogo-bg"
                          , style 
                            [ ("width", "100%") 
                            , ("height", "100%")
                            , ("background","#FFFFFFFF")
                            , ("border", "5px solid "++emp.cor)
                            ]

                          ]
                          []

                      ]

status_empresa : Model -> Dialogo -> Html Msg
status_empresa model dialogo =
   case model.meu_jogo of
          Nothing ->
            error model dialogo
          Just jogo ->
            let
              windowHeight = if model.tela.viewportWidth < 500 then
                              (toString (model.tela.viewportHeight-100)) ++ "px"
                             else
                              (toString (model.tela.viewportHeight-50)) ++ "px"
              marginTop = if model.tela.viewportWidth < 500 then
                            "100px"
                          else
                            "50px"
              windowWidth = case model.tela.viewportWidth < 400 of
                              True ->
                                 "100%"
                              False ->
                                "420px"

              empresa = List.filter(\e ->
                  e.eid == dialogo.titulo
                ) jogo.empresas
            in
              case List.head empresa of
                Nothing ->
                  error model dialogo
                Just emp ->
                  let
                    me1 = Jogador.me model.me jogo.jogadores
                    acoes = Jogador.pega_acoes me1 emp
                  in
                    div 
                      [ class "dialogo" 
                      , style [ ("height", windowHeight)
                              , ("width", windowWidth)
                              , ("margin-top",marginTop)

                              ]
                      ]
                      [
                        div
                          [ class "dialogo-con", style [("width", windowWidth)]]
                          [ div 
                              [ class "dialogo-titulo"]
                              [ text ("Ver empresa "++emp.nome++" ")
                              , a [onClick (MinimizarDiag dialogo)] [text "minimizar"]
                              ]
                          , hr [] []
                          , div
                              [ class "dialogo-conteudo"
                              , style [
                                        ("text-align","center")
                                      , ("height",(toString (model.tela.viewportHeight-140)) ++ "px")]
                              
                              ]
                              [
                                img [src emp.icon, style [("width","80px")] ] []
                              , div []
                                    [b [] [text emp.nome] ]
                              , div [] 
                                    [ small [] [text "Suas ações: "]
                                    , br [] []
                                    , text (toString acoes) 
                                    ]
                              , div [] 
                                    [ small [] [text "Preço atual das ações: "]
                                    , br [] []
                                    , text ("R$ "++(toString emp.preco) ++ ",00")
                                    ]
                              , 
                                case jogo.turno.peca of
                                  Nothing ->
                                    div [] [ 
                                      case (not emp.disponivel) of
                                        True -> div [] [ text "Você só pode fazer alguma transação com a empresa quando você posicionar sua peça"]
                                        False -> div [] []

                                    ]
                                  Just p ->  
                                    div [ class "buttons" ] [
                                     case (not emp.disponivel) && me1.vez of
                                        True ->
                                          if acoes > 0 then
                                            div 
                                              [class "button is-dark", onClick (VendeAcao emp jogo)]
                                              [ text "Vender 1 Ação"]
                                          else
                                            div [] []
                                        False ->
                                          div [] []
                                    , case (not emp.disponivel) && me1.vez && emp.limite >0 of
                                        True ->
                                          div 
                                            [class "button is-success", onClick (CompraAcao emp jogo)]
                                            [ text "Comprar 1 Ação"]
                                        False ->
                                          div [] []

                                    ]
                              , div
                                [ class "buttons"]
                                [
                                 div 
                                    [class "button is-warning", onClick (CancelarDiag dialogo)]
                                    [ text "Fechar"]

                                ]
                              , div []
                                    [
                                      table [ class "table", style [("width","100%"),("padding-bottom","25px")]] 
                                            [ thead []
                                                    [ tr [] 
                                                         [ th [] [ text "Nome"]
                                                         , th [] [ text "Acões"]
                                                         ]
                                                    ]
                                            , tbody []
                                                    (List.map(\x -> 
                                                      let
                                                        nome = case x.nome of
                                                                "" -> x.uid
                                                                n -> n

                                                        ac1 = case (List.filter(\x -> x.empresa == emp.eid) x.acoes) |> List.head of
                                                                Nothing -> 0
                                                                Just a -> a.acoes
                                                          
                                                      in
                                                        tr []
                                                           [ td [] [text nome]
                                                           , td [] [text (toString ac1)]
                                                           ]

                                                    ) jogo.jogadores)
                                            ]

                                    ]

                              ]
                          
                               
                          ]
                      , div 
                          [ class "dialogo-bg"
                          , style 
                            [ ("width", "100%") 
                            , ("height", "100%")
                            , ("background","#FFFFFFFF")
                            , ("border", "5px solid "++emp.cor)
                            ]

                          ]
                          []

                      ]

nova_empresa : Model -> Dialogo -> Html Msg
nova_empresa model dialogo =
   case model.meu_jogo of
          Nothing ->
            error model dialogo
          Just jogo ->
            let
              windowHeight = (toString (model.tela.viewportHeight)) ++ "px"
              windowWidth = case model.tela.viewportWidth < 400 of
                              True ->
                                 "100%"
                              False ->
                                "420px"

              empresas = List.filter(\e ->
                  e.disponivel
                ) jogo.empresas
            in
              div 
                [ class "dialogo" 
                , style [ ("height", windowHeight)
                        , ("width", windowWidth)
                        ]
                ]
                [
                  
                  div
                    [ class "dialogo-con"]
                    [ div 
                        [ class "dialogo-titulo"]
                        [ text "Escolha uma empresa para fundar"
                        , a [onClick (MinimizarDiag dialogo)] [text "minimizar"]
                        ]
                    , div
                        [ class "dialogo-conteudo"
                        , style [("height",(toString (model.tela.viewportHeight-50)) ++ "px")]
                        ]
                        [
                          p [] [text "Parabéns, você já pode fundar uma nova empresa, para isso, clique na empresa que você deseja fundar."]
                        , div []
                          (List.map (\x -> 
                            div 
                              [class "empresa-choose", onClick (SelecionaNovaEmpresa dialogo x)]
                              [ img [ src x.icon] []
                              , span [] [text x.nome]
                              , div [ class "empresa-choose-peso"] 
                                [ 
                                  case x.peso of
                                    1 -> text "$"
                                    2 -> text "$$"
                                    3 -> text "$$$"
                                    _ -> text ""
                                ]
                              ]
                          ) empresas)
                        , div [ class "is-clearfix"] []
                        ]
                        
                    
                    ]
                , div 
                    [ class "dialogo-bg"
                    , style 
                      [ ("width", "100%") 
                      , ("height", "100%")
                      , ("background","#FFFFFFFF")
                      ]

                    ]
                    []
                ]
funde_empresa : Model -> Dialogo -> Html Msg
funde_empresa model dialogo =
   case model.meu_jogo of
          Nothing ->
            error model dialogo
          Just jogo ->
            let

              windowHeight = (toString (model.tela.viewportHeight)) ++ "px"
              windowWidth = case model.tela.viewportWidth < 400 of
                              True ->
                                 "100%"
                              False ->
                                "420px"

              empresas =  
                case jogo.turno.atualizar_pecas of
                  Nothing -> []
                  Just ap ->
                    let
                      empresas1 = List.map(\p ->
                                case (ListX.find (\em -> Just em.eid == p.empresa) jogo.empresas) of
                                  Nothing -> []
                                  Just emp -> [emp]
                                 ) ap.pecas
                      empresas2 = List.concat empresas1
                      empresas3 = ListX.uniqueBy (\x -> x.eid) empresas2

                    in
                      empresas3
              (majoritarias, falencia) = Fn.empresas_majo empresas

              bonus = 
                (List.map (\x->
                  Fn.pega_bonus_do_jogador_da_empresa x jogo
                ) falencia)
              _ = Debug.log "bonus" bonus
                -- {primeiro: List Jogador, segundo: List Jogador, os_dois: List Jogador}
              
            in
              div 
                [ class "dialogo" 
                , style [ ("height", windowHeight)
                        , ("width", windowWidth)
                        ]
                ]
                [
                  div
                    [ class "dialogo-con"]
                    [ div 
                        [ class "dialogo-titulo"]
                        [ text "FUSÃO DE EMPRESAS"
                        , a [onClick (MinimizarDiag dialogo)] [text "minimizar"]
                        ]
                    , hr [] []
                    , div
                        [ class "dialogo-conteudo"
                        , style [("height",(toString (model.tela.viewportHeight-75)++"px"))]
                        ]
                        [
                        case List.length majoritarias of
                          1 ->
                            render_opt_funde_empresa model jogo dialogo majoritarias falencia empresas bonus
                          _ ->
                            case jogo.turno.atualizar_pecas of
                              Nothing ->
                                render_opt_funde_empresa_escolher model dialogo majoritarias falencia empresas jogo
                              Just ac ->
                                case ac.empresa of
                                  Nothing ->
                                    render_opt_funde_empresa_escolher model dialogo majoritarias falencia empresas jogo
                                  Just emp ->
                                    let
                                      majoritaria1 = List.filter(\x -> x.eid == emp.eid) empresas
                                      falencia1 = List.filter(\x -> x.eid /= emp.eid) empresas
                                      bonus1 = 
                                        (List.map (\x->
                                          Fn.pega_bonus_do_jogador_da_empresa x jogo
                                        ) falencia1)
                                    in
                                      render_opt_funde_empresa model jogo dialogo majoritaria1 falencia1 empresas bonus1
                            
                        ]
                        
                         
                    ]
                , div 
                    [ class "dialogo-bg"
                    , style 
                      [ ("width", "100%") 
                      , ("height", "100%")
                      , ("background","#FFFFFFFF")
                      ]

                    ]
                    []
                ]
render_opt_funde_empresa_escolher : Model -> Dialogo -> List Empresa -> List Empresa -> List Empresa -> Jogo -> Html Msg
render_opt_funde_empresa_escolher model dialogo majoritarias falencia empresas jogo =
  let
    
    empresas_nome = (List.map (\x-> 
                               span [style [("margin-right","10px")]]
                                [
                                 img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                                , text  x.nome 
                                ]
                              ) empresas)
    majoritarias_nome = (List.map (\x-> 
                        span [style [("margin-right","10px")]]
                          [
                           img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                          , text  x.nome 
                          ]
                    ) majoritarias)
    falencia_nome = (List.map (\x-> 
                        span [style [("margin-right","10px")]]
                          [
                           img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                          , text  x.nome 
                          ]
                    ) falencia)
    peca = case jogo.turno.peca of
            Nothing -> "N/A"
            Just p -> p.linha ++ (toString p.coluna)
                    --|> List.intersperse " e "
  in

    div []
      [
        p [ style [("text-align","center")]] 
          [ text ("A peça " ++ peca ++" fez uma fusão nas empresas ")
          , div [] 
              (List.map (\x-> 
                  x
                ) empresas_nome)
          ]
      , hr [] []
      , p [ style [("text-align","center")]] 
          [ text "As empresas  "
          , div [] 
              (List.map (\x-> 
                  x
                ) majoritarias_nome)
          , if model.me.uid == jogo.turno.jogador then
              text " tiveram a mesma quantidade de peças e por isso você precisa decidir qual a empresa que deve continuar exisitindo "
            else
              text " tiveram a mesma quantidade de peças e por isso o jogador da vez precisa decidir qual a empresa que deve continuar exisitindo."
          ]
      , if model.me.uid == jogo.turno.jogador then
          div []
          (List.map (\x -> 
            div 
              [class "empresa-choose", onClick (SelecionaEmpresaParaFusao dialogo x jogo falencia)]
              [ img [ src x.icon] []
              , span [] [text x.nome]
              , div [ class "empresa-choose-peso"] 
                [ 
                ]
              ]
          ) majoritarias)
        else
          div
              [ class "buttons"]
              [ div 
                  [class "button is-light", onClick (CancelarDiag dialogo)]
                  [ text "Fechar"]

              ]
          
      
      ] 

render_opt_funde_empresa: Model -> Jogo -> Dialogo -> List Empresa -> List Empresa -> List Empresa -> List BonusAcionista -> Html Msg
render_opt_funde_empresa model jogo dialogo majoritarias falencia empresas bonus =
  let
    
    empresas_nome = (List.map (\x-> 
                               span [style [("margin-right","10px")]]
                                [
                                 img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                                , text  x.nome 
                                ]
                              ) empresas)
    majoritarias_nome = (List.map (\x-> 
                        span [style [("margin-right","10px")]]
                          [
                           img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                          , text  x.nome 
                          ]
                    ) majoritarias)
    falencia_nome = (List.map (\x-> 
                        span [style [("margin-right","10px")]]
                          [
                           img [ src x.icon, style [("width", "20px"),("margin-right","5px")]] []
                          , text  x.nome 
                          ]
                    ) falencia)
    peca = case jogo.turno.peca of
            Nothing -> "N/A"
            Just p -> p.linha ++ (toString p.coluna)

    _ = Debug.log "bonus1" bonus
    bonus1 = bonus
  in
   case (List.length falencia_nome) of
      1 -> -------------------PARA UMA EMPRESA A FALÊNCIA
        case List.head falencia of
          Nothing -> div [] [text "Ocorreu algum erro. Não foi encontrado empresa pra falência"]
          Just fal ->
            let
              _ = Debug.log "bonus2" bonus1
            in
              div []
                [
                p [ style [("text-align","center")]] 
                  [ text ("A peça " ++ peca ++" fez uma fusão na empresa ")
                  , b [] 
                      (List.map (\x-> 
                          x
                      ) falencia_nome)
                  , text "com a empresa "
                  , case List.head majoritarias of
                      Nothing -> text ""
                      Just emp -> 
                        b [] 
                          [ 
                            span []
                            [
                             img [ src emp.icon, style [("width", "20px"),("margin-right","5px")]] []
                            , text(emp.nome ++ ".")
                            ]
                        
                        ]
                  ]
                , render_falencia_bonus fal majoritarias falencia_nome bonus1
                , case List.head majoritarias of
                    Nothing ->
                      div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]
                    Just emp -> 
                      if model.me.uid == jogo.turno.jogador then
                        div
                          [ class "buttons"]
                          [ div 
                              [class "button is-light",onClick (SelecionaFusaoEmpresa dialogo emp empresas bonus1 falencia)]
                              [ text "Fechar"]

                          ] 
                      else
                        div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]
                ]
            
      _ ->  ------------------PARA VÁRIAS EMPRESAS A FALÊNCIA
        if (List.length falencia) == 0 then
            let
              bonus = (List.filterMap (\x -> 
                        if x.jogador == model.me.uid then
                          Just x.valor
                        else
                          Nothing
                      ) jogo.turno.bonus) |> List.foldr (+) 0
            in
              div []
                [ p [ style [("text-align","center")]] 
                  [ text ("A peça " ++ peca ++" fez uma fusão com a empresa ")
                  , case List.head majoritarias of
                      Nothing -> text ""
                      Just emp -> 
                        b [] 
                          [ 
                            div []
                            [
                             img [ src emp.icon, style [("width", "20px"),("margin-right","5px")]] []
                            , text(emp.nome ++ ".")
                            ]
                        
                        ]
                  , text (" E você recebeu um bonus de R$ " ++ (toString bonus) ++ " por essa fusão")
                  ]
                , case List.head majoritarias of
                    Nothing ->
                      div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]
                    Just emp -> 
                      if model.me.uid == jogo.turno.jogador then
                        div
                          [ class "buttons"]
                          [ div 
                              [class "button is-light",onClick (SelecionaFusaoEmpresa dialogo emp empresas bonus1 falencia)]
                              [ text "Fechar"]

                          ] 
                      else
                        div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]
                ]
        else
          let
            bonus1 = bonus
          in
            div []
              [ p [ style [("text-align","center")]] 
                [ text "A peça que você colocou fez uma fusão nas empresas "
                , b [] 
                    (List.map (\x-> 
                        x
                    ) falencia_nome)
                , text "com a empresa "
                , case List.head majoritarias of
                    Nothing -> text ""
                    Just emp -> 
                      b [] 
                        [ 
                          div []
                          [
                           img [ src emp.icon, style [("width", "20px"),("margin-right","5px")]] []
                          , text(emp.nome ++ ".")
                          ]
                      
                      ]
                ]
              , hr [] []
              ,  p [ style [("text-align","center")]] 
                  [ b [] [text "As seguintes empresas quebraram: "]
                  , hr [] []
                  , div [] 
                      (List.map (\x-> 
                        div []
                          [ render_falencia_bonus x majoritarias falencia_nome bonus1
                          , hr [] []
                          ]
                      ) falencia)
                  ]
              , case List.head majoritarias of
                    Nothing ->
                      div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]
                    Just emp -> 
                      if model.me.uid == jogo.turno.jogador then
                        div
                          [ class "buttons"]
                          [ div 
                              [class "button is-light",onClick (SelecionaFusaoEmpresa dialogo emp empresas bonus falencia)]
                              [ text "Fechar"]

                          ] 
                      else
                        div
                        [ class "buttons"]
                        [ div 
                            [class "button is-light", onClick (CancelarDiag dialogo)]
                            [ text "Fechar"]

                        ]

              ]

render_falencia_bonus : Empresa -> List Empresa -> List (Html Msg) -> List BonusAcionista -> Html Msg   
render_falencia_bonus fal majoritarias falencia_nome bonus = 
  let
    _ = Debug.log "AQUI" bonus
    bonus1 = List.head (List.filter(\x -> x.empresa == fal.eid ) bonus)

  in
    div []
      [ p [ style [("text-align","center")]] 
          [ text "Isso levou a empresa "
          , b [] 
              [
              span [style [("margin-right","10px")]]
              [
               img [ src fal.icon, style [("width", "20px"),("margin-right","5px")]] []
              , text  fal.nome 
              ]
              ]
          , text "a falência. "
          ]
      , case bonus1 of
          Nothing ->
            p []
              [ text ("Não foi gerado nenhum bônus porque não há acionistas nessa empresa")
              ]
          Just bn ->
            case bn.os_dois of
              Just bd ->
                let
                  valor1 = (toFloat bd.valor)/(toFloat (List.length bd.jogadores))
                in
                p []
                  (List.map (\x ->
                    span []
                    [ text "E gerou bônus de "
                    , b [style [("color","green")]] [text ("R$ " ++ (toString valor1)++",00")]
                    , text " para o acionista majoritário "
                    , b [] [ text x.nome ]
                    ]
                  ) bd.jogadores)
              Nothing ->
                case bn.primeiro of
                  Nothing ->
                    p []
                    [ text ("Não foi gerado nenhum bônus para o acionista majoritário da empresa (error)")
                    ]
                  Just bp ->
                      let
                        valor1 = (toFloat bp.valor)/(toFloat (List.length bp.jogadores))
                      in
                       div []
                       [ p []
                          (List.map (\x ->
                            span []
                            [ text "E gerou bônus de R$ "
                            , b [style [("color","green")]] [text ("R$ " ++ (toString valor1)++",00")]
                            , text " para o acionista majoritário "
                            , b [] [ text x.nome ]
                            ]
                          ) bp.jogadores)
                       , case bn.segundo of
                          Nothing ->
                            p []
                            [ text ("Não foi gerado nenhum bônus para o segundo maior acionista da empresa")
                            ]
                          Just bs ->
                           let
                              valor1 = (toFloat bs.valor)/(toFloat (List.length bs.jogadores))
                           in
                             p []
                              (List.map (\x ->
                                span []
                                [ text "E gerou bônus de R$ "
                                , b [style [("color","green")]] [text ("R$ " ++ (toString valor1)++",00")]
                                , text " para o segundo maior acionista  "
                                , b [] [ text x.nome ]
                                ]
                              ) bs.jogadores)
                       ]

      ]
        
seleciona_imagem_usuario : Model -> Dialogo -> Html Msg
seleciona_imagem_usuario model dialogo =
  let
    windowHeight = (toString (model.tela.viewportHeight)) ++ "px"
    windowWidth = case model.tela.viewportWidth < 400 of
                    True ->
                       "100%"
                    False ->
                      "400px"

    d = (Debug.log "asas" windowWidth)
    avatares = [
      {url = "/imgs/players/player1.png"}
    , {url = "/imgs/players/player2.png"}
    , {url = "/imgs/players/player3.png"}
    , {url = "/imgs/players/player4.png"}
    , {url = "/imgs/players/player5.png"}
    , {url = "/imgs/players/player6.png"}
    , {url = "/imgs/players/player7.png"}
    , {url = "/imgs/players/player8.png"}
    , {url = "/imgs/players/player9.png"}
    , {url = "/imgs/players/player10.png"}
    , {url = "/imgs/players/player11.png"}
    , {url = "/imgs/players/player12.png"}
    , {url = "/imgs/players/player13.png"}
    , {url = "/imgs/players/player14.png"}
    , {url = "/imgs/players/player15.png"}
    , {url = "/imgs/players/player16.png"}

    ]

  in
    div 
      [ class "dialogo" 
      , style [ ("height", windowHeight)
              , ("width", windowWidth)
              ]
      ]
      [
        div
          [ class "dialogo-con"]
          [ div 
              [ class "dialogo-titulo"]
              [ text "Escolha uma avatar"]
          , div
              [ class "dialogo-conteudo"
              , style [("height",(toString (model.tela.viewportHeight-50)) ++ "px")]
              ]
               (List.map (\x -> 
                  img [class "avatar-choose", src x.url, onClick (SelecionaAvatar dialogo x.url) ] []
                ) avatares)
          ]
      , div 
          [ class "dialogo-bg"
          , style 
            [ ("width", "100%") 
            , ("height", "100%")
            , ("background","#FFFFFFFF")
            ]

          ]
          []
    ]
sem_vez : Model -> Dialogo -> Html Msg
sem_vez model dialogo = 
  let
    windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
    windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
  in
    div 
      [ class "dialogo" 
      , style [ ("height", "100%")
              , ("width", "100%")
              ]
      ]
      [
        div
          [ class "dialogo-con"
          , style [
                ("text-align", "center")
              , ("height", "100%")
              , ("width", "100%")


              ]
          ]
          [
            div 
              [ class "dialogo-titulo"]
              [ text dialogo.titulo]
          , div
              [ class "dialogo-conteudo"
              , style 
                [ ("height", windowHeight)
                , ("width", windowWidth)
                , ("vertical-align","middle")
                , ("display", "table-cell")

                ]
              ]
              [ div [ style [("font-size","35px")]]
                    [ text dialogo.conteudo ]
              ]
          , div
              [ class "buttons"]
              [ div 
                  [class "button is-light", onClick (CancelarDiag dialogo)]
                  [ text "Fechar"]

              ]
          ]
        
      , div 
          [ class "dialogo-bg"
          , style 
            [ ("width","100%") 
            , ("height","100%")
            , ("background","#FFC2C2AA")
            ]

          ]
          []
      ]

error : Model -> Dialogo -> Html Msg
error model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#FFF")
                  , ("text-shadow","0px 0px 1px #000")
                  , ("height", "100%")
                  , ("width", "100%")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")

                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  ]
              , div
                  [ class "buttons"]
                  [ div 
                      [class "button is-light", onClick (CancelarDiag dialogo)]
                      [ text "Fechar"]

                  ]
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#FF0000BB")
                ]

              ]
              []
          ]
info : Model -> Dialogo -> Html Msg
info model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#FFF")
                  , ("text-shadow","0px 0px 1px #000")
                  , ("height", "100%")
                  , ("width", "100%")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")

                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  ]
              , div
                  [ class "buttons"]
                  [ div 
                      [class "button is-light", onClick (CancelarDiag dialogo)]
                      [ text "Fechar"]

                  ]
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#7DEAFAAA")
                ]

              ]
              []
          ]
jogada_encerrada : Model -> Dialogo -> Html Msg
jogada_encerrada model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#000")
                  , ("height", "100%")
                  , ("width", "100%")
                  , ("text-shadow","0px 0px 1px #000")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")


                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  , p []
                    [text "Agora é a vez do outro jogador"]
                  ]
              , div
                  [ class "buttons"]
                  [  div 
                      [class "button is-black", onClick (CancelarDiag dialogo)]
                      [ text "Certo"]

                  ]
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#1A9BE555")
                ]

              ]
              []
          ]
questao : Model -> Dialogo -> Html Msg
questao model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#000")
                  , ("height", "100%")
                  , ("width", "100%")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")

                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  ]
              , div
                  [ class "buttons"]
                  [ div 
                      [class "button is-light", onClick (FinalizarJogada dialogo)]
                      [ text "Sim, quero finalizar minha jogada"]
                  , br [] []
                  , div 
                      [class "button is-warning", onClick (CancelarDiag dialogo)]
                      [ text "Cancelar"]

                  ]
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#8DFCD2CC")
                ]

              ]
              []
          ]
processo : Model -> Dialogo -> Html Msg
processo model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#000")
                  , ("height", "100%")
                  , ("width", "100%")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")

                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  ]
             
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#56D0F2")
                ]

              ]
              []
          ]
encerrar_jogo : Model -> Dialogo -> Html Msg
encerrar_jogo model dialogo =
  let
        windowHeight = (toString (model.tela.viewportHeight-300)) ++ "px"
        windowWidth = (toString (model.tela.viewportWidth)) ++ "px"
      in
        div 
          [ class "dialogo" 
          , style [ ("height", "100%")
                  , ("width", "100%")
                  ]
          ]
          [
            div
              [ class "dialogo-con"
              , style [
                    ("text-align", "center")
                  , ("color","#000")
                  , ("height", "100%")
                  , ("width", "100%")


                  ]
              ]
              [
                div 
                  [ class "dialogo-titulo"]
                  [ text dialogo.titulo]
              , div
                  [ class "dialogo-conteudo"
                  , style 
                    [ ("height", windowHeight)
                    , ("width", windowWidth)
                    , ("vertical-align","middle")
                    , ("display", "table-cell")

                    ]
                  ]
                  [ div [ style [("font-size","30px")]]
                        [ text dialogo.conteudo ]
                  ]
              , div
                  [ class "buttons"]
                  [ div 
                      [class "button is-danger is-large", onClick (EncerrarJogo dialogo)]
                      [ text "ENCERRAR JOGO"]
                  , br [] []
                  , div 
                      [class "button is-warning", onClick (CancelarDiag dialogo)]
                      [ text "Cancelar"]

                  ]
              ]
            
          , div 
              [ class "dialogo-bg"
              , style 
                [ ("width","100%") 
                , ("height","100%")

                , ("background","#FF6060EE")
                ]

              ]
              []
          ]