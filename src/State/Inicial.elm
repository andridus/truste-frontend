module State.Inicial exposing (..)

import Model exposing (..)
import MainModel exposing (..)
import Msg exposing (..)

import Array
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Utils.Jogador as Jogador
import State.Jogos as Jogos

import State.Jogo.View as JogoView

error : Model -> Html Msg
error model = 
  div 
    [ id "main-cartel" ]
    [ div 
      [ class "is-fullheight cartel"]
      [
        div [ class "hero-body" ]
        [
          div 
          [ class "container has-text-centered" ]
          [
            div 
            [ class "column"]
            [
              h3 
              [ class "title"] 
              [ text "Truste"]
            , p 
              [ class "subtitle has-text-grey"] 
              [ text "Controle a concorrência"]
            , h3 
              [ class "has-text-black"] 
              [ text "AGUARDE..."]
            , h4 
              []
              [ text "Você não está conectado ao servidor. Aguarde alguns instantes e tente novamente apertando F5 do seu navegador"]



            ]
          ]
        ]
      ]
    ]
    

render : Model -> Html Msg
render model =
  div 
    [ id "main-cartel" ]
    [ 
      div [ style [("width","100%")
                  ,("background","#FFF")
                  ,("text-align","center")
                  ,("padding-top","50px")
                  ,("padding-bottom","10px")
                  ] ]
        [
          h3 
          [ class "title"] 
          [ text "Truste"]
        , p 
          [ class "subtitle has-text-grey"] 
          [ text "Controle a concorrência"]
        ]
    , div 
      [ class "is-fullheight cartel"]
      [
        div [ class "hero-body" ]
        [
          div 
          [ class "container has-text-centered" ]
          [
            div 
            [ class "column"]
            [

             h3 
              [ class "has-text-white"] 
              [ text "Jogos"]
            , div 
              [ class "button"
              , (style [("margin","20px")])
              , onClick AbreNovoJogo]
              [
                img [ src "/icos/267-plus.svg"] []
              , div [ style [("margin-left","10px")]] []
              , text "Novo Jogo"
              ]
            , Jogos.render model model.jogos 
            ]
          ]
        ]
      ]
    ]
fim_do_jogo : Model -> Jogo -> Html Msg
fim_do_jogo model jogo =
  div 
    [ id "main-cartel" ]
    [ div 
      [ class "is-fullheight cartel"]
      [
        div [ class "hero-body" ]
        [
          div 
            [ class "container has-text-centered" ]
            [
              div 
              [ class "column"]
              [ h3 
                [ class "title has-text-white"] 
                [ text "Fim do Jogo"]
              , h4 
                [ class "subtitle has-text-white"] 
                [ text "Hanking"]
              , table 
                  [ class "table", style [("width","100%")]]
                  [ thead 
                      []
                      [ tr 
                          []
                          [ td [] [ text "Nome"] 
                          , td [] [ text "Dinheiro"]
                          ]
                      ]
                  , tbody
                      []
                      (List.indexedMap (\i j ->
                        let
                          nome = 
                            case j.nome of
                              "" -> "Sem Nome"
                              _ -> j.nome

                        in
                          tr
                            []
                            [ td [] [text ((toString (i+1))++" "++nome)]
                            , td [] [text ("R$ " ++ (toString j.din))]
                            ]
                      ) jogo.jogadores)
                  ]
              , div 
                  [ class "buttons"]
                  [ a
                    [ class "button is-info", href "/"]
                    [ text "Voltar para tela inicial"]

                  ]

              ]
            ]
        ]
      ]
    ]
  

meu_jogo : Model -> Jogo -> Html Msg
meu_jogo model jogo =
  if jogo.criador_id == model.me.uid then
    let
      me1 = Jogador.me model.me jogo.jogadores
      todos_pronto = case model.meu_jogo of
                      Nothing -> True
                      Just jogo -> 
                        if (List.length jogo.jogadores) <= 1 then
                          True
                        else
                          not (Jogador.todos_pronto jogo me1.jogo_id)
      estou_pronto = case me1.pronto of
                      True -> "is-success"
                      False -> "is-light"

    in
      case jogo.inicializando of
        True-> 
          Html.map JogoMsg <| JogoView.view model jogo
        False ->
          div 
            [ id "main-cartel" ]
            [ div 
              [ class "is-fullheight cartel"]
              [
                div [ class "hero-body" ]
                [
                  div 
                  [ class "container has-text-centered" ]
                  [
                    div 
                    [ class "column"]
                    [ 
                      b [] [text "Seu jogo:"]
                    , br [] []
                    , input [ value jogo.nome, onInput AtualizaNomeJogo, class "input", placeholder "Nome do Jogo", style [("width","300px")]] []
                    , div [ style [("margin-top","20px")]] []
                    , b [] [text "Você:"]
                    , div
                        [ class "box"
                        , style [("width","150px"), ("height","150px"), ("margin","auto"),("margin-bottom","10px")]
                        , onClick AbreDialogoSelecionaImagemUsuario
                        ]
                        [ img [ src me1.avatar, style [("height","110px")]] []
                        ]
                    , input 
                        [ class "input"
                        , placeholder "Seu nome aqui"
                        , style [("width","300px")]
                        , value me1.nome
                        , onInput AlteraNomeDoJogador]
                        []
                    , div 
                        [ class "buttons"]
                        [ button 
                          [ class ("button "++estou_pronto), onClick EstouPronto]
                          [ text "Estou Pronto"]
                        , button
                          [ class "button is-info", disabled todos_pronto, onClick IniciandoJogo]
                          [ 
                          if (List.length jogo.jogadores) <= 1 then
                            text "Não pode jogar sozinho"
                          else
                            text "Começar Jogo"
                          ]

                        , button
                          [ class "button is-danger", onClick SairDoJogo]
                          [ text "Sair do Jogo"]
                        ]

                    ]
                  , div 
                    [ class "container has-text-left", style [("margin-top", "30px")] ]
                    [
                      div 
                      [ class "columns"]
                      [ div
                          [ class "column"]
                          [ h3 
                              [ class "has-text-white"] 
                              [ text "Jogadores"]
                          , jogadores jogo jogo.jogadores
                          ]
                      {-, div
                          [ class "column", style [("background", "#FFF")]]
                          [ h3 
                              [ class "has-text-grey"] 
                              [ text "Configurações do Jogo"]
                          , hr [] []
                          , div 
                              [ class "field"]
                              [ label
                                  [ class "label"]
                                  [ text "Ativar Bonus por falência de empresa?" ]
                              , div 
                                  [ class "control"]
                                  [ div
                                      [ class "select"]
                                      [ select 
                                          []
                                          [ option [] [text "Sim"]
                                          , option [] [text "Não"]
                                          ]                              
                                      ]
                                  ]
                              ]
                          , div 
                              [ class "field"]
                              [ label
                                  [ class "label"]
                                  [ text "Tamanho do Jogo" ]
                              , div 
                                  [ class "control"]
                                  [ div
                                      [ class "select"]
                                      [ select 
                                          []
                                          [ option [] [text "Pequeno (2 jogadores)"]
                                          , option [] [text "Médio (6 Jogadores)"]
                                          , option [] [text "Grande ( 10 Jogadores)"]
                                          ]                              
                                      ]
                                  ]
                              ]
                          ]-}  
                      ]
                    ]
                  ]
                ]
              ]
            ]
    else
      let
        me1 = Jogador.me model.me jogo.jogadores
        todos_pronto = case model.meu_jogo of
                        Nothing -> False
                        Just jogo -> not (Jogador.todos_pronto jogo me1.jogo_id)
        estou_pronto = case me1.pronto of
                        True -> "is-success"
                        False -> "is-light"
      in
        case jogo.inicializando of
          True-> 
            Html.map JogoMsg <| JogoView.view model jogo
          False ->
            div 
              [ id "main-cartel" ]
              [ div 
                [ class "is-fullheight cartel"]
                [
                  div [ class "hero-body" ]
                  [
                    div 
                    [ class "container has-text-centered" ]
                    [
                      div 
                      [ class "column"]
                      [ 
                        b [] [text "Seu jogo:"]
                      , br [] []
                      , h3 [] [text jogo.nome]
                      , div [ style [("margin-top","20px")]] []
                      , b [] [text "Você:"]
                      , div
                          [ class "box"
                          , style [("width","150px"), ("height","150px"), ("margin","auto"),("margin-bottom","10px")]
                          , onClick AbreDialogoSelecionaImagemUsuario
                          ]
                          [ img [ src me1.avatar, style [("height","110px")]] []
                          ]
                      , input 
                          [ class "input"
                          , placeholder "Seu nome aqui"
                          , style [("width","300px")]
                          , value me1.nome
                          , onInput AlteraNomeDoJogador]
                          []
                      , div 
                          [ class "buttons"]
                          [ button 
                            [ class ("button "++estou_pronto), onClick EstouPronto]
                            [ text "Estou Pronto"]
                          , button
                            [ class "button is-danger", onClick SairDoJogo]
                            [ text "Sair do Jogo"]
                          ]


                      ]
                    , div 
                      [ class "container has-text-left", style [("margin-top", "30px")] ]
                      [
                        div 
                        [ class "columns"]
                        [ div
                            [ class "column"]
                            [ h3 
                                [ class "has-text-white"] 
                                [ text "Jogadores"]
                            , jogadores1 jogo jogo.jogadores
                            ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
jogadores1 : Jogo -> List Jogador -> Html Msg
jogadores1 jogo jogadores =
  table 
    [ class "table is-striped", style [("background", "#FFF"),("width","100%")]] 
    [
      thead 
        []
        [ 

        ]
    , tbody
        []
        (List.map (\jogador ->
          let
            pronto = case jogador.pronto of
                      True ->
                        "#08BD87"
                      False ->
                        "transparent"
            nome = 
              case jogador.nome of
                "" -> jogador.uid
                _ -> jogador.nome
          in
            tr
              [ style [("background",pronto)]]
              [ td [] 
                  [ img [src jogador.avatar, style [("width","40px")]][]
                  , text nome
                  ]
              , td [] [ ]
              ]

           ) jogadores)      

    ]
jogadores : Jogo -> List Jogador -> Html Msg
jogadores jogo jogadores =
  let 
    lista =  Array.initialize ((List.length jogadores) - jogo.max_users) (always 0) |> Array.toList
  in
    table 
      [ class "table is-striped", style [("background", "#FFF"),("width","100%")]] 
      [
        thead 
          []
          [ 

          ]
      , tbody
          []
          (List.concat [(List.map (\jogador ->
              let
                pronto = case jogador.pronto of
                          True ->
                            "#08BD87"
                          False ->
                            "transparent"
                nome = 
                  case jogador.nome of
                    "" -> jogador.uid
                    _ -> jogador.nome
              in
                tr
                  [ style [("background",pronto)]]
                  [ td [] 
                      [ img [src jogador.avatar, style [("width","40px")]][]
                      ,text nome
                      ]
                  {-, td [] [ select 
                              [ class "input" ]
                              [ option [] [ text "Aberto"]
                              , option [] [ text "Fechado"]
                              , option [] [ text "Robô"] 
                              ]
                          ]-}
                  ]
  
               ) jogadores)
            , List.map(\x ->
                tr [] 
                   [ td [ style [("height","30px")]] [ b [] [text "ninguém"] ] 
                   , td []  
                      [ select 
                        [ class "input" ]
                        [ option [] [ text "Aberto"]
                        , option [] [ text "Fechado"]
                        , option [] [ text "Robô"] 
                        ]
                      ]
                   ]
              ) lista
  
            ])

      ]
    