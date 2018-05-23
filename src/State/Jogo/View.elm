module State.Jogo.View exposing (..)


import Model exposing (..)
import MainModel exposing (..)
import State.Jogo.Msg exposing (..)
import List.Extra as ListX
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils.Jogador as Jogador
import OpenSolid.Svg as OS
import OpenSolid.BoundingBox2d as BB
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Uuid
import Utils.Fn as Fn
---BLOCKS
import Plugs.Blocks as B

view : Model -> Jogo -> Html Msg
view model jogo =
  div []
  [

    menu_principal model jogo
  , jogadores model jogo
  , pecas model jogo
  , empresas model jogo
  --, acoes model.me jogo
  , buttons model jogo
  , if model.opts.high_graphs then
      Svg.svg [ style [("width",(toString model.tela.viewportWidth)++"px")
                      ,("height",(toString model.tela.viewportHeight)++"px")]] [board_high model jogo ]
    else
      Svg.svg [ style [("width",(toString model.tela.viewportWidth)++"px")
                      ,("height",(toString model.tela.viewportHeight)++"px")]] [board_low model jogo ]
  , if model.me.uid == jogo.criador_id then
      sair_do_jogo model jogo
    else
      div [] []
  ]

buttons : Model -> Jogo -> Html Msg
buttons model jogo =
  div [ class "buttons button-pos-opts" ]
    [
      if model.opts.high_graphs then
       div [ class "button is-small is-black ", onClick ToggleGraficos] [ text "Tabela"]
      else
       div [ class "button is-small is-black", onClick ToggleGraficos] [ text "Mapa"]
    ,  div [ class "button is-small is-black", onClick MaisZoom] [ text "+"]
    ,  div [ class "button is-small is-black", onClick MenosZoom] [ text "-"]
    ]

sair_do_jogo : Model -> Jogo -> Html Msg
sair_do_jogo model jogo =
  div [ class "sair_do_jogo"]
      [ div [ class "button is-danger", onClick SobreEncerrarJogo] [text "X"]]

empresas : Model -> Jogo -> Html Msg
empresas model jogo =
  let 
    me1 = Jogador.me model.me jogo.jogadores
    empresas = jogo.empresas
    top = case model.tela.viewportWidth < 500 of
                      True ->
                         "130px"
                      False ->
                        "60px"
  in
    div
      [class "empresas-na-tela", style [("top",top)]]
      (List.map (\e->
        let
          classe_disponivel = if e.disponivel then
                          "empresa-na-tela empresa-disponivel"
                       else
                          "empresa-na-tela empresa-nao-disponivel"
          qtd_acoes = 
            case (ListX.find(\x -> x.empresa == e.eid) me1.acoes) of
              Nothing -> ""
              Just emp -> toString emp.acoes
        in
          div
            [ class classe_disponivel
            , onClick (StatusEmpresa jogo e) 
            , title e.nome]
            [ 
             div [ class "acao-label"] [text qtd_acoes]
            , img [ src e.icon] []

            ]
        
      ) empresas)

acoes : Jogador -> Jogo -> Html Msg
acoes me jogo =
  let
    me1 = Jogador.me me jogo.jogadores
  in
    div
      [ class "acoes-na-tela"]
      (List.map (\a ->
        div 
          [class "acao-na-tela"]
          [ text "Acao"]

      ) me1.acoes)
    

pecas : Model -> Jogo -> Html Msg
pecas model jogo =
  let
    me1 = Jogador.me model.me jogo.jogadores
    minhas_pecas1 = case jogo.turno.peca of
      Nothing -> me1.pecas
      Just p ->
        (List.filter(\b -> 
          if p.linha == b.linha && p.coluna == b.coluna then
            False
          else
            True
        ) me1.pecas)
        
    width = case model.tela.viewportWidth < 370 of
              True ->
                "308px"
              False ->
                "370px"
    pwidth = case model.tela.viewportWidth < 370 of
              True ->
                 "51px"
              False ->
                "60px"
  in
    ul
      [ class "pecas-na-tela", style [("width",width)]]
      (List.map (\p ->
        li 
          [ class "peca-na-tela"
          , style [("width",pwidth)]
          , onMouseOver (SobreAPeca jogo p)
          , onMouseOut SobreAPecaSai
          , onDoubleClick (InserePeca jogo p)
          ]
          [ text (p.linha++(toString p.coluna))]

      ) minhas_pecas1)  

jogadores : Model -> Jogo -> Html Msg
jogadores model jogo = 
  let
    top = case model.tela.viewportWidth < 500 of
                      True ->
                         "130px"
                      False ->
                        "60px"

  in
    ul
      [ class "jogadores-na-tela", style [("top",top)]]
      (List.map (\j -> 
        let 
          vez = if j.vez then "jogador-vez" else ""
        in
          li 
            [ class ("jogador-na-tela "++ vez), title j.nome]
            [ 
              img [ src j.avatar ] [] 
            ]
      ) jogo.jogadores)
 
  
    



menu_principal : Model -> Jogo -> Html Msg
menu_principal model jogo =
  let
    me1 = Jogador.me model.me jogo.jogadores
    vez = if not me1.vez then "topbar-vez" else ""
  in
    div []
        [
          div 
            [class ("nav-cartel-topbar "++vez), style [("background","#FFF")] ]
            [ div
                [ class "nav-cartel-item", style [("display","block")]]
                [
                  if me1.vez then
                    a [ class "navbar-item finaliza-jogada is-pulled-left"
                      , style [("display","block"),("padding","14px")]
                      , onClick (FinalizaJogada jogo)
                      ]
                      [ text "Finaliza Jogada"
                      ]
                  else
                    div [style [("float","left")
                               ,("font-size","18px")
                               ,("margin-left", "15px")
                               ,("margin-top","10px")
                               ]] [ text "Não é sua vez de jogar"]
                , case jogo.turno.jogador == model.me.uid of
                    False -> div [] []
                    True ->
                      if model.tela.viewportWidth > 500 then
                        render_acoeszinhas me1 jogo
                      else
                        div [] []
                      

                , a [ class "navbar-item cartel-money is-pulled-right"
                    , style [("display","block"),("padding","14px")]
                    ]
                    [ text ("R$ "++(toString me1.din))]
                , a [ class "navbar-burguer"]
                    [ span [] []
                    , span [] []
                    , span [] []
                    ]
                ]
            ]
        , case jogo.turno.jogador == model.me.uid of
            True ->
              if model.tela.viewportWidth <= 500 then
                div [ style [("margin-top","70px"),("position","absolute")]]
                  [render_acoeszinhas me1 jogo
                  , div [class "is-clearfix"] []
                  ]
                else
                  div [] []
            False -> 
              div [ style [("margin-top","70px")]]
                  [
                  ]
        ]
    
render_acoeszinhas : Jogador -> Jogo -> Html Msg
render_acoeszinhas me jogo=
  let
      acoes_vendidas = List.filter(\x ->
                       x.jogador == me.uid
                      ) jogo.turno.acoes_vendidas
      acoes_compradas = List.filter(\x ->
                       x.jogador == me.uid
                      ) jogo.turno.acoes_compradas
  in
    div [ style [("background","#FFFFFF")]]
      [
        case jogo.turno.peca of
          Just pe ->
            div [ class "peca-cc"] [ text (pe.linha ++ (toString pe.coluna))]
          Nothing ->
            span [] []
      , if (List.length acoes_compradas) > 0 then
          b [ class "label-cc"] [text "Compras: "]
        else
          span [] []
      
      , div []
          (List.map(\l -> 
            let
              emp = ListX.find(\f -> f.eid == l.empresa ) jogo.empresas
            in
              case emp of
                Nothing ->
                  div [] [ ]
                Just emp1 ->
                  div [class "acoezinhas", style [("background", emp1.cor)]]
                    [ img [ src emp1.icon] []]
                  
            ) acoes_compradas)
      , if (List.length acoes_vendidas) > 0 then
          b [class "label-cc"] [text "Vendas: "]
        else
          span [] []
      , div []
          (List.map(\l -> 
            let
              emp = ListX.find(\f -> f.eid == l.empresa ) jogo.empresas
            in
              case emp of
                Nothing ->
                  div [] [ ]
                Just emp1 ->
                  div [class "acoezinhas", style [("background", emp1.cor)]]
                    [ img [ src emp1.icon] []]
                  
            ) acoes_vendidas)

    ]


--------------LOW GRAPHS
board_low : Model -> Jogo -> Svg.Svg Msg
board_low model jogo =
  let
    zoom =  model.zoom
    (w, h) = ((model.tela.viewportWidth-100)//2,(model.tela.viewportHeight-100)//2)
    (cx, cy) = 
      if model.tela.viewportWidth > 500 then
        ((model.tela.viewportWidth//2), (model.tela.viewportHeight//2))
      else
        ((model.tela.viewportWidth//2), ((model.tela.viewportHeight//2)-140))
    w1 = 40
    h1 = 40 
    wt = (toFloat (w1*11))
    ht = (toFloat (h1*10))
    tx = (toFloat cx) - (wt)/2 - 20
    ty = (toFloat cy) - (ht)/2


  in
    Svg.g [style [("width",(toString model.tela.viewportWidth)++"px")
                  ,("height",(toString model.tela.viewportHeight)++"px")
                  ,("transform-origin","50% 50%")
                  ]
          , SvgA.transform ("scale("++(toString zoom)++") translate("++(toString tx)++","++(toString ty)++")")
          ]
          [ B.sobre_color
          , B.na_peca_color
          , B.jogador_color
          , Svg.g [
                  ]
              (   (List.map (peca_low jogo) jogo.board)
               ++ (List.map label_a_low jogo.colunas)
               ++ (List.map label_b_low jogo.linhas)
              )
          ]


peca_low : Jogo -> Peca -> Svg.Svg Msg
peca_low jogo peca =
  let
    c = toFloat peca.coluna
    l = (toFloat (Fn.converte_linha_para_numero peca.linha))
    w = 50
    px = ((w*(c-1)))
    py = ((w*l))
    empresa_evento = 
      case ListX.find(\x -> peca.empresa == Just x.eid) jogo.empresas of
        Nothing -> SvgE.onClick NoOp
        Just emp -> SvgE.onClick (StatusEmpresa jogo emp)
  in
    Svg.g [ empresa_evento
          --, onDoubleClick (InserePeca jogo peca)
          , SvgA.transform ("scale(0.8) translate("++(toString px)++","++(toString py)++")")
          , SvgA.width ((toString w) ++ "px")
          , SvgA.fill "#E1DE1E"]
          [
            B.square_low jogo peca []

          ]

label_a_low : Int -> Svg.Svg Msg
label_a_low i =
  let
    w = 40
    px = w * (i-1) + 20
    py = 25
  in
  Svg.text_  [ SvgA.width ((toString w) ++ "px"),SvgA.textAnchor "middle", SvgA.transform ("translate("++(toString px)++","++(toString py)++")") ] [ Svg.text (toString i)]


label_b_low : String -> Svg.Svg Msg
label_b_low j =
  let
    i = (Fn.converte_linha_para_numero j)
    h = 40
    px = -30
    py = h * i + 30
  in
  Svg.text_  [ SvgA.transform ("translate("++(toString px)++","++(toString py)++")") ] [ Svg.text j]


------------ HIGH GRAPHS
board_high : Model -> Jogo -> Svg.Svg Msg
board_high model jogo =
  let
    zoom =  model.zoom
    (w, h) = ((model.tela.viewportWidth-100)//2,(model.tela.viewportHeight-100)//2)
    (cx, cy) = 
      if model.tela.viewportWidth > 500 then
        ((model.tela.viewportWidth//2), (model.tela.viewportHeight//2))
      else
        ((model.tela.viewportWidth//2), ((model.tela.viewportHeight//2)-140))
    w1 = 50
    h1 = 30 
    s = sqrt (12*9) * w1
    wt = if zoom < 1 then s*zoom else s
    ht = (toFloat (h1*3))
    tx = (toFloat cx) - (wt)/5
    ty = (toFloat cy) - (ht)/2 - (toFloat cy)/3


  in
    Svg.g [ style [("width",(toString model.tela.viewportWidth)++"px")
                  ,("height",(toString model.tela.viewportHeight)++"px")
                  ,("transform-origin","50% 50%")
                  ]
          , SvgA.transform ("scale("++(toString zoom)++") translate("++(toString tx)++","++(toString ty)++")")
          ]
          [ B.sobre_color
          , B.na_peca_color
          , B.jogador_color
          , Svg.g [
                  ]
              (   (List.map (peca_high jogo) jogo.board)
               ++ (List.map label_a jogo.colunas)
               ++ (List.map label_b jogo.linhas)
              )
          ]
label_a : Int -> Svg.Svg Msg
label_a i =
  let
    w = 25
    h = 15
    px = w * i + 35
    py = h * i + 15
  in
  Svg.text_  [ SvgA.transform ("translate("++(toString px)++","++(toString py)++")") ] [ Svg.text (toString i)]

label_b : String -> Svg.Svg Msg
label_b j =
  let
    i = (Fn.converte_linha_para_numero j)
    w = 25
    h = 15
    px = (w*i)  - (w * i )*2 + 45
    py = h * i + 25
  in
  Svg.text_  [ SvgA.transform ("translate("++(toString px)++","++(toString py)++")") ] [ Svg.text j]

peca_high : Jogo -> Peca -> Svg.Svg Msg
peca_high jogo peca =
  let
    c = toFloat peca.coluna
    l = (toFloat (Fn.converte_linha_para_numero peca.linha))
    w = 50
    px = ((50*c)/2)*2 - (50*(c-1)/2) - (26*l)
    py = ((29*c)/2)*2 - ((29*c-1)/2) + (15*l)

  in
    Svg.g [ SvgA.transform ("translate("++(toString px)++","++(toString py)++")")
          , SvgA.width ((toString w) ++ "px")
          , SvgA.fill "#E1DE1E"]
          [
            B.terrain_solo jogo peca []
          ,  case peca.jogador of
              Nothing ->
                case peca.solo of
                  2 ->
                    B.grass_1 []
                  3 ->
                    B.grass_2 []
                  4 ->
                    B.grass_3 []
                  5 ->
                    B.grass_4 []
                  _ ->
                    Svg.g [] []
                  
              Just j ->
                case peca.empresa of
                  Nothing ->
                    case peca.solo of
                      2 ->
                        B.grass_1_oc []
                      3 ->
                        B.grass_2_oc []
                      4 ->
                        B.grass_3_oc []
                      5 ->
                        B.grass_4_oc []
                      _ ->
                        B.solo_oc []
                  Just e ->
                    if peca.sede then
                      Svg.g [] []
                    else
                      case peca.solo of
                        2 ->
                          B.grass_1 []
                        3 ->
                          B.grass_2 []
                        4 ->
                          B.grass_3 []
                        5 ->
                          B.grass_4 []
                        _ ->
                          Svg.g [] []

          ]


