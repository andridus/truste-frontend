module Plugs.Blocks exposing (..)

import Svg
import Svg.Attributes as SA
import Model

import OpenSolid.BoundingBox3d as BB3d


-------------------LOW
base_na_peca_low : Svg.Svg msg
base_na_peca_low =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo_square_on.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.transform "translate(0,0)"
             ] ) []

base_jogador_low : Svg.Svg msg
base_jogador_low =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo_square_player.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.filter "url(#jogador_color)"
             , SA.transform "translate(0,0)"
             ] ) []
base_empresa_low : Model.Jogo -> String -> Svg.Svg msg
base_empresa_low jogo emp =
  case List.filter (\x -> x.eid == emp ) jogo.empresas |> List.head of
    Nothing -> Svg.g [] []
    Just e ->
      let
        w = 50
        h = 50
      in
        Svg.g [] 
         [
            Svg.image ([ SA.xlinkHref ("/imgs/assets/grass_solo_square_emp"++e.construcao++".svg")
                 , SA.width ((toString w)++"px")
                 , SA.height ((toString h)++"px")
                 , SA.transform "translate(0,0)"
                 ] ) []
         ]
base_sobre_low : Svg.Svg msg
base_sobre_low =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo_square_over.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.filter "url(#sobre_color)"
             , SA.transform "translate(0,0)"
             ] ) []

square_low : Model.Jogo -> Model.Peca -> List (Svg.Attribute msg)-> Svg.Svg msg
square_low jogo peca attrs =
  let
    w = 50
    h = 50
  in
    Svg.g []
      [
      
        Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo_square.svg"
                 , SA.width ((toString w)++"px")
                 , SA.height ((toString h)++"px")
                 ] ++ attrs) []
      ,  if peca.sobre then base_sobre_low else Svg.g [] []
      ,  if peca.na_peca then base_na_peca_low else Svg.g [] []
      ,  case peca.jogador  of
          Just j -> 
            case peca.empresa of
              Nothing ->
                base_jogador_low 
              Just e ->
                base_empresa_low jogo e
          Nothing -> Svg.g [] []
        
      ]


--------------------HIGH
jogador_color : Svg.Svg msg
jogador_color =
  Svg.filter [ SA.id "jogador_color"]
    [
      Svg.feColorMatrix [ SA.type_ "hueRotate"
                        , SA.values  "50"
                        ] [  ]
    ]

sobre_color : Svg.Svg msg
sobre_color =
  Svg.filter [ SA.id "sobre_color"]
    [
      Svg.feColorMatrix [ SA.type_ "hueRotate"
                        , SA.values  "25"
                        ] [  ]
    ]
na_peca_color : Svg.Svg msg
na_peca_color =
  Svg.filter [ SA.id "na_peca_color"]
    [
      Svg.feColorMatrix [ SA.type_ "hueRotate"
                        , SA.values  "200"
                        ] [  ]
    ]
base : Svg.Svg msg
base =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.transform "translate(0,0)"
             ] ) []

base_sobre : Svg.Svg msg
base_sobre =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.filter "url(#sobre_color)"
             , SA.transform "translate(0,0)"
             ] ) []
base_jogador : Svg.Svg msg
base_jogador =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.filter "url(#jogador_color)"
             , SA.transform "translate(0,0)"
             ] ) []

base_empresa : Model.Jogo -> String -> Svg.Svg msg
base_empresa jogo emp =
  case List.filter (\x -> x.eid == emp ) jogo.empresas |> List.head of
    Nothing -> Svg.g [] []
    Just e ->
      let
        w = 50
        h = 50
      in
        Svg.g [] 
         [
            Svg.image ([ SA.xlinkHref ("/imgs/assets/grass_solo_emp_"++e.construcao++".svg")
                 , SA.width ((toString w)++"px")
                 , SA.height ((toString h)++"px")
                 , SA.transform "translate(0,0)"
                 ] ) []
          ,  Svg.image [ SA.xlinkHref ("/imgs/assets/construcao_empresa_"++e.construcao++".svg")
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] []
          ]
base_empresa_cor : Model.Jogo -> String -> Svg.Svg msg
base_empresa_cor jogo emp =
  case List.filter (\x -> x.eid == emp ) jogo.empresas |> List.head of
    Nothing -> Svg.g [] []
    Just e ->
      let
        w = 50
        h = 50
      in
      Svg.g [] 
         [
            Svg.image ([ SA.xlinkHref ("/imgs/assets/grass_solo_emp_"++e.construcao++".svg")
                 , SA.width ((toString w)++"px")
                 , SA.height ((toString h)++"px")
                 , SA.transform "translate(0,0)"
                 ] ) []
          ]

base_na_peca : Svg.Svg msg
base_na_peca =
  let
    w = 50
    h = 50
  in
  Svg.image ([ SA.xlinkHref "/imgs/assets/grass_solo.svg"
             , SA.width ((toString w)++"px")
             , SA.height ((toString h)++"px")
             , SA.filter "url(#na_peca_color)"
             , SA.transform "translate(0,0)"
             ] ) []




terrain_solo : Model.Jogo -> Model.Peca -> List (Svg.Attribute msg)-> Svg.Svg msg
terrain_solo jogo peca attrs =
  let

    w = 50
    h = 50
  in
    Svg.g []
      [
      
        Svg.image ([ SA.xlinkHref "/imgs/assets/terrain_grass_solo.svg"
                 , SA.width ((toString w)++"px")
                 , SA.height ((toString h)++"px")
                 ] ++ attrs) []
      ,  if peca.sobre then base_sobre else Svg.g [] []
      ,  if peca.na_peca then base_na_peca else Svg.g [] []
      ,  case peca.jogador  of
          Just j -> 
            case peca.empresa of
              Nothing ->
                base_jogador 
              Just e ->
                if peca.sede then
                  base_empresa jogo e
                else
                  base_empresa_cor jogo e
          Nothing -> Svg.g [] []
        
      ]
    

solo_oc : List (Svg.Attribute msg)-> Svg.Svg msg
solo_oc attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/solo_oc.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


grass_1_oc : List (Svg.Attribute msg)-> Svg.Svg msg
grass_1_oc attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_4_oc.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_2_oc : List (Svg.Attribute msg)-> Svg.Svg msg
grass_2_oc attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_2_oc.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_3_oc : List (Svg.Attribute msg)-> Svg.Svg msg
grass_3_oc attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_3_oc.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_4_oc : List (Svg.Attribute msg)-> Svg.Svg msg
grass_4_oc attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_1_oc.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


grass_1 : List (Svg.Attribute msg)-> Svg.Svg msg
grass_1 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_4.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_2 : List (Svg.Attribute msg)-> Svg.Svg msg
grass_2 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_2.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_3 : List (Svg.Attribute msg)-> Svg.Svg msg
grass_3 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_3.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

grass_4 : List (Svg.Attribute msg)-> Svg.Svg msg
grass_4 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/grass_1.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []



casa_1 : List (Svg.Attribute msg)-> Svg.Svg msg
casa_1 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/casa_1.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


casa_2 : List (Svg.Attribute msg)-> Svg.Svg msg
casa_2 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/casa_2.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


casa_3 : List (Svg.Attribute msg)-> Svg.Svg msg
casa_3 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/casa_3.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


casa_4 : List (Svg.Attribute msg)-> Svg.Svg msg
casa_4 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/casa_4.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []


casa_5 : List (Svg.Attribute msg)-> Svg.Svg msg
casa_5 attrs =
  let
    w = 50
    h = 50
  in
    Svg.image ([ SA.xlinkHref "/imgs/assets/casa_5.svg"
               , SA.width ((toString w)++"px")
               , SA.height ((toString h)++"px")
               ] ++ attrs) []

