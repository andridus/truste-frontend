module State.Jogos exposing (..)

import MainModel exposing (..)
import Model exposing (..)
import Msg exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Array

render : Model -> List JogoParaExibir -> Html Msg
render model jogos = 
  case jogos of
    [] ->
      div [] []
    _ ->
      table 
        [ class "table is-bordered"
        , style [("width","100%")]
        ]
        [ thead 
          [] 
         [ tr
              []
              [ th [] [ text "Jogo" ]
              , th [] [ text "Jogadores"]
              , th [ style [("width", "150px")]] [ text "Criador"]
              , th [] [ text ""]
              ]
          ]
        , tbody
          []
          (List.map 
            (\jogo ->
              let
                nome = case jogo.nome of
                        "" -> jogo.jid
                        j -> j

                criador = 
                  (List.filter(\x -> x.uid == jogo.criador_id) model.usuarios) |> List.head

                icon = case jogo.nome of
                        "" -> jogo.jid
                        j -> j
                jogadores = (Array.initialize jogo.jogadores (\n ->
                    img [src "/imgs/user.png"] []
                  )) |> Array.toList
              in
                tr 
                  [] 
                  [ td 
                      []
                      [ text nome ]
                  , td [] 
                       jogadores
                  , td
                      []
                      [ 
                        case criador of
                          Nothing -> div [] []
                          Just c -> img [src c.avatar, style [("width","20px")]] []
                      , case criador of
                          Nothing -> text jogo.criador_id
                          Just c -> 
                            case c.nome of
                              "" -> text c.uid
                              cn -> text cn
                      ]
                  , td 
                      []
                      [
                        if not jogo.iniciado then
                          div [ class "button is-warning", onClick (EntrarNoJogo jogo.jid jogo.criador_id)] [text "Entrar"]
                        else
                          button [ class "button is-danger", disabled True] [text "Jogo já iniciado"]
                      ]
                ]
            ) jogos )
        ]
        
