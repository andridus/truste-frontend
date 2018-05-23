module State.Jogo.Update exposing (..)


import Model exposing (..)
import MainModel exposing (..)
import State.Jogo.Msg exposing (..)
import Utils.Jogador as Jogador
import Utils.Fn as Fn
import Ports.Sound as Sound
import Cmd.Extra as CmdX
import List.Extra as ListX

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      ToggleGraficos -> 
        let
          opts = model.opts
          opts1 = { opts | high_graphs = not model.opts.high_graphs}
        in
          { model | opts = opts1} ! []
            
      SobreEncerrarJogo ->
        let
          dialogos = model.dialogos
          dialogo = 
            novo_dialogo 
              "ENCERRAR_JOGO" 
              "Alerta" 
              "Você tem certeza de que deseja encerrar esse jogo? Não é possível voltar atrás!" ""
          dialogos2 = dialogo :: dialogos
        in
          ({ model | dialogos = dialogos2}, Cmd.none )
      
      MaisZoom ->
        let
          zoom = model.zoom
          zoom1 = zoom + 0.1
        in
         ({model | zoom = zoom1}, Cmd.none)
      MenosZoom ->
        let
          zoom = model.zoom
          zoom1 = zoom - 0.1
        in
         ({model | zoom = zoom1}, Cmd.none)
      StatusEmpresa jogo empresa ->
        let
          dialogo = novo_dialogo_status_empresa empresa.eid
          dialogos = model.dialogos
          dialogos1 = dialogo :: dialogos
          empresas = Fn.conta_pecas_empresas jogo.board jogo.empresas
          jogo1 = {jogo| empresas = empresas}
        in
         ({model | dialogos = dialogos1, meu_jogo = Just jogo1}, Cmd.none)
      ComprarAcao jogo empresa ->
      -- let
         -- dialogo = novo_dialogo_comprar_acao 
         -- dialogos = model.dialogos
         -- dialogos1 = dialogo :: dialogos
       -- in
        (model, Cmd.none)
      FinalizaJogada jogo ->
        let
          turno = jogo.turno
          dialogos = model.dialogos
          dialogos_todos = List.filter (\x-> x.tipo == "VENDER_ACOES_EMPRESA_FUNDIDA") dialogos
          empresas = List.filter(\x -> x.disponivel == False ) jogo.empresas
          (model1, cmd) = case turno.peca of
              Nothing -> 
                let
                  dialogo = novo_dialogo "ERROR" "Alerta" "Você não pode finalizar seu turno sem jogar nenhuma peça" ""
                  dialogos1 = dialogo :: dialogos
                in
                  ({ model | dialogos = dialogos1 }, Sound.play_sound("ERROR"))
              Just p -> 
                if (List.length dialogos_todos) > 0 then
                  let
                    dialogo = novo_dialogo "ERROR" "Alerta" "Você não pode finalizar jogada enquanto todos os jogadores não decidirem sobre as suas ações que restaram das empresas falidas. Aguarde.." ""
                    dialogos2 = dialogo :: dialogos
                  in
                    ({ model | dialogos = dialogos2 }, Sound.play_sound("ERROR"))
                else
                  if List.length empresas > 0 then 
                    if List.length turno.acoes_compradas == 0 then
                      let
                        dialogo = novo_dialogo "QUESTAO" "Alerta" "Tem certeza de que não quer comprar nenhuma ação?" ""
                        dialogos2 = dialogo :: dialogos
                      in
                        ({ model | dialogos = dialogos2 }, Sound.play_sound("QUESTION"))
                    else
                      let
                        dialogo = novo_dialogo "QUESTAO" "Alerta" "Finalizar jogada?" ""
                        dialogos2 = dialogo :: dialogos
                      in
                        ({ model | dialogos = dialogos2 }, Sound.play_sound("QUESTION"))
                      
                  else
                    let
                        dialogo = novo_dialogo "QUESTAO" "Alerta" "Finalizar jogada?" ""
                        dialogos2 = dialogo :: dialogos
                      in
                        ({ model | dialogos = dialogos2 }, Sound.play_sound("QUESTION"))
            
        in
          (model1, cmd)
      
      InserePeca jogo p ->
        let
          me1 = Jogador.me model.me jogo.jogadores
        in

          case me1.vez of
            True ->
              case jogo.turno.peca of
                Nothing ->
                  let
                    dialogos = model.dialogos
                    me = model.me

                    board = List.map(\b ->
                        if p.linha == b.linha && p.coluna == b.coluna then
                          {b | jogador = Just model.me.uid}
                        else
                          b
                      ) jogo.board

                    
                   
                    
                    jogadores1 = jogo.jogadores
                    

                    turno = jogo.turno
                    p1 = {p | jogador = Just model.me.uid}
                    turno0 = { turno |  peca = (Just p1) } 
                    

                    jogo0 = {jogo | board = board} 
                    (dialogos1, turno1, jogo1, cmd) = 
                      case Fn.verifica_jogada jogo p of
                        ("NULO", jogada) ->
                          (dialogos, turno0, jogo0, Sound.play_sound("PIECE"))
                        ("NOVA_EMPRESA", jogada) ->
                          let
                            (dialogos1, turno1) = 
                              let
                                empresas1 = List.filter(\e ->
                                      e.disponivel
                                    ) jogo0.empresas
                                _ = Debug.log "a" empresas1
                              in
                                case List.head empresas1 of
                                    Nothing ->
                                      let
                                        dialogo = novo_dialogo "ERROR" "Alerta" "Não existem empresas disponíveis para você fundar. Jogada inválida!" ""
                                        dialogos0 = dialogo :: dialogos
                                        turno1 = { turno0 |  peca = Nothing } 
                                      in
                                        (dialogos0, turno1)
                                    Just e ->
                                      let
                                        dialogo = novo_dialogo_nova_empresa 
                                        dialogos0 = dialogo :: dialogos
                                        turno1 = { turno0 | atualizar_pecas = Just jogada}
                                      in
                                        (dialogos0, turno1)
                                
                                
                          in
                            (dialogos1, turno1, jogo0, Sound.play_sound("NOVA_EMPRESA"))
                        ("FUNDE_EMPRESA", jogada) ->
                          let
                            empresas0 = Fn.conta_pecas_empresas board jogo.empresas

                            empresas =  
                                  let
                                    empresas1 = List.map(\p ->
                                              case (ListX.find (\em -> Just em.eid == p.empresa) empresas0) of
                                                Nothing -> []
                                                Just emp -> [emp]
                                               ) jogada.pecas
                                    empresas2 = List.concat empresas1
                                    empresas3 = ListX.uniqueBy (\x -> x.eid) empresas2

                                  in
                                    empresas3
                            empresas_maior_que_12 = List.filter(\x -> x.pecas >= 12) empresas
                            _ = Debug.log "pecas " empresas
                            (dialogos1, turno1, cmd) = 
                                if (List.length empresas_maior_que_12) > 1 then
                                  let
                                    dialogo = novo_dialogo "ERROR" "Alerta" "Não pode fundir empresas com quantidade de peças maiores que 12!" ""
                                    dialogos0 = dialogo :: dialogos
                                    turno1 = { turno0 |  peca = Nothing } 
                                  in
                                    (dialogos0, turno1, Sound.play_sound("ERROR"))
                                else
                                  let
                                    dialogo = novo_dialogo_funde_empresas
                                    dialogos1 = dialogo :: dialogos
                                    turno1 = { turno0 | atualizar_pecas = Just jogada}
                                  in
                                    (dialogos1, turno1,Cmd.batch[Sound.play_sound("NOVA_EMPRESA"), CmdX.perform EnviaTurnoAtualizado, CmdX.perform EnviaDialogoInformacao])

                            jogo1 = { jogo0 | empresas = empresas0}
                          in
                            (dialogos1, turno1, jogo1, cmd)
                        ("AMPLIA_EMPRESA", jogada) ->
                          let
                            
                            peca = [p1]
                            pecas = jogada.pecas
                            pecas1 =  List.concat [peca, pecas]
                            pecas2 = case jogada.empresa of
                                Nothing -> 
                                  pecas1
                                Just emp -> 
                                  List.map (\x -> {x | empresa = Just emp.eid } ) pecas1

                            jogada1 = {jogada | pecas = pecas2}
                            board1 = Fn.atualiza_board board jogada1.pecas

                            d = (Debug.log "AMPLIA_EMPRESA" jogada1)
                            jogo1 = { jogo0 | board = board1}
                            turno1 = { turno0 | atualizar_pecas = Just jogada1}
                          in
                          (dialogos, turno1, jogo1, Sound.play_sound("PIECE"))
                        _ ->
                          (dialogos, turno0, jogo0, Sound.play_sound("ERROR"))




                    board1 = 
                      case turno1.peca of
                        Nothing -> jogo.board
                        Just p ->
                          List.map(\b ->
                            if p.linha == b.linha && p.coluna == b.coluna then
                              {b | jogador = Just model.me.uid}
                            else
                              b
                          ) jogo1.board
                    
                    
                    cmd1 = case turno1.peca of
                      Nothing ->
                        cmd
                      Just p ->
                        Cmd.batch [ cmd, (CmdX.perform EnviaBoard)]

                    jogo2 = Just { jogo1 | board = board1, turno = turno1, jogadores = jogadores1}




                  in
                    ({model | meu_jogo = jogo2, dialogos = dialogos1},cmd1)
                Just p ->
                  let
                    d =1
                    dialogo = novo_dialogo "ERROR" "Alerta" ("Você já posicionou a peça "++(p.linha++(toString p.coluna))++" nessa rodada!") ""
                    dialogos = model.dialogos
                    dialogos1 = dialogo :: dialogos
                  in
                    ({model| dialogos = dialogos1}, Sound.play_sound("ERROR"))

              
            False ->
              let
                d =1
                dialogo = novo_dialogo "SEM_VEZ" "Alerta" "Não é sua vez de jogar!" ""
                dialogos = model.dialogos
                dialogos1 = dialogo :: dialogos
              in
                ({model| dialogos = dialogos1}, Sound.play_sound("ERROR"))

          
            


      SobreAPeca jogo p ->
        let
          board = List.map(\b ->
              if p.linha == b.linha && p.coluna == b.coluna then
                {b | na_peca = True}
              else
                let
                  b1 = {b | na_peca = False}
                in
                  if p.linha == b.linha || p.coluna == b.coluna then
                    {b1 | sobre = True} 
                  else
                    {b1 | sobre = False} 
            ) jogo.board
          jogo1 = Just { jogo | board = board }

        in
          ({model | meu_jogo = jogo1}, Sound.play_sound("SWIPE"))

      SobreAPecaSai ->
        case model.meu_jogo of
          Nothing -> model ! []
          Just jogo ->
            let
              board = List.map(\b ->
                    {b | na_peca = False, sobre = False}
                ) jogo.board

              jogo1 = Just { jogo | board = board }

            in
              ({model | meu_jogo = jogo1}, Sound.play_sound("SWIPE"))


      _ ->
        (model, Cmd.none)