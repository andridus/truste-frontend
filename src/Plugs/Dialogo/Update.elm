module Plugs.Dialogo.Update exposing (..)
import Model exposing (..)
import MainModel exposing (..)
import Plugs.Dialogo.Msg exposing (..)
import Utils.Jogador as Jogador
import Utils.Fn as Fn
import List.Extra as ListX
import Ports.Sound as Sound
import Cmd.Extra as CmdX

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EncerrarJogo dialogo->
          let
            dialogos = List.filter (\x -> 
                  x /= dialogo
                ) model.dialogos
            model1 = { model | dialogos=dialogos}
          in
            (model1, Cmd.batch [CmdX.perform PerformSairDoJogo, Sound.play_sound("CLICK")])
    CompraAcao empresa jogo ->
      let
        
        (me1, jogo1, dialogos1, cmd) =
          let
            me0 = Jogador.me model.me jogo.jogadores
          in 
            if List.length jogo.turno.acoes_compradas < 3 then
              if me0.din >= empresa.preco then 
                let
                  turno = jogo.turno
                  acao = {empresa = empresa.eid, jogador = model.me.uid, acoes = 1, valor = empresa.preco}
                  turno1 = 
                    case List.member acao turno.acoes_vendidas of
                      True ->
                        let 
                          avs = ListX.remove acao turno.acoes_vendidas
                        in
                          {turno | acoes_vendidas = avs}
                      False ->
                        let
                          ac = turno.acoes_compradas
                          ac1 = acao :: ac
                        in
                         {turno | acoes_compradas = ac1}
                  me0 = Jogador.me model.me jogo.jogadores
                  me01 = { me0 | online = True}
                  me1 = Jogador.comprar_acao me01 empresa
                  ac = turno.acoes_compradas
                  jogo1 = Jogador.atualiza_acoes jogo me1
                  jogadores1 = Jogador.atualiza_me me1 jogo.jogadores
                  empresas = Fn.reduz_uma_acao_da_empresa empresa jogo.empresas
                  jogo2 = {jogo1 | turno = turno1, empresas = empresas, jogadores = jogadores1}
                in
                  (me1, jogo2, model.dialogos, Sound.play_sound("AMONEY"))
             
            else
              let
                dialogo = novo_dialogo "ERROR" "Alerta" "Você não pode mais comprar ações, você não tem dinheiro para isso" ""
                dialogos = model.dialogos
                dialogos1 = dialogo :: dialogos
              in
                (model.me, jogo, dialogos1, Sound.play_sound("ERROR"))
          else
            let
              dialogo = novo_dialogo "ERROR" "Alerta" "Você já atingiu o limite de compra de ações (Máximo 3 por turno)" ""
              dialogos = model.dialogos
              dialogos1 = dialogo :: dialogos
            in
              (model.me, jogo, dialogos1,Sound.play_sound("ERROR"))

      in
        ({ model | me = me1, dialogos = dialogos1, meu_jogo = Just jogo1} ,  Cmd.batch [CmdX.perform EnviaTurnoAtualizado, cmd])

    VendeAcao empresa jogo ->
      let
        (me1, jogo1) = 
            let
              turno = jogo.turno
              acao = {empresa = empresa.eid, jogador = model.me.uid, acoes = 1, valor = empresa.preco}
              turno1 = 
                case List.member acao turno.acoes_compradas of
                  True ->
                    let 
                      acs = ListX.remove acao turno.acoes_compradas
                    in
                      {turno | acoes_compradas = acs}
                  False ->
                    let
                      av = turno.acoes_vendidas
                      av1 = acao :: av
                    in
                     {turno | acoes_vendidas = av1}

              me0 = Jogador.me model.me jogo.jogadores
              me1 = Jogador.vender_acao me0 empresa
              jogo1 = Jogador.atualiza_acoes jogo me1
              jogadores1 = Jogador.atualiza_me me1 jogo.jogadores
              empresas = Fn.aumenta_uma_acao_da_empresa empresa jogo.empresas
              jogo2 = {jogo1 | turno = turno1, empresas = empresas, jogadores = jogadores1}
            in
              (me1, jogo2)
      in
        ({ model | me = me1, meu_jogo = Just jogo1} ,  Cmd.batch [CmdX.perform EnviaTurnoAtualizado, Sound.play_sound("MONEY")])

    CompraAcaoFusao empresa jogador jogo ->
      let
        (me1, jogo1) = 
            let
              turno = jogo.turno
              acao = {empresa = empresa.eid, jogador = model.me.uid, acoes = 1, valor = empresa.preco}
              turno1 = 
                case List.member acao turno.acoes_vendidas_fusao of
                  True ->
                    let 
                      acs = ListX.remove acao turno.acoes_vendidas_fusao
                    in
                      {turno | acoes_vendidas_fusao = acs}
                  False ->
                    let
                      av = turno.acoes_vendidas_fusao
                      av1 = acao :: av
                    in
                     {turno | acoes_vendidas_fusao = av1}

              me0 = Jogador.me model.me jogo.jogadores
              me1 = Jogador.comprar_acao me0 empresa
              me2 = {me1 | online = True}
              jogo1 = Jogador.atualiza_acoes jogo me2
              jogadores1 = Jogador.atualiza_me me2 jogo.jogadores
              empresas = Fn.aumenta_uma_acao_da_empresa empresa jogo.empresas
              jogo2 = {jogo1 | turno = turno1, empresas = empresas, jogadores = jogadores1}
            in
              (me2, jogo2)
      in
        ({ model | me = me1, meu_jogo = Just jogo1} ,  Cmd.batch [Sound.play_sound("AMONEY")])

    VendeAcaoFusao empresa jogador jogo ->
      let
        (me1, jogo1) = 
            let
              turno = jogo.turno
              acao = {empresa = empresa.eid, jogador = model.me.uid, acoes = 1, valor = empresa.preco}
              turno1 = 
                let
                  av = turno.acoes_vendidas_fusao
                  av1 = acao :: av
                in
                 {turno | acoes_vendidas_fusao = av1}

              me0 = Jogador.me model.me jogo.jogadores
              me1 = Jogador.vender_acao me0 empresa
              me2 = {me1 | online = True}
              jogo1 = Jogador.atualiza_acoes jogo me2
              jogadores1 = Jogador.atualiza_me me2 jogo.jogadores
              empresas = Fn.aumenta_uma_acao_da_empresa empresa jogo.empresas
              jogo2 = {jogo1 | turno = turno1, empresas = empresas, jogadores = jogadores1}
            in
              (me2, jogo2)
      in
        ({ model | me = me1, meu_jogo = Just jogo1} ,  Cmd.batch[Sound.play_sound("MONEY")])

    FinalizarJogada dialogo->
      case model.meu_jogo of
        Nothing ->
          (model, Cmd.none)
        Just jogo ->
          let

            model1 = case jogo.criador_id == model.me.uid of
              True -> Fn.finaliza_jogada model model.me.uid jogo
              False -> 
                model

            dialogos = List.filter (\x -> 
                  x /= dialogo
                ) model1.dialogos
          in
            ({model1 | dialogos = dialogos}, Cmd.batch [CmdX.perform EnviaTurno, Sound.play_sound("CLICK")])
    OkDiag dialogo->
      let
        dialogos = List.filter (\x -> 
              x /= dialogo
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch[Sound.play_sound("CLICK")])
    SelecionaEmpresaParaFusao dialogo empresa jogo falencias->
      let
        dialogos = List.filter (\x -> 
              x /= dialogo
            ) model.dialogos

        turno = case jogo.turno.atualizar_pecas of
          Nothing -> jogo.turno
          Just ac ->
            let
              falencias1 = List.filter (\x -> x.eid /= empresa.eid) falencias
              ac1 = {ac | empresa = Just empresa, falencias = falencias1}
              turno = jogo.turno
            in
              {turno | atualizar_pecas = Just ac1}
        dialogo = novo_dialogo_funde_empresas
        dialogos1 = dialogo :: dialogos
        jogo1 = Just { jogo | turno = turno}

      in
        ({model | dialogos = dialogos1, meu_jogo = jogo1}, Cmd.batch[CmdX.perform EnviaTurnoAtualizado,Sound.play_sound("CLICK")]  )


    SelecionaFusaoEmpresa dialogo empresa empresas1 bonus falencia->
      let
        (dialogos, me, jogo2) = case model.meu_jogo of
                  Nothing -> (model.dialogos, model.me, Nothing )
                  Just jogo -> 
                    case jogo.turno.atualizar_pecas of
                      Nothing -> (model.dialogos, model.me, Just jogo)
                      Just ap ->
                        let
                          me = model.me

                          peca = case jogo.turno.peca of
                            Nothing -> []
                            Just pe -> [pe]
                          pecas =  ap.pecas
                          pecas1 =  List.concat [peca, pecas]
                          pecas2 = (List.map (\p1 ->
                              {p1 | empresa = Just empresa.eid}
                            ) pecas1)
                          falencias1 = List.filter (\x -> x.eid /= empresa.eid) falencia
                          ap1 = {ap | empresa = Just empresa, pecas = pecas2, falencias = falencias1}
                          board0 =List.map (\x -> 
                              if x.empresa == Just empresa.eid then
                                {x | sede = False}
                              else
                                x
                            ) jogo.board
                          board = Fn.atualiza_board board0 pecas2
                          empresas_id = List.map (\x ->
                              x.eid
                            ) empresas1
                          board1 = List.map (\x ->
                              case x.empresa of
                                Nothing -> x
                                Just empr ->
                                  case List.member empr empresas_id of
                                    True ->
                                      {x | empresa = Just empresa.eid}
                                    False ->
                                      x 
                            ) board

                          empresas2 = List.map (\e -> 
                              case List.member e.eid empresas_id of
                                    True ->
                                      {e |disponivel = True}
                                    False ->
                                      e 
                            ) jogo.empresas
                          empresas = List.map (\e -> 

                              if e.eid == empresa.eid then
                                {e | disponivel = False}
                              else
                                e
                            ) empresas2


                          turno = jogo.turno
                          bn_1 = turno.bonus
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
                                      List.concat([bn_1, os_dois])
                                  Nothing ->
                                    case bonus.primeiro of
                                      Nothing ->
                                        bn_1
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
                                            List.concat([primeiro,segundo, bn_1])
                              ) bonus) |> List.concat

                          empresas_falencia = (List.map(\x -> x.empresa) bonus2) |> ListX.unique
                          jogadores_para_vender_acoes = (List.filterMap (\x -> 
                                  let
                                    maped = List.filterMap(\y -> 
                                      if List.member y.empresa empresas_falencia then
                                        Just {jogador = x.uid, empresa = y.empresa }
                                      else
                                        Nothing
                                      ) x.acoes
                                    _ = Debug.log "maped" maped
                                  in
                                    if List.length maped > 0 then
                                      Just maped
                                    else 
                                      Nothing
                                  ) jogo.jogadores) |>  List.concat

                          
                          _ = Debug.log "jogadores para" (empresas_falencia,jogadores_para_vender_acoes)
                          dialogos = model.dialogos
                          dialogos1 = 
                            case jogadores_para_vender_acoes of
                              [] ->
                                model.dialogos
                              jj -> 
                                let
                                  dialogos1 = List.map(\x ->
                                      novo_dialogo_vender_acoes_empresa_fundida x.empresa  x.jogador
                                    ) jj
                                in
                                  List.concat([dialogos1, dialogos])
                              
                          

                          jogadores = Jogador.atualizar_bonus bonus2 jogo.jogadores
                          turno1 = {turno | atualizar_pecas = (Just ap1), bonus = bonus2}
                          jogo2 = { jogo | empresas = empresas, turno = turno1, board = board1, jogadores = jogadores}
                        in
                          (dialogos1, me, Just jogo2)
        dialogos1 = List.filter (\x -> 
            x /= dialogo
          ) dialogos
      in
        ({model | me = me,  meu_jogo = jogo2, dialogos = dialogos1}, Cmd.batch [CmdX.perform EnviaPagaBonus, CmdX.perform EnviaTurnoAtualizado, CmdX.perform EnviaDialogoFusao, Sound.play_sound("CLICK")])
    SelecionaNovaEmpresa dialogo empresa ->
      let
        (me, jogo2) = case model.meu_jogo of
                  Nothing -> (model.me, Nothing )
                  Just jogo -> 
                    case jogo.turno.atualizar_pecas of
                      Nothing -> (model.me, Just jogo)
                      Just ap ->
                        let
                          me = model.me
                          peca = case jogo.turno.peca of
                            Nothing -> []
                            Just pe -> [{pe|sede = True}]
                          pecas = ap.pecas
                          pecas1 =  List.concat [peca, pecas]
                          pecas2 = (List.map (\p1 ->
                              {p1 | empresa = Just empresa.eid}
                            ) pecas1)
                          ap1 = {ap | empresa = Just empresa, pecas = pecas2}

                          board = Fn.atualiza_board jogo.board pecas2
                          empresas = List.map (\e -> 
                              if e.eid == empresa.eid then
                                {e | disponivel = False}
                              else
                                e
                            ) jogo.empresas

                          acoes = case (ListX.find(\e -> e.empresa == empresa.eid) me.acoes) of
                                    Nothing ->
                                      let
                                        acao = {empresa = empresa.eid, acoes = 1}
                                        acoes1 = acao :: me.acoes 
                                      in
                                        acoes1
                                    Just a ->
                                      List.map (\l -> 
                                        if l.empresa == a.empresa then
                                          { l | acoes = (l.acoes+1)}
                                        else
                                          l

                                        ) me.acoes 


                          me1 = { me | acoes = acoes}
                          jogo1 = Jogador.atualiza_acoes jogo me1
                          turno = jogo.turno
                          turno1 = {turno | atualizar_pecas = (Just ap1)}
                          jogo2 = { jogo1 | empresas = empresas, turno = turno1, board = board}
                        in
                          (me1, Just jogo2)
        dialogos = List.filter (\x -> 
            x /= dialogo
          ) model.dialogos
      in
        ({model | me = me,  meu_jogo = jogo2, dialogos = dialogos}, Cmd.batch [CmdX.perform EnviaTurnoAtualizado,Sound.play_sound("CLICK")])
    
    ConfirmaVendaAcaoFusao dialogo->
      let
        dialogos = List.filter (\x -> 
              x /= dialogo
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch [CmdX.perform EnviaVendaAcaoFusao,  CmdX.perform (EnviaCancelaWebSocketDiag dialogo),Sound.play_sound("CLICK")])

    CancelarWebsocketDiag dialogo->
      let
        dialogos = List.filter (\x -> 
              x /= dialogo
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch [CmdX.perform EnviaTurnoAtualizado,  CmdX.perform (EnviaCancelaWebSocketDiag dialogo),Sound.play_sound("CLICK")])

    CancelarDiag dialogo->
      let
        dialogos = List.filter (\x -> 
              x /= dialogo
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch[Sound.play_sound("CLICK")])

    MinimizarDiag dialogo->
      let
        dialogos = List.map (\x -> 
              if x == dialogo then
                {x| minimizado = True}
              else
                x
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch[Sound.play_sound("CLICK")])
    MaximizarDiag dialogo->
      let
        dialogos = List.map (\x -> 
              if x == dialogo then
                {x| minimizado = False}
              else
                x
            ) model.dialogos
      in
        ({model | dialogos = dialogos}, Cmd.batch[Sound.play_sound("CLICK")])

    SelecionaAvatar dialogo url ->
      let
        jogo = case model.meu_jogo of
                  Nothing -> Nothing 
                  Just jogo -> Just (Jogador.atualiza_avatar jogo model.me url)
        me = model.me
        me1 = {me | avatar = url}
        dialogos = List.filter (\x -> 
            x /= dialogo
          ) model.dialogos
      in
        ({model | meu_jogo = jogo, dialogos = dialogos, me = me1}, Cmd.batch[CmdX.perform EnviaMe,Sound.play_sound("CLICK")])
    _ -> 
      (model, Cmd.none)