server <- function(input, output, session) {
  planilha_base <- reactiveVal(NULL)
  resultado <- reactiveVal(NULL)
  mensagens <- reactiveVal(NULL)
  arquivo_atual <- reactiveVal(NULL)

  # ---------------------------------------------------------
  # Estrutura do experimento
  # ---------------------------------------------------------

  aplicar_estrutura <- function(dados, estrutura) {
    colunas <- names(dados)
    updateRadioButtons(session, "delineamento", selected = estrutura$delineamento)
    updateSelectInput(session, "coluna_bloco", choices = colunas, selected = estrutura$bloco)
    updateSelectizeInput(session, "colunas_fatores", choices = colunas, selected = estrutura$fatores)
    updateSelectizeInput(session, "colunas_respostas", choices = colunas, selected = estrutura$respostas)
  }

  carregar_planilha <- function(dados, estrutura = sugerir_estrutura(dados)) {
    planilha_base(dados)
    resultado(NULL)
    mensagens(NULL)
    aplicar_estrutura(dados, estrutura)
  }

  dados_atuais <- reactive({
    base <- planilha_base()
    req(base)
    editada <- input$planilha
    if (is.null(editada)) {
      return(base)
    }
    dados <- tryCatch(hot_to_r(editada), error = function(e) NULL)
    if (is.null(dados) || !identical(names(dados), names(base))) {
      return(base)
    }
    dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
    dados[] <- lapply(dados, function(coluna) {
      texto <- as.character(coluna)
      texto[is.na(texto)] <- ""
      texto
    })
    dados
  })

  # ---------------------------------------------------------
  # Entrada: digitar
  # ---------------------------------------------------------

  fatores_digitados <- function() {
    n <- as.integer(input$n_fatores)
    lapply(seq_len(n), function(i) {
      list(
        nome = trimws(input[[paste0("fator_nome_", i)]] %||% ""),
        niveis = separar_lista(input[[paste0("fator_niveis_", i)]])
      )
    })
  }

  validar_digitacao <- function(fatores, respostas, n_rep) {
    erros <- character()
    nomes <- vapply(fatores, function(f) f$nome, character(1))
    for (i in seq_along(fatores)) {
      if (!nzchar(fatores[[i]]$nome)) {
        erros <- c(erros, sprintf("Informe o nome do fator %d.", i))
      }
      if (length(fatores[[i]]$niveis) < 2) {
        erros <- c(erros, sprintf("O fator %d precisa de pelo menos dois níveis diferentes.", i))
      }
    }
    if (length(respostas) == 0) {
      erros <- c(erros, "Informe pelo menos uma variável resposta.")
    }
    todos <- c(nomes, respostas, "Bloco", "Repetição")
    if (any(duplicated(tolower(todos[nzchar(todos)])))) {
      erros <- c(erros, "Os nomes de fatores e variáveis resposta devem ser diferentes entre si e diferentes de Bloco e Repetição.")
    }
    if (is.na(n_rep) || n_rep < 2 || n_rep > 50) {
      erros <- c(erros, "Informe entre 2 e 50 repetições ou blocos.")
    }
    n_linhas <- prod(vapply(fatores, function(f) length(f$niveis), numeric(1))) * max(n_rep, 0, na.rm = TRUE)
    if (length(erros) == 0 && n_linhas > 5000) {
      erros <- c(erros, "A planilha teria mais de 5.000 linhas. Reduza o número de níveis ou de repetições.")
    }
    erros
  }

  montar_planilha_digitada <- function(mostrar_erros = TRUE) {
    fatores <- fatores_digitados()
    respostas <- separar_lista(input$respostas_digitar)
    n_rep <- suppressWarnings(as.integer(input$n_repeticoes))
    erros <- validar_digitacao(fatores, respostas, n_rep)
    if (length(erros) > 0) {
      if (mostrar_erros) {
        showNotification(tagList(lapply(erros, tags$div)), type = "error", duration = 8)
      }
      return(invisible(FALSE))
    }

    delineamento <- input$delineamento %||% "DIC"
    dados <- gerar_planilha_modelo(fatores, n_rep, delineamento, respostas)
    carregar_planilha(dados, list(
      delineamento = delineamento,
      bloco = if (identical(delineamento, "DBC")) "Bloco" else "",
      fatores = vapply(fatores, function(f) f$nome, character(1)),
      respostas = respostas
    ))
    invisible(TRUE)
  }

  session$onFlushed(function() {
    isolate(if (is.null(planilha_base())) montar_planilha_digitada(mostrar_erros = FALSE))
  }, once = TRUE)

  observeEvent(input$montar_planilha, {
    if (isTRUE(montar_planilha_digitada())) {
      showNotification("Planilha montada. Digite ou cole os valores das variáveis resposta.", type = "message", duration = 5)
    }
  })

  # ---------------------------------------------------------
  # Entrada: importar
  # ---------------------------------------------------------

  importar_arquivo <- function(aba = NULL) {
    arquivo <- arquivo_atual()
    req(arquivo)
    dados <- tryCatch(
      ler_arquivo_dados(arquivo$datapath, arquivo$name, aba),
      error = function(e) {
        showNotification(paste("Não foi possível ler o arquivo:", conditionMessage(e)), type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(dados)) {
      carregar_planilha(dados_para_planilha(dados))
      showNotification(
        sprintf("Arquivo importado: %d linhas e %d colunas. Confira a estrutura sugerida.", nrow(dados), ncol(dados)),
        type = "message",
        duration = 6
      )
    }
  }

  observeEvent(input$importar_colados, {
    dados <- tryCatch(
      ler_texto_colado(input$dados_colados, isTRUE(input$colado_cabecalho)),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(dados)) {
      carregar_planilha(dados_para_planilha(dados))
      showNotification(
        sprintf("Dados colados: %d linhas e %d colunas. Confira a estrutura sugerida.", nrow(dados), ncol(dados)),
        type = "message",
        duration = 6
      )
    }
  })

  observeEvent(input$arquivo_dados, {
    arquivo_atual(input$arquivo_dados)
    importar_arquivo()
  })

  output$seletor_aba <- renderUI({
    arquivo <- arquivo_atual()
    req(arquivo)
    req(tolower(tools::file_ext(arquivo$name)) %in% c("xlsx", "xls"))
    abas <- tryCatch(ler_planilhas_excel(arquivo$datapath), error = function(e) character())
    req(length(abas) > 1)
    selectInput("aba_excel", "Aba da planilha", choices = abas, selected = abas[1])
  })

  observeEvent(input$aba_excel, {
    importar_arquivo(input$aba_excel)
  }, ignoreInit = TRUE)

  output$baixar_modelo <- downloadHandler(
    filename = function() "modelo_ranova.xlsx",
    content = function(file) {
      writexl::write_xlsx(dados_exemplo(), file)
    }
  )

  # ---------------------------------------------------------
  # Entrada: exemplo
  # ---------------------------------------------------------

  observeEvent(input$carregar_exemplo, {
    dados <- dados_exemplo()
    carregar_planilha(dados, list(
      delineamento = "DBC",
      bloco = "Bloco",
      fatores = c("Dose", "Cultivar"),
      respostas = names(dados)[4:6]
    ))
    showNotification("Exemplo carregado. Clique em Analisar para ver os resultados.", type = "message", duration = 5)
  })

  # ---------------------------------------------------------
  # Planilha editável
  # ---------------------------------------------------------

  output$planilha <- renderRHandsontable({
    dados <- planilha_base()
    req(dados)
    altura <- min(460, 32 + 24 * max(nrow(dados), 8))
    rhandsontable(
      dados,
      rowHeaders = TRUE,
      stretchH = "all",
      height = altura,
      useTypes = TRUE,
      search = FALSE
    ) |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      hot_cols(colWidths = 120, manualColumnResize = TRUE) |>
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$resumo_planilha <- renderUI({
    dados <- planilha_base()
    req(dados)
    atual <- dados_atuais()
    sprintf("%d LINHAS · %d COLUNAS", nrow(atual), ncol(atual))
  })

  output$baixar_dados <- downloadHandler(
    filename = function() paste0("ranova_dados_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      dados <- dados_atuais()
      writexl::write_xlsx(dados, file)
    }
  )

  # ---------------------------------------------------------
  # Análise
  # ---------------------------------------------------------

  observeEvent(input$analisar, {
    dados <- tryCatch(dados_atuais(), error = function(e) NULL)
    if (is.null(dados)) {
      mensagens(list(tipo = "erro", itens = "Monte, importe ou carregue uma planilha antes de analisar."))
      return()
    }

    prep <- preparar_dados_analise(
      dados = dados,
      delineamento = input$delineamento,
      bloco = input$coluna_bloco,
      fatores = input$colunas_fatores,
      respostas = input$colunas_respostas
    )
    if (!isTRUE(prep$ok)) {
      mensagens(list(tipo = "erro", itens = prep$erros))
      return()
    }

    opcoes <- list(
      alpha = as.numeric(input$alpha),
      digitos = as.integer(input$digitos),
      digitos_anova = max(2L, as.integer(input$digitos) + 1L),
      formato = input$formato_anova,
      tipo_se = input$tipo_se
    )

    analise <- withProgress(message = "Analisando o experimento...", value = 0.3, {
      executar_analise(prep, opcoes)
    })

    avisos <- character()
    if (prep$linhas_descartadas > 0) {
      avisos <- c(avisos, sprintf("%d linha(s) com fatores ou blocos em branco foram ignoradas.", prep$linhas_descartadas))
    }
    n_ausentes <- sum(vapply(prep$respostas, function(v) sum(is.na(prep$dados[[v]])), numeric(1)))
    if (n_ausentes > 0) {
      avisos <- c(avisos, sprintf("%d valor(es) ausente(s) nas variáveis resposta foram desconsiderados no ajuste do modelo.", n_ausentes))
    }
    if (!isTRUE(analise$anova$ok)) {
      mensagens(list(tipo = "erro", itens = c("Não foi possível ajustar a ANOVA com os dados informados.", analise$anova$erro)))
      return()
    }

    mensagens(if (length(avisos) > 0) list(tipo = "aviso", itens = avisos) else NULL)
    resultado(analise)
    updateTabsetPanel(session, "abas_resultado", selected = "anova")
  })

  output$mensagens_analise <- renderUI({
    msg <- mensagens()
    req(msg)
    classe <- if (identical(msg$tipo, "erro")) "aviso-analise aviso-erro" else "aviso-analise"
    div(
      class = classe,
      role = "alert",
      icon(if (identical(msg$tipo, "erro")) "exclamation-circle" else "info-circle"),
      div(
        tags$strong(if (identical(msg$tipo, "erro")) "Confira os dados" else "Observações"),
        tags$ul(lapply(msg$itens, tags$li))
      )
    )
  })

  opcoes_fatores <- function(prep) {
    stats::setNames(prep$fatores, rotulo(prep, prep$fatores))
  }

  opcoes_respostas <- function(prep) {
    stats::setNames(prep$respostas, rotulo(prep, prep$respostas))
  }

  output$painel_resultados <- renderUI({
    res <- resultado()
    if (is.null(res)) {
      return(div(
        class = "nenhum-resultado",
        icon("chart-column"),
        tags$b("Nenhuma análise ainda"),
        tags$span("Monte ou importe a planilha, confira a estrutura do experimento e clique em Analisar.")
      ))
    }

    prep <- res$prep
    varios_fatores <- length(prep$fatores) >= 2
    fatores <- opcoes_fatores(prep)
    respostas <- opcoes_respostas(prep)

    abas <- list(
      tabPanel(
        title = "ANOVA",
        value = "anova",
        uiOutput("resumo_significancia"),
        div(class = "tabela-rolagem", tabela_html(res$anova$valor))
      ),
      tabPanel(
        title = "Pressupostos",
        value = "pressupostos",
        if (isTRUE(res$diagnostico$ok)) tabela_diagnostico_html(res$diagnostico$valor) else tags$p(class = "texto-erro", res$diagnostico$erro),
        tags$p(class = "explicacao nota-resultado", "Shapiro-Wilk avalia a normalidade dos resíduos e Levene avalia a homogeneidade de variâncias entre tratamentos. Quando algum pressuposto não for atendido, considere transformar a variável ou usar outro modelo."),
        selectInput("var_residuos", "Resíduos da variável", choices = respostas, width = "100%"),
        plotOutput("grafico_residuos", height = 330)
      ),
      tabPanel(
        title = "Médias",
        value = "medias",
        uiOutput("aviso_medias_interacao"),
        lapply(names(res$medias), function(fator) {
          item <- res$medias[[fator]]
          div(
            class = "tabela-rolagem bloco-tabela",
            if (isTRUE(item$ok)) tabela_html(item$valor) else tags$p(class = "texto-erro", paste0(rotulo(prep, fator), ": ", item$erro))
          )
        }),
        tags$p(class = "explicacao nota-resultado", "Médias seguidas pela mesma letra não diferem entre si pelo teste t (2 níveis) ou Tukey (3 ou mais níveis).")
      )
    )

    if (varios_fatores) {
      abas[[length(abas) + 1]] <- tabPanel(
        title = "Interação",
        value = "interacao",
        div(
          class = "grade-campos",
          selectInput("fator_linha", "Fator nas linhas", choices = fatores, selected = prep$fatores[1]),
          selectInput("fator_coluna", "Fator nas colunas", choices = fatores, selected = prep$fatores[2])
        ),
        uiOutput("tabela_interacao")
      )
    }

    abas[[length(abas) + 1]] <- tabPanel(
      title = "Gráficos",
      value = "graficos",
      div(class = "titulo-grafico", "Médias com letras"),
      div(
        class = "grade-campos",
        selectInput("var_grafico", "Variável resposta", choices = respostas),
        selectInput("fator_grafico", "Fator", choices = fatores)
      ),
      plotOutput("grafico_medias", height = 360),
      controles_exportacao("medias", largura = 17, altura = 11),
      if (varios_fatores) {
        tagList(
          tags$hr(),
          div(class = "titulo-grafico", "Gráfico de interação"),
          div(
            class = "grade-campos",
            selectInput("fator_x", "Fator no eixo X", choices = fatores, selected = prep$fatores[1]),
            selectInput("fator_traco", "Fator nas linhas (cores)", choices = fatores, selected = prep$fatores[2])
          ),
          plotOutput("grafico_interacao", height = 360),
          controles_exportacao("interacao", largura = 17, altura = 11)
        )
      }
    )

    tagList(
      div(
        class = "etiquetas-resultado",
        tags$span(class = "etiqueta", if (identical(prep$delineamento, "DBC")) "DBC" else "DIC"),
        tags$span(class = "etiqueta", paste(rotulo(prep, prep$fatores), collapse = " × ")),
        tags$span(class = "etiqueta", sprintf("%d observações", prep$n_obs)),
        tags$span(class = "etiqueta", paste0("Significância ", formatC(res$opcoes$alpha * 100, format = "f", digits = 0), "%"))
      ),
      do.call(tabsetPanel, c(list(id = "abas_resultado"), abas))
    )
  })

  output$info_relatorio <- renderText({
    res <- resultado()
    if (is.null(res)) {
      return("Faça a análise para liberar o relatório.")
    }
    paste0(
      "Análise de ", length(res$prep$respostas), " variável(is) resposta com ",
      length(res$prep$fatores), " fator(es) e ", res$prep$n_obs, " observações."
    )
  })

  output$botao_relatorio <- renderUI({
    if (is.null(resultado())) {
      return(tags$button(class = "btn btn-pdf", disabled = "disabled", icon("download"), " Baixar relatório PDF"))
    }
    downloadButton("baixar_relatorio", "Baixar relatório PDF", icon = icon("download"), class = "btn-pdf")
  })

  significancia_atual <- reactive({
    res <- resultado()
    req(res, isTRUE(res$significancia$ok))
    res$significancia$valor
  })

  output$resumo_significancia <- renderUI({
    res <- resultado()
    req(res)
    prep <- res$prep
    sig <- significancia_atual()
    interacoes <- sig[sig$interacao & sig$significativo, , drop = FALSE]
    principais <- sig[!sig$interacao & sig$significativo, , drop = FALSE]

    negrito <- function(x) paste0("<strong>", htmltools::htmlEscape(x), "</strong>")
    itens <- character()
    for (i in seq_len(nrow(interacoes))) {
      itens <- c(itens, paste0(
        negrito(rotulo(prep, interacoes$variavel[i])), ": interação ",
        negrito(rotulo_termo(prep, interacoes$termo[i])), " significativa (p = ",
        formatar_p(interacoes$p[i]), "). Interprete pelo desdobramento na aba Interação."
      ))
    }
    sem_interacao <- setdiff(unique(principais$variavel), unique(interacoes$variavel))
    for (v in sem_interacao) {
      termos <- principais$termo[principais$variavel == v]
      itens <- c(itens, paste0(
        negrito(rotulo(prep, v)), ": efeito significativo de ",
        htmltools::htmlEscape(paste(vapply(termos, function(t) rotulo_termo(prep, t), character(1)), collapse = ", ")),
        ". Veja a aba Médias."
      ))
    }
    sem_efeito <- setdiff(prep$respostas, unique(sig$variavel[sig$significativo]))
    for (v in sem_efeito) {
      itens <- c(itens, paste0(negrito(rotulo(prep, v)), ": nenhum efeito significativo dos fatores."))
    }

    div(
      class = "leitura-rapida",
      tags$strong("Leitura rápida"),
      tags$ul(lapply(itens, function(item) tags$li(HTML(item))))
    )
  })

  output$aviso_medias_interacao <- renderUI({
    res <- resultado()
    req(res, length(res$prep$fatores) >= 2)
    sig <- significancia_atual()
    variaveis <- unique(sig$variavel[sig$interacao & sig$significativo])
    req(length(variaveis) > 0)
    div(
      class = "aviso-grupos",
      icon("triangle-exclamation"),
      span(paste0(
        "Há interação significativa para ", paste(rotulo(res$prep, variaveis), collapse = ", "),
        ". Nesses casos, as médias de cada fator isolado devem ser interpretadas com cautela; prefira o desdobramento na aba Interação."
      ))
    )
  })

  output$grafico_residuos <- renderPlot({
    res <- resultado()
    req(res, input$var_residuos %in% res$prep$respostas)
    grafico_residuos(res$prep, input$var_residuos)
  }, res = 96)

  fatores_interacao <- reactive({
    res <- resultado()
    req(res, input$fator_linha, input$fator_coluna)
    validate(need(input$fator_linha != input$fator_coluna, "Escolha fatores diferentes para linhas e colunas."))
    req(all(c(input$fator_linha, input$fator_coluna) %in% res$prep$fatores))
    c(input$fator_linha, input$fator_coluna)
  })

  output$tabela_interacao <- renderUI({
    res <- resultado()
    pares <- fatores_interacao()
    tabela <- tabela_interacao(res$prep, res$opcoes, pares[1], pares[2])
    if (!isTRUE(tabela$ok)) {
      return(tags$p(class = "texto-erro", paste("Não foi possível gerar o desdobramento:", tabela$erro)))
    }
    tagList(
      div(class = "tabela-rolagem", tabela_html(tabela$valor)),
      tags$p(
        class = "explicacao nota-resultado",
        "Letras maiúsculas comparam os níveis do fator nas colunas dentro de cada linha; letras minúsculas comparam os níveis do fator nas linhas dentro de cada coluna.",
        if (length(res$prep$fatores) == 3) " Com três fatores, as médias são ajustadas sobre os níveis do fator não exibido."
      )
    )
  })

  grafico_medias_atual <- reactive({
    res <- resultado()
    req(res, input$var_grafico %in% res$prep$respostas, input$fator_grafico %in% res$prep$fatores)
    grafico <- tentar(grafico_medias(res$prep, res$opcoes, input$var_grafico, input$fator_grafico, rotulos()))
    validate(need(isTRUE(grafico$ok), paste("Não foi possível gerar o gráfico:", grafico$erro)))
    grafico$valor
  })

  grafico_interacao_atual <- reactive({
    res <- resultado()
    req(res, input$var_grafico %in% res$prep$respostas, input$fator_x, input$fator_traco)
    validate(need(input$fator_x != input$fator_traco, "Escolha fatores diferentes para o eixo X e para as linhas."))
    grafico <- tentar(grafico_interacao(res$prep, input$var_grafico, input$fator_x, input$fator_traco, rotulos()))
    validate(need(isTRUE(grafico$ok), paste("Não foi possível gerar o gráfico:", grafico$erro)))
    grafico$valor
  })

  output$grafico_medias <- renderPlot(grafico_medias_atual(), res = 96)
  output$grafico_interacao <- renderPlot(grafico_interacao_atual(), res = 96)

  nome_grafico <- function(prefixo, nome, formato) {
    paste0("ranova_", prefixo, "_", nome_seguro(nome), ".", if (identical(formato, "tiff")) "tif" else "png")
  }

  # Baixa um gráfico no formato, resolução e tamanho (cm) escolhidos nos controles `prefixo_*`.
  download_grafico <- function(prefixo, grafico, nome) {
    downloadHandler(
      filename = function() nome_grafico(prefixo, nome(), input[[paste0(prefixo, "_formato")]] %||% "png"),
      content = function(file) {
        largura <- max(5, min(60, as.numeric(input[[paste0(prefixo, "_largura")]] %||% 17)))
        altura <- max(4, min(60, as.numeric(input[[paste0(prefixo, "_altura")]] %||% 11)))
        salvar_grafico(file, grafico(), input[[paste0(prefixo, "_formato")]] %||% "png",
                       input[[paste0(prefixo, "_dpi")]] %||% 300, largura / 2.54, altura / 2.54)
      }
    )
  }

  output$baixar_medias <- download_grafico("medias", grafico_medias_atual, function() rotulo(resultado()$prep, input$var_grafico))
  output$baixar_interacao <- download_grafico("interacao", grafico_interacao_atual, function() rotulo(resultado()$prep, input$var_grafico))

  # ---------------------------------------------------------
  # Nomes nos gráficos e painel agrupado
  # ---------------------------------------------------------

  rotulos <- reactive({
    res <- resultado()
    if (is.null(res)) return(list())
    ids <- c(res$prep$respostas, res$prep$fatores)
    valores <- lapply(ids, function(id) input[[paste0("rotulo_", id)]] %||% "")
    stats::setNames(valores, ids)
  })

  output$painel_graficos_ui <- renderUI({
    res <- resultado()
    if (is.null(res)) {
      return(div(
        class = "nenhum-resultado",
        icon("images"),
        tags$b("Disponível após a análise"),
        tags$span("Depois de analisar, escolha as variáveis, a ordem e os nomes para montar um painel com letras A, B, C...")
      ))
    }
    prep <- res$prep
    fatores <- opcoes_fatores(prep)
    varios <- length(prep$fatores) >= 2
    tagList(
      fluidRow(
        column(6,
          radioButtons("painel_tipo", "Tipo de gráfico", inline = TRUE,
            choices = c("Médias com letras" = "medias", if (varios) c("Interação" = "interacao")), selected = "medias"),
          conditionalPanel("input.painel_tipo == 'medias'",
            selectInput("painel_fator", "Fator no eixo X", choices = fatores, width = "100%")),
          if (varios) conditionalPanel("input.painel_tipo == 'interacao'",
            div(class = "grade-campos",
              selectInput("painel_fator_x", "Fator no eixo X", choices = fatores, selected = prep$fatores[1]),
              selectInput("painel_fator_traco", "Fator nas cores", choices = fatores, selected = prep$fatores[2])))
        ),
        column(6,
          selectizeInput("painel_variaveis", "Variáveis, na ordem do painel", choices = opcoes_respostas(prep),
            selected = prep$respostas, multiple = TRUE, width = "100%",
            options = list(plugins = list("remove_button"), placeholder = "Escolha as variáveis")),
          div(class = "grade-campos",
            selectInput("painel_colunas", "Gráficos por linha", choices = 1:4, selected = min(2, length(prep$respostas))),
            div(class = "caixa-letras", checkboxInput("painel_letras", "Identificar com letras (A, B, C...)", TRUE))
          )
        )
      ),
      div(class = "explicacao", "A ordem das variáveis no campo acima define a posição e a letra de cada gráfico. Para mudar a ordem, remova (×) e selecione de novo."),
      uiOutput("painel_nomes"),
      div(class = "area-painel", plotOutput("painel_grafico", height = "auto")),
      controles_exportacao("painel", largura = 17, altura = 14),
      checkboxInput("painel_no_pdf", "Incluir este painel no relatório PDF", TRUE)
    )
  })

  output$painel_nomes <- renderUI({
    res <- resultado()
    req(res)
    prep <- res$prep
    fatores_usados <- if (identical(input$painel_tipo, "interacao")) c(input$painel_fator_x, input$painel_fator_traco) else input$painel_fator
    ids <- c(input$painel_variaveis, fatores_usados)
    ids <- ids[ids %in% c(prep$respostas, prep$fatores)]
    req(length(ids) > 0)
    div(
      class = "caixa-nomes",
      div(class = "titulo-legenda", icon("pen"), " Nomes nos gráficos (eixos e legendas)"),
      div(class = "grade-nomes", lapply(ids, function(id) {
        textInput(paste0("rotulo_", id), rotulo(prep, id), value = isolate(input[[paste0("rotulo_", id)]]) %||% rotulo(prep, id), width = "100%")
      }))
    )
  })

  painel_atual <- reactive({
    res <- resultado()
    req(res, length(input$painel_variaveis) > 0, input$painel_tipo)
    variaveis <- input$painel_variaveis[input$painel_variaveis %in% res$prep$respostas]
    req(length(variaveis) > 0)
    if (identical(input$painel_tipo, "interacao")) {
      req(input$painel_fator_x, input$painel_fator_traco)
      validate(need(input$painel_fator_x != input$painel_fator_traco, "Escolha fatores diferentes para o eixo X e para as cores."))
    } else {
      req(input$painel_fator %in% res$prep$fatores)
    }
    ncol <- as.integer(input$painel_colunas %||% 2)
    grafico <- tentar(painel_graficos(
      res$prep, res$opcoes, variaveis, input$painel_tipo,
      fator = input$painel_fator, fator_x = input$painel_fator_x, fator_traco = input$painel_fator_traco,
      rotulos = rotulos(), ncol = ncol, letras = isTRUE(input$painel_letras)
    ))
    validate(need(isTRUE(grafico$ok), paste("Não foi possível montar o painel:", grafico$erro)))
    list(grafico = grafico$valor, dimensoes = dimensoes_painel(length(variaveis), ncol))
  })

  output$painel_grafico <- renderPlot({
    painel_atual()$grafico
  }, res = 96, height = function() {
    dims <- tryCatch(painel_atual()$dimensoes, error = function(e) NULL)
    if (is.null(dims)) 300 else round(dims$altura / dims$largura * min(900, session$clientData$output_painel_grafico_width %||% 800))
  })

  output$baixar_painel <- download_grafico("painel", function() painel_atual()$grafico, function() "painel")

  output$baixar_relatorio <- downloadHandler(
    filename = function() paste0("ranova_relatorio_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf"),
    content = function(file) {
      res <- resultado()
      req(res)
      pares <- if (length(res$prep$fatores) >= 2) {
        escolhidos <- c(input$fator_linha, input$fator_coluna)
        if (length(escolhidos) == 2 && escolhidos[1] != escolhidos[2]) escolhidos else res$prep$fatores[1:2]
      }
      painel <- if (isTRUE(input$painel_no_pdf)) tryCatch(painel_atual(), error = function(e) NULL)
      withProgress(message = "Gerando relatório PDF...", value = 0.4, {
        gerar_relatorio_pdf(
          res, file, pares[1], pares[2],
          titulo = input$titulo_relatorio,
          responsavel = input$responsavel_relatorio,
          descricao = input$descricao_relatorio,
          rotulos = rotulos(),
          painel = painel
        )
      })
    }
  )
}
