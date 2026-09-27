fator_ui <- function(i, nome, niveis) {
  div(
    class = "linha-fator",
    textInput(paste0("fator_nome_", i), paste("Fator", i), value = nome, width = "100%"),
    textInput(paste0("fator_niveis_", i), "Níveis (separados por ;)", value = niveis, width = "100%")
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$meta(name = "author", content = "Marlenildo"),
    tags$meta(name = "description", content = "Ranova: análise de variância de experimentos fatoriais em DIC e DBC, com médias, desdobramento, gráficos e relatório em PDF."),
    tags$link(rel = "icon", type = "image/png", href = "img/favicon.png"),
    tags$title("Ranova · Análise de variância de experimentos fatoriais"),
    htmltools::htmlDependency(
      name = "lightable",
      version = "0.0.1",
      src = system.file("lightable-0.0.1", package = "kableExtra"),
      stylesheet = "lightable.css"
    ),
    # Aviso antes de sair da página quando já há dados na sessão (nada é gravado)
    tags$script(HTML(
      "(function() {
         var temDados = false;
         function marcar() { temDados = true; }
         document.addEventListener('change', marcar, true);
         document.addEventListener('paste', marcar, true);
         document.addEventListener('keydown', function(e) { if (e.target.closest('.handsontable')) marcar(); }, true);
         window.addEventListener('beforeunload', function(e) {
           if (!temDados) return;
           e.preventDefault();
           e.returnValue = '';
         });
       })();"
    ))
  ),

  div(class = "cabecalho-app",
    tags$img(src = "img/logo_app.png", class = "logo-app", alt = "Logo do Ranova"),
    div(class = "titulo-area",
      div(class = "titulo", "Ranova"),
      div(class = "descricao-app", "Análise de variância de experimentos fatoriais"),
      div(class = "subtitulo", "Digite ou importe seus dados e gere ANOVA, médias com letras, desdobramentos, gráficos e relatório em PDF.")
    )
  ),

  div(class = "painel painel-informacao",
    div(class = "titulo-legenda", icon("info-circle"), " Guia rápido da análise"),
    div(class = "grade-coordenadas",
      div(class = "coord-item", tags$span("DIC", class = "coord-sigla"), tags$span("y ~ A * B * C", class = "coord-faixa"), tags$span("Inteiramente casualizado", class = "coord-descricao")),
      div(class = "coord-item", tags$span("DBC", class = "coord-sigla"), tags$span("y ~ bloco + A * B * C", class = "coord-faixa"), tags$span("Blocos casualizados", class = "coord-descricao")),
      div(class = "coord-item", tags$span("CV", class = "coord-sigla"), tags$span("√QMres / média × 100", class = "coord-faixa"), tags$span("Precisão experimental", class = "coord-descricao")),
      div(class = "coord-item", tags$span("t · Tukey", class = "coord-sigla"), tags$span("2 níveis · 3 ou mais", class = "coord-faixa"), tags$span("Comparação de médias", class = "coord-descricao")),
      div(class = "coord-item", tags$span("aA", class = "coord-sigla"), tags$span("minúsculas · maiúsculas", class = "coord-faixa"), tags$span("Linhas · colunas no desdobramento", class = "coord-descricao"))
    )
  ),

  fluidRow(
    class = "grade-trabalho",

    ## Coluna esquerda: entrada e configuração ----
    column(4,
      cartao(1, "Dados",
        tabsetPanel(id = "modo_entrada", type = "pills",
          tabPanel("Montar", value = "digitar",
            div(class = "explicacao espaco-topo", "Informe os fatores e as variáveis. A planilha ao lado é montada com todas as combinações de tratamentos."),
            div(class = "grade-campos",
              selectInput("n_fatores", "Fatores", choices = c("1 fator" = 1, "2 fatores" = 2, "3 fatores" = 3), selected = 2),
              numericInput("n_repeticoes", "Repetições ou blocos", value = 4, min = 2, max = 50, step = 1)
            ),
            fator_ui(1, "Dose", "0; 50; 100; 150"),
            conditionalPanel("input.n_fatores >= 2", fator_ui(2, "Cultivar", "A; B")),
            conditionalPanel("input.n_fatores >= 3", fator_ui(3, "Época", "Seca; Chuvosa")),
            textInput("respostas_digitar", "Variáveis resposta (separadas por ;)", value = "Produtividade; Altura", width = "100%"),
            actionButton("montar_planilha", "Montar planilha", icon = icon("table-cells"), class = "btn-adicionar btn-bloco")
          ),
          tabPanel("Importar", value = "importar",
            div(class = "explicacao espaco-topo", HTML("Uma linha por parcela e uma coluna para cada informação: <b>bloco</b> (no DBC), <b>fatores</b> e <b>variáveis resposta</b>. Vírgula decimal é aceita.")),
            fileInput("arquivo_dados", "Arquivo Excel ou CSV", width = "100%",
              accept = c(".xlsx", ".xls", ".csv", ".txt"), buttonLabel = "Escolher...", placeholder = "Nenhum arquivo"),
            uiOutput("seletor_aba"),
            downloadButton("baixar_modelo", "Baixar planilha modelo", icon = icon("download"), class = "btn-secundario btn-bloco")
          ),
          tabPanel("Exemplo", value = "exemplo",
            div(class = "explicacao espaco-topo", "Experimento fictício de melão em blocos casualizados (DBC): 4 blocos, fatorial 4 doses × 2 cultivares e três variáveis resposta."),
            actionButton("carregar_exemplo", "Carregar exemplo", icon = icon("flask"), class = "btn-adicionar btn-bloco")
          )
        )
      ),

      cartao(2, "Estrutura do experimento",
        radioButtons("delineamento", "Delineamento", inline = TRUE, choices = c("DIC" = "DIC", "DBC" = "DBC"), selected = "DIC"),
        conditionalPanel("input.delineamento == 'DBC'",
          selectInput("coluna_bloco", "Coluna de blocos", choices = character(), width = "100%")),
        selectizeInput("colunas_fatores", "Fatores (até 3)", choices = character(), multiple = TRUE, width = "100%",
          options = list(maxItems = MAX_FATORES, placeholder = "Selecione os fatores", plugins = list("remove_button"))),
        selectizeInput("colunas_respostas", "Variáveis resposta", choices = character(), multiple = TRUE, width = "100%",
          options = list(placeholder = "Selecione as variáveis", plugins = list("remove_button")))
      ),

      cartao(3, "Opções da análise",
        div(class = "grade-campos",
          selectInput("alpha", "Significância", choices = c("5%" = 0.05, "1%" = 0.01, "10%" = 0.10), selected = 0.05),
          selectInput("digitos", "Casas decimais", choices = 1:4, selected = 2)
        ),
        selectInput("formato_anova", "Formato da tabela de ANOVA", width = "100%",
          choices = c("Quadrado médio com asteriscos" = "qm_star", "F e p em colunas" = "f_p_colunas", "F (p) na mesma célula" = "f_p_inline")),
        selectInput("tipo_se", "Erro-padrão das médias", width = "100%",
          choices = c("Do modelo (ajustado)" = "modelo", "Descritivo (dos dados)" = "descritivo")),
        actionButton("analisar", "Analisar", icon = icon("play"), class = "btn-analisar btn-bloco"),
        uiOutput("mensagens_analise")
      )
    ),

    ## Coluna direita: planilha, resultados, painel e relatório ----
    column(8,
      cartao(NULL, "Planilha de dados", icone = "table", etiqueta = uiOutput("resumo_planilha", inline = TRUE),
        div(class = "planilha", rHandsontableOutput("planilha")),
        div(class = "explicacao nota-planilha",
          icon("keyboard"), " Ctrl+C / Ctrl+V para copiar e colar do Excel · botão direito para inserir ou remover linhas.",
          downloadLink("baixar_dados", tagList(icon("file-excel"), " Baixar planilha (.xlsx)"), class = "link-baixar")
        )
      ),

      cartao(4, "Resultados", etiqueta = "ATUALIZA AO ANALISAR", classe = "painel-visual",
        uiOutput("painel_resultados")
      ),

      cartao(5, "Painel de gráficos", etiqueta = "A · B · C · D", classe = "painel-grupos",
        uiOutput("painel_graficos_ui")
      ),

      cartao(6, "Relatório em PDF", etiqueta = "PRONTO PARA IMPRESSÃO", classe = "painel-relatorio",
        div(class = "explicacao", "Gera o PDF (A4 retrato) com resumo, leitura rápida, pressupostos, ANOVA, médias, desdobramento da interação, gráficos e o painel montado acima."),
        br(),
        fluidRow(
          column(6, textInput("titulo_relatorio", "Título do relatório", value = "Relatório de análise de variância", width = "100%")),
          column(6, textInput("responsavel_relatorio", "Responsável (opcional)", placeholder = "Nome do avaliador ou laboratório", width = "100%"))
        ),
        textAreaInput("descricao_relatorio", "Descrição do experimento / observações", width = "100%", rows = 3,
          placeholder = "Ex.: ensaio de adubação nitrogenada em melão, safra 2026..."),
        fluidRow(
          column(7, div(class = "explicacao", textOutput("info_relatorio"))),
          column(5, div(class = "area-botao-pdf", uiOutput("botao_relatorio")))
        )
      )
    )
  ),

  div(class = "nota-privacidade", icon("lock"), span(TEXTO_PRIVACIDADE)),

  div(class = "rodape-app",
    span("Desenvolvido por"),
    tags$img(src = "img/logo_marlenildo.png", class = "logo-rodape", alt = "Marlenildo Soluções em Curso"),
    span(class = "versao-app",
      tags$a(href = "https://github.com/Marlenildo/ranova-app/blob/main/CHANGELOG.md", target = "_blank", rel = "noopener",
             title = "Ver novidades desta versão", paste0("Ranova v", VERSAO_APP)),
      span(class = "versao-pacote", paste0(" · pacote ranova ", VERSAO_PACOTE))
    )
  )
)
