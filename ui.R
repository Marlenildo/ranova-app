fator_ui <- function(i, nome, niveis) {
  fluidRow(
    class = "linha-fator",
    column(4, textInput(paste0("fator_nome_", i), paste("Fator", i), value = nome, width = "100%")),
    column(8, textInput(paste0("fator_niveis_", i), "Níveis (separados por ;)", value = niveis, width = "100%"))
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$meta(name = "author", content = "Marlenildo"),
    tags$meta(name = "description", content = "Ranova: análise de variância de experimentos fatoriais em DIC e DBC, com médias, desdobramento, gráficos e relatório."),
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
      div(class = "subtitulo", "Digite ou importe seus dados e gere ANOVA, médias com letras, desdobramentos e gráficos prontos para o relatório.")
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

  div(class = "painel",
    div(class = "cabecalho-secao",
      div(h4(icon("table"), " Dados do experimento")),
      div(class = "tag-secao", uiOutput("resumo_planilha", inline = TRUE))
    ),
    tabsetPanel(id = "modo_entrada",
      tabPanel("Montar planilha", value = "digitar",
        br(),
        fluidRow(
          column(3,
            selectInput("n_fatores", "Número de fatores", width = "100%",
              choices = c("1 fator" = 1, "2 fatores" = 2, "3 fatores" = 3), selected = 2),
            numericInput("n_repeticoes", "Repetições ou blocos", value = 4, min = 2, max = 50, step = 1, width = "100%")
          ),
          column(9,
            fator_ui(1, "Dose", "0; 50; 100; 150"),
            conditionalPanel("input.n_fatores >= 2", fator_ui(2, "Cultivar", "A; B")),
            conditionalPanel("input.n_fatores >= 3", fator_ui(3, "Época", "Seca; Chuvosa")),
            textInput("respostas_digitar", "Variáveis resposta (separadas por ;)", value = "Produtividade; Altura", width = "100%")
          )
        ),
        actionButton("montar_planilha", "Montar planilha", icon = icon("table-cells"), class = "btn-adicionar"),
        br(), br(),
        div(class = "explicacao", "A planilha é montada com todas as combinações de tratamentos em cada repetição (ou bloco). Depois, digite os valores ou cole direto do Excel.")
      ),
      tabPanel("Importar arquivo", value = "importar",
        br(),
        fluidRow(
          column(6, fileInput("arquivo_dados", "Arquivo Excel ou CSV", width = "100%",
            accept = c(".xlsx", ".xls", ".csv", ".txt"), buttonLabel = "Escolher...", placeholder = "Nenhum arquivo selecionado")),
          column(3, uiOutput("seletor_aba")),
          column(3, div(class = "area-botao-modelo", downloadButton("baixar_modelo", "Planilha modelo", icon = icon("download"), class = "btn-secundario")))
        ),
        div(class = "explicacao", HTML("Use uma linha por parcela e uma coluna para cada informação: <b>bloco</b> (no DBC), <b>fatores</b> e <b>variáveis resposta</b>. Números com vírgula decimal, como <b>28,4</b>, são aceitos. Também é possível colar os dados direto na planilha abaixo."))
      ),
      tabPanel("Exemplo", value = "exemplo",
        br(),
        div(class = "explicacao", "Experimento fictício de melão em blocos casualizados (DBC): 4 blocos, fatorial 4 doses × 2 cultivares e três variáveis resposta."),
        actionButton("carregar_exemplo", "Carregar exemplo", icon = icon("flask"), class = "btn-adicionar")
      )
    ),
    tags$hr(),
    h4("Planilha"),
    div(class = "planilha", rHandsontableOutput("planilha")),
    div(class = "explicacao nota-planilha",
      icon("keyboard"), " Ctrl+C / Ctrl+V para copiar e colar do Excel · botão direito para inserir ou remover linhas.",
      downloadLink("baixar_dados", tagList(icon("file-excel"), " Baixar planilha (.xlsx)"), class = "link-baixar")
    )
  ),

  div(class = "painel painel-estrutura",
    div(class = "cabecalho-secao",
      div(h4(icon("sitemap"), " Estrutura e opções da análise"))
    ),
    fluidRow(
      column(3,
        radioButtons("delineamento", "Delineamento", inline = TRUE, choices = c("DIC" = "DIC", "DBC" = "DBC"), selected = "DIC"),
        conditionalPanel("input.delineamento == 'DBC'",
          selectInput("coluna_bloco", "Coluna de blocos", choices = character(), width = "100%"))
      ),
      column(4,
        selectizeInput("colunas_fatores", "Fatores (até 3)", choices = character(), multiple = TRUE, width = "100%",
          options = list(maxItems = MAX_FATORES, placeholder = "Selecione os fatores")),
        selectizeInput("colunas_respostas", "Variáveis resposta", choices = character(), multiple = TRUE, width = "100%",
          options = list(placeholder = "Selecione as variáveis"))
      ),
      column(5,
        fluidRow(
          column(6, selectInput("alpha", "Significância", choices = c("5%" = 0.05, "1%" = 0.01, "10%" = 0.10), selected = 0.05, width = "100%")),
          column(6, selectInput("digitos", "Casas decimais", choices = 1:4, selected = 2, width = "100%"))
        ),
        selectInput("formato_anova", "Formato da tabela de ANOVA", width = "100%",
          choices = c("Quadrado médio com asteriscos" = "qm_star", "F e p em colunas" = "f_p_colunas", "F (p) na mesma célula" = "f_p_inline")),
        selectInput("tipo_se", "Erro-padrão das médias", width = "100%",
          choices = c("Do modelo (ajustado)" = "modelo", "Descritivo (dos dados)" = "descritivo"))
      )
    ),
    actionButton("analisar", "Analisar", icon = icon("play"), class = "btn-analisar"),
    uiOutput("mensagens_analise")
  ),

  div(class = "painel painel-visual",
    div(class = "cabecalho-secao",
      div(h4(icon("chart-column"), " Resultados")),
      div(class = "tag-secao tag-verde", "ATUALIZA AO ANALISAR")
    ),
    uiOutput("painel_resultados")
  ),

  div(class = "painel painel-relatorio",
    div(class = "cabecalho-secao", div(h4(icon("file-lines"), " Relatório")), div(class = "tag-secao", "PRONTO PARA IMPRESSÃO")),
    div(class = "explicacao", "Um clique gera o relatório completo com ANOVA, pressupostos, médias, desdobramento da interação e gráficos. Abra no navegador para ler, imprimir ou salvar em PDF."),
    br(),
    fluidRow(
      column(6, textInput("titulo_relatorio", "Título do relatório", value = "Relatório de análise de variância", width = "100%")),
      column(6, textInput("responsavel_relatorio", "Responsável (opcional)", placeholder = "Nome do avaliador ou laboratório", width = "100%"))
    ),
    textAreaInput("descricao_relatorio", "Descrição do experimento / observações", width = "100%", rows = 3,
      placeholder = "Ex.: ensaio de adubação nitrogenada em melão, safra 2026..."),
    fluidRow(
      column(8, div(class = "explicacao", textOutput("info_relatorio"))),
      column(4, div(class = "area-botao-pdf", uiOutput("botao_relatorio")))
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
