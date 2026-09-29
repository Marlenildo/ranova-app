fator_ui <- function(i, nome, niveis) {
  div(
    class = "linha-fator",
    textInput(paste0("fator_nome_", i), paste("Fator", i), value = nome, width = "100%"),
    textInput(paste0("fator_niveis_", i), "Níveis (separados por ;)", value = niveis, width = "100%")
  )
}

ui <- fluidPage(
  tags$head(
    tags$script(async = NA, src = "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-3130340973057636", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$meta(name = "author", content = "Marlenildo"),
    tags$meta(name = "description", content = "Ranova: análise de variância de experimentos simples, fatoriais e em parcelas subdivididas, em DIC e DBC, com médias, desdobramento, gráficos e relatórios em PDF e HTML."),
    tags$link(rel = "icon", type = "image/png", href = "img/favicon.png"),
    tags$title("Ranova · Análise de variância de experimentos"),
    # Botão "Copiar tabela": copia sem a formatação do app
    tags$script(HTML(JS_COPIAR_TABELA)),
    # Estilo clássico das tabelas (texto e filetes pretos), trocado na hora pela opção "Estilo das tabelas"
    tags$style(HTML(CSS_TABELAS_CLASSICAS)),
    tags$script(HTML(
      "$(document).on('change', 'input[name=estilo_tabela]', function() {
         document.body.classList.toggle('tabelas-classicas', this.value === 'classico');
       });"
    )),
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
      div(class = "descricao-app", "Análise de variância de experimentos"),
      div(class = "subtitulo", "Experimentos simples, fatoriais, em parcelas subdivididas e subsubdivididas, em DIC ou DBC: ANOVA, testes de médias, desdobramentos, gráficos e relatórios em PDF e HTML.")
    )
  ),

  tags$details(class = "painel painel-informacao painel-apresentacao",
    tags$summary(class = "cabecalho-secao",
      div(class = "titulo-legenda", icon("circle-info"), " O que o Ranova faz",
          span(class = "dica-expandir", "clique para ver os recursos e o guia rápido")),
      div(class = "tag-secao tag-verde", "GRATUITO · SEM CADASTRO")
    ),
    p(class = "explicacao intro-app", "Análise de variância completa de experimentos agrícolas e biológicos, do lançamento dos dados ao relatório, em poucos cliques e sem programar."),
    div(class = "grade-recursos",
      div(class = "recurso",
        div(class = "recurso-icone", icon("table-cells")),
        div(tags$b("Delineamentos e arranjos"),
          tags$ul(
            tags$li("Inteiramente casualizado (DIC) e blocos casualizados (DBC)"),
            tags$li("Um fator (experimento simples) ou 2 e 3 fatores em esquema fatorial"),
            tags$li("Parcelas subdivididas, inclusive com esquema fatorial nas parcelas ou nas subparcelas, e parcelas subsubdivididas")
          ))),
      div(class = "recurso",
        div(class = "recurso-icone", icon("keyboard")),
        div(tags$b("Seus dados, do seu jeito"),
          tags$ul(
            tags$li("Monte a planilha a partir dos fatores e digite"),
            tags$li("Cole direto do Excel ou importe .xlsx, .xls e .csv"),
            tags$li("Vírgula decimal aceita; colunas reconhecidas sozinhas")
          ))),
      div(class = "recurso",
        div(class = "recurso-icone", icon("calculator")),
        div(tags$b("ANOVA e pressupostos"),
          tags$ul(
            tags$li("Quadro da ANOVA com F, p e CV (erros a e b na subdividida)"),
            tags$li("Shapiro-Wilk, Levene e quatro gráficos de resíduos"),
            tags$li("Aponta outliers e pontos influentes e permite usar a média das repetições")
          ))),
      div(class = "recurso",
        div(class = "recurso-icone", icon("arrow-down-wide-short")),
        div(tags$b("Testes de médias"),
          tags$ul(
            tags$li("Tukey, t (LSD), Bonferroni, Duncan, SNK, Scott-Knott e Dunnett"),
            tags$li("Médias com letras e desdobramento da interação (aA)"),
            tags$li("Erro correto em cada comparação, inclusive na subdividida")
          ))),
      div(class = "recurso",
        div(class = "recurso-icone", icon("chart-column")),
        div(tags$b("Gráficos para publicar"),
          tags$ul(
            tags$li("Médias com letras e interação em linhas ou barras"),
            tags$li("Cores, paletas (inclusive cinza), nomes, fonte, tamanho e negrito"),
            tags$li("Painel A, B, C… e download em PNG ou TIFF, 150 a 600 dpi, no tamanho em cm")
          ))),
      div(class = "recurso",
        div(class = "recurso-icone", icon("file-lines")),
        div(tags$b("Relatórios prontos"),
          tags$ul(
            tags$li("PDF (A4) para imprimir e anexar"),
            tags$li("HTML com o visual do app, para abrir no navegador ou enviar"),
            tags$li("Planilha de dados em .xlsx; nada fica armazenado")
          )))
    ),
    div(class = "titulo-legenda guia-titulo", icon("book-open"), " Guia rápido"),
    div(class = "grade-coordenadas",
      div(class = "coord-item", tags$span("DIC · DBC", class = "coord-sigla"), tags$span("1 fator ou esquema fatorial", class = "coord-faixa"), tags$span("Tratamentos sorteados juntos", class = "coord-descricao")),
      div(class = "coord-item", tags$span("PS", class = "coord-sigla"), tags$span("erros (a) · (b) · (c)", class = "coord-faixa"), tags$span("Fatores sorteados em níveis de parcela", class = "coord-descricao")),
      div(class = "coord-item", tags$span("CV", class = "coord-sigla"), tags$span("√QM erro / média × 100", class = "coord-faixa"), tags$span("Precisão experimental", class = "coord-descricao")),
      div(class = "coord-item", tags$span("a · b · c", class = "coord-sigla"), tags$span("mesma letra = não difere", class = "coord-faixa"), tags$span("Comparação de médias", class = "coord-descricao")),
      div(class = "coord-item", tags$span("aA", class = "coord-sigla"), tags$span("minúsculas · maiúsculas", class = "coord-faixa"), tags$span("Linhas · colunas no desdobramento", class = "coord-descricao"))
    ),
    p(class = "explicacao dica-copiar", icon("copy"), " Cada tabela de resultados tem o botão ", tags$b("Copiar tabela:"),
      " ele copia só os números e títulos, sem cores e fontes do app. No Word, cole e a tabela assume o estilo do seu documento; no Excel, cada valor vai para uma célula.")
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
          tabPanel("Colar", value = "colar",
            div(class = "explicacao espaco-topo", HTML("Copie as células no Excel, planilha ou outra fonte e cole abaixo: <b>uma linha por parcela</b>, com colunas separadas por tabulação. Vírgula decimal, como <b>28,4</b>, é aceita.")),
            div(class = "paste-box", textAreaInput("dados_colados", NULL, width = "100%", rows = 8,
              placeholder = "Bloco\tDose\tCultivar\tProdutividade\n1\t0\tA\t28,4\n1\t50\tA\t31,9\n1\t0\tB\t33,1\n...")),
            checkboxInput("colado_cabecalho", "A primeira linha contém os nomes das colunas", TRUE),
            actionButton("importar_colados", "Usar dados colados", icon = icon("paste"), class = "btn-adicionar btn-bloco")
          ),
          tabPanel("Exemplo", value = "exemplo",
            div(class = "explicacao espaco-topo", "Dados fictícios de melão para conhecer o app."),
            radioButtons("exemplo_tipo", NULL, choices = c(
              "DBC em esquema fatorial 4 × 2: doses × cultivares, 4 blocos" = "DBC",
              "DBC em parcelas subdivididas: irrigação nas parcelas e cultivar nas subparcelas, 4 blocos" = "PSDBC"
            ), selected = "DBC"),
            actionButton("carregar_exemplo", "Carregar exemplo", icon = icon("flask"), class = "btn-adicionar btn-bloco")
          )
        )
      ),

      cartao(2, "Estrutura do experimento",
        selectInput("delineamento", "Delineamento", choices = DELINEAMENTOS, selected = "DIC", width = "100%"),
        conditionalPanel("input.delineamento != 'DIC'",
          selectInput("coluna_bloco", "Coluna de blocos (DBC) ou de repetição (parcelas em DIC)", choices = character(), width = "100%")),
        selectizeInput("colunas_fatores", "Fatores (até 3)", choices = character(), multiple = TRUE, width = "100%",
          options = list(maxItems = MAX_FATORES, placeholder = "Selecione os fatores", plugins = list("remove_button"))),
        conditionalPanel("input.delineamento == 'DIC' || input.delineamento == 'DBC'",
          div(class = "explicacao dica-subdividida", icon("circle-info"),
              HTML(" Com <b>um fator</b>, é um experimento simples. Com <b>dois ou três</b>, os fatores são analisados em <b>esquema fatorial</b> (todas as combinações sorteadas juntas nas parcelas)."))),
        conditionalPanel("input.delineamento == 'PSDIC' || input.delineamento == 'PSDBC'",
          div(class = "explicacao dica-subdividida", icon("layer-group"),
              HTML(" Com <b>dois fatores</b> (parcela subdividida simples), o 1º fica nas parcelas e o 2º nas subparcelas. Com <b>três</b>, escolha onde fica o esquema fatorial:"),
              selectInput("arranjo_ps", NULL, choices = ARRANJOS_PS, selected = "parcela", width = "100%"))),
        conditionalPanel("input.delineamento == 'PSSDIC' || input.delineamento == 'PSSDBC'",
          div(class = "explicacao dica-subdividida", icon("layer-group"),
              HTML(" Escolha <b>três fatores</b>, na ordem: parcelas, subparcelas e subsubparcelas."))),
        conditionalPanel("input.delineamento == 'PSDIC' || input.delineamento == 'PSSDIC'",
          div(class = "explicacao dica-subdividida", icon("hashtag"),
              " No DIC com parcelas, a coluna de repetição identifica cada parcela (1, 2, 3...) dentro da combinação dos fatores da parcela.")),
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
        radioButtons("estilo_tabela", "Estilo das tabelas", inline = TRUE, width = "100%",
          choices = c("Moderno" = "moderno", "Clássico (artigo)" = "classico")),
        selectInput("teste_medias", "Teste de médias", choices = OPCOES_TESTES, selected = "auto", width = "100%"),
        conditionalPanel("input.teste_medias == 'dunnett'",
          div(class = "explicacao dica-subdividida", icon("flag"), " Dunnett compara cada nível com o controle: o primeiro nível de cada fator na planilha (em ordem numérica, quando os níveis são números).")),
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

      cartao(6, "Relatório", etiqueta = "PDF · HTML", classe = "painel-relatorio",
        div(class = "explicacao", HTML("Resumo, leitura rápida, pressupostos, discrepantes, ANOVA, médias, desdobramento da interação, gráficos e o painel montado acima. <b>PDF</b> (A4 retrato) para imprimir e anexar; <b>HTML</b> com o visual do app, navegação por seções e imagens embutidas, para abrir no navegador ou enviar por e-mail.")),
        br(),
        fluidRow(
          column(6, textInput("titulo_relatorio", "Título do relatório", value = "Relatório de análise de variância", width = "100%")),
          column(6, textInput("responsavel_relatorio", "Responsável (opcional)", placeholder = "Nome do avaliador ou laboratório", width = "100%"))
        ),
        textAreaInput("descricao_relatorio", "Descrição do experimento / observações", width = "100%", rows = 3,
          placeholder = "Ex.: ensaio de adubação nitrogenada em melão, safra 2026..."),
        fluidRow(
          column(5, div(class = "explicacao", textOutput("info_relatorio"))),
          column(7, div(class = "area-botao-pdf botoes-relatorio", uiOutput("botao_relatorio")))
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
