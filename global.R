# Funções e dependências compartilhadas pela aplicação

library(shiny)
library(ggplot2)
library(rhandsontable)
library(ranova)

options(knitr.table.format = "html")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Versão lida do DESCRIPTION (fonte única; atualize lá e no CHANGELOG.md).
VERSAO_APP <- tryCatch(unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1]), error = function(e) "dev")

VERSAO_PACOTE <- tryCatch(as.character(utils::packageVersion("ranova")), error = function(e) "dev")

CORES_APP <- list(
  navy = "#173B5B", blue = "#2A5C92", ink = "#263B4D", muted = "#627589",
  line = "#D9E3EB", canvas = "#F4F7FA", soft = "#EAF2FA", green = "#4D965D",
  red = "#B94B4B", amber = "#A86F00"
)

PALETA_FATORES <- c("#2A5C92", "#4D965D", "#C0924A", "#B94B4B", "#173B5B", "#7FA7CF", "#8BC39A", "#627589")

MAX_FATORES <- 3

TEXTO_PRIVACIDADE <- paste(
  "Os dados ficam apenas na sessão aberta do navegador e são usados somente para a análise.",
  "Nada é gravado em banco de dados, arquivos ou cookies; ao fechar ou atualizar a página, as informações são descartadas."
)

# ---------------------------------------------------------
# Componentes de interface
# ---------------------------------------------------------

# Cartão numerado (passo a passo) no estilo dos painéis do Croma.
cartao <- function(numero = NULL, titulo, ..., icone = NULL, etiqueta = NULL, classe = NULL) {
  div(
    class = paste("painel cartao", classe),
    div(
      class = "cabecalho-secao",
      h4(class = "titulo-cartao",
         if (!is.null(numero)) tags$span(class = "numero-passo", numero) else if (!is.null(icone)) icon(icone),
         titulo),
      if (!is.null(etiqueta)) div(class = "tag-secao", etiqueta)
    ),
    ...
  )
}

# Formato (PNG ou TIFF), resolução (dpi) e tamanho (cm) para baixar um gráfico.
controles_exportacao <- function(prefixo, largura = 17, altura = 11) {
  div(
    class = "caixa-exportacao",
    div(class = "exportacao-campos",
      radioButtons(paste0(prefixo, "_formato"), "Formato", choices = c("PNG" = "png", "TIFF" = "tiff"), selected = "png", inline = TRUE),
      selectInput(paste0(prefixo, "_dpi"), "Resolução", choices = c("150 dpi" = 150, "300 dpi" = 300, "600 dpi" = 600), selected = 300, width = "108px"),
      numericInput(paste0(prefixo, "_largura"), "Largura (cm)", value = largura, min = 5, max = 60, step = 0.5, width = "92px"),
      numericInput(paste0(prefixo, "_altura"), "Altura (cm)", value = altura, min = 4, max = 60, step = 0.5, width = "92px")
    ),
    downloadButton(paste0("baixar_", prefixo), "Baixar gráfico", icon = icon("download"), class = "btn-secundario")
  )
}

# ---------------------------------------------------------
# Leitura e conversão de dados
# ---------------------------------------------------------

texto_vazio <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

converter_numero <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  texto <- trimws(as.character(x))
  texto[texto == ""] <- NA_character_
  texto <- gsub("\\s+", "", texto)
  com_virgula <- !is.na(texto) & grepl(",", texto, fixed = TRUE)
  texto[com_virgula] <- gsub(".", "", texto[com_virgula], fixed = TRUE)
  texto[com_virgula] <- sub(",", ".", texto[com_virgula], fixed = TRUE)
  suppressWarnings(as.numeric(texto))
}

valores_invalidos <- function(x) {
  original_preenchido <- !texto_vazio(x)
  convertido <- converter_numero(x)
  which(original_preenchido & is.na(convertido))
}

separar_lista <- function(texto) {
  if (is.null(texto) || length(texto) == 0) {
    return(character())
  }
  itens <- unlist(strsplit(as.character(texto), "[;\n]"))
  itens <- trimws(itens)
  unique(itens[itens != ""])
}

detectar_separador_csv <- function(caminho) {
  linhas <- readLines(caminho, n = 5, warn = FALSE, encoding = "UTF-8")
  linhas <- linhas[linhas != ""]
  if (length(linhas) == 0) {
    return(",")
  }
  contagem <- vapply(c(";", ",", "\t"), function(sep) {
    sum(lengths(regmatches(linhas, gregexpr(sep, linhas, fixed = TRUE))))
  }, numeric(1))
  names(contagem)[which.max(contagem)]
}

ler_planilhas_excel <- function(caminho) {
  readxl::excel_sheets(caminho)
}

ler_arquivo_dados <- function(caminho, nome_arquivo, aba = NULL) {
  extensao <- tolower(tools::file_ext(nome_arquivo))

  dados <- if (extensao %in% c("xlsx", "xls")) {
    readxl::read_excel(caminho, sheet = if (is.null(aba)) 1 else aba, .name_repair = "minimal")
  } else if (extensao %in% c("csv", "txt")) {
    utils::read.table(
      caminho,
      header = TRUE,
      sep = detectar_separador_csv(caminho),
      quote = "\"",
      dec = ".",
      colClasses = "character",
      check.names = FALSE,
      na.strings = c("", "NA"),
      encoding = "UTF-8",
      fileEncoding = "UTF-8",
      comment.char = "",
      strip.white = TRUE
    )
  } else {
    stop("Formato não suportado. Envie um arquivo .xlsx, .xls ou .csv.")
  }

  limpar_tabela(dados, "O arquivo não possui dados preenchidos.")
}

# Padroniza nomes de colunas e remove colunas e linhas totalmente vazias.
limpar_tabela <- function(dados, mensagem_vazio = "Não há dados preenchidos.") {
  dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
  nomes <- trimws(names(dados))
  nomes[nomes == ""] <- paste0("Coluna ", which(nomes == ""))
  names(dados) <- make.unique(nomes, sep = " ")

  preenchida <- vapply(dados, function(coluna) any(!texto_vazio(coluna)), logical(1))
  dados <- dados[, preenchida, drop = FALSE]
  linhas_preenchidas <- apply(dados, 1, function(linha) any(!texto_vazio(linha)))
  dados <- dados[linhas_preenchidas, , drop = FALSE]
  rownames(dados) <- NULL

  if (ncol(dados) == 0 || nrow(dados) == 0) {
    stop(mensagem_vazio)
  }

  dados
}

# Lê dados colados do Excel, planilha ou outra fonte: uma linha por parcela,
# colunas separadas por tabulação (ou ponto e vírgula, quando não há tabulação).
ler_texto_colado <- function(texto, cabecalho = TRUE) {
  linhas <- unlist(strsplit(gsub("\r", "", texto %||% ""), "\n"))
  linhas <- linhas[nzchar(trimws(linhas))]
  if (length(linhas) == 0) stop("Cole os dados na caixa de texto antes de importar.")
  separador <- if (any(grepl("\t", linhas))) "\t" else if (any(grepl(";", linhas, fixed = TRUE))) ";" else "\\s+"
  partes <- lapply(linhas, function(l) trimws(strsplit(l, separador, perl = TRUE)[[1]]))
  n_col <- max(lengths(partes))
  if (n_col < 2) stop("Não foi possível separar as colunas. Copie as células direto do Excel (separadas por tabulação).")
  matriz <- do.call(rbind, lapply(partes, function(p) c(p, rep("", n_col - length(p)))))
  if (cabecalho) {
    nomes <- matriz[1, ]
    matriz <- matriz[-1, , drop = FALSE]
  } else {
    nomes <- paste("Coluna", seq_len(n_col))
  }
  if (nrow(matriz) == 0) stop("Há apenas o cabeçalho; cole também as linhas com os dados.")
  dados <- as.data.frame(matriz, stringsAsFactors = FALSE)
  names(dados) <- nomes
  limpar_tabela(dados, "Os dados colados estão vazios.")
}

dados_para_planilha <- function(dados) {
  dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
  dados[] <- lapply(dados, function(coluna) {
    if (is.numeric(coluna)) {
      texto <- format(coluna, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
      texto[is.na(coluna)] <- ""
      texto
    } else {
      texto <- as.character(coluna)
      texto[is.na(texto)] <- ""
      texto
    }
  })
  dados
}

# ---------------------------------------------------------
# Nomes seguros para fórmulas
# ---------------------------------------------------------

nome_seguro <- function(nomes) {
  seguro <- iconv(nomes, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  seguro[is.na(seguro)] <- ""
  seguro <- gsub("[^A-Za-z0-9_]+", "_", seguro)
  seguro <- gsub("^_+|_+$", "", seguro)
  seguro[seguro == ""] <- "var"
  seguro <- ifelse(grepl("^[A-Za-z]", seguro), seguro, paste0("v_", seguro))
  make.unique(seguro, sep = "_")
}

# ---------------------------------------------------------
# Planilha para digitação
# ---------------------------------------------------------

gerar_planilha_modelo <- function(fatores, n_repeticoes, delineamento, respostas) {
  niveis <- lapply(fatores, function(f) f$niveis)
  names(niveis) <- vapply(fatores, function(f) f$nome, character(1))

  tratamentos <- do.call(
    expand.grid,
    c(rev(niveis), list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  )
  tratamentos <- tratamentos[, names(niveis), drop = FALSE]

  coluna_rep <- if (identical(delineamento, "DBC")) "Bloco" else "Repetição"
  planilha <- do.call(rbind, lapply(seq_len(n_repeticoes), function(r) {
    cbind(stats::setNames(data.frame(as.character(r), stringsAsFactors = FALSE), coluna_rep), tratamentos)
  }))

  for (resposta in respostas) {
    planilha[[resposta]] <- ""
  }
  rownames(planilha) <- NULL
  dados_para_planilha(planilha)
}

dados_exemplo <- function() {
  set.seed(2026)
  dados <- expand.grid(
    Bloco = as.character(1:4),
    Dose = c("0", "50", "100", "150"),
    Cultivar = c("Gália", "Amarelo"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  dose <- as.numeric(dados$Dose)
  cultivar_b <- dados$Cultivar == "Amarelo"
  bloco <- as.numeric(dados$Bloco)
  dados$`Produtividade (t/ha)` <- round(
    28 + 0.09 * dose - 0.0004 * dose^2 + 3.5 * cultivar_b + 0.02 * dose * cultivar_b +
      0.6 * bloco + stats::rnorm(nrow(dados), 0, 1.4),
    2
  )
  dados$`Sólidos solúveis (°Brix)` <- round(
    10.5 + 0.006 * dose + 1.1 * cultivar_b + stats::rnorm(nrow(dados), 0, 0.45),
    2
  )
  dados$`Massa do fruto (kg)` <- round(
    1.6 + 0.003 * dose + 0.2 * cultivar_b + stats::rnorm(nrow(dados), 0, 0.12),
    3
  )
  dados <- dados[order(as.numeric(dados$Bloco)), ]
  rownames(dados) <- NULL
  dados_para_planilha(dados)
}

# ---------------------------------------------------------
# Sugestão de estrutura a partir das colunas
# ---------------------------------------------------------

coluna_parece_numerica <- function(coluna) {
  preenchidos <- coluna[!texto_vazio(coluna)]
  length(preenchidos) == 0 || all(!is.na(converter_numero(preenchidos)))
}

coluna_parece_fator_numerico <- function(coluna) {
  preenchidos <- trimws(as.character(coluna[!texto_vazio(coluna)]))
  if (length(preenchidos) < 4) {
    return(FALSE)
  }
  contagem <- table(preenchidos)
  length(contagem) >= 2 && length(contagem) <= length(preenchidos) / 2 && length(unique(as.integer(contagem))) == 1
}

sugerir_estrutura <- function(dados) {
  nomes <- names(dados)
  if (length(nomes) == 0) {
    return(list(delineamento = "DIC", bloco = "", fatores = character(), respostas = character()))
  }
  normalizados <- tolower(nome_seguro(nomes))
  eh_bloco <- grepl("^(bloco|blocos|block|blocks)(_|$)", normalizados)
  eh_repeticao <- grepl("^(rep|repeticao|repeticoes|repetition|parcela|unidade)(_|$)", normalizados)

  bloco <- if (any(eh_bloco)) nomes[which(eh_bloco)[1]] else ""
  candidatos <- nomes[!eh_bloco & !eh_repeticao]
  numericas <- vapply(dados[candidatos], coluna_parece_numerica, logical(1))
  fator_numerico <- vapply(dados[candidatos], coluna_parece_fator_numerico, logical(1))

  fatores <- candidatos[!numericas | fator_numerico]
  respostas <- candidatos[numericas & !fator_numerico]

  list(
    delineamento = if (nzchar(bloco)) "DBC" else "DIC",
    bloco = bloco,
    fatores = utils::head(fatores, MAX_FATORES),
    respostas = respostas
  )
}

# ---------------------------------------------------------
# Preparação e validação para análise
# ---------------------------------------------------------

preparar_dados_analise <- function(dados, delineamento, bloco, fatores, respostas) {
  erros <- character()
  bloco <- if (identical(delineamento, "DBC") && !is.null(bloco) && nzchar(bloco)) bloco else NULL

  if (length(fatores) == 0) {
    erros <- c(erros, "Selecione pelo menos um fator.")
  }
  if (length(fatores) > MAX_FATORES) {
    erros <- c(erros, "Selecione no máximo três fatores.")
  }
  if (length(respostas) == 0) {
    erros <- c(erros, "Selecione pelo menos uma variável resposta.")
  }
  if (identical(delineamento, "DBC") && is.null(bloco)) {
    erros <- c(erros, "Selecione a coluna de blocos para o delineamento em blocos casualizados (DBC).")
  }

  usadas <- c(bloco, fatores, respostas)
  repetidas <- unique(usadas[duplicated(usadas)])
  if (length(repetidas) > 0) {
    erros <- c(erros, paste0("A mesma coluna foi usada em mais de uma função: ", paste(repetidas, collapse = ", "), "."))
  }
  faltantes <- setdiff(usadas, names(dados))
  if (length(faltantes) > 0) {
    erros <- c(erros, paste0("Colunas não encontradas na planilha: ", paste(faltantes, collapse = ", "), "."))
  }
  if (length(erros) > 0) {
    return(list(ok = FALSE, erros = erros))
  }

  base <- dados[, usadas, drop = FALSE]
  classificadoras <- c(bloco, fatores)
  linhas_validas <- apply(base[, classificadoras, drop = FALSE], 1, function(linha) all(!texto_vazio(linha)))
  linhas_descartadas <- sum(!linhas_validas & apply(base, 1, function(linha) any(!texto_vazio(linha))))
  base <- base[linhas_validas, , drop = FALSE]

  if (nrow(base) == 0) {
    return(list(ok = FALSE, erros = "Nenhuma linha possui todos os fatores preenchidos."))
  }

  for (resposta in respostas) {
    invalidas <- valores_invalidos(base[[resposta]])
    if (length(invalidas) > 0) {
      erros <- c(erros, sprintf(
        "A variável \"%s\" possui valores não numéricos (linhas %s).",
        resposta,
        paste(utils::head(invalidas, 5), collapse = ", ")
      ))
    }
    valores <- converter_numero(base[[resposta]])
    if (sum(!is.na(valores)) < 3) {
      erros <- c(erros, sprintf("A variável \"%s\" precisa de pelo menos três valores numéricos.", resposta))
    } else if (stats::var(valores, na.rm = TRUE) == 0) {
      erros <- c(erros, sprintf("A variável \"%s\" não possui variação entre as observações.", resposta))
    }
  }

  for (fator in fatores) {
    n_niveis <- length(unique(trimws(as.character(base[[fator]]))))
    if (n_niveis < 2) {
      erros <- c(erros, sprintf("O fator \"%s\" precisa ter pelo menos dois níveis.", fator))
    }
  }
  if (!is.null(bloco) && length(unique(trimws(as.character(base[[bloco]])))) < 2) {
    erros <- c(erros, "A coluna de blocos precisa ter pelo menos dois blocos.")
  }
  if (length(erros) > 0) {
    return(list(ok = FALSE, erros = erros))
  }

  nomes_seguros <- nome_seguro(usadas)
  mapa <- stats::setNames(usadas, nomes_seguros)
  seguro <- function(x) if (is.null(x)) NULL else names(mapa)[match(x, mapa)]

  analise <- base
  names(analise) <- nomes_seguros
  for (coluna in seguro(classificadoras)) {
    valores <- trimws(as.character(analise[[coluna]]))
    niveis <- unique(valores)
    numeros <- suppressWarnings(converter_numero(niveis))
    if (all(!is.na(numeros))) {
      niveis <- niveis[order(numeros)]
    }
    analise[[coluna]] <- factor(valores, levels = niveis)
  }
  for (coluna in seguro(respostas)) {
    analise[[coluna]] <- converter_numero(analise[[coluna]])
  }

  combinacoes <- interaction(analise[seguro(fatores)], drop = TRUE)
  todas <- prod(vapply(analise[seguro(fatores)], nlevels, numeric(1)))
  if (nlevels(combinacoes) < todas) {
    return(list(ok = FALSE, erros = "Existem combinações de tratamentos sem observações. Confira se todos os níveis dos fatores foram combinados na planilha."))
  }
  if (min(table(combinacoes)) < 2 && is.null(bloco)) {
    return(list(ok = FALSE, erros = "Cada tratamento precisa de pelo menos duas repetições para estimar o erro experimental."))
  }

  dic_vars <- data.frame(
    var = names(mapa),
    sigla = unname(mapa),
    label = unname(mapa),
    description = unname(mapa),
    stringsAsFactors = FALSE
  )

  list(
    ok = TRUE,
    dados = analise,
    delineamento = delineamento,
    bloco = seguro(bloco),
    fatores = seguro(fatores),
    respostas = seguro(respostas),
    mapa = mapa,
    dic_vars = dic_vars,
    linhas_descartadas = linhas_descartadas,
    linha_planilha = which(linhas_validas),
    n_obs = nrow(analise)
  )
}

rotulo <- function(prep, x) {
  unname(ifelse(x %in% names(prep$mapa), prep$mapa[x], x))
}

# ---------------------------------------------------------
# Análise com o pacote ranova
# ---------------------------------------------------------

silenciar <- function(expr) {
  suppressWarnings(suppressMessages(expr))
}

tabela_html <- function(tabela) {
  HTML(as.character(tabela))
}

tentar <- function(expr) {
  tryCatch(
    list(ok = TRUE, valor = silenciar(expr)),
    error = function(e) list(ok = FALSE, erro = conditionMessage(e))
  )
}

formatar_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(p < 0.0001, "< 0,0001", formatC(p, digits = 4, format = "f", decimal.mark = ","))
  )
}

termos_significativos <- function(prep, alpha) {
  linhas <- lapply(prep$respostas, function(v) {
    modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, v, prep$bloco, prep$fatores))
    tab <- summary(modelo)[[1]]
    termos <- trimws(rownames(tab))
    data.frame(
      variavel = v,
      termo = termos,
      p = tab$`Pr(>F)`,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, linhas)
  resultado <- resultado[!is.na(resultado$p) & resultado$termo != "Residuals", , drop = FALSE]
  if (!is.null(prep$bloco)) {
    resultado <- resultado[resultado$termo != prep$bloco, , drop = FALSE]
  }
  resultado$interacao <- grepl(":", resultado$termo, fixed = TRUE)
  resultado$significativo <- resultado$p < alpha
  resultado
}

rotulo_termo <- function(prep, termo) {
  partes <- strsplit(termo, ":", fixed = TRUE)[[1]]
  paste(rotulo(prep, partes), collapse = " × ")
}

tabela_diagnostico <- function(prep, alpha) {
  bruto <- silenciar(anova_diagnostico(
    dados = prep$dados,
    variaveis = prep$respostas,
    bloco = prep$bloco,
    fatores = prep$fatores,
    alpha = alpha,
    mostrar_graficos = FALSE
  ))$dados_brutos

  data.frame(
    `Variável` = rotulo(prep, bruto$Variavel),
    `p (Shapiro-Wilk)` = formatar_p(bruto$p_normalidade),
    `Normalidade` = ifelse(is.na(bruto$p_normalidade), "Não avaliado", ifelse(bruto$p_normalidade < alpha, "Não atendida", "Atendida")),
    `p (Levene)` = formatar_p(bruto$p_homogeneidade),
    `Homogeneidade` = ifelse(is.na(bruto$p_homogeneidade), "Não avaliado", ifelse(bruto$p_homogeneidade < alpha, "Não atendida", "Atendida")),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

tabela_diagnostico_html <- function(tabela) {
  classe <- function(valor) if (valor == "Atendida") "ranova-ok" else if (valor == "Não atendida") "ranova-alerta" else ""
  tags$div(
    class = "tabela-rolagem",
    tags$table(
      class = "table ranova-diag-table",
      tags$thead(tags$tr(lapply(names(tabela), tags$th))),
      tags$tbody(lapply(seq_len(nrow(tabela)), function(i) {
        tags$tr(
          tags$td(tabela[i, 1]),
          tags$td(tabela[i, 2]),
          tags$td(tags$span(class = paste("ranova-pill", classe(tabela[i, 3])), tabela[i, 3])),
          tags$td(tabela[i, 4]),
          tags$td(tags$span(class = paste("ranova-pill", classe(tabela[i, 5])), tabela[i, 5]))
        )
      }))
    )
  )
}

executar_analise <- function(prep, opcoes) {
  argumentos <- list(dados = prep$dados, bloco = prep$bloco, fatores = prep$fatores)

  anova <- tentar(anova_fatorial_qm_tabela(
    dados = prep$dados,
    variaveis = prep$respostas,
    bloco = prep$bloco,
    fatores = prep$fatores,
    dic_vars = prep$dic_vars,
    formato = opcoes$formato,
    digitos = opcoes$digitos_anova,
    caption = "Resumo da análise de variância."
  ))

  significancia <- tentar(termos_significativos(prep, opcoes$alpha))
  diagnostico <- tentar(tabela_diagnostico(prep, opcoes$alpha))

  medias <- lapply(prep$fatores, function(fator) {
    tentar(tabela_medias_fatorial(
      dados = prep$dados,
      variaveis = prep$respostas,
      fator_interesse = fator,
      bloco = prep$bloco,
      fatores = prep$fatores,
      dic_vars = prep$dic_vars,
      digitos = opcoes$digitos,
      tipo_se = opcoes$tipo_se,
      caption = paste0("Médias ajustadas ± erro-padrão para ", rotulo(prep, fator), ".")
    ))
  })
  names(medias) <- prep$fatores

  list(
    prep = prep,
    opcoes = opcoes,
    anova = anova,
    significancia = significancia,
    diagnostico = diagnostico,
    medias = medias,
    gerado_em = Sys.time()
  )
}

tabela_interacao <- function(prep, opcoes, fator_linha, fator_coluna) {
  tentar(tabela_interacao_fatorial_multivariaveis(
    dados = prep$dados,
    variaveis = prep$respostas,
    fator_linha = fator_linha,
    fator_coluna = fator_coluna,
    bloco = prep$bloco,
    fatores = prep$fatores,
    dic_vars = prep$dic_vars,
    digitos = opcoes$digitos,
    alpha = opcoes$alpha,
    tipo_se = opcoes$tipo_se,
    caption = paste0("Desdobramento da interação ", rotulo(prep, fator_linha), " × ", rotulo(prep, fator_coluna), ".")
  ))
}

# ---------------------------------------------------------
# Gráficos
# ---------------------------------------------------------

FONTES_GRAFICO <- c("Sem serifa (Arial)" = "sans", "Com serifa (Times)" = "serif", "Monoespaçada" = "mono")

# Fonte, tamanho e contorno escolhidos pelo usuário. O tamanho informado vale para os
# gráficos da tela (base 13); painel e PDF usam a mesma proporção sobre a base deles.
aparencia_grafico <- function(estilo, base_size) {
  estilo <- estilo %||% list()
  tamanho <- suppressWarnings(as.numeric(estilo$tamanho %||% 13))
  if (length(tamanho) != 1 || is.na(tamanho)) tamanho <- 13
  contorno <- estilo$contorno %||% CORES_APP$navy
  if (isTRUE(estilo$sem_contorno) || !nzchar(contorno)) contorno <- NA
  list(
    tamanho = base_size * max(6, min(30, tamanho)) / 13,
    fonte = if ((estilo$fonte %||% "sans") %in% FONTES_GRAFICO) estilo$fonte %||% "sans" else "sans",
    contorno = contorno,
    negrito = !isFALSE(estilo$negrito)
  )
}

tema_ranova <- function(base_size = 13, familia = "sans", negrito = TRUE) {
  theme_bw(base_size = base_size, base_family = familia) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "#9FB3C4"),
      axis.title = element_text(face = if (negrito) "bold" else "plain", color = CORES_APP$ink),
      axis.text = element_text(color = CORES_APP$ink),
      legend.position = "bottom",
      legend.title = element_text(face = if (negrito) "bold" else "plain"),
      plot.margin = margin(12, 12, 8, 8)
    )
}

# Rótulo usado nos gráficos: nome digitado pelo usuário ou o nome original da coluna.
rotulo_grafico <- function(prep, x, rotulos = NULL) {
  valor <- trimws(rotulos[[x]] %||% "")
  if (nzchar(valor)) valor else rotulo(prep, x)
}

# Cores das barras: `estilo$modo` "unica" (todas iguais) ou "niveis" (uma por nível do fator),
# com paleta pronta ou cores escolhidas (`estilo$cor_unica` e `estilo$cores[[fator]]`).
# Função de rótulos dos níveis de um fator: nomes digitados pelo usuário ou os originais.
rotulos_niveis <- function(prep, fator, rotulos = NULL) {
  niveis <- levels(prep$dados[[fator]])
  novos <- rotulos[[paste0("niveis:", fator)]]
  if (length(novos) == length(niveis)) {
    novos <- trimws(novos)
    niveis_saida <- ifelse(nzchar(novos), novos, niveis)
  } else {
    niveis_saida <- niveis
  }
  mapa <- stats::setNames(niveis_saida, niveis)
  function(x) unname(ifelse(x %in% names(mapa), mapa[x], x))
}

grafico_medias <- function(prep, opcoes, resposta, fator, rotulos = NULL, base_size = 13, estilo = NULL) {
  estilo <- estilo %||% list()
  ap <- aparencia_grafico(estilo, base_size)
  base_size <- ap$tamanho
  medias <- silenciar(medias_fatorial_cld(
    dados = prep$dados,
    resposta = resposta,
    fator_interesse = fator,
    bloco = prep$bloco,
    fatores = prep$fatores,
    alpha = opcoes$alpha,
    tipo_se = opcoes$tipo_se
  ))
  medias$nivel <- factor(medias$nivel, levels = levels(prep$dados[[fator]]))
  medias$grupo <- trimws(medias$grupo)
  topo <- max(medias$media + medias$se, na.rm = TRUE)

  paleta <- estilo$paleta %||% "ranova"
  barras <- if (identical(estilo$modo, "niveis")) {
    list(geom_col(aes(fill = .data$nivel), color = ap$contorno, width = 0.65),
         scale_fill_manual(values = cores_niveis(levels(medias$nivel), paleta, estilo$cores[[fator]]), guide = "none"))
  } else {
    cor <- if (identical(paleta, "personalizada") && grepl("^#[0-9A-Fa-f]{6}", estilo$cor_unica %||% "")) {
      estilo$cor_unica
    } else if (paleta %in% names(PALETAS)) {
      PALETAS[[paleta]]$cores(1)[1]
    } else {
      CORES_APP$blue
    }
    list(geom_col(fill = cor, color = ap$contorno, width = 0.65))
  }

  ggplot(medias, aes(x = .data$nivel, y = .data$media)) +
    barras +
    geom_errorbar(aes(ymin = .data$media - .data$se, ymax = .data$media + .data$se), width = 0.18, color = CORES_APP$ink) +
    geom_text(aes(y = .data$media + .data$se, label = .data$grupo), vjust = -0.6, size = base_size * 0.33, fontface = "bold", color = CORES_APP$ink, family = ap$fonte) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12)), limits = c(0, topo * 1.12)) +
    scale_x_discrete(labels = rotulos_niveis(prep, fator, rotulos)) +
    labs(x = rotulo_grafico(prep, fator, rotulos), y = rotulo_grafico(prep, resposta, rotulos)) +
    tema_ranova(base_size, ap$fonte, ap$negrito)
}

PALETAS <- list(
  ranova = list(nome = "Ranova (azul, verde, dourado)", cores = function(n) rep(PALETA_FATORES, length.out = n)),
  cinza = list(nome = "Tons de cinza", cores = function(n) if (n == 1) "#555555" else grDevices::gray(seq(0.18, 0.78, length.out = n))),
  azul = list(nome = "Tons de azul", cores = function(n) grDevices::colorRampPalette(c("#173B5B", "#2A5C92", "#9DC0E3"))(n)),
  verde = list(nome = "Tons de verde", cores = function(n) grDevices::colorRampPalette(c("#1F4D2B", "#4D965D", "#B5DCB9"))(n)),
  terra = list(nome = "Terra", cores = function(n) grDevices::colorRampPalette(c("#5B3A1E", "#C0924A", "#E8D3A8"))(n)),
  contraste = list(nome = "Alto contraste (daltônicos)", cores = function(n) rep(c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#000000"), length.out = n)),
  viridis = list(nome = "Viridis", cores = function(n) grDevices::hcl.colors(n, "viridis"))
)

OPCOES_PALETAS <- c(stats::setNames(names(PALETAS), vapply(PALETAS, `[[`, character(1), "nome")), "Escolher cada cor" = "personalizada")

# Cores dos níveis de um fator: paleta pronta ou cores escolhidas uma a uma.
cores_niveis <- function(niveis, paleta = "ranova", personalizadas = NULL) {
  n <- length(niveis)
  base <- PALETAS[[if (paleta %in% names(PALETAS)) paleta else "ranova"]]$cores(n)
  if (identical(paleta, "personalizada") && length(personalizadas) > 0) {
    escolhidas <- vapply(seq_len(n), function(i) {
      cor <- personalizadas[[i]] %||% ""
      if (grepl("^#[0-9A-Fa-f]{6}", cor)) cor else PALETA_FATORES[(i - 1) %% length(PALETA_FATORES) + 1]
    }, character(1))
    base <- escolhidas
  }
  stats::setNames(base, niveis)
}

# Gráfico de interação em linhas ou barras. Nas barras, as letras seguem o desdobramento:
# minúsculas comparam os níveis do eixo X em cada cor; maiúsculas, as cores em cada nível do eixo X.
grafico_interacao <- function(prep, resposta, fator_x, fator_traco, rotulos = NULL, base_size = 13,
                              estilo = NULL, opcoes = NULL) {
  estilo <- estilo %||% list()
  ap <- aparencia_grafico(estilo, base_size)
  base_size <- ap$tamanho
  tipo <- estilo$tipo %||% "linhas"
  niveis_traco <- levels(prep$dados[[fator_traco]])
  cores <- cores_niveis(niveis_traco, estilo$paleta %||% "ranova", estilo$cores[[fator_traco]])
  rotulos_eixos <- list(
    labs(x = rotulo_grafico(prep, fator_x, rotulos), y = rotulo_grafico(prep, resposta, rotulos),
         color = rotulo_grafico(prep, fator_traco, rotulos), fill = rotulo_grafico(prep, fator_traco, rotulos)),
    scale_x_discrete(labels = rotulos_niveis(prep, fator_x, rotulos))
  )
  nomes_traco <- rotulos_niveis(prep, fator_traco, rotulos)

  if (identical(tipo, "barras")) {
    opcoes <- opcoes %||% list(alpha = 0.05, tipo_se = "modelo")
    medias <- interacao_letras(prep, opcoes, resposta, fator_x, fator_traco)
    medias$x <- factor(medias$linha, levels = levels(prep$dados[[fator_x]]))
    medias$grupo <- factor(medias$coluna, levels = niveis_traco)
    medias$letra <- paste0(medias$letra_lin, medias$letra_col)
    topo <- max(medias$media + medias$se, na.rm = TRUE)
    desvio <- position_dodge(width = 0.8)
    return(
      ggplot(medias, aes(x = .data$x, y = .data$media, fill = .data$grupo)) +
        geom_col(position = desvio, width = 0.75, color = ap$contorno, linewidth = 0.3) +
        geom_errorbar(aes(ymin = .data$media - .data$se, ymax = .data$media + .data$se), position = desvio, width = 0.2, color = CORES_APP$ink) +
        geom_text(aes(y = .data$media + .data$se, label = .data$letra), position = desvio, vjust = -0.55,
                  size = base_size * 0.27, fontface = "bold", color = CORES_APP$ink, family = ap$fonte) +
        scale_fill_manual(values = cores, labels = nomes_traco) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.14)), limits = c(0, topo * 1.14)) +
        rotulos_eixos +
        tema_ranova(base_size, ap$fonte, ap$negrito)
    )
  }

  modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, resposta, prep$bloco, prep$fatores))
  medias <- as.data.frame(silenciar(emmeans::emmeans(modelo, stats::as.formula(paste("~", fator_x, "*", fator_traco)))))
  ggplot(medias, aes(x = .data[[fator_x]], y = .data$emmean, group = .data[[fator_traco]], color = .data[[fator_traco]])) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(aes(ymin = .data$emmean - .data$SE, ymax = .data$emmean + .data$SE), width = 0.12) +
    geom_point(size = base_size * 0.22) +
    scale_color_manual(values = cores, labels = nomes_traco) +
    rotulos_eixos +
    tema_ranova(base_size, ap$fonte, ap$negrito)
}

# Painel com vários gráficos na ordem escolhida, identificados por letras (A, B, C...).
painel_graficos <- function(prep, opcoes, variaveis, tipo = c("medias", "interacao"), fator = NULL,
                            fator_x = NULL, fator_traco = NULL, rotulos = NULL, ncol = 2, letras = TRUE, estilo = NULL,
                            estilo_medias = NULL) {
  tipo <- match.arg(tipo)
  graficos <- lapply(variaveis, function(v) {
    if (identical(tipo, "medias")) {
      grafico_medias(prep, opcoes, v, fator, rotulos, base_size = 11, estilo = estilo_medias)
    } else {
      grafico_interacao(prep, v, fator_x, fator_traco, rotulos, base_size = 11, estilo = estilo, opcoes = opcoes)
    }
  })
  ncol <- max(1L, min(as.integer(ncol), length(graficos)))
  silenciar(ggpubr::ggarrange(
    plotlist = graficos,
    ncol = ncol,
    nrow = ceiling(length(graficos) / ncol),
    labels = if (letras) LETTERS[seq_along(graficos)] else NULL,
    font.label = list(size = 15 * aparencia_grafico(estilo_medias %||% estilo, 11)$tamanho / 11, face = "bold",
                      color = CORES_APP$navy, family = aparencia_grafico(estilo_medias %||% estilo, 11)$fonte),
    common.legend = identical(tipo, "interacao"),
    legend = "bottom",
    align = "hv"
  ))
}

dimensoes_painel <- function(n, ncol) {
  ncol <- max(1L, min(as.integer(ncol), n))
  list(largura = 3.6 * ncol + 0.4, altura = 3.1 * ceiling(n / ncol) + 0.4)
}

# Quatro gráficos de diagnóstico do modelo (os mesmos de plot() para lm/aov), com as
# observações mais extremas identificadas pelo número da linha na planilha.
grafico_residuos <- function(prep, resposta, base_size = 11, aparencia = NULL) {
  ap <- aparencia_grafico(aparencia, base_size)
  base_size <- ap$tamanho
  modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, resposta, prep$bloco, prep$fatores))
  usados <- as.integer(rownames(stats::model.frame(modelo)))
  d <- data.frame(
    linha = prep$linha_planilha[usados],
    ajustado = stats::fitted(modelo),
    residuo = stats::residuals(modelo),
    padronizado = suppressWarnings(stats::rstandard(modelo)),
    alavancagem = stats::hatvalues(modelo),
    cook = suppressWarnings(stats::cooks.distance(modelo))
  )
  d <- d[is.finite(d$padronizado), , drop = FALSE]
  d$raiz <- sqrt(abs(d$padronizado))
  destaque <- d$linha %in% utils::head(d$linha[order(-abs(d$padronizado))], 3)
  rotulos_pontos <- function() geom_text(data = d[destaque, , drop = FALSE], aes(label = .data$linha), vjust = -0.8,
                                         size = base_size * 0.26, color = CORES_APP$red, fontface = "bold", family = ap$fonte)
  pontos <- function() list(geom_point(color = CORES_APP$blue, size = 2.1, alpha = 0.85),
                            scale_y_continuous(expand = expansion(mult = c(0.08, 0.14))))
  suave <- function() geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = CORES_APP$red, linewidth = 0.6, span = 1)

  g1 <- ggplot(d, aes(x = .data$ajustado, y = .data$residuo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CORES_APP$muted) +
    pontos() + suave() + rotulos_pontos() +
    labs(x = "Valores ajustados", y = "Resíduos", title = "Resíduos × ajustados") + tema_ranova(base_size, ap$fonte, ap$negrito)

  qq <- stats::qqnorm(d$padronizado, plot.it = FALSE)
  d$teorico <- qq$x
  g2 <- ggplot(d, aes(x = .data$teorico, y = .data$padronizado)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = CORES_APP$muted) +
    pontos() +
    geom_text(data = d[destaque, , drop = FALSE], aes(label = .data$linha), vjust = -0.8, size = base_size * 0.26, color = CORES_APP$red, fontface = "bold", family = ap$fonte) +
    labs(x = "Quantis teóricos", y = "Resíduos padronizados", title = "Normal Q-Q") + tema_ranova(base_size, ap$fonte, ap$negrito)

  g3 <- ggplot(d, aes(x = .data$ajustado, y = .data$raiz)) +
    pontos() + suave() + rotulos_pontos() +
    labs(x = "Valores ajustados", y = expression(sqrt("|Resíduos padronizados|")), title = "Escala-locação") + tema_ranova(base_size, ap$fonte, ap$negrito)

  p <- modelo$rank
  faixa_h <- range(d$alavancagem)
  hs <- seq(max(0.001, faixa_h[1] * 0.9), min(0.999, max(faixa_h[2] * 1.1, faixa_h[1] + 0.01)), length.out = 80)
  contornos <- do.call(rbind, lapply(c(0.5, 1), function(D) {
    r <- sqrt(D * p * (1 - hs) / hs)
    rbind(data.frame(h = hs, r = r, D = D, lado = "sup"), data.frame(h = hs, r = -r, D = D, lado = "inf"))
  }))
  lim_y <- range(c(d$padronizado, -3, 3))
  contornos <- contornos[contornos$r >= lim_y[1] & contornos$r <= lim_y[2], , drop = FALSE]
  g4 <- ggplot(d, aes(x = .data$alavancagem, y = .data$padronizado)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = CORES_APP$muted) +
    geom_line(data = contornos, aes(x = .data$h, y = .data$r, group = interaction(.data$D, .data$lado)),
              linetype = "dotted", color = CORES_APP$red, inherit.aes = FALSE) +
    pontos() + rotulos_pontos() +
    labs(x = "Alavancagem", y = "Resíduos padronizados", title = "Resíduos × alavancagem",
         caption = "Linhas pontilhadas: distância de Cook 0,5 e 1") + tema_ranova(base_size, ap$fonte, ap$negrito) +
    theme(plot.caption = element_text(size = base_size * 0.7, color = CORES_APP$muted))

  silenciar(ggpubr::ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2))
}

# ---------------------------------------------------------
# Valores discrepantes e pontos influentes
# ---------------------------------------------------------

LIMITE_OUTLIER <- 3

# Para cada variável, marca observações com resíduo studentizado |t| > 3 (possível outlier)
# ou com distância de Cook > 4/(n - p) e |t| > 2 (ponto influente).
detectar_discrepantes <- function(prep) {
  linhas <- lapply(prep$respostas, function(v) {
    modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, v, prep$bloco, prep$fatores))
    usados <- as.integer(rownames(stats::model.frame(modelo)))
    t <- suppressWarnings(stats::rstudent(modelo))
    cook <- suppressWarnings(stats::cooks.distance(modelo))
    n <- length(usados)
    limite_cook <- 4 / max(1, n - modelo$rank)
    outlier <- is.finite(t) & abs(t) > LIMITE_OUTLIER
    influente <- is.finite(cook) & cook > limite_cook & is.finite(t) & abs(t) > 2
    idx <- which(outlier | influente)
    if (length(idx) == 0) return(NULL)
    i_dados <- usados[idx]
    tratamento <- apply(prep$dados[i_dados, prep$fatores, drop = FALSE], 1, function(x) paste(paste(rotulo(prep, prep$fatores), x), collapse = " · "))
    if (!is.null(prep$bloco)) tratamento <- paste0(tratamento, " · ", rotulo(prep, prep$bloco), " ", prep$dados[i_dados, prep$bloco])
    data.frame(
      id = paste0(v, "|", prep$linha_planilha[i_dados]),
      variavel = v,
      linha = prep$linha_planilha[i_dados],
      indice = i_dados,
      tratamento = tratamento,
      observado = prep$dados[[v]][i_dados],
      ajustado = stats::fitted(modelo)[idx],
      t = t[idx],
      cook = cook[idx],
      limite_cook = limite_cook,
      classificacao = ifelse(outlier[idx] & influente[idx], "Outlier e influente", ifelse(outlier[idx], "Possível outlier", "Ponto influente")),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, linhas)
  if (is.null(resultado)) return(NULL)
  resultado[order(-abs(resultado$t)), , drop = FALSE]
}

# Média das demais repetições do mesmo tratamento (mesma combinação de níveis dos fatores).
media_repeticoes <- function(prep, variavel, indice) {
  chave <- interaction(prep$dados[prep$fatores], drop = TRUE)
  mesmas <- which(chave == chave[indice])
  mesmas <- setdiff(mesmas, indice)
  valores <- prep$dados[[variavel]][mesmas]
  if (all(is.na(valores))) return(NA_real_)
  mean(valores, na.rm = TRUE)
}

# Troca os valores escolhidos pela média das demais repetições e devolve a planilha
# atualizada e o registro das trocas (para o relatório).
substituir_discrepantes <- function(planilha, prep, discrepantes, ids) {
  escolhidos <- discrepantes[discrepantes$id %in% ids, , drop = FALSE]
  registro <- data.frame()
  for (k in seq_len(nrow(escolhidos))) {
    item <- escolhidos[k, ]
    novo <- media_repeticoes(prep, item$variavel, item$indice)
    if (is.na(novo)) next
    coluna <- rotulo(prep, item$variavel)
    planilha[item$linha, coluna] <- format(round(novo, 4), trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
    registro <- rbind(registro, data.frame(
      variavel = coluna, linha = item$linha, tratamento = item$tratamento,
      original = item$observado, novo = novo, motivo = item$classificacao, stringsAsFactors = FALSE
    ))
  }
  list(planilha = planilha, registro = registro)
}

# Exporta um gráfico em PNG ou TIFF (LZW) na resolução escolhida.
salvar_grafico <- function(arquivo, grafico, formato = "png", dpi = 300, largura = 7, altura = 4.5) {
  dpi <- as.numeric(dpi)
  if (identical(formato, "tiff")) {
    ggsave(arquivo, grafico, device = "tiff", width = largura, height = altura, units = "in",
           dpi = dpi, compression = "lzw", bg = "white")
  } else {
    ggsave(arquivo, grafico, device = "png", width = largura, height = altura, units = "in",
           dpi = dpi, bg = "white")
  }
}

# ---------------------------------------------------------
# Tabelas em formato de dados (usadas no PDF)
#
# Seguem a mesma lógica das funções do pacote ranova
# (modelo, emmeans, multcomp::cld com t para 2 níveis e Tukey para 3 ou mais).
# ---------------------------------------------------------

num_pt <- function(x, digitos = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = "."))
}

estrelas <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
}

anova_dados <- function(prep, formato = "qm_star", digitos = 3) {
  tabelas <- lapply(prep$respostas, function(v) {
    modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, v, prep$bloco, prep$fatores))
    tab <- summary(modelo)[[1]]
    list(
      fv = trimws(rownames(tab)), gl = tab$Df, qm = tab$`Mean Sq`, f = tab$`F value`, p = tab$`Pr(>F)`,
      cv = sqrt(utils::tail(tab$`Mean Sq`, 1)) / mean(prep$dados[[v]], na.rm = TRUE) * 100
    )
  })
  ref <- tabelas[[1]]
  fv <- ref$fv
  fv[fv == "Residuals"] <- "Resíduo"
  fv <- vapply(fv, function(t) if (t == "Resíduo") t else rotulo_termo(prep, t), character(1))

  colunas <- list()
  destaque <- list()
  for (i in seq_along(prep$respostas)) {
    t <- tabelas[[i]]
    nome <- rotulo(prep, prep$respostas[i])
    if (identical(formato, "f_p_colunas")) {
      colunas[[paste0(nome, "\nF")]] <- num_pt(t$f, digitos)
      colunas[[paste0(nome, "\np")]] <- ifelse(is.na(t$p), "", ifelse(t$p < 0.0001, "< 0,0001", num_pt(t$p, 4)))
      destaque[[paste0(nome, "\nF")]] <- !is.na(t$p) & t$p < 0.05
      destaque[[paste0(nome, "\np")]] <- !is.na(t$p) & t$p < 0.05
    } else if (identical(formato, "f_p_inline")) {
      colunas[[nome]] <- ifelse(is.na(t$f), "", paste0(num_pt(t$f, digitos), " (", ifelse(t$p < 0.0001, "< 0,0001", num_pt(t$p, 4)), ")"))
      destaque[[nome]] <- !is.na(t$p) & t$p < 0.05
    } else {
      colunas[[nome]] <- trimws(paste(num_pt(t$qm, digitos), estrelas(t$p)))
      destaque[[nome]] <- !is.na(t$p) & t$p < 0.05
    }
  }
  corpo <- data.frame(FV = fv, GL = as.character(ref$gl), colunas, check.names = FALSE, stringsAsFactors = FALSE)
  cv <- vapply(tabelas, function(t) num_pt(t$cv, 2), character(1))
  linha_cv <- c("CV (%)", "", if (identical(formato, "f_p_colunas")) as.vector(rbind(cv, "")) else cv)
  corpo <- rbind(corpo, stats::setNames(as.list(linha_cv), names(corpo)))
  marca <- as.data.frame(lapply(names(corpo), function(n) c(destaque[[n]] %||% rep(FALSE, nrow(corpo) - 1), FALSE)),
                         col.names = names(corpo), check.names = FALSE)
  nota <- switch(formato,
    f_p_colunas = "F = valor do teste F; p = valor-p. Em destaque, efeitos significativos a 5%.",
    f_p_inline = "F (p) = valor do teste F com o valor-p entre parênteses. Em destaque, efeitos significativos a 5%.",
    "Valores de quadrado médio. * p < 0,05; ** p < 0,01; *** p < 0,001."
  )
  list(tabela = corpo, destaque = marca, nota = nota)
}

medias_dados <- function(prep, opcoes, fator) {
  colunas <- lapply(prep$respostas, function(v) {
    m <- silenciar(medias_fatorial_cld(prep$dados, v, fator, prep$bloco, prep$fatores, alpha = opcoes$alpha, tipo_se = opcoes$tipo_se))
    m <- m[match(levels(prep$dados[[fator]]), as.character(m$nivel)), ]
    paste0(num_pt(m$media, opcoes$digitos), " ± ", num_pt(m$se, opcoes$digitos), " ", trimws(m$grupo))
  })
  names(colunas) <- rotulo(prep, prep$respostas)
  data.frame(stats::setNames(list(levels(prep$dados[[fator]])), rotulo(prep, fator)), colunas,
             check.names = FALSE, stringsAsFactors = FALSE)
}

# Médias da combinação de dois fatores com letras: minúsculas comparam os níveis de
# `fator_linha` dentro de cada nível de `fator_coluna`; maiúsculas, o contrário.
interacao_letras <- function(prep, opcoes, resposta, fator_linha, fator_coluna) {
  modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, resposta, prep$bloco, prep$fatores))
  ajuste <- function(fator) if (nlevels(prep$dados[[fator]]) == 2) "none" else "tukey"
  letras <- function(formula, fator, conjunto) {
    em <- silenciar(emmeans::emmeans(modelo, stats::as.formula(formula)))
    cld <- as.data.frame(silenciar(multcomp::cld(em, alpha = opcoes$alpha, adjust = ajuste(fator), Letters = conjunto, reversed = TRUE)))
    data.frame(linha = as.character(cld[[fator_linha]]), coluna = as.character(cld[[fator_coluna]]),
               media = cld$emmean, se = cld$SE, letra = trimws(cld$.group), stringsAsFactors = FALSE)
  }
  col <- letras(paste("~", fator_coluna, "|", fator_linha), fator_coluna, LETTERS)
  lin <- letras(paste("~", fator_linha, "|", fator_coluna), fator_linha, letters)
  base <- merge(col, lin[, c("linha", "coluna", "letra")], by = c("linha", "coluna"), suffixes = c("_col", "_lin"))
  if (identical(opcoes$tipo_se, "descritivo")) {
    se <- stats::aggregate(prep$dados[[resposta]], list(linha = prep$dados[[fator_linha]], coluna = prep$dados[[fator_coluna]]),
                           function(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    base$se <- se$x[match(paste(base$linha, base$coluna), paste(se$linha, se$coluna))]
  }
  base
}

interacao_dados <- function(prep, opcoes, resposta, fator_linha, fator_coluna) {
  base <- interacao_letras(prep, opcoes, resposta, fator_linha, fator_coluna)
  base$texto <- paste0(num_pt(base$media, opcoes$digitos), " ± ", num_pt(base$se, opcoes$digitos), " ", base$letra_lin, base$letra_col)
  niveis_l <- levels(prep$dados[[fator_linha]])
  niveis_c <- levels(prep$dados[[fator_coluna]])
  saida <- data.frame(stats::setNames(list(niveis_l), rotulo(prep, fator_linha)), check.names = FALSE, stringsAsFactors = FALSE)
  for (nc in niveis_c) {
    saida[[nc]] <- base$texto[match(paste(niveis_l, nc), paste(base$linha, base$coluna))]
  }
  saida
}

# ---------------------------------------------------------
# Relatório em PDF (A4 retrato, desenhado com grid)
# ---------------------------------------------------------

ler_imagem <- function(caminho) {
  if (!file.exists(caminho) || !requireNamespace("png", quietly = TRUE)) return(NULL)
  tryCatch(png::readPNG(caminho), error = function(e) NULL)
}

# Largura (em polegadas) de um texto no tamanho de fonte indicado.
largura_texto <- function(x, tamanho, negrito = FALSE) {
  linhas <- unlist(strsplit(as.character(x), "\n", fixed = TRUE))
  if (length(linhas) == 0) return(0)
  max(vapply(linhas, function(l) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(l, gp = grid::gpar(fontsize = tamanho, fontface = if (negrito) "bold" else "plain"))), "in", valueOnly = TRUE)
  }, numeric(1)))
}

quebrar_texto <- function(texto, largura_in, tamanho) {
  caracteres <- max(20, floor(largura_in / (tamanho * 0.0075)))
  unlist(lapply(strsplit(texto, "\n", fixed = TRUE)[[1]], function(p) if (nzchar(trimws(p))) strwrap(p, caracteres) else ""))
}

# Divide as colunas de resposta em blocos que cabem na largura da página.
dividir_colunas <- function(n, por) {
  if (n <= por) return(list(seq_len(n)))
  split(seq_len(n), ceiling(seq_len(n) / por))
}

ALTURA_LINHA_TABELA <- 0.27

altura_tabela <- function(tabela, titulo = NULL, nota = NULL) {
  linhas_cab <- max(1, vapply(names(tabela), function(n) length(strsplit(n, "\n", fixed = TRUE)[[1]]), numeric(1)))
  (if (!is.null(titulo)) 0.34 else 0) + 0.18 + 0.17 * linhas_cab + nrow(tabela) * ALTURA_LINHA_TABELA +
    (if (!is.null(nota)) 0.3 else 0.12)
}

# Tabela no estilo do Croma: cabeçalho azul-marinho, linhas zebradas e destaque opcional por célula.
desenhar_tabela <- function(tabela, x, y, largura_max, titulo = NULL, nota = NULL, destaque = NULL,
                            ultima_linha_resumo = FALSE, tamanho = 8.6) {
  if (!is.null(titulo)) {
    grid::grid.text(titulo, x = grid::unit(x, "in"), y = grid::unit(y - 0.12, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 10, fontface = "bold", col = CORES_APP$navy))
    y <- y - 0.34
  }
  textos <- as.matrix(tabela)
  textos[is.na(textos)] <- ""
  cabecalhos <- names(tabela)
  larguras <- vapply(seq_along(cabecalhos), function(j) {
    max(largura_texto(cabecalhos[j], tamanho - 0.6, TRUE), max(vapply(textos[, j], largura_texto, numeric(1), tamanho = tamanho, negrito = j == 1))) + 0.24
  }, numeric(1))
  if (sum(larguras) > largura_max) {
    fator <- largura_max / sum(larguras)
    larguras <- larguras * fator
    tamanho <- max(6.2, tamanho * fator)
  } else if (sum(larguras) < largura_max * 0.7) {
    larguras <- larguras * (largura_max * 0.7 / sum(larguras))
  }
  largura_total <- sum(larguras)
  xs <- x + c(0, cumsum(larguras))
  linhas_cab <- max(vapply(cabecalhos, function(n) length(strsplit(n, "\n", fixed = TRUE)[[1]]), numeric(1)))
  altura_cab <- 0.18 + 0.17 * linhas_cab

  grid::grid.rect(x = grid::unit(x, "in"), y = grid::unit(y, "in"), width = grid::unit(largura_total, "in"),
                  height = grid::unit(altura_cab, "in"), just = c("left", "top"),
                  gp = grid::gpar(fill = CORES_APP$navy, col = NA))
  for (j in seq_along(cabecalhos)) {
    grid::grid.text(cabecalhos[j], x = grid::unit(if (j == 1) xs[j] + 0.1 else xs[j] + larguras[j] / 2, "in"),
                    y = grid::unit(y - altura_cab / 2, "in"), just = c(if (j == 1) "left" else "center", "center"),
                    gp = grid::gpar(fontsize = tamanho - 0.6, fontface = "bold", col = "#FFFFFF", lineheight = 0.95))
  }
  y <- y - altura_cab
  for (i in seq_len(nrow(textos))) {
    resumo <- ultima_linha_resumo && i == nrow(textos)
    grid::grid.rect(x = grid::unit(x, "in"), y = grid::unit(y, "in"), width = grid::unit(largura_total, "in"),
                    height = grid::unit(ALTURA_LINHA_TABELA, "in"), just = c("left", "top"),
                    gp = grid::gpar(fill = if (resumo) CORES_APP$soft else if (i %% 2 == 0) "#F7FAFC" else "#FFFFFF", col = NA))
    for (j in seq_along(cabecalhos)) {
      marcado <- !is.null(destaque) && isTRUE(destaque[i, j])
      if (marcado) {
        grid::grid.rect(x = grid::unit(xs[j] + 0.04, "in"), y = grid::unit(y - 0.04, "in"),
                        width = grid::unit(larguras[j] - 0.08, "in"), height = grid::unit(ALTURA_LINHA_TABELA - 0.08, "in"),
                        just = c("left", "top"), gp = grid::gpar(fill = "#FFF3CD", col = NA))
      }
      grid::grid.text(textos[i, j], x = grid::unit(if (j == 1) xs[j] + 0.1 else xs[j] + larguras[j] / 2, "in"),
                      y = grid::unit(y - ALTURA_LINHA_TABELA / 2, "in"), just = c(if (j == 1) "left" else "center", "center"),
                      gp = grid::gpar(fontsize = tamanho, fontface = if (j == 1 || marcado || resumo) "bold" else "plain",
                                      col = if (j == 1 || resumo) CORES_APP$navy else CORES_APP$ink))
    }
    grid::grid.lines(x = grid::unit(c(x, x + largura_total), "in"), y = grid::unit(rep(y - ALTURA_LINHA_TABELA, 2), "in"),
                     gp = grid::gpar(col = if (i == nrow(textos)) CORES_APP$navy else "#E7EEF4", lwd = if (i == nrow(textos)) 1.1 else 0.6))
    y <- y - ALTURA_LINHA_TABELA
  }
  if (!is.null(nota)) {
    grid::grid.text(nota, x = grid::unit(x, "in"), y = grid::unit(y - 0.15, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 7.2, fontface = "italic", col = CORES_APP$muted))
  }
}

gerar_relatorio_pdf <- function(resultado, arquivo, fator_linha = NULL, fator_coluna = NULL,
                                titulo = "Relatório de análise de variância", responsavel = "", descricao = "",
                                rotulos = NULL, painel = NULL, estilo = NULL, estilo_medias = NULL) {
  prep <- resultado$prep
  opcoes <- resultado$opcoes
  titulo <- trimws(titulo %||% "")
  if (!nzchar(titulo)) titulo <- "Relatório de análise de variância"
  responsavel <- trimws(responsavel %||% "")
  descricao <- trimws(descricao %||% "")
  data_hora <- format(Sys.time(), "%d/%m/%Y às %H:%M")

  L <- 8.27; A <- 11.69
  margem <- 0.55
  largura_util <- L - 2 * margem
  topo_corpo <- A - 1.12
  base_corpo <- 0.78
  logo_app <- ler_imagem("www/img/logo_app.png")
  logo_autor <- ler_imagem("www/img/logo_marlenildo.png")

  # ------ Blocos de conteúdo: cada um tem seção, altura e função de desenho ------
  blocos <- list()
  bloco <- function(secao, altura, desenhar, nova_pagina = FALSE) {
    blocos[[length(blocos) + 1]] <<- list(secao = secao, altura = altura, desenhar = desenhar, nova_pagina = nova_pagina)
  }
  rotulo_secao <- function(texto, x, y) {
    grid::grid.text(toupper(texto), x = grid::unit(x, "in"), y = grid::unit(y, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = CORES_APP$blue))
  }

  # Página 1: resumo
  sig <- if (isTRUE(resultado$significancia$ok)) resultado$significancia$valor else NULL
  leitura <- character()
  if (!is.null(sig)) {
    for (v in prep$respostas) {
      s_v <- sig[sig$variavel == v & sig$significativo, , drop = FALSE]
      inter <- s_v[s_v$interacao, , drop = FALSE]
      if (nrow(inter) > 0) {
        leitura <- c(leitura, paste0(rotulo(prep, v), ": interação ", paste(vapply(inter$termo, function(t) rotulo_termo(prep, t), character(1)), collapse = ", "),
                                     " significativa (p = ", paste(formatar_p(inter$p), collapse = "; "), "); interprete pelo desdobramento."))
      } else if (nrow(s_v) > 0) {
        leitura <- c(leitura, paste0(rotulo(prep, v), ": efeito significativo de ", paste(vapply(s_v$termo, function(t) rotulo_termo(prep, t), character(1)), collapse = ", "), "."))
      } else {
        leitura <- c(leitura, paste0(rotulo(prep, v), ": nenhum efeito significativo dos fatores."))
      }
    }
  }
  niveis_txt <- vapply(prep$fatores, function(f) paste0(rotulo(prep, f), " (", nlevels(prep$dados[[f]]), " níveis: ",
                                                            paste(utils::head(levels(prep$dados[[f]]), 8), collapse = ", "),
                                                            if (nlevels(prep$dados[[f]]) > 8) ", ..." else "", ")"), character(1))
  itens <- list(
    c("Fatores", paste(niveis_txt, collapse = "; ")),
    c("Variáveis resposta", paste(rotulo(prep, prep$respostas), collapse = ", ")),
    c("Comparação de médias", "Teste t (2 níveis) ou Tukey (3 ou mais níveis)"),
    c("Erro-padrão das médias", if (identical(opcoes$tipo_se, "descritivo")) "Descritivo (dos dados)" else "Do modelo (médias ajustadas)")
  )
  if (nzchar(responsavel)) itens <- c(itens, list(c("Responsável", responsavel)))
  itens <- c(itens, list(c("Emissão", data_hora)))
  if (!is.null(resultado$substituicoes) && nrow(resultado$substituicoes) > 0) {
    leitura <- c(leitura, paste0(nrow(resultado$substituicoes), " valor(es) discrepante(s) substituído(s) pela média das repetições (ver tabela abaixo)."))
  }
  itens_linhas <- lapply(itens, function(item) quebrar_texto(item[2], largura_util - 2.1, 9))
  linhas_leitura <- unlist(lapply(leitura, function(t) {
    q <- quebrar_texto(t, largura_util - 0.5, 8.8)
    c(paste0("•  ", q[1]), if (length(q) > 1) paste0("    ", q[-1]))
  }))
  linhas_descricao <- if (nzchar(descricao)) quebrar_texto(descricao, largura_util, 9) else "Nenhuma descrição informada."
  linhas_descricao <- utils::head(linhas_descricao, 14)
  altura_resumo <- 0.85 + 1.05 + 0.34 + sum(vapply(itens_linhas, function(l) 0.32 + 0.19 * (length(l) - 1), numeric(1))) +
    0.45 + 0.24 + 0.19 * length(linhas_leitura) + 0.5 + 0.3 + 0.19 * length(linhas_descricao)

  bloco("Resumo", altura_resumo, function(y) {
    grid::grid.text(titulo, x = grid::unit(margem, "in"), y = grid::unit(y - 0.18, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 17, fontface = "bold", col = CORES_APP$navy))
    grid::grid.text("Análise de variância com comparação de médias, gerada com o pacote R ranova.",
                    x = grid::unit(margem, "in"), y = grid::unit(y - 0.46, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 9, col = CORES_APP$muted))

    # Indicadores no padrão dos cartões do Croma
    tratamentos <- prod(vapply(prep$fatores, function(f) nlevels(prep$dados[[f]]), numeric(1)))
    kpis <- list(
      c("Delineamento", prep$delineamento, if (identical(prep$delineamento, "DBC")) paste(nlevels(prep$dados[[prep$bloco]]), "blocos") else "inteiramente casualizado"),
      c("Tratamentos", as.character(tratamentos), paste(vapply(prep$fatores, function(f) nlevels(prep$dados[[f]]), numeric(1)), collapse = " × ")),
      c("Observações", as.character(prep$n_obs), paste(length(prep$respostas), if (length(prep$respostas) == 1) "variável resposta" else "variáveis resposta")),
      c("Significância", paste0(formatC(opcoes$alpha * 100, format = "f", digits = 0), "%"), "nível dos testes")
    )
    yk <- y - 0.72
    lk <- (largura_util - 3 * 0.14) / 4
    for (i in seq_along(kpis)) {
      xk <- margem + (i - 1) * (lk + 0.14)
      grid::grid.rect(x = grid::unit(xk, "in"), y = grid::unit(yk, "in"), width = grid::unit(lk, "in"), height = grid::unit(0.85, "in"),
                      just = c("left", "top"), gp = grid::gpar(fill = CORES_APP$canvas, col = CORES_APP$line))
      grid::grid.rect(x = grid::unit(xk, "in"), y = grid::unit(yk, "in"), width = grid::unit(0.05, "in"), height = grid::unit(0.85, "in"),
                      just = c("left", "top"), gp = grid::gpar(fill = CORES_APP$blue, col = NA))
      grid::grid.text(toupper(kpis[[i]][1]), x = grid::unit(xk + 0.17, "in"), y = grid::unit(yk - 0.17, "in"), just = c("left", "center"),
                      gp = grid::gpar(fontsize = 7, fontface = "bold", col = CORES_APP$muted))
      grid::grid.text(kpis[[i]][2], x = grid::unit(xk + 0.17, "in"), y = grid::unit(yk - 0.43, "in"), just = c("left", "center"),
                      gp = grid::gpar(fontsize = 15, fontface = "bold", col = CORES_APP$navy))
      grid::grid.text(kpis[[i]][3], x = grid::unit(xk + 0.17, "in"), y = grid::unit(yk - 0.68, "in"), just = c("left", "center"),
                      gp = grid::gpar(fontsize = 7.2, col = CORES_APP$muted))
    }

    yy <- yk - 0.85 - 0.38
    rotulo_secao("Identificação", margem, yy)
    yy <- yy - 0.34
    for (k in seq_along(itens)) {
      linhas <- itens_linhas[[k]]
      grid::grid.text(itens[[k]][1], x = grid::unit(margem, "in"), y = grid::unit(yy, "in"), just = c("left", "center"),
                      gp = grid::gpar(fontsize = 8.8, col = CORES_APP$muted))
      for (j in seq_along(linhas)) {
        grid::grid.text(linhas[j], x = grid::unit(margem + 1.9, "in"), y = grid::unit(yy - (j - 1) * 0.19, "in"), just = c("left", "center"),
                        gp = grid::gpar(fontsize = 9, fontface = "bold", col = CORES_APP$ink))
      }
      yy <- yy - 0.19 * (length(linhas) - 1)
      grid::grid.lines(x = grid::unit(c(margem, margem + largura_util), "in"), y = grid::unit(rep(yy - 0.13, 2), "in"), gp = grid::gpar(col = "#EEF3F7"))
      yy <- yy - 0.32
    }

    yy <- yy - 0.13
    rotulo_secao("Leitura rápida", margem, yy)
    altura_leitura <- 0.24 + 0.19 * length(linhas_leitura)
    grid::grid.rect(x = grid::unit(margem, "in"), y = grid::unit(yy - 0.2, "in"), width = grid::unit(largura_util, "in"),
                    height = grid::unit(altura_leitura, "in"), just = c("left", "top"), gp = grid::gpar(fill = "#F8FBF9", col = "#D5E8DA"))
    grid::grid.rect(x = grid::unit(margem, "in"), y = grid::unit(yy - 0.2, "in"), width = grid::unit(0.05, "in"),
                    height = grid::unit(altura_leitura, "in"), just = c("left", "top"), gp = grid::gpar(fill = CORES_APP$green, col = NA))
    for (k in seq_along(linhas_leitura)) {
      grid::grid.text(linhas_leitura[k], x = grid::unit(margem + 0.18, "in"), y = grid::unit(yy - 0.34 - (k - 1) * 0.19, "in"),
                      just = c("left", "center"), gp = grid::gpar(fontsize = 8.8, col = CORES_APP$ink))
    }

    yy <- yy - 0.2 - altura_leitura - 0.4
    rotulo_secao("Descrição do experimento / observações", margem, yy)
    for (k in seq_along(linhas_descricao)) {
      grid::grid.text(linhas_descricao[k], x = grid::unit(margem, "in"), y = grid::unit(yy - 0.3 - (k - 1) * 0.19, "in"), just = c("left", "center"),
                      gp = grid::gpar(fontsize = 9, fontface = if (nzchar(descricao)) "plain" else "italic", col = if (nzchar(descricao)) "#3E5467" else "#9AAAB8"))
    }
  })

  if (isTRUE(resultado$diagnostico$ok)) local({
    diag <- resultado$diagnostico$valor
    marca <- matrix(FALSE, nrow(diag), ncol(diag))
    marca[, 3] <- diag[[3]] == "Não atendida"
    marca[, 5] <- diag[[5]] == "Não atendida"
    names(diag) <- c("Variável", "p\nShapiro-Wilk", "Normalidade", "p\nLevene", "Homogeneidade")
    nota_diag <- "Shapiro-Wilk (normalidade dos resíduos) e Levene (homogeneidade). Em destaque, pressupostos não atendidos."
    bloco("Resumo", altura_tabela(diag, "Pressupostos da ANOVA", nota_diag) + 0.2, function(y) {
      desenhar_tabela(diag, margem, y, largura_util, titulo = "Pressupostos da ANOVA", destaque = marca, nota = nota_diag)
    })
  })

  # Valores substituídos e discrepantes
  subs <- resultado$substituicoes
  if (!is.null(subs) && nrow(subs) > 0) local({
    tab <- data.frame(
      "Variável" = subs$variavel, "Linha" = as.character(subs$linha), "Tratamento" = subs$tratamento,
      "Valor original" = num_pt(subs$original, 3), "Média usada" = num_pt(subs$novo, 3), "Motivo" = subs$motivo,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    nota <- "Valores trocados pela média das demais repetições do mesmo tratamento antes da análise."
    bloco("Resumo", altura_tabela(tab, "Valores substituídos", nota) + 0.2, function(y) {
      desenhar_tabela(tab, margem, y, largura_util, titulo = "Valores substituídos", nota = nota, tamanho = 8)
    })
  })
  disc <- resultado$discrepantes
  if (!is.null(disc) && nrow(disc) > 0) local({
    disc <- utils::head(disc, 15)
    tab <- data.frame(
      "Variável" = rotulo(prep, disc$variavel), "Linha" = as.character(disc$linha), "Tratamento" = disc$tratamento,
      "Observado" = num_pt(disc$observado, 3), "t" = num_pt(disc$t, 2), "Cook" = num_pt(disc$cook, 3), "Situação" = disc$classificacao,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    marca <- matrix(FALSE, nrow(tab), ncol(tab))
    marca[, 7] <- grepl("outlier", disc$classificacao, ignore.case = TRUE)
    nota <- "Outlier: resíduo studentizado |t| > 3. Influente: Cook > 4/(n - p) com |t| > 2. Confira esses valores na planilha."
    bloco("Resumo", altura_tabela(tab, "Possíveis valores discrepantes", nota) + 0.2, function(y) {
      desenhar_tabela(tab, margem, y, largura_util, titulo = "Possíveis valores discrepantes", nota = nota, destaque = marca, tamanho = 8)
    })
  })

  # Diagnóstico dos resíduos: quatro gráficos por variável, duas variáveis por página
  primeira_diag <- TRUE
  for (v in prep$respostas) local({
    v <- v
    grafico <- tentar(grafico_residuos(prep, v, base_size = 9, aparencia = estilo_medias))
    if (!isTRUE(grafico$ok)) return(NULL)
    altura <- (topo_corpo - base_corpo) / 2 - 0.05
    bloco("Diagnóstico dos resíduos", altura, function(y) {
      grid::grid.text(paste0("Diagnóstico dos resíduos: ", rotulo(prep, v)), x = grid::unit(margem, "in"), y = grid::unit(y - 0.12, "in"),
                      just = c("left", "center"), gp = grid::gpar(fontsize = 10, fontface = "bold", col = CORES_APP$navy))
      vp <- grid::viewport(x = grid::unit(margem, "in"), y = grid::unit(y - 0.28, "in"), width = grid::unit(largura_util, "in"),
                           height = grid::unit(altura - 0.4, "in"), just = c("left", "top"))
      print(grafico$valor, vp = vp)
    }, nova_pagina = primeira_diag)
    primeira_diag <<- FALSE
  })

  # ANOVA
  por_bloco <- if (identical(opcoes$formato, "f_p_colunas")) 3 else 4
  anova <- anova_dados(prep, opcoes$formato, opcoes$digitos_anova)
  grupos <- dividir_colunas(length(prep$respostas), por_bloco)
  for (g in seq_along(grupos)) {
    idx <- grupos[[g]]
    cols <- if (identical(opcoes$formato, "f_p_colunas")) c(1, 2, as.vector(rbind(2 + 2 * idx - 1, 2 + 2 * idx))) else c(1, 2, 2 + idx)
    tab <- anova$tabela[, cols, drop = FALSE]
    marca <- as.matrix(anova$destaque[, cols, drop = FALSE])
    titulo_tab <- if (length(grupos) > 1) paste0("Resumo da análise de variância (parte ", g, " de ", length(grupos), ")") else "Resumo da análise de variância"
    local({
      tab <- tab; marca <- marca; titulo_tab <- titulo_tab
      bloco("Análise de variância", altura_tabela(tab, titulo_tab, anova$nota) + 0.2, function(y) {
        desenhar_tabela(tab, margem, y, largura_util, titulo = titulo_tab, nota = anova$nota, destaque = marca, ultima_linha_resumo = TRUE)
      }, nova_pagina = g == 1)
    })
  }

  # Médias
  primeira_media <- TRUE
  for (fator in prep$fatores) {
    medias <- tentar(medias_dados(prep, opcoes, fator))
    if (!isTRUE(medias$ok)) next
    for (idx in dividir_colunas(length(prep$respostas), 3)) {
      tab <- medias$valor[, c(1, 1 + idx), drop = FALSE]
      titulo_tab <- paste0("Médias ± erro-padrão por ", rotulo(prep, fator))
      nota <- "Médias seguidas pela mesma letra na coluna não diferem entre si pelo teste t (2 níveis) ou Tukey (3 ou mais níveis)."
      local({
        tab <- tab; titulo_tab <- titulo_tab; nota <- nota
        bloco("Médias", altura_tabela(tab, titulo_tab, nota) + 0.2, function(y) {
          desenhar_tabela(tab, margem, y, largura_util, titulo = titulo_tab, nota = nota)
        }, nova_pagina = primeira_media)
      })
      primeira_media <- FALSE
    }
  }

  # Desdobramento
  if (!is.null(fator_linha) && !is.null(fator_coluna)) {
    primeira <- TRUE
    for (v in prep$respostas) {
      inter <- tentar(interacao_dados(prep, opcoes, v, fator_linha, fator_coluna))
      if (!isTRUE(inter$ok)) next
      titulo_tab <- paste0(rotulo(prep, v), ": ", rotulo(prep, fator_linha), " × ", rotulo(prep, fator_coluna))
      nota <- paste0("Minúsculas comparam as linhas dentro de cada coluna; maiúsculas comparam as colunas dentro de cada linha.",
                     if (length(prep$fatores) == 3) " Médias ajustadas sobre os níveis do fator não exibido." else "")
      local({
        tab <- inter$valor; titulo_tab <- titulo_tab; nota <- nota
        bloco("Desdobramento da interação", altura_tabela(tab, titulo_tab, nota) + 0.2, function(y) {
          desenhar_tabela(tab, margem, y, largura_util, titulo = titulo_tab, nota = nota)
        }, nova_pagina = primeira)
      })
      primeira <- FALSE
    }
  }

  # Gráficos: quatro por página (2 × 2)
  graficos <- list()
  for (v in prep$respostas) {
    for (fator in prep$fatores) {
      graficos[[length(graficos) + 1]] <- list(
        grafico = tentar(grafico_medias(prep, opcoes, v, fator, rotulos, base_size = 10, estilo = estilo_medias)),
        legenda = paste0(rotulo_grafico(prep, v, rotulos), " em função de ", rotulo_grafico(prep, fator, rotulos))
      )
    }
    if (!is.null(fator_linha) && !is.null(fator_coluna)) {
      graficos[[length(graficos) + 1]] <- list(
        grafico = tentar(grafico_interacao(prep, v, fator_linha, fator_coluna, rotulos, base_size = 10, estilo = estilo, opcoes = opcoes)),
        legenda = paste0("Interação ", rotulo_grafico(prep, fator_linha, rotulos), " × ", rotulo_grafico(prep, fator_coluna, rotulos), ": ", rotulo_grafico(prep, v, rotulos))
      )
    }
  }
  graficos <- Filter(function(g) isTRUE(g$grafico$ok), graficos)
  if (length(graficos) > 0) {
    for (pagina in split(seq_along(graficos), ceiling(seq_along(graficos) / 6))) {
      local({
        itens <- graficos[pagina]
        numeros <- pagina
        bloco("Gráficos", topo_corpo - base_corpo, function(y) {
          lg <- (largura_util - 0.3) / 2
          ag <- (y - base_corpo - 0.3) / 3
          for (k in seq_along(itens)) {
            coluna <- (k - 1) %% 2; linha <- (k - 1) %/% 2
            x0 <- margem + coluna * (lg + 0.3)
            y0 <- y - linha * (ag + 0.15)
            grid::grid.text(paste0("Figura ", numeros[k], ". ", itens[[k]]$legenda), x = grid::unit(x0, "in"), y = grid::unit(y0 - 0.1, "in"),
                            just = c("left", "center"), gp = grid::gpar(fontsize = 8.5, fontface = "bold", col = CORES_APP$navy))
            vp <- grid::viewport(x = grid::unit(x0, "in"), y = grid::unit(y0 - 0.22, "in"), width = grid::unit(lg, "in"),
                                 height = grid::unit(ag - 0.25, "in"), just = c("left", "top"))
            print(itens[[k]]$grafico$valor, vp = vp)
          }
        }, nova_pagina = TRUE)
      })
    }
  }

  # Painel de gráficos montado pelo usuário
  if (!is.null(painel)) {
    bloco("Painel de gráficos", topo_corpo - base_corpo, function(y) {
      dims <- painel$dimensoes
      altura_disp <- y - base_corpo - 0.1
      escala <- min(largura_util / dims$largura, altura_disp / dims$altura)
      w <- dims$largura * escala; h <- dims$altura * escala
      vp <- grid::viewport(x = grid::unit(margem + (largura_util - w) / 2, "in"), y = grid::unit(y - 0.05, "in"),
                           width = grid::unit(w, "in"), height = grid::unit(h, "in"), just = c("left", "top"))
      print(painel$grafico, vp = vp)
    }, nova_pagina = TRUE)
  }

  # ------ Paginação ------
  paginas <- list()
  atual <- NULL
  y <- topo_corpo
  for (b in blocos) {
    cabe <- !is.null(atual) && !b$nova_pagina && (y - b$altura) >= base_corpo && identical(atual$secao, b$secao)
    if (!cabe) {
      if (!is.null(atual)) paginas[[length(paginas) + 1]] <- atual
      atual <- list(secao = b$secao, itens = list())
      y <- topo_corpo
    }
    atual$itens[[length(atual$itens) + 1]] <- list(y = y, desenhar = b$desenhar)
    y <- y - b$altura
  }
  if (!is.null(atual)) paginas[[length(paginas) + 1]] <- atual
  total <- length(paginas)

  if (capabilities("cairo")) {
    grDevices::cairo_pdf(arquivo, width = L, height = A, onefile = TRUE, family = "sans")
  } else {
    grDevices::pdf(arquivo, width = L, height = A, title = titulo)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  for (n in seq_along(paginas)) {
    grid::grid.newpage()
    pg <- paginas[[n]]
    # Cabeçalho branco, como no Minhas Entregas
    if (!is.null(logo_app)) {
      grid::grid.raster(logo_app, x = grid::unit(margem, "in"), y = grid::unit(A - 0.52, "in"),
                        width = grid::unit(0.5, "in"), height = grid::unit(0.5, "in"), just = c("left", "center"))
    }
    grid::grid.text("Ranova", x = grid::unit(margem + 0.62, "in"), y = grid::unit(A - 0.44, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 15, fontface = "bold", col = CORES_APP$navy))
    grid::grid.text("Análise de variância de experimentos fatoriais", x = grid::unit(margem + 0.62, "in"), y = grid::unit(A - 0.64, "in"),
                    just = c("left", "center"), gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = CORES_APP$blue))
    grid::grid.text(toupper(pg$secao), x = grid::unit(L - margem, "in"), y = grid::unit(A - 0.44, "in"), just = c("right", "center"),
                    gp = grid::gpar(fontsize = 8.2, fontface = "bold", col = CORES_APP$navy))
    grid::grid.text(paste("Página", n, "de", total), x = grid::unit(L - margem, "in"), y = grid::unit(A - 0.64, "in"),
                    just = c("right", "center"), gp = grid::gpar(fontsize = 7.8, col = CORES_APP$muted))
    grid::grid.lines(x = grid::unit(c(margem, L - margem), "in"), y = grid::unit(rep(A - 0.86, 2), "in"),
                     gp = grid::gpar(col = CORES_APP$navy, lwd = 2))
    # Rodapé com a logo do autor
    grid::grid.lines(x = grid::unit(c(margem, L - margem), "in"), y = grid::unit(rep(0.58, 2), "in"), gp = grid::gpar(col = CORES_APP$line, lwd = 0.8))
    grid::grid.text("Desenvolvido por", x = grid::unit(margem, "in"), y = grid::unit(0.33, "in"), just = c("left", "center"),
                    gp = grid::gpar(fontsize = 7.2, col = "#587086"))
    if (!is.null(logo_autor)) {
      alt <- 0.36
      grid::grid.raster(logo_autor, x = grid::unit(margem + 0.95, "in"), y = grid::unit(0.33, "in"),
                        width = grid::unit(alt * ncol(logo_autor) / nrow(logo_autor), "in"), height = grid::unit(alt, "in"), just = c("left", "center"))
    }
    titulo_rodape <- if (nchar(titulo) > 60) paste0(substr(titulo, 1, 59), "…") else titulo
    grid::grid.text(titulo_rodape, x = grid::unit(L - margem, "in"), y = grid::unit(0.42, "in"), just = c("right", "center"),
                    gp = grid::gpar(fontsize = 7.2, fontface = "bold", col = "#587086"))
    grid::grid.text(paste0("Gerado em ", data_hora, "  |  Ranova v", VERSAO_APP, " · pacote ranova ", VERSAO_PACOTE),
                    x = grid::unit(L - margem, "in"), y = grid::unit(0.25, "in"), just = c("right", "center"),
                    gp = grid::gpar(fontsize = 7, col = "#587086"))
    for (item in pg$itens) item$desenhar(item$y)
  }
  invisible(arquivo)
}
