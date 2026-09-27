# Gera a logo do Ranova (www/img/logo_app.png e www/img/favicon.png).
# Desenho plano, no mesmo traço do Croma e do Minhas Entregas: três médias de
# tratamentos (barras com pontas arredondadas, cores CIELCH) sobre a linha de base
# e o erro-padrão da maior média, em azul institucional.
# Uso: Rscript scripts/gerar_logo_app.R

library(colorspace)

MEDIAS <- c(.34, .62, .9)
MATIZES <- c(250, 148, 78)

desenhar_logo <- function(escala = 1) {
  par(mar = c(0, 0, 0, 0), bg = "transparent")
  plot.new(); plot.window(c(-1, 1), c(-1, 1), asp = 1)

  espessura <- 92 * escala
  base <- -.72
  posicoes <- c(-.5, 0, .5)

  for (i in seq_along(MEDIAS)) {
    cor <- hex(polarLAB(L = if (MATIZES[i] > 200) 54 else 64,
                        C = if (MATIZES[i] > 200) 34 else 44, H = MATIZES[i]), fixup = TRUE)
    segments(posicoes[i], base, posicoes[i], base + MEDIAS[i] * 1.3,
             col = cor, lwd = espessura, lend = "round")
  }

  # Erro-padrão da maior média
  topo <- base + MEDIAS[3] * 1.3
  segments(.5, topo + .12, .5, topo + .3, col = "#173B5B", lwd = espessura * .38, lend = "round")
  segments(.36, topo + .3, .64, topo + .3, col = "#173B5B", lwd = espessura * .38, lend = "round")

  # Linha de base
  segments(-.86, -.9, .86, -.9, col = "#173B5B", lwd = espessura * .38, lend = "round")
}

tipo <- if (capabilities("aqua")) "quartz" else "cairo"
dir.create("www/img", showWarnings = FALSE, recursive = TRUE)
png("www/img/logo_app.png", width = 512, height = 512, bg = "transparent", type = tipo)
desenhar_logo(); dev.off()
png("www/img/favicon.png", width = 64, height = 64, bg = "transparent", type = tipo)
desenhar_logo(escala = 64 / 512); dev.off()
