# Gera a logo do Ranova (www/img/logo_app.png e www/img/favicon.png).
# Desenho plano, no mesmo traço do Croma e do Minhas Entregas: um quadro de
# bordas arredondadas em azul institucional com três médias de tratamentos
# (barras com pontas arredondadas, cores CIELCH) crescendo dentro dele.
# Uso: Rscript scripts/gerar_logo_app.R

library(grid)
library(colorspace)

MEDIAS <- c(.36, .58, .8)
MATIZES <- c(250, 148, 78)
AZUL <- "#173B5B"

desenhar_logo <- function(lado) {
  grid.newpage()
  u <- function(x) unit(x, "npc")
  espessura_quadro <- lado * .075

  # Quadro de bordas arredondadas
  grid.roundrect(u(.5), u(.5), u(.84), u(.84), r = unit(.2, "snpc"),
                 gp = gpar(col = AZUL, fill = "white", lwd = espessura_quadro * 72 / 96, linejoin = "round"))

  # Barras: largas, com pontas arredondadas, apoiadas numa base comum
  largura <- lado * .2 * 72 / 96
  base <- .29
  posicoes <- c(.31, .5, .69)
  for (i in seq_along(MEDIAS)) {
    cor <- hex(polarLAB(L = if (MATIZES[i] > 200) 54 else 64,
                        C = if (MATIZES[i] > 200) 34 else 44, H = MATIZES[i]), fixup = TRUE)
    grid.segments(u(posicoes[i]), u(base), u(posicoes[i]), u(base + MEDIAS[i] * .52),
                  gp = gpar(col = cor, lwd = largura, lineend = "round"))
  }
}

tipo <- if (capabilities("aqua")) "quartz" else "cairo"
dir.create("www/img", showWarnings = FALSE, recursive = TRUE)
png("www/img/logo_app.png", width = 512, height = 512, bg = "transparent", type = tipo)
desenhar_logo(512); dev.off()
png("www/img/favicon.png", width = 64, height = 64, bg = "transparent", type = tipo)
desenhar_logo(64); dev.off()
