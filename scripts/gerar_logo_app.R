# Gera a logo do Ranova (www/img/logo_app.png e www/img/favicon.png).
# Desenho plano, sem fundo, no mesmo traço do Minhas Entregas: um quadro de
# bordas arredondadas em azul institucional e, dentro dele, três médias de
# tratamentos (barras com pontas arredondadas, cores CIELCH).
# Quadro e barras têm a espessura dos arcos do Minhas Entregas; `escala` a ajusta
# ao tamanho da imagem (1 = 512 px).
# Uso: Rscript scripts/gerar_logo_app.R

library(colorspace)

MEDIAS <- c(.3, .5, .72)
MATIZES <- c(250, 148, 78)

# Contorno de um quadrado de bordas arredondadas centrado na origem
quadro <- function(meio_lado, raio, cor, espessura) {
  canto <- function(cx, cy, de) {
    angulo <- seq(de, de + pi / 2, length.out = 40)
    cbind(cx + raio * cos(angulo), cy + raio * sin(angulo))
  }
  d <- meio_lado - raio
  pontos <- rbind(canto(d, d, 0), canto(-d, d, pi / 2), canto(-d, -d, pi), canto(d, -d, 3 * pi / 2))
  polygon(pontos[, 1], pontos[, 2], border = cor, col = NA, lwd = espessura, ljoin = "round")
}

desenhar_logo <- function(escala = 1) {
  par(mar = c(0, 0, 0, 0), bg = "transparent")
  plot.new(); plot.window(c(-1, 1), c(-1, 1), asp = 1)

  espessura <- 70 * escala
  quadro(.8, .3, "#173B5B", espessura)

  base <- -.4
  posicoes <- c(-.36, 0, .36)
  for (i in seq_along(MEDIAS)) {
    cor <- hex(polarLAB(L = if (MATIZES[i] > 200) 54 else 64,
                        C = if (MATIZES[i] > 200) 34 else 44, H = MATIZES[i]), fixup = TRUE)
    segments(posicoes[i], base, posicoes[i], base + MEDIAS[i] * 1.1,
             col = cor, lwd = espessura, lend = "round")
  }
}

tipo <- if (capabilities("aqua")) "quartz" else "cairo"
dir.create("www/img", showWarnings = FALSE, recursive = TRUE)
png("www/img/logo_app.png", width = 512, height = 512, bg = "transparent", type = tipo)
desenhar_logo(); dev.off()
png("www/img/favicon.png", width = 64, height = 64, bg = "transparent", type = tipo)
desenhar_logo(escala = 64 / 512); dev.off()
