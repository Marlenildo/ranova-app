# Changelog

Todas as mudanças relevantes do Ranova são registradas aqui.

O formato segue o [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/)
e o projeto usa [Versionamento Semântico](https://semver.org/lang/pt-BR/):
`MAIOR.MENOR.CORREÇÃO`.

## [Não lançado]

## [1.6.0] - 2026-09-27

### Adicionado

- **Nomes dos níveis dos fatores** editáveis nos gráficos (por exemplo, "0" →
  "Controle"), junto com os nomes das variáveis e dos fatores, numa caixa
  recolhível em Resultados → Gráficos. Valem para eixos, legendas, painel e
  PDF; a planilha e as tabelas continuam com os nomes originais.

### Alterado

- Os nomes nos gráficos saem do cartão do painel e passam para a aba Gráficos,
  valendo para todas as variáveis e fatores (antes, só para os escolhidos no
  painel).

## [1.5.0] - 2026-09-27

### Adicionado

- Cores também no **gráfico de médias de um fator**: uma cor para todas as
  barras ou uma cor por nível, com as mesmas paletas da interação (inclusive
  tons de cinza) ou cores escolhidas. Vale para o gráfico, o painel e o PDF.

## [1.4.0] - 2026-09-27

### Adicionado

- Gráfico de interação em **linhas ou barras**. Nas barras, as letras do
  desdobramento aparecem sobre cada média (minúsculas comparam o eixo X dentro
  de cada cor; maiúsculas, as cores dentro de cada nível do eixo X).
- **Cores dos grupos**: paletas prontas (Ranova, tons de cinza, azul, verde,
  terra, alto contraste para daltônicos e viridis) ou cor escolhida para cada
  nível. Vale para o gráfico, o painel e o PDF.
- **Quatro gráficos de diagnóstico** dos resíduos (resíduos × ajustados, Normal
  Q-Q, escala-locação e resíduos × alavancagem com curvas de Cook), com as
  linhas da planilha mais extremas identificadas.
- Aba **Discrepantes**: indica possíveis outliers (resíduo studentizado acima de
  ±3) e pontos influentes (distância de Cook acima de 4/(n − p) com resíduo
  acima de ±2), com valor observado, ajustado e média das repetições. Os valores
  escolhidos podem ser **substituídos pela média das demais repetições do mesmo
  tratamento**, com nova análise automática e opção de desfazer.
- PDF com a tabela de valores substituídos, a de possíveis discrepantes e os
  quatro gráficos de diagnóstico de cada variável.

## [1.3.0] - 2026-09-27

### Adicionado

- Aba **Colar** no cartão Dados, como no Croma: cole as células copiadas do
  Excel, planilha ou outra fonte (colunas separadas por tabulação, ponto e
  vírgula ou espaços), com opção de primeira linha como cabeçalho. Vírgula
  decimal é aceita.

## [1.2.0] - 2026-09-27

### Alterado

- Relatório em PDF passa para **A4 retrato**: primeira página com indicadores
  (delineamento, tratamentos, observações e significância), identificação,
  leitura rápida e descrição; pressupostos, ANOVA, médias e desdobramento em
  tabelas na largura da página; gráficos em grade de 2 × 3, numerados.
- Tabelas do PDF divididas em partes quando há muitas variáveis resposta.

### Corrigido

- Destaque da tabela de pressupostos no PDF marcava células de outra tabela.

## [1.1.0] - 2026-09-27

### Adicionado

- **Painel de gráficos**: escolha das variáveis e da ordem, nomes personalizados
  para eixos e legendas e identificação por letras (A, B, C, D...), para médias
  com letras ou interação.
- Exportação dos gráficos em **PNG ou TIFF** (LZW), com resolução de 150, 300 ou
  600 dpi e largura e altura em centímetros.
- **Relatório em PDF** (A4 paisagem) no padrão do Croma, com cabeçalho branco como
  no Minhas Entregas: resumo do experimento, leitura rápida, pressupostos, ANOVA
  com efeitos significativos em destaque, médias, desdobramento da interação,
  gráficos numerados e o painel de gráficos.

### Alterado

- Interface em duas colunas no computador, com cartões numerados: dados,
  estrutura e opções à esquerda; planilha, resultados, painel de gráficos e
  relatório à direita. No celular, os cartões ficam empilhados.
- Os nomes definidos no painel valem também para os gráficos individuais e para
  o PDF.
- Passa a exigir o pacote `ranova` 0.4.5, em que a tabela de ANOVA mostra
  `Resíduo` e deixa GL em branco na linha do CV.

### Removido

- Relatório em HTML, substituído pelo PDF.

## [1.0.0] - 2026-09-27

Primeira versão, com o nome **Ranova**.

### Adicionado

- Identidade visual alinhada ao Croma e ao Minhas Entregas: logo própria
  (médias de tratamentos com erro-padrão), favicon e logo do autor no rodapé
  do app e do relatório.
- Entrada de dados por planilha montada a partir dos fatores, importação de
  Excel/CSV e exemplo pronto; planilha editável que aceita colar do Excel.
- Sugestão automática de bloco, fatores e variáveis resposta.
- Análise em DIC ou DBC com 1 a 3 fatores usando o pacote `ranova`: ANOVA com
  CV, leitura rápida dos efeitos significativos, pressupostos (Shapiro-Wilk,
  Levene e resíduos), médias com letras, desdobramento da interação e gráficos
  com download em PNG.
- Relatório HTML pronto para impressão, com título, responsável e descrição.
