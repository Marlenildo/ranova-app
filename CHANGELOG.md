# Changelog

Todas as mudanças relevantes do Ranova são registradas aqui.

O formato segue o [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/)
e o projeto usa [Versionamento Semântico](https://semver.org/lang/pt-BR/):
`MAIOR.MENOR.CORREÇÃO`.

## [Não lançado]

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
