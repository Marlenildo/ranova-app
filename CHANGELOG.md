# Changelog

Todas as mudanças relevantes do Ranova são registradas aqui.

O formato segue o [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/)
e o projeto usa [Versionamento Semântico](https://semver.org/lang/pt-BR/):
`MAIOR.MENOR.CORREÇÃO`.

## [Não lançado]

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
