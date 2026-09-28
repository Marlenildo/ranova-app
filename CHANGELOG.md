# Changelog

Todas as mudanças relevantes do Ranova são registradas aqui.

O formato segue o [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/)
e o projeto usa [Versionamento Semântico](https://semver.org/lang/pt-BR/):
`MAIOR.MENOR.CORREÇÃO`.

## [Não lançado]

## [2.2.0] - 2026-09-28

### Adicionado

- Botão **Copiar tabela** em todas as tabelas de resultados (ANOVA,
  pressupostos, discrepantes, médias e desdobramentos) e no relatório HTML.
  Copia a tabela sem cores e fontes do app: no Word vira uma tabela simples,
  que assume o estilo do documento; no Excel, cada valor vai para uma célula.

### Alterado

- O quadro "O que o Ranova faz" fica recolhido; um clique no título mostra os
  recursos e o guia rápido.

### Corrigido

- `cff-version` do `CITATION.cff` volta a 1.2.0 (tinha sido trocado pela
  versão do app).

## [2.1.1] - 2026-09-28

### Alterado

- Nomenclatura dos delineamentos: um fator é **experimento simples**; dois ou
  três fatores sorteados juntos, **esquema fatorial**; um fator nas parcelas e
  outro nas subparcelas, **parcelas subdivididas** (sem "fatorial"). "Esquema
  fatorial nas parcelas" ou "nas subparcelas" só aparece quando há dois fatores
  combinados naquele nível.
- Descrição do experimento no app, no PDF e no HTML no padrão de artigos, por
  exemplo "DBC em parcelas subdivididas: esquema fatorial A × B nas parcelas e
  C nas subparcelas".

## [2.1.0] - 2026-09-28

### Adicionado

- **Fatorial na parcela** (1º e 2º fatores na parcela, 3º na subparcela) e
  **fatorial na subparcela** (1º na parcela, 2º e 3º na subparcela), escolhidos
  no campo "arranjo" quando há três fatores em parcelas subdivididas.
- **Parcelas subsubdivididas** em DIC e DBC (parcela, subparcela e
  subsubparcela), com erros (a), (b) e (c) e um CV por estrato.
- Cada efeito é testado contra o erro do seu estrato, e as comparações de
  médias usam o erro do estrato ou o erro combinado de Satterthwaite, conforme
  o desdobramento.

### Alterado

- Passa a exigir o pacote `ranova` 0.6.0.

## [2.0.0] - 2026-09-28

### Adicionado

- **Parcelas subdivididas** em DIC e em DBC: o primeiro fator fica na parcela e
  o segundo na subparcela; ANOVA com erro (a), erro (b), CV a e CV b.
- **Testes de médias**: Tukey, t (LSD), Bonferroni, Duncan, SNK, Scott-Knott e
  Dunnett, além do automático (t para 2 níveis, Tukey para 3 ou mais). O erro
  de cada comparação segue o delineamento (na subdividida, erro combinado com
  graus de liberdade de Satterthwaite para a parcela dentro da subparcela).
- Seção **"O que o Ranova faz"** no topo, com delineamentos, entrada de dados,
  análises, testes, gráficos e relatórios, seguida do guia rápido.
- Exemplo de parcelas subdivididas em DBC (irrigação × cultivar).

### Alterado

- A análise passa a usar `ranova_ajuste()`, `ranova_anova()` e
  `ranova_medias()` do pacote `ranova` 0.5.0.
- Tabelas de ANOVA, médias e desdobramento no app com o mesmo estilo do
  relatório (vírgula decimal, efeitos em destaque e linhas de CV).
- PDF e HTML trazem o delineamento, o teste de médias usado e as notas
  correspondentes.

## [1.9.0] - 2026-09-28

### Adicionado

- **Relatório em HTML**, além do PDF: um único arquivo com o visual do app
  (cabeçalho com logo, menu de seções fixo no topo, cartões numerados,
  indicadores, leitura rápida, tabelas com efeitos em destaque, gráficos
  numerados, diagnóstico dos resíduos, discrepantes e painel). As imagens vão
  embutidas, então o arquivo abre em qualquer navegador, funciona no celular e
  pode ser enviado por e-mail ou impresso.

### Alterado

- O cartão 6 passa a se chamar **Relatório**, com os botões "Baixar PDF" e
  "Baixar HTML" lado a lado.

## [1.8.0] - 2026-09-27

### Adicionado

- Opção de deixar os **títulos dos eixos e da legenda em negrito ou não**.

### Alterado

- "Nomes nos gráficos" e "Aparência dos gráficos" viram barras recolhíveis,
  **fechadas por padrão**, com ícone, título e resumo do que ajustam; abrem só
  quando o usuário quiser editar.

## [1.7.0] - 2026-09-27

### Adicionado

- Caixa **Aparência dos gráficos** na aba Gráficos: **fonte** (sem serifa,
  com serifa ou monoespaçada), **tamanho da fonte** e **cor do contorno das
  barras** (ou sem contorno). Vale para médias, interação, diagnóstico dos
  resíduos, painel e PDF.

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
