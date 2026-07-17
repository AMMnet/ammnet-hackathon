# Tutorial: Pipeline de Análise de Dados Biomédicos Reprodutível com R e Quarto

Bem-vindo ao tutorial de análise reprodutível do Hackathon da AMMnet! Este guia passo-a-passo acompanha-o ao longo de toda a pipeline de investigação clínica e epidemiológica, desde a instalação dos requisitos básicos até à geração de um rascunho de manuscrito pronto para publicação em formato Microsoft Word (.docx), alinhado com as directrizes da PLOS ONE.

Esta pipeline opera sob Procedimentos Operativos Padrão (SOPs) estritos para gestão do espaço de trabalho, resolução de caminhos (paths), convenções de codificação, padrões estatísticos e esquemas de apresentação de resultados.

---

## 1. Pré-requisitos e Instalação de Software

Antes de começar, instale os seguintes pacotes de software no seu sistema:

1. **R (versão $\ge$ 4.0):** O ambiente central de computação estatística. Descarregue a partir da [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).
2. **RStudio Desktop:** O Ambiente de Desenvolvimento Integrado (IDE) recomendado. Descarregue a partir da [Posit](https://posit.co/download/rstudio-desktop/).
3. **Quarto CLI:** O sistema de publicação utilizado para compilar os relatórios dinâmicos. Descarregue a partir de [Quarto](https://quarto.org/docs/get-started/).
4. **Microsoft Word ou qualquer software compatível (ex., LibreOffice Writer, OpenOffice Writer, Google Docs):** Necessário para inspeccionar e rever o manuscrito final gerado (.docx).

### Instalar os Pacotes de R Necessários

Abra o R ou o RStudio e execute o seguinte comando para instalar todas as dependências de pacotes utilizadas nesta pipeline:

```r
install.packages(c(
  "here",         # Gestão robusta de caminhos relativos
  "readr",        # Importação limpa de dados
  "dplyr",        # Manipulação de dados e uso de pipes
  "tidyr",        # Formatação de dados no padrão tidy
  "lubridate",    # Processamento de datas
  "knitr",        # Geração dinâmica de documentos
  "quarto",       # Compilar documentos Quarto a partir da consola de R
  "gtsummary",    # Tabelas clínicas de resumo e de regressão
  "broom",        # Resultados de modelos estatísticos em formato tidy
  "survival",     # Modelação de tempo-até-ao-evento (análise de sobrevivência)
  "survminer",    # Visualização de curvas de sobrevivência de Kaplan-Meier
  "ggplot2",      # Gráficos e figuras personalizadas
  "flextable",    # Motor de renderização nativo de tabelas para Word
  "officer",      # Suporte para estilização de documentos Word
  "sessioninfo",  # Registo de bibliotecas e auditorias de ambiente
  "styler",       # Formatação automática de código
  "lintr"         # Auditoria de regras e sintaxe de código
))
```

---

## 2. Estrutura de Directórios e Configuração do Espaço de Trabalho

### Organização do Projecto

Crie uma estrutura de pastas limpa. Abra o seu terminal ou explorador de ficheiros e inicialize as seguintes pastas na raiz do seu projecto:

```text
hl01_reproducible_analysis_r_quarto_pt/
├── .Rprofile               # Opções globais aplicadas na inicialização
├── .here                   # Ficheiro âncora para resolução de caminhos relativos
├── data/
│   ├── raw/
│   │   ├── dat/            # Ficheiros de dados brutos fornecidos (Apenas leitura)
│   │   └── dic/            # Dicionários de dados brutos fornecidos (Apenas leitura)
│   └── processed/
│       ├── dat/            # Ficheiros de dados limpos prontos para análise (.rds)
│       └── dic/            # Dicionários de dados prontos para análise (.md)
├── docs/                   # Protocolo do estudo e Plano de Análise Estatística (PAE)
├── scripts/                # Scripts de R numerados sequencialmente (s01, s02, s03)
└── reports/                # Ficheiros markdown do Quarto (r01, r02, r03)
    ├── aux/                # Bibliografia (.bib), estilos (.css) e folhas de estilo de citação (.csl)
    └── output/
        ├── cleaning/       # Erros de limpeza guardados (.rds) gerados pelo script de limpeza
        └── analysis/       # Resultados dos modelos estatísticos (.rds) gerados pelo script de análise
```

### Configuração de Opções Globais (`.Rprofile`)

Crie um ficheiro chamado `.Rprofile` na raiz do projecto. Escreva as seguintes linhas para aplicar arredondamento estatístico, suprimir avisos do tidyverse e formatar valores em falta globalmente:

```r
options(
  digits = 3,
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  knitr.kable.NA = "–",
  pillar.bold = TRUE,
  pillar.subtle_num = TRUE
)
```

### Âncora para Resolução de Caminhos (`.here`)

Crie um ficheiro vazio chamado `.here` na raiz do projecto. O pacote `{here}` utiliza este ficheiro para localizar a raiz do projecto. Isto garante que os caminhos relativos resolvidos através de `here::here()` permaneçam consistentes independentemente de o script ser executado a partir da raiz, da pasta `scripts/` ou compilado dentro de `reports/`.

*Nunca utilize caminhos absolutos (ex. `"C:/User/Documents/..."`) ou a função `setwd()` nos seus scripts.*

---

## 3. Limpeza de Dados e Verificação de Anomalias (`scripts/s01_clean_data.r`)

Coloque os seus ficheiros brutos fornecidos (`wsdd_data.csv`, `wsdd_data_dictionary.html`, `study_protocol.pdf` e `sap.pdf`) nas respectivas pastas:

- Dados brutos em `data/raw/dat/`
- Dicionário bruto em `data/raw/dic/`
- Protocolo e PAE em `docs/`

### Implementação do Script de Limpeza

Crie o ficheiro `scripts/s01_clean_data.r`. O script executa verificações de integridade lógica iniciais:

1. **Carregar Dados Brutos:** Utilize `readr::read_csv()` e declare explicitamente os valores que devem ser tratados como dados em falta:

   ```r
   raw_data <- readr::read_csv(
     here::here("data", "raw", "dat", "wsdd_data.csv"),
     na = c("", "NA", ".", "N/A")
   )
   ```

2. **Examinar Estrutura:** Verifique as colunas e formatos usando `utils::glimpse(raw_data)`.
3. **Validação de Anomalias:** Verifique o alinhamento de datas e limites lógicos:
   - Verifique se `time_to_first_episode` excede `follow_up_time`.
   - Identifique eventos pós-censura (participantes perdidos de seguimento, mas com episódios registados após a data de censura).
   - No nosso caso, isto identificou exactamente **10 registos** contendo eventos pós-censura inválidos.
4. **Registar Erros e Exportar:** Guarde estes 10 registos como um ficheiro RDS em `reports/output/cleaning/cleaning_errors.rds` para o relatório, exclua-os da coorte e exporte os restantes 1.190 registos limpos para `data/processed/dat/clean_data.rds`.

### Compilar o Relatório de Limpeza

Crie o ficheiro `reports/r01_data_cleaning.qmd` mapeado para o ficheiro CSL `reports/aux/plos.csl`. Escreva blocos de código para carregar os dados brutos, limpos e o log de erros, e depois apresente as exclusões dinamicamente em tabelas.

Execute o comando de compilação no terminal:

```bash
quarto render reports/r01_data_cleaning.qmd
```

Isto gera o relatório HTML `reports/r01_data_cleaning.html` que resume o ciclo de vida da limpeza de dados.

---

## 4. Processamento da Coorte e Codificação de Variáveis (`scripts/s02_process_data.r`)

Crie o ficheiro `scripts/s02_process_data.r` para construir o conjunto de dados analítico final:

1. **Formatar Variáveis Categóricas:** Recodifique os factores e estabeleça os grupos de referência basais usando o português de Moçambique:
   - `water_source`: nível de referência `"Água de torneira"`
   - `diarrhoea_status`: nível de referência `"Não"` (derivado de `num_episodes > 0` sendo `"Sim"` ou `"Não"`)
   - `sanitation`: nível de referência `"Não"` (referência de ausência)
   - `socioeconomic_status`: nível de referência `"Baixo"`
   - `education_head`: nível de referência `"Nenhuma"`
2. **Calcular Variáveis Derivadas:**
   - Tempo líquido de pessoa em risco em anos: `person_time_years = (follow_up_time - (num_episodes * 3)) / 365.25` (assumindo 3 dias de duração por episódio).
3. **Exportar Coorte e Dicionário:**
   - Guarde os dados analíticos finais em `data/processed/dat/analysis_cohort.rds`.
   - Gere um dicionário de dados em markdown descrevendo as variáveis processadas e exporte-o para `data/processed/dic/analysis_cohort_dictionary.md`.

---

## 5. Análise Estatística de Dados (`scripts/s03_analise_data.r`)

Crie o ficheiro `scripts/s03_analise_data.r` para executar os modelos de regressão delineados no PAE:

1. **Regressão Logística Multivariável (Risco Binário):**

   ```r
   fit_logistic_adj <- glm(
     diarrhoea_status ~ water_source + age + sex + sanitation + socioeconomic_status + education_head,
     data = cohort,
     family = binomial(link = "logit")
   )
   ```

2. **Regressão Binomial Negativa Multivariável (Taxas de Contagem):**

   ```r
   fit_nb_adj <- MASS::glm.nb(
     num_episodes ~ water_source + age + sex + sanitation + socioeconomic_status + education_head + offset(log(person_time_years)),
     data = cohort
   )
   ```

3. **Regressão de Riscos Proporcionais de Cox (Tempo-até-ao-Primeiro-Episódio):**

   ```r
   cohort_survival <- cohort |>
     dplyr::mutate(
       survival_time = ifelse(is.na(time_to_first_episode), follow_up_time, time_to_first_episode),
       event = ifelse(diarrhoea_status == "Sim", 1, 0)
     )

   fit_cox_adj <- survival::coxph(
     survival::Surv(survival_time, event) ~ water_source + age + sex + sanitation + socioeconomic_status + education_head,
     data = cohort_survival
   )
   ```

4. **Verificações de Diagnóstico:**
   - Avalie a colinearidade através de Factores de Inflação de Variância (VIF) a partir do modelo ajustado.
   - Avalie o pressuposto de riscos proporcionais usando resíduos de Schoenfeld: `survival::cox.zph(fit_cox_adj)`.
5. **Exportar Objectos do Modelo:** Guarde os modelos e os resultados de diagnóstico como ficheiros RDS em `reports/output/analysis/` (ex. `logistic_models.rds`, `nb_models.rds`, `cox_models.rds` e `vif_results.rds`).

---

## 6. Desenvolvimento dos Rascunhos do Manuscrito (`reports/r03_manuscript.qmd` & `reports/r03_manuscript_no_crossref.qmd`)

Para disponibilizar um documento com referências cruzadas e um rascunho em Word livre de avisos de validação de XML, criamos duas versões do manuscrito:

1. **`reports/r03_manuscript.qmd` (Referências Cruzadas para HTML e Word):** Configurado para gerar ambos os formatos. No HTML, a paginação e as referências cruzadas do Quarto (`#| label: tbl-...` e `#| tbl-cap: ...` nas tabelas, citadas com `@tbl-...`) funcionam perfeitamente. No Word, compila com sucesso, mas pode apresentar avisos de recuperação de tags OpenXML ao abrir.
2. **`reports/r03_manuscript_no_crossref.qmd` (Word Livre de Avisos):** Configurado exclusivamente para o Word. Utiliza cabeçalhos manuais e referências textuais padrão (ex. "Tabela 1") para evitar conflitos no validador do Word.

### Convenções de Construção do Manuscrito

#### A. Posicionamento do Bloco Setup
Coloque o bloco R `setup` no **topo** do documento, logo após o cabeçalho de metadados YAML e **antes da secção `# Resumo`**:

````yaml
---
title: "Fonte de Água e Doença Diarreica: Um Estudo de Coorte"
author: "Arsénio Nhacolo"
format: docx
bibliography: aux/references.bib
csl: aux/plos.csl
---

```{r}
#| label: setup
#| include: false
# (Carregar bibliotecas, importar ficheiros RDS de modelos/dados e calcular variáveis inline aqui)
```

# Resumo
...
````

Isto permite que as variáveis dinâmicas (ex. `` `r diarrhoea_cases` ``) sejam definidas *antes* de serem compiladas no texto do Resumo.

#### B. Evitar o Aviso de XML "Ambiguous cell mapping" no Word
Para garantir que o documento de Word abre de forma limpa:

1. **Remova** `#| label: tbl-...` e `#| tbl-cap: ...` dos blocos de tabela.
2. Utilize etiquetas de blocos comuns (ex. `#| label: demographics-table`).
3. Adicione títulos em Markdown directamente acima dos blocos:
   ````markdown
   **Tabela 1: Características basais da população do estudo.**
   ```{r}
   #| label: demographics-table
   #| echo: false

   cohort |>
     dplyr::select(...) |>
     gtsummary::tbl_summary(...) |>
     gtsummary::as_flex_table()
   ````
4. Cite as tabelas no texto usando referências textuais normais (ex. `"resumidas na Tabela 1."` em vez de `"resumidas na @tbl-demographics."`).

#### C. Integração da Curva de Sobrevivência
No Word, a tabela de risco inferior do `survminer::ggsurvplot` causa quebras de página incorretas. Defina `risk.table = FALSE` e imprima apenas o gráfico principal do ggplot (`print(p_km$plot)`) para obter uma imagem vectorial limpa:
```markdown
print(p_km$plot)
```

---

## 7. Compilação e Verificação da Pipeline

### Executando os Passos Individualmente

Para executar e verificar toda a pipeline, execute os scripts de R e compile os documentos do Quarto pela seguinte ordem:

```bash
# 1. Limpar o conjunto de dados bruto e registar anomalias
Rscript scripts/s01_clean_data.r

# 2. Codificar factores e gerar coorte analítica
Rscript scripts/s02_process_data.r

# 3. Executar regressões e extrair diagnósticos
Rscript scripts/s03_analise_data.r

# 4. Compilar o relatório de limpeza de dados (HTML)
quarto render reports/r01_data_cleaning.qmd

# 5. Compilar o relatório de análise epidemiológica (HTML)
quarto render reports/r02_data_analysis.qmd

# 6. Compilar os manuscritos
# Manuscrito com referências cruzadas (HTML e Word):
quarto render reports/r03_manuscript.qmd

# Manuscrito de Word livre de avisos:
quarto render reports/r03_manuscript_no_crossref.qmd
```

### Executar os Passos a partir do RStudio

Se preferir utilizar a interface gráfica do RStudio em vez da linha de comandos:

- **Para os Scripts de R (`scripts/s01_clean_data.r`, etc.):** Abra o ficheiro do script no RStudio e clique no botão **Run** no canto superior direito do painel de edição (ou pressione `Ctrl+Enter` / `Cmd+Return` para executar as linhas seleccionadas). Também pode executar o script completo a partir da consola do R utilizando `source("scripts/s01_clean_data.r")`.
- **Para os Relatórios Quarto (`reports/r01_data_cleaning.qmd`, etc.):** Abra o ficheiro `.qmd` e clique no botão **Render** na barra de ferramentas do editor (ou pressione `Ctrl+Shift+K` / `Cmd+Shift+K`). Em alternativa, pode executar `quarto::quarto_render("reports/r01_data_cleaning.qmd")` directamente na consola do R.

### Executar Toda a Pipeline via Script Batch no Windows (Opcional)

Se estiver a correr o projecto num ambiente Windows, pode executar toda a pipeline de processamento e compilação com um único comando utilizando o ficheiro `run_pipeline.bat`.

Abra o seu terminal (Prompt de Comando ou PowerShell) no directório raiz do projecto e execute:

```cmd
run_pipeline.bat
```
ou pode simplesmente fazer o duplo clique no ficheiro  `run_pipeline.bat` para executá-lo.

Este script executará cada passo de processamento de dados, fará a gestão dinâmica das pastas e caminhos de compilação, verificará erros e compilará todos os documentos automaticamente.

---

## 8. Qualidade do Código, Formatação e Auditoria (Linting)

Para manter uma base de código legível, consistente e profissional, o projecto segue directrizes rigorosas de estilo através dos pacotes `{styler}` e `{lintr}`.

### Directrizes de Código Aplicadas
* **Atribuição:** Utilize sempre `<-` para atribuições (use `=` apenas dentro de funções).
* **Pipes:** Utilize exclusivamente o operador nativo de R `|>` para encadeamento.
* **Operadores:** Adicione espaços em redor de todos os operadores infixos (`<-`, `|>`, `==`, `+`).
* **Comprimento de Linha:** Mantenha um limite máximo de 80 caracteres por linha.
* **Resolução de Conflitos:** Identifique explicitamente a origem das funções com o formato `pacote::funcao()` (ex. `dplyr::select()`) para evitar conflitos de namespace.

### Executar a Formatação e Auditoria Manualmente
Pode executar estas ferramentas directamente a partir da consola do R ou RStudio:
```r
# Formatar automaticamente todos os scripts de R na pasta 'scripts'
styler::style_dir("scripts")

# Auditar o estilo de escrita e identificar potenciais erros de sintaxe
lintr::lint_dir("scripts")
```

### Executar através do Script Utilitário
Pode também correr a auditoria de qualidade directamente a partir da linha de comandos usando o script de R `run_style_and_lint.r` disponível na raiz:

```bash
Rscript run_style_and_lint.r
```

### Ignorar Regras em Casos Excepcionais
Ao criar blocos longos de caracteres multi-linha (como a tabela descritiva do dicionário de dados), a quebra de linhas para manter o limite de 80 caracteres quebraria a formatação do markdown. Nestas situações, pode desactiva a auditoria localmente usando comentários de exclusão:
```r
# nolint start: line_length_linter
dictionary_content <- "
| Variável | Descrição |
| :--- | :--- |
| `water_source` | Fonte principal de água de beber |
"
# nolint end: line_length_linter
```

---

Parabéns! Concluiu a pipeline de análise de dados reprodutível em língua portuguesa.
