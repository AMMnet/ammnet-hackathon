## s02_process_data.r
## Criar coortes prontas para análise e dicionários processados.

# 1. Pacotes e Configuração --------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(forcats)

# 2. Importar Dados Limpos ----------------------------------------------------
clean_data <- readr::read_rds(
  here::here("data", "processed", "dat", "clean_data.rds")
)

# 3. Processamento de Variáveis e Selecção da Coorte --------------------------
# Converter variáveis categóricas em factores com grupos de referência basais
# e calcular métricas epidemiológicas derivadas.
analysis_cohort <- clean_data |>
  dplyr::mutate(
    # Sexo: Masculino é a referência
    sex = factor(sex, levels = c(0, 1), labels = c("Masculino", "Feminino")),

    # Exposição (Fonte de Água): Água de torneira é a referência (não exposto)
    water_source = factor(
      water_source,
      levels = c(0, 1),
      labels = c("Água de torneira", "Água de poço")
    ),

    # Saneamento: Não é a referência
    sanitation = factor(
      sanitation,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),

    # Nível Socioeconómico: Baixo é a referência
    socioeconomic_status = factor(
      socioeconomic_status,
      levels = c(1, 2, 3),
      labels = c("Baixo", "Médio", "Alto")
    ),

    # Escolaridade do Chefe do Agregado: Nenhuma é a referência
    education_head = factor(
      education_head,
      levels = c(1, 2, 3, 4),
      labels = c("Nenhuma", "Primário", "Secundário", "Terciário")
    ),

    # Perda de Seguimento: Concluído é a referência
    lost_to_follow_up = factor(
      lost_to_follow_up,
      levels = c(0, 1),
      labels = c("Concluído", "Perdido")
    ),

    # Estado de Diarreia (Desfecho Binário): Não é a referência
    diarrhoea_status = factor(
      ifelse(num_episodes > 0, "Sim", "Não"),
      levels = c("Não", "Sim")
    ),

    # Tempo Líquido de Pessoa em Risco (Dias)
    net_person_time_days = follow_up_time - (num_episodes * 3),

    # Tempo de Pessoa em Risco (Anos)
    person_time_years = net_person_time_days / 365.25
  )

# 4. Guardar Coorte Processada ------------------------------------------------
readr::write_rds(
  analysis_cohort,
  here::here("data", "processed", "dat", "analysis_cohort.rds")
)

# 5. Criar Dicionário de Dados Processados ------------------------------------
# Construir um dicionário de dados em markdown representando as variáveis finais
# nolint start: line_length_linter
dictionary_content <- "
# Dicionário de Dados Processados

**Conjunto de dados:** `analysis_cohort.rds`
**Estudo:** Fonte de Água e Doença Diarreica — Estudo de Coorte Prospectivo
**Versão:** 1.1 (Processado)
**Data:** 2026-06-10

---

## Tabela de Referência de Variáveis

| Variável | Descrição | Classe / Tipo | Grupo de Referência / Codificação |
| :--- | :--- | :--- | :--- |
| `id` | ID do Participante | Inteiro | Identificador sequencial |
| `enrolment_date` | Data de recrutamento | Data | AAAA-MM-DD |
| `age` | Idade no recrutamento | Numérico | 1–85 anos |
| `sex` | Sexo biológico | Factor | Masculino (ref), Feminino |
| `water_source` | Fonte principal de água de beber | Factor | Água de torneira (ref), Água de poço |
| `sanitation` | Acesso a saneamento melhorado | Factor | Não (ref), Sim |
| `household_size` | Número de membros do agregado familiar | Numérico | Contínuo (1–15) |
| `socioeconomic_status` | Nível socioeconómico do agregado familiar | Factor | Baixo (ref), Médio, Alto |
| `education_head` | Nível de escolaridade do chefe da casa | Factor | Nenhuma (ref), Primário, Secundário, Terciário |
| `num_episodes` | Total de episódios de diarreia no seguimento | Numérico | Contagem (0, 1, 2, ...) |
| `time_to_first_episode` | Dias até ao primeiro episódio de diarreia | Numérico | Contínuo (1–365) |
| `follow_up_time` | Duração total do seguimento | Numérico | Dias (1–365) |
| `lost_to_follow_up` | Estado de perda de seguimento | Factor | Concluído (ref), Perdido |
| `diarrhoea_status` | Apresentou >= 1 episódio de diarreia | Factor | Não (ref), Sim |
| `net_person_time_days` | Tempo de pessoa líquido sob observação (dias) | Numérico | `follow_up_time - (num_episodes * 3)` |
| `person_time_years` | Tempo de pessoa líquido sob observação (anos) | Numérico | `net_person_time_days / 365.25` |
"
# nolint end: line_length_linter

dictionary_dir <- here::here("data", "processed", "dic")
if (!dir.exists(dictionary_dir)) {
  dir.create(dictionary_dir, recursive = TRUE)
}

writeLines(
  text = trimws(dictionary_content),
  con = file.path(dictionary_dir, "analysis_cohort_dictionary.md")
)

cat(
  "Processamento de variáveis da coorte e geração do dicionário concluídos.\n"
)
cat("Dimensões da coorte:", paste(dim(analysis_cohort), collapse = " x "), "\n")
