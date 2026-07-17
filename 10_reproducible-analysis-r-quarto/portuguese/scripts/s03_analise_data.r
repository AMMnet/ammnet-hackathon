## s03_analise_data.r
## Executar os modelos de análise de dados estatísticos principais.

# 1. Pacotes e Configuração --------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(survival)

# 2. Importar Coorte Processada -----------------------------------------------
cohort <- readr::read_rds(
  here::here("data", "processed", "dat", "analysis_cohort.rds")
)

# 3. Regressão Logística: Risco de Diarreia -----------------------------------
# O desfecho primário é o estado de diarreia (Não/Sim).
# A exposição primária é a fonte de água principal: água de poço vs
# água de torneira.

# Modelo Bruto
fit_logistic_crude <- glm(
  diarrhoea_status ~ water_source,
  data = cohort,
  family = binomial(link = "logit")
)

# Modelo Ajustado (Sem selecção stepwise; ajustado a priori com base no PAE)
fit_logistic_adj <- glm(
  diarrhoea_status ~ water_source + age + sex + sanitation +
    socioeconomic_status + education_head,
  data = cohort,
  family = binomial(link = "logit")
)

# 4. Regressão Binomial Negativa: Taxas de Diarreia ---------------------------
# Desfecho: num_episodes (Contagem)
# Offset: log(person_time_years) para modelar taxas

# Modelo Bruto
fit_nb_crude <- MASS::glm.nb(
  num_episodes ~ water_source + offset(log(person_time_years)),
  data = cohort
)

# Modelo Ajustado
fit_nb_adj <- MASS::glm.nb(
  num_episodes ~ water_source + age + sex + sanitation +
    socioeconomic_status + education_head + offset(log(person_time_years)),
  data = cohort
)

# 5. Análise de Sobrevivência: Tempo-até-ao-Primeiro-Episódio -----------------
# Criar variáveis de tempo e evento
cohort_survival <- cohort |>
  dplyr::mutate(
    survival_time = ifelse(
      is.na(time_to_first_episode),
      follow_up_time,
      time_to_first_episode
    ),
    event = ifelse(diarrhoea_status == "Sim", 1, 0)
  )

# Modelo de Riscos Proporcionais de Cox Bruto
fit_cox_crude <- survival::coxph(
  survival::Surv(survival_time, event) ~ water_source,
  data = cohort_survival
)

# Modelo de Riscos Proporcionais de Cox Ajustado
fit_cox_adj <- survival::coxph(
  survival::Surv(survival_time, event) ~ water_source + age + sex +
    sanitation + socioeconomic_status + education_head,
  data = cohort_survival
)

# Testar o Pressuposto de Riscos Proporcionais
cox_ph_test <- survival::cox.zph(fit_cox_adj)

# 6. Diagnósticos de Colinearidade (VIF) --------------------------------------
# Cálculo manual robusto do VIF utilizando a matriz do modelo para suportar
# factores codificados como variáveis dummy correctamente sem dependências
# de pacotes.
calculate_vif <- function(model) {
  design_matrix <- model.matrix(model)
  # Excluir a coluna do intercepto
  design_matrix <- design_matrix[
    , colnames(design_matrix) != "(Intercept)",
    drop = FALSE
  ]

  vifs <- sapply(colnames(design_matrix), function(var_name) {
    dep_var <- design_matrix[, var_name]
    predictors_matrix <- design_matrix[
      , colnames(design_matrix) != var_name,
      drop = FALSE
    ]
    df_temp <- as.data.frame(predictors_matrix)
    df_temp$dep_var <- dep_var
    fit_temp <- lm(dep_var ~ ., data = df_temp)
    r_sq <- summary(fit_temp)$r.squared
    if (is.na(r_sq) || r_sq == 1) Inf else 1 / (1 - r_sq)
  })
  vifs
}

vif_results <- calculate_vif(fit_logistic_adj)

# 7. Guardar Resultados da Análise --------------------------------------------
output_dir <- here::here("reports", "output", "analysis")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Guardar como listas de modelos em R para alimentar o relatório Quarto
readr::write_rds(
  list(crude = fit_logistic_crude, adj = fit_logistic_adj),
  file.path(output_dir, "logistic_models.rds")
)

readr::write_rds(
  list(crude = fit_nb_crude, adj = fit_nb_adj),
  file.path(output_dir, "nb_models.rds")
)

readr::write_rds(
  list(crude = fit_cox_crude, adj = fit_cox_adj, ph_test = cox_ph_test),
  file.path(output_dir, "cox_models.rds")
)

readr::write_rds(
  vif_results,
  file.path(output_dir, "vif_results.rds")
)

cat(
  "Modelos estatísticos principais e análises de diagnóstico",
  "concluídos com sucesso.\n"
)
cat("Resultados guardados em:", output_dir, "\n")
