## s03_analise_data.r
## Perform core statistical data analysis models.

# 1. Packages and Setup -----------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(survival)

# 2. Import Processed Cohort -------------------------------------------------
cohort <- readr::read_rds(
  here::here("data", "processed", "dat", "analysis_cohort.rds")
)

# 3. Logistic Regression: Risk of Diarrhoea ----------------------------------
# Primary outcome is diarrhoea status (Yes/No).
# Primary exposure is primary water source (Tap water/Well water).

# Crude Model
fit_logistic_crude <- glm(
  diarrhoea_status ~ water_source,
  data = cohort,
  family = binomial(link = "logit")
)

# Adjusted Model (No stepwise selection; adjusted a priori based on SAP)
fit_logistic_adj <- glm(
  diarrhoea_status ~ water_source + age + sex + sanitation +
    socioeconomic_status + education_head,
  data = cohort,
  family = binomial(link = "logit")
)

# 4. Negative Binomial Regression: Rates of Diarrhoea ------------------------
# Outcome: num_episodes (Count)
# Offset: log(person_time_years) to model rates

# Crude Model
fit_nb_crude <- MASS::glm.nb(
  num_episodes ~ water_source + offset(log(person_time_years)),
  data = cohort
)

# Adjusted Model
fit_nb_adj <- MASS::glm.nb(
  num_episodes ~ water_source + age + sex + sanitation +
    socioeconomic_status + education_head + offset(log(person_time_years)),
  data = cohort
)

# 5. Survival Analysis: Time-to-First-Episode --------------------------------
# Create time and event variables
cohort_survival <- cohort |>
  dplyr::mutate(
    survival_time = ifelse(
      is.na(time_to_first_episode),
      follow_up_time,
      time_to_first_episode
    ),
    event = ifelse(diarrhoea_status == "Yes", 1, 0)
  )

# Crude Cox Proportional Hazards Model
fit_cox_crude <- survival::coxph(
  survival::Surv(survival_time, event) ~ water_source,
  data = cohort_survival
)

# Adjusted Cox Proportional Hazards Model
fit_cox_adj <- survival::coxph(
  survival::Surv(survival_time, event) ~ water_source + age + sex +
    sanitation + socioeconomic_status + education_head,
  data = cohort_survival
)

# Test Proportional Hazards Assumption
cox_ph_test <- survival::cox.zph(fit_cox_adj)

# 6. Collinearity Diagnostics (VIF) ------------------------------------------
# Robust manual calculation of VIF using the model matrix to support
# dummy-coded factors correctly without package dependencies.
calculate_vif <- function(model) {
  design_matrix <- model.matrix(model)
  # Exclude the intercept column
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

# 7. Save Analysis Results ---------------------------------------------------
output_dir <- here::here("reports", "output", "analysis")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save as R lists of models to feed the Quarto report
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
  "Core statistical modeling and diagnostic analyses completed successfully.\n"
)
cat("Results saved to:", output_dir, "\n")
