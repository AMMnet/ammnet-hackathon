## s02_process_data.r
## Build analysis-ready cohorts and processed dictionaries.

# 1. Packages and Setup -----------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(forcats)

# 2. Import Cleaned Data -----------------------------------------------------
clean_data <- readr::read_rds(
  here::here("data", "processed", "dat", "clean_data.rds")
)

# 3. Variable Processing and Cohort Selection -------------------------------
# Convert categorical variables to factor variables with base reference groups
# and calculate derived epidemiological metrics.
analysis_cohort <- clean_data |>
  dplyr::mutate(
    # Sex: Male is reference
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),

    # Exposure (Water Source): Tap water is reference (unexposed)
    water_source = factor(
      water_source,
      levels = c(0, 1),
      labels = c("Tap water", "Well water")
    ),

    # Sanitation: No is reference
    sanitation = factor(
      sanitation,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),

    # Socioeconomic Status: Low is reference
    socioeconomic_status = factor(
      socioeconomic_status,
      levels = c(1, 2, 3),
      labels = c("Low", "Medium", "High")
    ),

    # Education of Household Head: None is reference
    education_head = factor(
      education_head,
      levels = c(1, 2, 3, 4),
      labels = c("None", "Primary", "Secondary", "Tertiary")
    ),

    # Lost to Follow-up: Completed is reference
    lost_to_follow_up = factor(
      lost_to_follow_up,
      levels = c(0, 1),
      labels = c("Completed", "Lost")
    ),

    # Diarrhoea Status (Binary Outcome): No is reference
    diarrhoea_status = factor(
      ifelse(num_episodes > 0, "Yes", "No"),
      levels = c("No", "Yes")
    ),

    # Net Person-Time at Risk (Days)
    net_person_time_days = follow_up_time - (num_episodes * 3),

    # Person-Time at Risk (Years)
    person_time_years = net_person_time_days / 365.25
  )

# 4. Save Processed Cohort --------------------------------------------------
readr::write_rds(
  analysis_cohort,
  here::here("data", "processed", "dat", "analysis_cohort.rds")
)

# 5. Create Processed Data Dictionary ---------------------------------------
# Construct a markdown data dictionary representing the final cohort variables
# nolint start: line_length_linter
dictionary_content <- "
# Processed Data Dictionary

**Dataset:** `analysis_cohort.rds`
**Study:** Water Source and Diarrhoeal Disease — Prospective Cohort Study
**Version:** 1.1 (Processed)
**Date:** 2026-06-10

---

## Variable Reference Table

| Variable | Description | Class / Type | Reference Group / Coding |
| :--- | :--- | :--- | :--- |
| `id` | Participant ID | Integer | Sequential identifier |
| `enrolment_date` | Date of enrolment | Date | YYYY-MM-DD |
| `age` | Age at enrolment | Numeric | 1–85 years |
| `sex` | Biological sex | Factor | Male (ref), Female |
| `water_source` | Primary drinking water source | Factor | Tap water (ref), Well water |
| `sanitation` | Access to improved sanitation | Factor | No (ref), Yes |
| `household_size` | Number of household members | Numeric | Continuous (1–15) |
| `socioeconomic_status` | Household socio-economic status | Factor | Low (ref), Medium, High |
| `education_head` | Highest education level of household head | Factor | None (ref), Primary, Secondary, Tertiary |
| `num_episodes` | Total diarrhoea episodes during follow-up | Numeric | Count (0, 1, 2, ...) |
| `time_to_first_episode` | Days to first diarrhoea episode | Numeric | Continuous (1–365) |
| `follow_up_time` | Total follow-up duration | Numeric | Days (1–365) |
| `lost_to_follow_up` | Participant loss to follow-up status | Factor | Completed (ref), Lost |
| `diarrhoea_status` | Experienced >= 1 diarrhoea episode | Factor | No (ref), Yes |
| `net_person_time_days` | Net person-time under observation (days) | Numeric | `follow_up_time - (num_episodes * 3)` |
| `person_time_years` | Net person-time under observation (years) | Numeric | `net_person_time_days / 365.25` |
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

cat("Cohort variable processing and dictionary generation completed.\n")
cat("Cohort dimensions:", paste(dim(analysis_cohort), collapse = " x "), "\n")
