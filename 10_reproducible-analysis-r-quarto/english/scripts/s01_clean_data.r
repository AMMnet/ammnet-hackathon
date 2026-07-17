## s01_clean_data.r
## Clean raw data, check inconsistencies, and output cleaning errors.

# 1. Packages and Setup -----------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(tibble)

# Enforce project-wide settings via .Rprofile (loaded automatically in R)

# 2. Import Raw Data --------------------------------------------------------
raw_data <- readr::read_csv(
  here::here("data", "raw", "dat", "wsdd_data.csv"),
  na = c("", "NA", ".", "N/A"),
  show_col_types = FALSE
)

# 3. Discrepancy Checks -----------------------------------------------------
# Initialize list for logging errors
error_log <- tibble::tibble()

# Check 1: No episodes (num_episodes = 0) but time_to_first_episode is not NA
err_zero_episodes_time <- raw_data |>
  dplyr::filter(num_episodes == 0 & !is.na(time_to_first_episode)) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = as.character(time_to_first_episode),
    description = "No episodes recorded but time_to_first_episode is not NA"
  )

# Check 2: Episodes recorded (num_episodes > 0) but time_to_first_episode is NA
err_episodes_no_time <- raw_data |>
  dplyr::filter(num_episodes > 0 & is.na(time_to_first_episode)) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = as.character(time_to_first_episode),
    description = "Episodes recorded but time_to_first_episode is NA"
  )

# Check 3: Time to first episode occurs after follow-up time
err_time_after_followup <- raw_data |>
  dplyr::filter(time_to_first_episode > follow_up_time) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = paste0(
      "time_to_first_episode = ", time_to_first_episode,
      ", follow_up_time = ", follow_up_time
    ),
    description = "Time to first episode is greater than follow-up time"
  )

# Check 4: Lost to follow-up coding discrepancies
err_followup_complete_time <- raw_data |>
  dplyr::filter(lost_to_follow_up == 0 & follow_up_time < 365) |>
  dplyr::transmute(
    id = id,
    variable = "follow_up_time",
    value = as.character(follow_up_time),
    description = "Completed follow-up but follow_up_time is less than 365"
  )

# Check 5: Age inclusion criteria (Age >= 1 year)
err_age_limit <- raw_data |>
  dplyr::filter(age < 1) |>
  dplyr::transmute(
    id = id,
    variable = "age",
    value = as.character(age),
    description = "Age is less than 1 year (inclusion criteria violation)"
  )

# Check 6: Invalid categorical variables range
err_sex_range <- raw_data |>
  dplyr::filter(!sex %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "sex", value = as.character(sex),
    description = "Invalid sex value (must be 0 or 1)"
  )

err_water_range <- raw_data |>
  dplyr::filter(!water_source %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "water_source", value = as.character(water_source),
    description = "Invalid water_source value (must be 0 or 1)"
  )

err_sanitation_range <- raw_data |>
  dplyr::filter(!sanitation %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "sanitation", value = as.character(sanitation),
    description = "Invalid sanitation value (must be 0 or 1)"
  )

err_ses_range <- raw_data |>
  dplyr::filter(!socioeconomic_status %in% c(1, 2, 3)) |>
  dplyr::transmute(
    id = id,
    variable = "socioeconomic_status",
    value = as.character(socioeconomic_status),
    description = "Invalid socioeconomic_status value (must be 1, 2, or 3)"
  )

err_education_range <- raw_data |>
  dplyr::filter(!education_head %in% c(1, 2, 3, 4)) |>
  dplyr::transmute(
    id = id,
    variable = "education_head",
    value = as.character(education_head),
    description = "Invalid education_head value (must be 1, 2, 3, or 4)"
  )

# Combine all errors
error_log <- dplyr::bind_rows(
  err_zero_episodes_time,
  err_episodes_no_time,
  err_time_after_followup,
  err_followup_complete_time,
  err_age_limit,
  err_sex_range,
  err_water_range,
  err_sanitation_range,
  err_ses_range,
  err_education_range
)

# 4. Save Error Log ---------------------------------------------------------
readr::write_rds(
  error_log,
  here::here("reports", "output", "cleaning", "cleaning_errors.rds")
)

# 5. Clean Dataset and Save -------------------------------------------------
# We exclude the 10 participants with invalid post-censoring events to maintain
# data integrity for follow-up and survival analysis.
invalid_ids <- err_time_after_followup$id

clean_data <- raw_data |>
  dplyr::filter(!id %in% invalid_ids)

readr::write_rds(
  clean_data,
  here::here("data", "processed", "dat", "clean_data.rds")
)

cat("Data cleaning completed successfully.\n")
cat("Total raw records:", nrow(raw_data), "\n")
cat("Total cleaning errors found:", nrow(error_log), "\n")
cat("Total clean records saved:", nrow(clean_data), "\n")
