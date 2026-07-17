## s01_clean_data.r
## Limpar dados brutos, verificar inconsistências e exportar erros de limpeza.

# 1. Pacotes e Configuração --------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(tibble)

# Opções globais aplicadas via .Rprofile (carregado automaticamente)

# 2. Importar Dados Brutos ----------------------------------------------------
raw_data <- readr::read_csv(
  here::here("data", "raw", "dat", "wsdd_data.csv"),
  na = c("", "NA", ".", "N/A"),
  show_col_types = FALSE
)

# 3. Verificações de Discrepâncias --------------------------------------------
# Inicializar tabela para registo de erros
error_log <- tibble::tibble()

# Inconsistência 1: Nenhum episódio mas tempo até ao primeiro episódio não é NA
err_zero_episodes_time <- raw_data |>
  dplyr::filter(num_episodes == 0 & !is.na(time_to_first_episode)) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = as.character(time_to_first_episode),
    description = paste0(
      "Nenhum episódio registado mas o tempo até ao ",
      "primeiro episódio não é NA"
    )
  )

# Inconsistência 2: Episódios registados mas tempo até ao primeiro episódio é NA
err_episodes_no_time <- raw_data |>
  dplyr::filter(num_episodes > 0 & is.na(time_to_first_episode)) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = as.character(time_to_first_episode),
    description = paste0(
      "Episódios registados mas o tempo até ao ",
      "primeiro episódio é NA"
    )
  )

# Inconsistência 3: Tempo até ao primeiro episódio ocorre após tempo de
# seguimento
err_time_after_followup <- raw_data |>
  dplyr::filter(time_to_first_episode > follow_up_time) |>
  dplyr::transmute(
    id = id,
    variable = "time_to_first_episode",
    value = paste0(
      "time_to_first_episode = ", time_to_first_episode,
      ", follow_up_time = ", follow_up_time
    ),
    description = paste0(
      "O tempo até ao primeiro episódio é maior do ",
      "que o tempo de seguimento"
    )
  )

# Inconsistência 4: Discrepâncias na codificação de perdas de seguimento
err_followup_complete_time <- raw_data |>
  dplyr::filter(lost_to_follow_up == 0 & follow_up_time < 365) |>
  dplyr::transmute(
    id = id,
    variable = "follow_up_time",
    value = as.character(follow_up_time),
    description = paste0(
      "Seguimento concluído mas o tempo de seguimento ",
      "é inferior a 365"
    )
  )

# Inconsistência 5: Critério de inclusão de idade (Idade >= 1 ano)
err_age_limit <- raw_data |>
  dplyr::filter(age < 1) |>
  dplyr::transmute(
    id = id,
    variable = "age",
    value = as.character(age),
    description = paste0(
      "A idade é inferior a 1 ano ",
      "(violação do critério de inclusão)"
    )
  )

# Inconsistência 6: Valores fora dos limites para variáveis categóricas
err_sex_range <- raw_data |>
  dplyr::filter(!sex %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "sex", value = as.character(sex),
    description = "Valor inválido para o sexo (deve ser 0 ou 1)"
  )

err_water_range <- raw_data |>
  dplyr::filter(!water_source %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "water_source", value = as.character(water_source),
    description = paste0(
      "Valor inválido para a fonte de água principal ",
      "(deve ser 0 ou 1)"
    )
  )

err_sanitation_range <- raw_data |>
  dplyr::filter(!sanitation %in% c(0, 1)) |>
  dplyr::transmute(
    id = id, variable = "sanitation", value = as.character(sanitation),
    description = "Valor inválido para o saneamento (deve ser 0 ou 1)"
  )

err_ses_range <- raw_data |>
  dplyr::filter(!socioeconomic_status %in% c(1, 2, 3)) |>
  dplyr::transmute(
    id = id,
    variable = "socioeconomic_status",
    value = as.character(socioeconomic_status),
    description = paste0(
      "Valor inválido para o nível socioeconómico ",
      "(deve ser 1, 2 ou 3)"
    )
  )

err_education_range <- raw_data |>
  dplyr::filter(!education_head %in% c(1, 2, 3, 4)) |>
  dplyr::transmute(
    id = id,
    variable = "education_head",
    value = as.character(education_head),
    description = paste0(
      "Valor inválido para a escolaridade do chefe ",
      "da casa (deve ser 1, 2, 3 ou 4)"
    )
  )

# Combinar todos os erros encontrados
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

# 4. Guardar Registo de Erros -------------------------------------------------
readr::write_rds(
  error_log,
  here::here("reports", "output", "cleaning", "cleaning_errors.rds")
)

# 5. Filtrar Conjunto de Dados e Guardar --------------------------------------
# Excluímos os 10 participantes com eventos pós-censura inválidos para manter
# a integridade dos dados para o seguimento e análise de sobrevivência.
invalid_ids <- err_time_after_followup$id

clean_data <- raw_data |>
  dplyr::filter(!id %in% invalid_ids)

readr::write_rds(
  clean_data,
  here::here("data", "processed", "dat", "clean_data.rds")
)

cat("Limpeza de dados concluída com sucesso.\n")
cat("Total de registos brutos:", nrow(raw_data), "\n")
cat("Total de erros de limpeza encontrados:", nrow(error_log), "\n")
cat("Total de registos limpos guardados:", nrow(clean_data), "\n")
