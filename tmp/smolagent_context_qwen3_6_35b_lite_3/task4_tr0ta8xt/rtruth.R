options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('haven', 'dplyr', 'purrr', 'here', 'labelled', 'readr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(haven)  # for reading SPSS/Stata files
  library(dplyr)  # for data manipulation
  library(purrr)  # for functional programming (map, reduce)
  library(here)  # for file paths
  library(labelled)  # for handling labelled data
  library(readr)  # for reading delimited files
})

# Data path
# Set folder path (change as needed)
data_path <- 'data/input/' 


# Read required datasets only
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S6yp <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE)
S7yp <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8sc <- read_delim(file.path(data_path, "ns8_2015_self_completion.tab"), show_col_types = FALSE)
S9mi <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE)

sexuality_vars <- list(
  S1 = S1yp %>% select(NSID),
  S4 = S4yp %>% select(NSID),
  S6 = S6yp %>% select(NSID, sori19 = W6SexualityYP),
  S7 = S7yp %>% select(NSID, sori20 = W7SexualityYP),
  S8 = S8sc %>% select(NSID, sori25 = W8SEXUALITY),
  S9 = S9mi %>% select(NSID, sori32 = W9SORI)
)

# Merge by ID
sexuality_all <- reduce(sexuality_vars, full_join, by = "NSID")

# Labels
sexuality_labels_substantive <- c(
  "Heterosexual/straight" = 1L,
  "Gay/lesbian" = 2L,
  "Bisexual" = 3L,
  "Other" = 4L
)

common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)

# recode missing values and response categories
sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      sori19 == 1 ~ 1,
      sori19 == 2 ~ 2,
      sori19 == 3 ~ 3,
      sori19 == 4 ~ 4,
      sori19 == -3 ~ -3,
      sori19 %in% c(-92, -9, -100, -97) ~ -9,
      sori19 %in% c(-1, -8) ~ -8,
      sori19 == -91 ~ -1, # Not applicable
      sori19 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori20 = case_when(
      sori20 == 1 ~ 1,
      sori20 == 2 ~ 2,
      sori20 == 3 ~ 3,
      sori20 == 4 ~ 4,
      sori20 == -3 ~ -3,
      sori20 %in% c(-92, -9, -100, -97) ~ -9,
      sori20 %in% c(-1, -8) ~ -8,
      sori20 == -91 ~ -1, # Not applicable
      sori20 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori25 = case_when(
      sori25 == 1 ~ 1,
      sori25 == 2 ~ 2,
      sori25 == 3 ~ 3,
      sori25 == 4 ~ 4,
      sori25 == -9 ~ -9,
      sori25 == -8 ~ -8,
      sori25 == -1 ~ -1, # Not applicable
      TRUE ~ -3
    ),
    sori32 = case_when(
      sori32 == 1 ~ 1,
      sori32 == 2 ~ 2,
      sori32 == 3 ~ 3,
      sori32 == 4 ~ 4,
      sori32 == 5 ~ -7,
      sori32 == -9 ~ -9,
      sori32 %in% c(-8) ~ -8,
      sori32 == -3 ~ -3,
      sori32 == -1 ~ -1, # Not applicable
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      starts_with("sori"),
      ~ {
        lvl <- c(sexuality_labels_substantive, common_missing_labels)
        factor(
          as.integer(.x),
          levels = unname(lvl),
          labels = names(lvl)
        )
      }
    )
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

  # Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(sexuality_all, output_data_path, row.names = FALSE)
