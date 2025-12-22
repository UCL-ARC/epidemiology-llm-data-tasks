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

# Required datasets
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S6yp <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8dv <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE)
S9dv <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE)

partnr_vars <- list(
  S1 = S1yp %>% select(NSID),
  S4 = S4yp %>% select(NSID),
  S6 = S6yp %>% select(NSID, partnr19 = W6MarStatYP),
  S8 = S8dv %>% select(NSID, partnradu25 = W8DMARSTAT),
  S9 = S9dv %>% select(NSID, partnradu32 = W9DMARSTAT)
)

partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

partnr_all <- partnr_all %>%
  mutate(
    partnr19 = case_when(
      partnr19 > 0 ~ partnr19 - 1,
      partnr19 == -92 ~ -9,
      partnr19 == -91 ~ -1,
      partnr19 == -1 ~ -8,
      partnr19 %in% c(-997, -97) ~ -3,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      partnradu25 == 1 ~ 0,
      partnradu25 %in% c(2, 6) ~ 1,
      partnradu25 %in% c(3, 7) ~ 2,
      partnradu25 %in% c(4, 8) ~ 3,
      partnradu25 %in% c(5, 9) ~ 4,
      partnradu25 == -9 ~ -9,
      partnradu25 %in% c(-8, -1) ~ -8,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      partnradu32 == 1 ~ 0,
      partnradu32 %in% c(2, 6) ~ 1,
      partnradu32 %in% c(3, 7) ~ 2,
      partnradu32 %in% c(4, 8) ~ 3,
      partnradu32 %in% c(5, 9) ~ 4,
      partnradu32 == -9 ~ -9,
      partnradu32 %in% c(-8, -1) ~ -8,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    partnradu25 = case_when(
      partnradu25 %in% 1:9 ~ partnradu25 - 1,
      partnradu25 == -9 ~ -9,
      partnradu25 %in% c(-8, -1) ~ -8,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      partnradu32 %in% 1:9 ~ partnradu32 - 1,
      partnradu32 == -9 ~ -9,
      partnradu32 %in% c(-8, -1) ~ -8,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      c(partnr19, partnr25, partnr32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -3, -8, -9),
        labels = c(
          "Single and never married or in a CP",
          "Married",
          "Separated but still legally married/in a CP",
          "Divorced/former CP",
          "Widowed/surviving CP",
          "Item not applicable",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(partnradu25, partnradu32),
      ~ factor(
        .x,
        levels = c(0:8, -1, -3, -8, -9),
        labels = c(
          "Single and never married or in a CP",
          "Married",
          "Separated but still legally married",
          "Divorced",
          "Widowed",
          "A civil partner",
          "Separated but still legally in a CP",
          "A former civil partner",
          "A surviving civil partner",
          "Item not applicable",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(partnr_all, output_data_path, row.names = FALSE)