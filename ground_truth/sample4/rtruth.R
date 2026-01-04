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

sexuality_all <- reduce(sexuality_vars, full_join, by = "NSID")

sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      sori19 %in% 1:4 ~ sori19,
      sori19 %in% c(-100, -97, -3) ~ -3,
      sori19 %in% c(-92, -9) ~ -9,
      sori19 %in% c(-91, -1, -8) ~ -8,
      sori19 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori20 = case_when(
      sori20 %in% 1:4 ~ sori20,
      sori20 %in% c(-100, -97, -3) ~ -3,
      sori20 %in% c(-92, -9) ~ -9,
      sori20 %in% c(-91, -1, -8) ~ -8,
      sori20 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori25 = case_when(
      sori25 %in% 1:4 ~ sori25,
      sori25 == -9 ~ -9,
      sori25 %in% c(-8, -1) ~ -8,
      TRUE ~ -3
    ),
    sori32 = case_when(
      sori32 %in% 1:4 ~ sori32,
      sori32 == 5 ~ -7,
      sori32 == -9 ~ -9,
      sori32 %in% c(-8, -1) ~ -8,
      sori32 == -3 ~ -3,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(
    starts_with("sori"),
    ~ factor(
      .x,
      levels = c(1, 2, 3, 4, -1, -2, -3, -7, -8, -9),
      labels = c(
        "Heterosexual/straight",
        "Gay/lesbian",
        "Bisexual",
        "Other",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Prefer not to say",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, sori19, sori20, sori25, sori32)

  # Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(sexuality_all, output_data_path, row.names = FALSE)
