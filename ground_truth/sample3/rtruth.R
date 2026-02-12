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

S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE)
S2yp <- read_delim(file.path(data_path, "wave_two_lsype_young_person_2020.tab"), show_col_types = FALSE)
S3fb <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE)
S4fb <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE)

lang_vars <- list(
  S1 = S1yp %>% select(NSID, lang_S1 = W1englangYP),
  S2 = S2yp %>% select(NSID, lang_S2 = W2EnglangYP),
  S3 = S3fb %>% select(NSID, lang_S3 = W3englangHH),
  S4 = S4fb %>% select(NSID, lang_S4 = W4EngLangHH)
)

lang_all <- reduce(lang_vars, full_join, by = "NSID")

lang_all <- lang_all %>%
  mutate(across(
    starts_with("lang_S"),
    ~ case_when(
      .x %in% c(-999, -998, -997, -995, -94) ~ -2,
      .x == -99 ~ -3,
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -1 ~ -8,
      TRUE ~ .x
    )
  )) %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) ~ lang_S1,
      !is.na(lang_S2) ~ lang_S2,
      !is.na(lang_S3) ~ lang_S3,
      !is.na(lang_S4) ~ lang_S4,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, lang)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(lang_all, output_data_path, row.names = FALSE)