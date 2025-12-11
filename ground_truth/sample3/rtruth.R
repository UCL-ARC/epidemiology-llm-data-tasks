# This script is used to run the Next Steps MSEU 
# Set CRAN mirror before installing packages
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

# Set folder path (change as needed)
DATA_PATH <- 'data/input/' # it's a constant so all caps


sweeps <- list(
  S1youngperson = "wave_one_lsype_young_person_2020.tab",
  S2youngperson = "wave_two_lsype_young_person_2020.tab",
  S3familybackground = "wave_three_lsype_family_background_2020.tab",
  S4familybackground = "wave_four_lsype_family_background_2020.tab")

#### language ####
# Load relevant language variables
lang_vars <- list(
  S1 = read_delim(file.path(DATA_PATH, sweeps$S1youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, lang_S1 = W1englangYP),
  S2 = read_delim(file.path(DATA_PATH, sweeps$S2youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, lang_S2 = W2EnglangYP),
  S3 = read_delim(file.path(DATA_PATH, sweeps$S3familybackground), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, lang_S3 = W3englangHH),
  S4 = read_delim(file.path(DATA_PATH, sweeps$S4familybackground), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, lang_S4 = W4EngLangHH)
)

# Merge
lang_all <- reduce(lang_vars, full_join, by = "NSID")

# Apply the recoding (recode missing values in Sweeps 1-4)
lang_all <- lang_all %>%
  mutate(across(starts_with("lang_S"), ~ case_when(
    .x %in% c(-999, -998, -997, -995, -94) ~ -2,  # error/information lost
    .x == -99 ~ -3,                        # not interviewed
    .x == -92 ~ -9,                        # refused
    .x == -91 ~ -1,                        # not applicable
    .x == -1  ~ -8,                        # don't know
    TRUE ~ .x
  )))

# Derive final language variable: use S1, else S2, else S4
lang_all <- lang_all %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) & lang_S1 < 1 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 < 1 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 < 1 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 < 1 ~ lang_S4,
      TRUE ~ -3  # Not interviewed/present
    )
  ) %>%
  select(NSID, lang)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(lang_all, output_data_path, row.names = FALSE)