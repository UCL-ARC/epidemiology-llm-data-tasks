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
  S4youngperson = "wave_four_lsype_young_person_2020.tab",
  S6youngperson = "wave_six_lsype_young_person_2020.tab",
  S7youngperson = "wave_seven_lsype_young_person_2020.tab",
  S8selfcompletion = "ns8_2015_self_completion.tab",
  S9maininterview = "ns9_2022_main_interview.tab"
)


#### sexual orientation ####
# Load sexuality variables
sexuality_vars <- list(
  S1 = read_delim(file.path(DATA_PATH, sweeps$S1youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID),
  S4 = read_delim(file.path(DATA_PATH, sweeps$S4youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID),
  S6 = read_delim(file.path(DATA_PATH, sweeps$S6youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, sori19 = W6SexualityYP),
  S7 = read_delim(file.path(DATA_PATH, sweeps$S7youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, sori20 = W7SexualityYP),
  S8 = read_delim(file.path(DATA_PATH, sweeps$S8selfcompletion), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, sori25 = W8SEXUALITY),
  S9 = read_delim(file.path(DATA_PATH, sweeps$S9maininterview), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, sori32 = W9SORI)
)

# Merge by ID
sexuality_all <- reduce(sexuality_vars, full_join, by = "NSID")

# recode missing values and response categories
sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      sori19 == 1 ~ 1,
      sori19 == 2 ~ 2,
      sori19 == 3 ~ 3,
      sori19 == 4 ~ 4,
      sori19 %in% c(-100, -97, -3) ~ -3,
      sori19 %in% c(-92, -9) ~ -9,
      sori19 %in% c(-91, -1, -8) ~ -8,
      sori19 %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    sori20 = case_when(
      sori20 == 1 ~ 1,
      sori20 == 2 ~ 2,
      sori20 == 3 ~ 3,
      sori20 == 4 ~ 4,
      sori20 %in% c(-100, -97, -3) ~ -3,
      sori20 %in% c(-92, -9) ~ -9,
      sori20 %in% c(-91, -1, -8) ~ -8,
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
      sori25 == -1 ~ -8,
      TRUE ~ -3
    ),
    sori32 = case_when(
      sori32 == 1 ~ 1,
      sori32 == 2 ~ 2,
      sori32 == 3 ~ 3,
      sori32 == 4 ~ 4,
      sori32 == 5 ~ -7,
      sori32 == -9 ~ -9,
      sori32 %in% c(-8, -1) ~ -8,
      sori32 == -3 ~ -3,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)



# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(sexuality_all, output_data_path, row.names = FALSE)