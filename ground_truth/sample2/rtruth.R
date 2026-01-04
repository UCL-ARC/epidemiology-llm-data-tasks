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

S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types = FALSE)
S2yp <- read_delim(file.path(data_path, "wave_two_lsype_young_person_2020.tab"), show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8dv <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE)
S9dv <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE)

eth_vars <- list(
  S1 = S1yp %>% select(NSID, eth_S1 = W1ethnic2YP),
  S2 = S2yp %>% select(NSID, eth_S2 = W2ethnicYP),
  S4 = S4yp %>% select(NSID, eth_S4 = w4ethnic2YP),
  S8 = S8dv %>% select(NSID, eth_S8 = W8DETHN15),
  S9 = S9dv %>% select(NSID, eth_S9 = W9DETHN15)
)

eth_all <- reduce(eth_vars, full_join, by = "NSID")

eth_all <- eth_all %>%
  mutate(across(
    c(eth_S1, eth_S2, eth_S4),
    ~ case_when(
      .x %in% c(-999, -998, -997, -94) ~ -2,
      .x == -99 ~ -3,
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -1 ~ -8,
      TRUE ~ .x
    )
  )) %>%
  mutate(
    eth = case_when(
      !is.na(eth_S1) & eth_S1 > 0 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 > 0 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 > 0 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 > 0 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 > 0 ~ eth_S9,
      !is.na(eth_S1) ~ eth_S1,
      !is.na(eth_S2) ~ eth_S2,
      !is.na(eth_S4) ~ eth_S4,
      !is.na(eth_S8) ~ eth_S8,
      !is.na(eth_S9) ~ eth_S9,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, eth)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(eth_all, output_data_path, row.names = FALSE)