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

# Define sweep file names
sweeps <- list(
  S1youngperson = "wave_one_lsype_young_person_2020.tab",
  S2youngperson = "wave_two_lsype_young_person_2020.tab",
  S4youngperson = "wave_four_lsype_young_person_2020.tab",
  S8derivedvariable = "ns8_2015_derived.tab",
  S9derivedvariable = "ns9_2022_derived_variables.tab"
)

#### ethnicity ####
# Load ethnicity variables from relevant sweeps
ethnicity_vars <- list(
  S1 = read_delim(file.path(DATA_PATH, sweeps$S1youngperson), delim = "\t",show_col_types = FALSE) %>% 
    select(NSID, eth_S1 = W1ethnic2YP),
  S2 = read_delim(file.path(DATA_PATH, sweeps$S2youngperson), delim = "\t",show_col_types = FALSE) %>% 
    select(NSID, eth_S2 = W2ethnicYP),
  S4 = read_delim(file.path(DATA_PATH, sweeps$S4youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, eth_S4 = w4ethnic2YP),
  S8 = read_delim(file.path(DATA_PATH, sweeps$S8derivedvariable), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, eth_S8 = W8DETHN15),
  S9 = read_delim(file.path(DATA_PATH, sweeps$S9derivedvariable), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "NSID")

# Harmonise missing values for S1–S4
# Create a vector of ethnicity variables
eth_vars <- c("eth_S1", "eth_S2", "eth_S4")

# Apply the recoding (recode missing values in Sweeps 1-4)
eth_all <- eth_all %>%
  mutate(across(all_of(eth_vars), ~ case_when(
    .x == -999 ~ -2,
    .x == -998 ~ -2,
    .x == -997 ~ -2,
    .x == -99  ~ -3,
    .x == -94  ~ -2,
    .x == -92  ~ -9,
    .x == -91  ~ -1,
    .x == -1   ~ -8,
    TRUE ~ .x
  )))

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    eth = case_when(
      !is.na(eth_S1) & eth_S1 > 0 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 > 0 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 > 0 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 > 0 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 > 0 ~ eth_S9,
      !is.na(eth_S1) & eth_S1 < 1 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 < 1 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 < 1 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 < 1 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 < 1 ~ eth_S9,
      TRUE ~ -3  # Not interviewed/present
    )
  )%>%
  select(NSID, eth)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(eth_all, output_data_path, row.names = FALSE)