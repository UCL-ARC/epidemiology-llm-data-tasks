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

# Load all required data files
S1 <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID)


S4 <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(
    NSID

  )


S8 <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>%
  select(NSID, bmi25_raw = W8DBMI)


S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, bmi32_raw = W9DBMI)




# BMI --------------------------------------------------------------------
# Load BMI data from relevant sweeps
bmi_vars <- list(S1=S1, S4=S4, S8=S8, S9=S9)

# Merge all BMI data by NSID
bmi_all <- reduce(bmi_vars, full_join, by = "NSID")

# Recode BMI variables
bmi_rec <- bmi_all %>%
  mutate(
    bmi25 = case_when(
      bmi25_raw > 0 ~ bmi25_raw,
      bmi25_raw == -9 ~ -9,
      bmi25_raw == -8 ~ -8,
      bmi25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    bmi32 = case_when(
      bmi32_raw > 0 ~ bmi32_raw,
      bmi32_raw == -9 ~ -9,
      bmi32_raw == -8 ~ -8,
      bmi32_raw == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(
    c(bmi25, bmi32),
    ~ labelled(
      .x,
      labels = c(
        "Item not applicable" = -1,
        "Script error/information lost" = -2,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  ))

bmi_rec %>%
  filter(bmi25_raw < 0 | is.na(bmi25_raw) | bmi25 < 0 | is.na(bmi25)) %>%
  count(bmi25_raw, bmi25)

bmi_rec %>%
  filter(bmi32_raw < 0 | is.na(bmi32_raw) | bmi32 < 0 | is.na(bmi32)) %>%
  count(bmi32_raw, bmi32)

bmi_all <- bmi_rec %>%
  select(NSID, bmi25, bmi32)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(bmi_all, output_data_path, row.names = FALSE)
