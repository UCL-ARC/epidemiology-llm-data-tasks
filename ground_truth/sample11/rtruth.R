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

S1 <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma14 = W1empsmum, ecoactdtpa14 = W1empsdad)

S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma15 = W2empsmum, ecoactdtpa15 = W2empsdad)

S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma16 = W3empsmum, ecoactdtpa16 = W3empsdad)

S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma17 = w4empsmum, ecoactdtpa17 = w4empsdad)

ecoactDT_parents_all <- reduce(list(S1, S2, S3, S4), full_join, by = "NSID")

recode_detailed <- function(x) {
  case_when(
    x == 1 ~ 1, # FT
    x == 2 ~ 2, # PT
    x == 3 ~ 3, # Unemployed
    x == 4 ~ 4, # Training
    x == 5 ~ 5, # Education
    x == 6 ~ 6, # Home
    x == 7 ~ 7, # Retired
    x == 8 ~ 8, # Sick/disabled
    x == 9 ~ 9, # Other
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -999 ~ -2,
    x %in% c(-996, -98, -99) ~ -3,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
}

# Apply recode to each sweep
ecoactDT_parents_all <- ecoactDT_parents_all %>%
  mutate(
    ecoactdtma14 = recode_detailed(ecoactdtma14),
    ecoactdtpa14 = recode_detailed(ecoactdtpa14),
    ecoactdtma15 = recode_detailed(ecoactdtma15),
    ecoactdtpa15 = recode_detailed(ecoactdtpa15),
    ecoactdtma16 = recode_detailed(ecoactdtma16),
    ecoactdtpa16 = recode_detailed(ecoactdtpa16),
    ecoactdtma17 = recode_detailed(ecoactdtma17),
    ecoactdtpa17 = recode_detailed(ecoactdtpa17)
  ) %>%
  mutate(across(
    c(starts_with("ecoactdtma"), starts_with("ecoactdtpa")),
    ~ factor(
      .x,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -2, -3, -8, -9),
      labels = c(
        "FT paid work",
        "PT paid work",
        "Unemployed",
        "Training",
        "Education",
        "Looking after home/family",
        "Retired from work altogether",
        "Sick/disabled",
        "Other",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, starts_with("ecoactdtma"), starts_with("ecoactdtpa"))

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(ecoactDT_parents_all, output_data_path, row.names = FALSE)