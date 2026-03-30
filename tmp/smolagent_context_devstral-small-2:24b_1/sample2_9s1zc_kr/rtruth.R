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

ethnicity_vars <- list(
  S1 = S1yp %>% select(NSID, eth_S1 = W1ethnic2YP),
  S2 = S2yp %>% select(NSID, eth_S2 = W2ethnicYP),
  S4 = S4yp %>% select(NSID, eth_S4 = w4ethnic2YP),
  S8 = S8dv %>% select(NSID, eth_S8 = W8DETHN15),
  S9 = S9dv %>% select(NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "NSID")

# Harmonise missing values for S1–S4
# Create a vector of ethnicity variables
eth_vars <- c("eth_S1", "eth_S2", "eth_S4")

# Apply the recoding (recode missing values in Sweeps 1-4)
eth_all <- eth_all %>%
  mutate(
    across(
      all_of(eth_vars),
      ~ case_when(
        .x %in% -999:-997 ~ -2,
        .x == -99 ~ -3,
        .x == -94 ~ -8,
        .x == -92 ~ -9,
        .x == -91 ~ -1,
        .x == -1 ~ -8,
        .default = .x
      )
    )
  )

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    eth = case_when(
      eth_S1 > 0 ~ eth_S1,
      eth_S2 > 0 ~ eth_S2,
      eth_S4 > 0 ~ eth_S4,
      eth_S8 > 0 ~ eth_S8,
      eth_S9 > 0 ~ eth_S9,
      eth_S1 < 1 ~ eth_S1,
      eth_S2 < 1 ~ eth_S2,
      eth_S4 < 1 ~ eth_S4,
      eth_S8 < 1 ~ eth_S8,
      eth_S9 < 1 ~ eth_S9,
      TRUE ~ -3 # Not interviewed/present
    )
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

eth_labels_substantive <- c(
  "White-British" = 1L,
  "White-Irish" = 2L,
  "Any other White background" = 3L,
  "Mixed-White and Black Caribbean" = 4L,
  "Mixed-White and Black African" = 5L,
  "Mixed-White and Asian" = 6L,
  "Any other Mixed background" = 7L,
  "Asian or Asian British-Indian" = 8L,
  "Asian or Asian British-Pakistani" = 9L,
  "Asian or Asian British-Bangladeshi" = 10L,
  "Any other Asian background" = 11L,
  "Black or Black British-Caribbean" = 12L,
  "Black or Black British-African" = 13L,
  "Any other Black background" = 14L,
  "Chinese" = 15L,
  "Any other background" = 16L
)

eth_all <- eth_all |>
  mutate(
    eth = as.integer(eth),
    eth = {
      eth_levels <- c(eth_labels_substantive, common_missing_labels)
      factor(
        eth,
        levels = unname(eth_levels),
        labels = names(eth_levels)
      )
    }
  ) |>
  select(NSID, eth)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(eth_all, output_data_path, row.names = FALSE)