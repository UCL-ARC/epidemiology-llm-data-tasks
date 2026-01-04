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

S1 <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID)

S4 <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID)

S8 <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>%
  select(NSID, inc25 = W8DINCB)

S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, inc32 = W9DINCB)

income_all <- reduce(list(S1, S4, S8, S9), full_join, by = "NSID")

# Recode
income_all <- income_all %>%
  mutate(
    inc25 = case_when(
      is.na(inc25) ~ -3,
      TRUE ~ inc25
    ),
    inc32 = case_when(
      is.na(inc32) ~ -3,
      TRUE ~ inc32
    )
  ) %>%
  mutate(across(
    c(inc25, inc32),
    ~ factor(
      .x,
      levels = c(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "less than £25 per week",
        "25-50",
        "50-90",
        "90-140",
        "140-240",
        "240-300",
        "300-350",
        "350-400",
        "400-500",
        "500-600",
        "600-700",
        "700-800",
        "800-900",
        "900-1200",
        "1200-1400",
        "more than 1400",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, inc25, inc32)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(income_all, output_data_path, row.names = FALSE)