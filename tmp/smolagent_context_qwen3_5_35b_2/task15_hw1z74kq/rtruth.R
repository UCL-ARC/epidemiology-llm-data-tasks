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
  select(NSID, inc25_raw = W8DINCB)

S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, inc32_raw = W9DINCB)


# Merge all income variables by NSID
income_all <- reduce(list(S1, S4, S8, S9), full_join, by = "NSID")

# Recode

common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)

income_rec <- income_all %>%
  mutate(
    inc25 = case_when(
      is.na(inc25_raw) ~ -3,
      TRUE ~ inc25_raw
    ),
    inc32 = case_when(
      is.na(inc32_raw) ~ -3,
      TRUE ~ inc32_raw
    )
  ) %>%
  mutate(
    across(
      c(inc25, inc32),
      ~ labelled(
        .x,
        labels = c(
          "less than £25 per week" = 1,
          "25-50" = 2,
          "50-90" = 3,
          "90-140" = 4,
          "140-240" = 5,
          "240-300" = 6,
          "300-350" = 7,
          "350-400" = 8,
          "400-500" = 9,
          "500-600" = 10,
          "600-700" = 11,
          "700-800" = 12,
          "800-900" = 13,
          "900-1200" = 14,
          "1200-1400" = 15,
          "more than 1400" = 16,
          common_missing_labels
        )
      )
    )
  )


# Extract derived variables
income_all <- income_rec %>%
  select(NSID, inc25, inc32) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(income_all, output_data_path, row.names = FALSE)