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
  select(NSID, incwhh14 = W1GrsswkHH)

S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, incwhh15 = W2GrsswkHH)

S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, incwhh16 = W3incestw)

S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, incwhh17 = w4IncEstW)


# Merge all household income variables by NSID
hh_income_all <- reduce(list(S1, S2, S3, S4), full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'incwhh*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ paste0(.x, "_raw"),
    contains("incwhh")
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

# Derive banded income for continuous measures (S1–S2)
convert_to_band <- function(x) {
  case_when(
    x < 0 ~ x,
    x < 50 ~ 1,
    x < 100 ~ 2,
    x < 200 ~ 3,
    x < 300 ~ 4,
    x < 400 ~ 5,
    x < 500 ~ 6,
    x < 600 ~ 7,
    x < 700 ~ 8,
    x < 800 ~ 9,
    x < 900 ~ 10,
    x < 1000 ~ 11,
    x >= 1000 ~ 12
  )
}

hh_income_rec <- hh_income_all %>%
  mutate(
    # Sweep 1
    incwhh14 = case_when(
      is.na(incwhh14_raw) ~ -3,
      incwhh14_raw %in% c(-92, -992) ~ -9,
      incwhh14_raw == -999 ~ -2,
      incwhh14_raw == -99 ~ -3,
      incwhh14_raw == -91 ~ -1,
      incwhh14_raw %in% c(-1, -94) ~ -8,
      incwhh14_raw == -3 ~ -1,
      TRUE ~ convert_to_band(incwhh14_raw)
    ),
    incwhhcnt14 = case_when(
      is.na(incwhh14_raw) ~ -3,
      incwhh14_raw %in% c(-92, -992) ~ -9,
      incwhh14_raw == -999 ~ -2,
      incwhh14_raw == -99 ~ -3,
      incwhh14_raw == -91 ~ -1,
      incwhh14_raw %in% c(-1, -94) ~ -8,
      incwhh14_raw == -3 ~ -1,
      TRUE ~ incwhh14_raw
    ),

    # Sweep 2
    incwhh15 = case_when(
      is.na(incwhh15_raw) ~ -3,
      incwhh15_raw %in% c(-92, -992) ~ -9,
      incwhh15_raw == -999 ~ -2,
      incwhh15_raw == -99 ~ -3,
      incwhh15_raw == -91 ~ -1,
      incwhh15_raw %in% c(-1, -94) ~ -8,
      incwhh15_raw == -3 ~ -1,
      TRUE ~ convert_to_band(incwhh15_raw)
    ),
    incwhhcnt15 = case_when(
      is.na(incwhh15_raw) ~ -3,
      incwhh15_raw %in% c(-92, -992) ~ -9,
      incwhh15_raw == -999 ~ -2,
      incwhh15_raw == -99 ~ -3,
      incwhh15_raw == -91 ~ -1,
      incwhh15_raw %in% c(-1, -94) ~ -8,
      incwhh15_raw == -3 ~ -1,
      TRUE ~ incwhh15_raw
    ),

    # Sweep 3
    incwhh16 = case_when(
      is.na(incwhh16_raw) ~ -3,
      incwhh16_raw == -99 ~ -3,
      incwhh16_raw == -92 ~ -9,
      incwhh16_raw == -1 ~ -8,
      incwhh16_raw >= 1 & incwhh16_raw <= 12 ~ incwhh16_raw
    ),

    # Sweep 4
    incwhh17 = case_when(
      is.na(incwhh17_raw) ~ -3,
      incwhh17_raw %in% c(-996, -99) ~ -3,
      incwhh17_raw == -92 ~ -9,
      incwhh17_raw == -1 ~ -8,
      incwhh17_raw >= 1 & incwhh17_raw <= 12 ~ incwhh17_raw
    )
  ) %>%
  mutate(
    across(
      c(incwhh14, incwhh15),
      ~ labelled(
        .x,
        labels = c(
          "less than £50 per week" = 1,
          "50-100" = 2,
          "100-200" = 3,
          "200-300" = 4,
          "300-400" = 5,
          "400-500" = 6,
          "500-600" = 7,
          "600-700" = 8,
          "700-800" = 9,
          "800-900" = 10,
          "900-1000" = 11,
          "1000 or more" = 12,
          common_missing_labels
        )
      )
    ),
    across(
      c(incwhhcnt14, incwhhcnt15),
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
    ),
    across(
      c(incwhh16, incwhh17),
      ~ labelled(
        .x,
        labels = c(
          "up to 49" = 1,
          "50-99" = 2,
          "100-199" = 3,
          "200-299" = 4,
          "300-399" = 5,
          "400-499" = 6,
          "500-599" = 7,
          "600-699" = 8,
          "700-799" = 9,
          "800-899" = 10,
          "900-999" = 11,
          "1000 or more" = 12,
          common_missing_labels
        )
      )
    )
  )

hh_income_all <- hh_income_rec %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17) %>%
  mutate(
    across(
      c(incwhh14, incwhh15, incwhh16, incwhh17),
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(hh_income_all, output_data_path, row.names = FALSE)