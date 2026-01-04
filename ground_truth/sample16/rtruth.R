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

hh_income_all <- reduce(list(S1, S2, S3, S4), full_join, by = "NSID")


# Derive banded income for continuous measures (S1–S2)
convert_to_band <- function(x) {
  case_when(
    x < 0 ~ x,
    x < 25 ~ 1,
    x < 50 ~ 2,
    x < 90 ~ 3,
    x < 140 ~ 4,
    x < 240 ~ 5,
    x < 300 ~ 6,
    x < 350 ~ 7,
    x < 400 ~ 8,
    x < 500 ~ 9,
    x < 600 ~ 10,
    x < 700 ~ 11,
    x < 800 ~ 12,
    x < 900 ~ 13,
    x < 1200 ~ 14,
    x < 1400 ~ 15,
    x >= 1400 ~ 16
  )
}

hh_income_all <- hh_income_all %>%
  mutate(
    # Sweep 1
    incwhh14 = case_when(
      is.na(incwhh14) ~ -3,
      incwhh14 == -92 ~ -9,
      incwhh14 %in% c(-999, -992, -94) ~ -2,
      incwhh14 == -99 ~ -3,
      incwhh14 == -91 ~ -1,
      incwhh14 == -1 ~ -8,
      incwhh14 == -3 ~ -3,
      TRUE ~ convert_to_band(incwhh14)
    ),
    incwhhcnt14 = case_when(
      is.na(incwhh14) ~ -3,
      incwhh14 == -92 ~ -9,
      incwhh14 %in% c(-999, -992, -94) ~ -2,
      incwhh14 == -99 ~ -3,
      incwhh14 == -91 ~ -1,
      incwhh14 == -1 ~ -8,
      incwhh14 == -3 ~ -3,
      TRUE ~ incwhh14
    ),

    # Sweep 2
    incwhh15 = case_when(
      is.na(incwhh15) ~ -3,
      incwhh15 == -92 ~ -9,
      incwhh15 %in% c(-999, -992, -94) ~ -2,
      incwhh15 == -99 ~ -3,
      incwhh15 == -91 ~ -1,
      incwhh15 == -1 ~ -8,
      incwhh15 == -3 ~ -3,
      TRUE ~ convert_to_band(incwhh15)
    ),
    incwhhcnt15 = case_when(
      is.na(incwhh15) ~ -3,
      incwhh15 == -92 ~ -9,
      incwhh15 %in% c(-999, -992, -94) ~ -2,
      incwhh15 == -99 ~ -3,
      incwhh15 == -91 ~ -1,
      incwhh15 == -1 ~ -8,
      incwhh15 == -3 ~ -3,
      TRUE ~ incwhh15
    ),

    # Sweep 3
    incwhh16 = case_when(
      is.na(incwhh16) ~ -3,
      incwhh16 == -99 ~ -3,
      incwhh16 == -92 ~ -9,
      incwhh16 == -1 ~ -8,
      incwhh16 >= 1 & incwhh16 <= 12 ~ incwhh16
    ),

    # Sweep 4
    incwhh17 = case_when(
      is.na(incwhh17) ~ -3,
      incwhh17 %in% c(-996, -99) ~ -3,
      incwhh17 == -92 ~ -9,
      incwhh17 == -1 ~ -8,
      incwhh17 >= 1 & incwhh17 <= 12 ~ incwhh17
    )
  ) %>%
  mutate(
    across(
      c(incwhh14, incwhh15),
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
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -1, -2, -3, -8, -9),
        labels = c(
          "up to 49",
          "50-99",
          "100-199",
          "200-299",
          "300-399",
          "400-499",
          "500-599",
          "600-699",
          "700-799",
          "800-899",
          "900-999",
          "1000 or more",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(hh_income_all, output_data_path, row.names = FALSE)