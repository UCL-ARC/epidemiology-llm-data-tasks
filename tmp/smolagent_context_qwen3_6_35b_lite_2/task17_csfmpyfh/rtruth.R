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

S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, imd15_raw = IMDRSCORE)

S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, imd16_raw = IMDRSCORE)

S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, imd32_raw = W9DIMDD)

imd_all <- reduce(list(S1, S4, S2, S3, S9), full_join, by = "NSID")


# Recode derived variables
imd_rec <- imd_all %>%
  mutate(
    imd15 = case_when(
      is.na(imd15_raw) ~ -3,
      imd15_raw == -94 ~ -8,
      TRUE ~ imd15_raw
    ),

    imd16 = case_when(
      is.na(imd16_raw) ~ -3,
      imd16_raw == -94 ~ -8,
      TRUE ~ imd16_raw
    ),

    imd32 = case_when(
      is.na(imd32_raw) ~ -3,
      imd32_raw == -8 ~ -8,
      TRUE ~ imd32_raw
    )
  ) %>%
  mutate(
    across(
      c(imd15, imd16, imd32),
      ~ labelled(
        .x,
        labels = c(
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Don’t know/insufficient information" = -8
        )
      )
    )
  )


imd_all <- imd_rec %>%
  select(NSID, imd15, imd16, imd32) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(imd_all, output_data_path, row.names = FALSE)