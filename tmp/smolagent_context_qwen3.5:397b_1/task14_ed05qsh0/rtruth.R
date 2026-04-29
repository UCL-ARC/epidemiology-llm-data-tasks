options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('dplyr', 'purrr', 'readr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

# Data path
data_path <- 'data/input/'

# Load and select variables
S1 <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, hown14 = W1hous12HH)
S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, hown15 = W2Hous12HH)
S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, hown16 = W3hous12HH)
S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, hown17 = W4Hous12HH)
S5 <- read_delim(file.path(data_path, "wave_five_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, s5_tenure_type = W5Hous12HH, s5_tenure_owned = W5Hous12BHH, s5_tenure_rented = W5Hous12CHH)
S6 <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, s6_tenure_type = W6Hous12YP, s6_tenure_owned = W6Hous12bYP, s6_tenure_rented = W6Hous12cYP)
S7 <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, s7_tenure_type = W7Hous12YP, s7_tenure_owned = W7Hous12bYP, s7_tenure_rented = W7Hous12cYP)
S8 <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"), show_col_types = FALSE) %>%
  select(NSID, hown25 = W8TENURE)
S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, hown32 = W9DTENURE)

# Merge all datasets and add _raw suffix
hown_all <- reduce(list(S1, S2, S3, S4, S5, S6, S7, S8, S9), full_join, by = "NSID") %>%
  rename_with(~ paste0(.x, "_raw"), contains("hown"))

# Helpers ----------------------------------------------------------------

# Detailed tenure from 3-variable sweeps (S5–S7)
recode_tenure_detailed <- function(tenure_type, tenure_owned, tenure_rented) {
  case_when(
    tenure_owned == 1  ~ 1L,
    tenure_owned == 2  ~ 2L,
    tenure_owned == 3  ~ 3L,
    tenure_rented == 1 ~ 4L,
    tenure_rented == 2 ~ 5L,
    tenure_rented == 3 ~ 6L,
    tenure_rented == 4 ~ 7L,
    tenure_type == 3 | tenure_owned == 4 | tenure_rented == 5 ~ 8L,
    tenure_owned == -999 | tenure_rented == -999 ~ -2L,
    tenure_owned == -92  | tenure_rented == -92  ~ -9L,
    tenure_owned == -91  | tenure_rented == -91  ~ -1L,
    tenure_owned == -1   | tenure_rented == -1   ~ -8L,
    .default = -3L
  )
}

# Collapsed tenure from 3-variable sweeps (S5–S7)
recode_tenure_collapsed <- function(tenure_type, tenure_owned, tenure_rented) {
  case_when(
    tenure_owned == 1  ~ 1L,
    tenure_owned == 2  ~ 2L,
    tenure_owned == 3  ~ 3L,
    tenure_rented %in% 1:3 ~ 4L,
    tenure_rented == 4 ~ 5L,
    tenure_type == 3 | tenure_owned == 4 | tenure_rented == 5 ~ 6L,
    tenure_owned == -999 | tenure_rented == -999 ~ -2L,
    tenure_owned == -92  | tenure_rented == -92  ~ -9L,
    tenure_owned == -91  | tenure_rented == -91  ~ -1L,
    tenure_owned == -1   | tenure_rented == -1   ~ -8L,
    .default = -3L
  )
}

# Detailed tenure labels
hownteen_levels <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, -1L, -2L, -3L, -8L, -9L)
hownteen_labels <- c(
  "Owned outright",
  "Being bought on a mortgage/bank loan",
  "Shared ownership (owns & rents property)",
  "Rented from a Council or New Town",
  "Rented from a Housing Association",
  "Rented privately",
  "Rent free",
  "Some other arrangement",
  "Item not applicable",
  "Script error/information lost",
  "Not asked at the fieldwork stage/participated/interviewed",
  "Don't know/insufficient information",
  "Refusal"
)

# Simple tenure labels
hown_levels <- c(1L, 2L, 3L, 4L, 5L, 6L, -1L, -2L, -3L, -8L, -9L)
hown_labels <- c(
  "Owned outright",
  "Owned, buying with help of mortgage/loan",
  "Part rent, part mortgage",
  "Rent it",
  "Live rent-free",
  "Other",
  "Item not applicable",
  "Script error/information lost",
  "Not asked at the fieldwork stage/participated/interviewed",
  "Don't know/insufficient information",
  "Refusal"
)

# Recode: detailed tenure ------------------------------------------------

hown_all <- hown_all %>%
  mutate(
    hownteen14 = case_when(
      hown14_raw > 0 ~ hown14_raw,
      hown14_raw == -999 ~ -2L, hown14_raw == -92 ~ -9L,
      hown14_raw == -91 ~ -1L, hown14_raw == -1 ~ -8L,
      is.na(hown14_raw) ~ -3L
    ),
    hownteen15 = case_when(
      hown15_raw > 0 ~ hown15_raw,
      hown15_raw %in% c(-998, -997, -995, -99) ~ -2L,
      hown15_raw == -92 ~ -9L, hown15_raw == -91 ~ -1L,
      hown15_raw == -1 ~ -8L, is.na(hown15_raw) ~ -3L
    ),
    hownteen16 = case_when(
      hown16_raw > 0 ~ hown16_raw,
      hown16_raw == -999 ~ -2L, hown16_raw == -92 ~ -9L,
      hown16_raw == -91 ~ -1L, hown16_raw == -1 ~ -8L,
      is.na(hown16_raw) ~ -3L
    ),
    hownteen17 = case_when(
      hown17_raw > 0 ~ hown17_raw,
      hown17_raw %in% c(-999, -997) ~ -2L, hown17_raw == -92 ~ -9L,
      hown17_raw == -91 ~ -1L, hown17_raw == -1 ~ -8L,
      is.na(hown17_raw) ~ -3L
    ),
    hownteen18 = recode_tenure_detailed(s5_tenure_type, s5_tenure_owned, s5_tenure_rented),
    hownteen19 = recode_tenure_detailed(s6_tenure_type, s6_tenure_owned, s6_tenure_rented),
    hownteen20 = recode_tenure_detailed(s7_tenure_type, s7_tenure_owned, s7_tenure_rented)
  )

# Recode: simple tenure --------------------------------------------------

hown_all <- hown_all %>%
  mutate(
    hown14 = case_when(
      hown14_raw == 1 ~ 1L, hown14_raw == 2 ~ 2L, hown14_raw == 3 ~ 3L,
      hown14_raw %in% 4:6 ~ 4L, hown14_raw == 7 ~ 5L, hown14_raw == 8 ~ 6L,
      hown14_raw == -999 ~ -2L, hown14_raw == -92 ~ -9L,
      hown14_raw == -91 ~ -1L, hown14_raw == -1 ~ -8L, is.na(hown14_raw) ~ -3L
    ),
    hown15 = case_when(
      hown15_raw == 1 ~ 1L, hown15_raw == 2 ~ 2L, hown15_raw == 3 ~ 3L,
      hown15_raw %in% 4:6 ~ 4L, hown15_raw == 7 ~ 5L, hown15_raw == 8 ~ 6L,
      hown15_raw %in% c(-998, -997, -995, -99) ~ -2L, hown15_raw == -92 ~ -9L,
      hown15_raw == -91 ~ -1L, hown15_raw == -1 ~ -8L, is.na(hown15_raw) ~ -3L
    ),
    hown16 = case_when(
      hown16_raw == 1 ~ 1L, hown16_raw == 2 ~ 2L, hown16_raw == 3 ~ 3L,
      hown16_raw %in% 4:6 ~ 4L, hown16_raw == 7 ~ 5L, hown16_raw == 8 ~ 6L,
      hown16_raw == -999 ~ -2L, hown16_raw == -92 ~ -9L,
      hown16_raw == -91 ~ -1L, hown16_raw == -1 ~ -8L, is.na(hown16_raw) ~ -3L
    ),
    hown17 = case_when(
      hown17_raw == 1 ~ 1L, hown17_raw == 2 ~ 2L, hown17_raw == 3 ~ 3L,
      hown17_raw %in% 4:6 ~ 4L, hown17_raw == 7 ~ 5L, hown17_raw == 8 ~ 6L,
      hown17_raw %in% c(-999, -997) ~ -2L, hown17_raw == -92 ~ -9L,
      hown17_raw == -91 ~ -1L, hown17_raw == -1 ~ -8L, is.na(hown17_raw) ~ -3L
    ),
    hown18 = recode_tenure_collapsed(s5_tenure_type, s5_tenure_owned, s5_tenure_rented),
    hown19 = recode_tenure_collapsed(s6_tenure_type, s6_tenure_owned, s6_tenure_rented),
    hown20 = recode_tenure_collapsed(s7_tenure_type, s7_tenure_owned, s7_tenure_rented),
    hown25 = case_when(
      hown25_raw == 1 ~ 1L, hown25_raw == 2 ~ 2L, hown25_raw == 3 ~ 3L,
      hown25_raw == 4 ~ 4L, hown25_raw == 5 ~ 5L, hown25_raw %in% 6:7 ~ 6L,
      hown25_raw == -9 ~ -9L, hown25_raw == -8 ~ -8L, hown25_raw == -1 ~ -1L,
      is.na(hown25_raw) ~ -3L
    ),
    hown32 = case_when(
      hown32_raw == 1 ~ 1L, hown32_raw == 2 ~ 2L, hown32_raw == 3 ~ 3L,
      hown32_raw == 4 ~ 4L, hown32_raw == 5 ~ 5L, hown32_raw %in% 6:7 ~ 6L,
      hown32_raw == -9 ~ -9L, hown32_raw == -8 ~ -8L, hown32_raw == -1 ~ -1L,
      is.na(hown32_raw) ~ -3L
    )
  )

# Convert to factors and select output -----------------------------------

hown_all <- hown_all %>%
  mutate(
    across(
      c(hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20),
      ~ factor(.x, levels = hownteen_levels, labels = hownteen_labels)
    ),
    across(
      c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32),
      ~ factor(.x, levels = hown_levels, labels = hown_labels)
    )
  ) %>%
  select(
    NSID,
    hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32,
    hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20
  )

# Write output
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)
write.csv(hown_all, file.path(getwd(), "data", "output", "output.csv"), row.names = FALSE)