options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('haven', 'dplyr', 'purrr', 'here', 'labelled', 'readr', 'stringr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(dplyr)      # for data manipulation
  library(purrr)      # for functional programming (map, reduce)
  library(here)       # for file paths
  library(labelled)   # for handling labelled data
  library(readr)      # for reading delimited files
  library(stringr)    # for string manipulation
})

# Data path
# Set folder path (change as needed)
data_path <- 'data/input/'

# Alcohol --------------------------------------------------------------------
# Load and Select Variables
alc_vars <- list(
  S1 = read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(
      NSID,
      alcever_S1 = W1alceverYP,
      alcmon_S1 = W1alcmonYP,
      alcfreq_S1 = W1alcfreqYP
    ),
  S2 = read_delim(file.path(data_path, "wave_two_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(NSID, alcever_S2 = W2alceverYP, alcfreq_S2 = W2alcfreqYP),
  S3 = read_delim(file.path(data_path, "wave_three_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(NSID, alcever_S3 = W3alceverYP, alcfreq_S3 = W3alcfreqYP),
  S4 = read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(NSID, alcever_S4 = W4AlcEverYP, alcfreq_S4 = W4AlcFreqYP),
  S6 = read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(NSID, alcever_S6 = W6AlcEverYP, alcfreq_S6 = W6AlcFreqYP),
  S7 = read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
    select(NSID, alcever_S7 = W7AlcEverYP, alcfreq_S7 = W7AlcFreqYP),
  S8 = read_delim(file.path(data_path, "ns8_2015_self_completion.tab"), show_col_types = FALSE) %>%
    select(NSID, audita25 = W8AUDIT1),
  S9 = read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE) %>%
    select(NSID, audita32 = W9AUDIT1)
)

# Merge all alcohol variables by NSID
alc_all <- reduce(alc_vars, full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'audit*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("audit")
  )

## First time had alcohol --------------------------------------------------------------------

# Helpers: Derive 'not drinking' from alcever and audita
# This will be used to derive never drinkers.
never_from_alcever <- function(x) {
  dplyr::case_when(
    x < 0 ~ NA_real_, # negative codes = missing
    x == 2 ~ 1, # "never"
    x == 1 ~ 0, # "ever"
    .default = NA_real_
  )
}

never_from_audita <- function(x) {
  dplyr::case_when(
    x < 0 ~ NA_real_, # negative codes = missing
    x == 1 ~ 1, # "never"
    x > 1 ~ 0, # "ever"
    .default = NA_real_
  )
}

alc_first_age_rec <- alc_all |>
  mutate(
    # Derive age first known drinking
    ever14 = if_else(alcever_S1 == 1 & alcmon_S1 == 1, 14, NA_real_),
    ever15 = if_else(alcever_S2 == 1, 15, NA_real_),
    ever16 = if_else(alcever_S3 == 1, 16, NA_real_),
    ever17 = if_else(alcever_S4 == 1, 17, NA_real_),
    ever19 = if_else(alcever_S6 == 1, 19, NA_real_),
    ever20 = if_else(alcever_S7 == 1, 20, NA_real_),
    ever25 = if_else(audita25_raw > 1, 25, NA_real_),
    ever32 = if_else(audita32_raw > 1, 32, NA_real_),

    first_age_raw = pmin(
      ever14,
      ever15,
      ever16,
      ever17,
      ever19,
      ever20,
      ever25,
      ever32,
      na.rm = TRUE
    ),

    # Derive known never drinking
    # recode to 1 = "never", 0 = "ever", NA = missing
    across(
      c(alcever_S1, alcever_S2, alcever_S3, alcever_S4, alcever_S6, alcever_S7),
      never_from_alcever,
      .names = "never_{.col}"
    ),
    across(
      c(audita25_raw, audita32_raw),
      never_from_audita,
      .names = "never_{.col}"
    ),
    # Derive never drinkers:
    # - 1 = all items observed & all never
    # - 0 = at least one drinker
    # - NA = no drinkers but some/all missing
    never_alc = case_when(
      # any 0 -> drinker
      if_any(starts_with("never_"), ~ dplyr::coalesce(.x == 0, FALSE)) ~ 0L,
      # all observed & 1
      if_all(starts_with("never_"), ~ !is.na(.x) & .x == 1) ~ 1L,
      # otherwise NA
      .default = NA
    ),
    # First age -> use the first age when not missing.
    # If never drinking, set to 99.
    # Anything else is missing.
    alcfst = case_when(
      !is.na(first_age_raw) ~ first_age_raw,
      never_alc == 1 ~ 99,
      .default = -8
    )
  ) |>
  select(-starts_with(c("ever", "never")), -first_age_raw)


# Add labels as factors
alc_first_age_rec <- alc_first_age_rec %>%
  mutate(
    alcfst = factor(
      alcfst,
      levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
      labels = c(
        "Age 14",
        "Age 15",
        "Age 16",
        "Age 17",
        "Age 19",
        "Age 20",
        "Age 25",
        "Age 32",
        "Never had alcohol",
        "Don't know/insufficient information"
      )
    )
  ) %>%
  select(NSID, alcfst)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(alc_first_age_rec, output_data_path, row.names = FALSE)
