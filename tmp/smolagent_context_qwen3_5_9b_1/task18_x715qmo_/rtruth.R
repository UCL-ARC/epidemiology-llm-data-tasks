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

S2 <- read_delim(file.path(data_path, "wave_two_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(
    NSID,
    ghq15_raw = W2ghq12scr,
    paste0(
      "W2",
      c(
        "concenYP",
        "nosleepYP",
        "usefulYP",
        "decideYP",
        "strainYP",
        "difficYP",
        "activYP",
        "probsYP",
        "depressYP",
        "noconfYP",
        "wthlessYP",
        "happyYP"
      )
    )
  )

S4 <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(
    NSID,
    ghq17_raw = W4ghq12scr,
    paste0(
      "W4",
      c(
        "ConcenYP",
        "NoSleepYP",
        "UsefulYP",
        "DecideYP",
        "StrainYP",
        "DifficYP",
        "ActivYP",
        "ProbsYP",
        "DepressYP",
        "NoConfYP",
        "WthlessYP",
        "HappyYP"
      )
    )
  )

S8 <- read_delim(file.path(data_path, "ns8_2015_self_completion.tab"), show_col_types = FALSE) %>%
  select(NSID, starts_with("W8GHQ12_"))

S8_derive <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>%
  select(NSID, ghq25_raw = W8DGHQSC)

S9 <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE) %>%
  select(NSID, starts_with("W9GHQ12_"))

S9_derive <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, ghq32_raw = W9DGHQSC)

# Combine into list for merging
ghq_vars <- list(
  S1 = S1,
  S2 = S2,
  S4 = S4,
  S8 = S8,
  S8_derive = S8_derive,
  S9 = S9,
  S9_derive = S9_derive
)

# Merge all sweeps by NSID
ghq_all <- reduce(ghq_vars, full_join, by = "NSID")

# Define item lists for sum scores
ghq_items <- list(
  ghqtl15 = paste0(
    "W2",
    c(
      "concenYP",
      "nosleepYP",
      "usefulYP",
      "decideYP",
      "strainYP",
      "difficYP",
      "activYP",
      "probsYP",
      "depressYP",
      "noconfYP",
      "wthlessYP",
      "happyYP"
    )
  ),
  ghqtl17 = paste0(
    "W4",
    c(
      "ConcenYP",
      "NoSleepYP",
      "UsefulYP",
      "DecideYP",
      "StrainYP",
      "DifficYP",
      "ActivYP",
      "ProbsYP",
      "DepressYP",
      "NoConfYP",
      "WthlessYP",
      "HappyYP"
    )
  ),
  ghqtl25 = paste0("W8GHQ12_", 1:12),
  ghqtl32 = paste0("W9GHQ12_", 1:12)
)

# Derive GHQ sum scores (0–12) with custom missing logic
ghq_rec <- ghq_all %>%
  mutate(
    ghqtl15 = {
      items <- select(., all_of(ghq_items$ghqtl15))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE)) # has missing value(s) in any of the GHQ items
      all_na <- apply(items, 1, function(x) all(is.na(x))) # all GHQ items are missing - not participating
      score <- rowSums(items, na.rm = TRUE) # sum GHQ items, ignoring NAs

      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl17 = {
      items <- select(., all_of(ghq_items$ghqtl17))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)

      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl25 = {
      items <- select(., all_of(ghq_items$ghqtl25))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)

      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    ghqtl32 = {
      items <- select(., all_of(ghq_items$ghqtl32))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)

      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    }
  ) %>%
  mutate(
    ghq15 = case_when(
      is.na(ghq15_raw) ~ -3,
      ghq15_raw %in% c(-96, -99) ~ -3,
      ghq15_raw %in% c(-97, -92) ~ -9,
      TRUE ~ ghq15_raw
    ),
    ghq17 = case_when(
      is.na(ghq17_raw) ~ -3,
      ghq17_raw %in% c(-96, -99) ~ -3,
      ghq17_raw %in% c(-97, -92) ~ -9,
      TRUE ~ ghq17_raw
    ),
    ghq25 = if_else(is.na(ghq25_raw), -3, ghq25_raw),
    ghq32 = if_else(is.na(ghq32_raw), -3, ghq32_raw)
  ) %>%
  mutate(
    across(
      c(ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32),
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
    )
  )

ghq_all <- ghq_rec %>%
  select(NSID, ghq15, ghq17, ghq25, ghq32, ghqtl15, ghqtl17, ghqtl25, ghqtl32)

# Convert GHQ item variables and sum scores to factors before saving
ghq_all <- ghq_all %>%
  mutate(
    across(
      -NSID,  # exclude NSID from conversion
      as.factor
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(ghq_all, output_data_path, row.names = FALSE)
