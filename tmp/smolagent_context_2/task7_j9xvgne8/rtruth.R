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

# Load datasets
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S6yp <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE)
S7yp <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8mi <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"), show_col_types = FALSE)
S9mi <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE)

# Select needed variables per sweep
educaim_vars <- list(
  S1 = S1yp %>% select(NSID),
  S4 = S4yp %>% select(NSID, educaim17_raw = w4saim),
  S6 = S6yp %>% select(NSID, educaim19_raw = W6Saim),
  S7 = S7yp %>% select(NSID, educaim20_raw = W7SAim),
  S8 = S8mi %>% select(NSID, W8ACTIVITY05, starts_with("W8ACQUC"), starts_with("W8VCQUC")),
  S9 = S9mi %>% select(NSID, W9ECONACT2, starts_with("W9ACQUC"), starts_with("W9VCQUC"))
)

# Merge by ID
educaim_all <- reduce(educaim_vars, full_join, by = "NSID")

# Harmonised labels (used for final factor conversion)
educaim_levels <- c(
  "NVQ 4-5", "NVQ 1-3", "None/entry", "Other",
  "None of these qualifications", "Not studying",
  "Item not applicable", "Script error/information lost",
  "Not asked at the fieldwork stage/participated/interviewed",
  "Don't know/insufficient information", "Refusal"
)
educaim_codes <- c(0L, 1L, 2L, 3L, 4L, 5L, -1L, -2L, -3L, -8L, -9L)

# Helper: TRUE if any variable in `vars` equals 1
has_any_tick <- function(vars) {
  dplyr::if_any(dplyr::all_of(vars), ~ .x == 1)
}

## --- S8 qualification variable groups ---
s8_nvq45       <- c("W8ACQUC0A","W8ACQUC0B","W8ACQUC0C","W8ACQUC0D","W8ACQUC0E","W8VCQUC0J","W8VCQUC0K")
s8_nvq13       <- c("W8ACQUC0F","W8ACQUC0G","W8ACQUC0H","W8ACQUC0I","W8ACQUC0J","W8ACQUC0K","W8ACQUC0L","W8ACQUC0M",
                     "W8VCQUC0A","W8VCQUC0B","W8VCQUC0C","W8VCQUC0E","W8VCQUC0F","W8VCQUC0G","W8VCQUC0H","W8VCQUC0I",
                     "W8VCQUC0L","W8VCQUC0M","W8VCQUC0N")
s8_entry_none  <- c("W8VCQUC0D")
s8_other       <- c("W8ACQUC0N","W8VCQUC0O")
s8_none_of     <- c("W8ACQUC0O","W8VCQUC0P")
s8_dk          <- c("W8ACQUC0P","W8VCQUC0Q")
s8_refusal     <- c("W8ACQUC0Q","W8VCQUC0R")

## --- S9 qualification variable groups ---
s9_nvq45       <- c("W9ACQUC0A","W9ACQUC0B","W9ACQUC0C","W9ACQUC0D","W9ACQUC0E","W9ACQUC0F",
                     "W9VCQUC0A","W9VCQUC0B","W9VCQUC0C","W9VCQUC0S","W9VCQUC0V","W9VCQUCAC")
s9_nvq13       <- c("W9ACQUC0G","W9ACQUC0H","W9ACQUC0I","W9ACQUC0J","W9ACQUC0K","W9ACQUC0L","W9ACQUC0M","W9ACQUC0O","W9ACQUC0P","W9ACQUC0Q",
                     "W9VCQUC0D","W9VCQUC0E","W9VCQUC0F","W9VCQUC0G","W9VCQUC0H","W9VCQUC0I","W9VCQUC0L","W9VCQUC0M","W9VCQUC0N",
                     "W9VCQUC0O","W9VCQUC0P","W9VCQUC0Q","W9VCQUC0R","W9VCQUC0T","W9VCQUC0U","W9VCQUC0W","W9VCQUC0X","W9VCQUC0Y",
                     "W9VCQUC0Z","W9VCQUCAA","W9VCQUCAB","W9VCQUCAD","W9VCQUCAE")
s9_entry_none  <- c("W9ACQUC0N")
s9_other       <- c("W9ACQUC0R","W9VCQUCAF")
s9_none_of     <- c("W9ACQUC0S","W9VCQUCAG")
s9_dk          <- c("W9ACQUC0T","W9VCQUCAH")
s9_refusal     <- c("W9ACQUC0U","W9VCQUCAI")

## --- Recode all sweeps ---
educaim_all <- educaim_all %>%
  mutate(
    # Sweep 4 (age 17): w4saim codes 1-11→NVQ1-3, 12→Other, 13→None, 14→Not studying
    educaim17 = case_when(
      educaim17_raw %in% 1:11    ~ 1L,   # NVQ 1-3
      educaim17_raw == 12        ~ 3L,   # Other
      educaim17_raw == 13        ~ 4L,   # None of these
      educaim17_raw == 14        ~ 5L,   # Not studying
      educaim17_raw == -94       ~ -2L,  # Script error
      educaim17_raw == -91       ~ -1L,  # Not applicable
      TRUE                       ~ -3L   # Not interviewed
    ),

    # Sweep 6 (age 19): W6Saim codes 1-4→NVQ4-5, 5-13→NVQ1-3, 14→Other, 15→None, 16→Not studying
    educaim19 = case_when(
      educaim19_raw %in% 1:4     ~ 0L,   # NVQ 4-5
      educaim19_raw %in% 5:13    ~ 1L,   # NVQ 1-3
      educaim19_raw == 14        ~ 3L,   # Other
      educaim19_raw == 15        ~ 4L,   # None of these
      educaim19_raw == 16        ~ 5L,   # Not studying
      TRUE                       ~ -3L
    ),

    # Sweep 7 (age 20): W7SAim codes 10-13→NVQ4-5, 1-9→NVQ1-3, 14→Other
    educaim20 = case_when(
      educaim20_raw %in% 10:13   ~ 0L,   # NVQ 4-5
      educaim20_raw %in% 1:9     ~ 1L,   # NVQ 1-3
      educaim20_raw == 14        ~ 3L,   # Other
      educaim20_raw == -94       ~ -8L,  # Insufficient info → DK
      educaim20_raw == -91       ~ 5L,   # Not applicable (not studying)
      TRUE                       ~ -3L
    )
  ) %>%

  # Sweep 8 (age 25): two-stage — activity then qualification tick-boxes
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0                ~ 5L,   # Not studying
      has_any_tick(s8_nvq45)           ~ 0L,
      has_any_tick(s8_nvq13)           ~ 1L,
      has_any_tick(s8_entry_none)      ~ 2L,
      has_any_tick(s8_other)           ~ 3L,
      has_any_tick(s8_none_of)         ~ 4L,
      W8ACTIVITY05 == -1               ~ -1L,
      W8ACTIVITY05 == -3               ~ -3L,
      has_any_tick(s8_dk) | W8ACTIVITY05 == -8 ~ -8L,
      has_any_tick(s8_refusal) | W8ACTIVITY05 == -9 ~ -9L,
      TRUE                             ~ -3L
    )
  ) %>%

  # Sweep 9 (age 32): two-stage — econ activity then qualification tick-boxes
  mutate(
    educaim32 = case_when(
      W9ECONACT2 %in% c(1:5, 8:14)    ~ 5L,   # Not studying
      has_any_tick(s9_nvq45)           ~ 0L,
      has_any_tick(s9_nvq13)           ~ 1L,
      has_any_tick(s9_entry_none)      ~ 2L,
      has_any_tick(s9_other)           ~ 3L,
      has_any_tick(s9_none_of)         ~ 4L,
      W9ECONACT2 == -1                 ~ -1L,
      W9ECONACT2 == -3                 ~ -3L,
      has_any_tick(s9_dk) | W9ECONACT2 == -8 ~ -8L,
      has_any_tick(s9_refusal) | W9ECONACT2 == -9 ~ -9L,
      TRUE                             ~ -3L
    )
  ) %>%

  # Convert all to factors and select final output
  mutate(
    across(
      c(educaim17, educaim19, educaim20, educaim25, educaim32),
      ~ factor(.x, levels = educaim_codes, labels = educaim_levels)
    )
  ) %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)
write.csv(educaim_all, file.path(getwd(), "data", "output", "output.csv"), row.names = FALSE)