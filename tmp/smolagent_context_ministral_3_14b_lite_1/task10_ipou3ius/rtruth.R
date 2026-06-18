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
  select(NSID, ecoact17_raw = W4empsYP)

S5 <- read_delim(file.path(data_path, "wave_five_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact18_raw = W5mainactYP)

S6 <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact19_raw = W6TCurrentAct)

S7 <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact20_raw = W7TCurrentAct)

S8 <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactadu25_raw = W8DACTIVITYC)

S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactadu32_raw = W9DACTIVITYC)

ecoact_all <- reduce(list(S1, S4, S5, S6, S7, S8, S9), full_join, by = "NSID")

common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)

# Harmonise missing values and derive economic activity variables
ecoact_rec <- ecoact_all %>%
  mutate(
    ## Sweep 4
    ecoact17 = case_when(
      ecoact17_raw %in% 1:2 ~ 1, # In paid work
      ecoact17_raw == 4 ~ 2, # Apprenticeship/government training scheme/training
      ecoact17_raw == 5 | ecoact17_raw == -91 ~ 3, # Education
      ecoact17_raw == 3 ~ 4, # Unemployed
      ecoact17_raw == 6 ~ 5, # Looking after home/family
      ecoact17_raw %in% c(7, 8, 9) ~ 6, # Sick/disabled, other, doing something else
      ecoact17_raw == -92 ~ -9,
      ecoact17_raw == -999 ~ -2,
      ecoact17_raw == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 5
    ecoact18 = case_when(
      ecoact18_raw == 3 ~ 1,
      ecoact18_raw %in% c(1, 5, 6) ~ 2,
      ecoact18_raw %in% c(2, 4) ~ 3,
      ecoact18_raw == 7 ~ 4,
      ecoact18_raw == 8 ~ 5,
      ecoact18_raw %in% 9:11 ~ 6,
      ecoact18_raw == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 6
    ecoact19 = case_when(
      ecoact19_raw == 3 ~ 1,
      ecoact19_raw %in% c(4, 5) ~ 2,
      ecoact19_raw %in% c(1, 2, 10) ~ 3,
      ecoact19_raw == 8 ~ 4,
      ecoact19_raw == 7 ~ 5,
      ecoact19_raw %in% c(6, 9, 11) ~ 6,
      ecoact19_raw == -91 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 7
    ecoact20 = case_when(
      ecoact20_raw == 3 ~ 1,
      ecoact20_raw %in% c(4, 5, 11) ~ 2,
      ecoact20_raw %in% c(1, 2, 9) ~ 3,
      ecoact20_raw == 8 ~ 4,
      ecoact20_raw == 7 ~ 5,
      ecoact20_raw %in% c(6, 10, 12:15) ~ 6,
      ecoact20_raw == -91 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 8
    ecoact25 = case_when(
      ecoactadu25_raw %in% c(1, 2) ~ 1,
      ecoactadu25_raw %in% c(6, 7) ~ 2,
      ecoactadu25_raw == 5 ~ 3,
      ecoactadu25_raw == 4 ~ 4,
      ecoactadu25_raw == 9 ~ 5,
      ecoactadu25_raw %in% c(3, 8, 10) ~ 6,
      ecoactadu25_raw == -9 ~ -9,
      ecoactadu25_raw == -8 ~ -8,
      ecoactadu25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 9
    ecoact32 = case_when(
      ecoactadu32_raw %in% c(1, 2) ~ 1,
      ecoactadu32_raw %in% c(6, 7) ~ 2,
      ecoactadu32_raw == 5 ~ 3,
      ecoactadu32_raw == 4 ~ 4,
      ecoactadu32_raw == 9 ~ 5,
      ecoactadu32_raw %in% c(3, 8, 10) ~ 6,
      ecoactadu32_raw == -9 ~ -9,
      ecoactadu32_raw == -8 ~ -8,
      ecoactadu32_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Detailed versions (S8, S9 only)
    ecoactadu25 = case_when(
      !is.na(ecoactadu25_raw) ~ ecoactadu25_raw,
      is.na(ecoactadu25_raw) ~ -3
    ),
    ecoactadu32 = case_when(
      !is.na(ecoactadu32_raw) ~ ecoactadu32_raw,
      is.na(ecoactadu32_raw) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32),
      ~ labelled(
        .x,
        labels = c(
          "In paid work" = 1L,
          "Apprenticeship/government training scheme/training" = 2L,
          "Education" = 3L,
          "Unemployed" = 4L,
          "Looking after home" = 5L,
          "Other" = 6L,
          common_missing_labels
        )
      )
    ),
    across(
      c(ecoactadu25, ecoactadu32),
      ~ labelled(
        .x,
        labels = c(
          "employee – in paid work" = 1L,
          "self employed" = 2L,
          "voluntary work" = 3L,
          "Unemployed" = 4L,
          "Education" = 5L,
          "Apprenticeship" = 6L,
          "government employment scheme" = 7L,
          "sick/disabled" = 8L,
          "Looking after home/family" = 9L,
          "Something else" = 10L,
          common_missing_labels
        )
      )
    )
  )

# Checks
ecoact_rec %>%
  count(ecoact17_raw, ecoact17)

ecoact_rec %>%
  count(ecoact18_raw, ecoact18)

ecoact_rec %>%
  count(ecoact19_raw, ecoact19)

ecoact_rec %>%
  count(ecoact20_raw, ecoact20)

ecoact_rec %>%
  count(ecoactadu25_raw, ecoact25)

ecoact_all <- ecoact_rec %>%
  select(
    NSID,
    ecoact17,
    ecoact18,
    ecoact19,
    ecoact20,
    ecoact25,
    ecoact32,
    ecoactadu25,
    ecoactadu32
  ) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(ecoact_all, output_data_path, row.names = FALSE)