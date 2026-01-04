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
  select(NSID, ecoact17 = W4empsYP)

S5 <- read_delim(file.path(data_path, "wave_five_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact18 = W5mainactYP)

S6 <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact19 = W6TCurrentAct)

S7 <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoact20 = W7TCurrentAct)

S8 <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactadu25 = W8DACTIVITYC)

S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactadu32 = W9DACTIVITYC)

ecoact_all <- reduce(list(S1, S4, S5, S6, S7, S8, S9), full_join, by = "NSID")

# Harmonise missing values and derive economic activity variables
ecoact_all <- ecoact_all %>%
  mutate(
    ## Sweep 4
    ecoact17 = case_when(
      ecoact17 %in% 1:2 ~ 1, # In paid work
      ecoact17 == 4 ~ 2, # Apprenticeship/government training scheme/training
      ecoact17 == 5 | ecoact17 == -91 ~ 3, # Education
      ecoact17 == 3 ~ 4, # Unemployed
      ecoact17 == 6 ~ 5, # Looking after home/family
      ecoact17 %in% c(7, 8, 9) ~ 6, # Sick/disabled, other, doing something else
      ecoact17 == -92 ~ -9,
      ecoact17 == -999 ~ -2,
      ecoact17 == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 5
    ecoact18 = case_when(
      ecoact18 == 3 ~ 1,
      ecoact18 %in% c(1, 5, 6) ~ 2,
      ecoact18 %in% c(2, 4) ~ 3,
      ecoact18 == 7 ~ 4,
      ecoact18 == 8 ~ 5,
      ecoact18 %in% 9:11 ~ 6,
      ecoact18 == -94 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 6
    ecoact19 = case_when(
      ecoact19 == 3 ~ 1,
      ecoact19 %in% c(4, 5) ~ 2,
      ecoact19 %in% c(1, 2, 10) ~ 3,
      ecoact19 == 8 ~ 4,
      ecoact19 == 7 ~ 5,
      ecoact19 %in% c(6, 9, 11) ~ 6,
      ecoact19 == -91 ~ -8,
      TRUE ~ -3
    ),
    ## Sweep 7
    ecoact20 = case_when(
      ecoact20 == 3 ~ 1,
      ecoact20 %in% c(4, 5, 11) ~ 2,
      ecoact20 %in% c(1, 2, 9) ~ 3,
      ecoact20 == 8 ~ 4,
      ecoact20 == 7 ~ 5,
      ecoact20 %in% c(6, 10, 12:15) ~ 6,
      ecoact20 == -91 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 8
    ecoact25 = case_when(
      ecoactadu25 %in% c(1, 2) ~ 1,
      ecoactadu25 %in% c(6, 7) ~ 2,
      ecoactadu25 == 5 ~ 3,
      ecoactadu25 == 4 ~ 4,
      ecoactadu25 == 9 ~ 5,
      ecoactadu25 %in% c(3, 8, 10) ~ 6,
      ecoactadu25 == -9 ~ -9,
      ecoactadu25 == -8 ~ -8,
      ecoactadu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Sweep 9
    ecoact32 = case_when(
      ecoactadu32 %in% c(1, 2) ~ 1,
      ecoactadu32 %in% c(6, 7) ~ 2,
      ecoactadu32 == 5 ~ 3,
      ecoactadu32 == 4 ~ 4,
      ecoactadu32 == 9 ~ 5,
      ecoactadu32 %in% c(3, 8, 10) ~ 6,
      ecoactadu32 == -9 ~ -9,
      ecoactadu32 == -8 ~ -8,
      ecoactadu32 == -1 ~ -1,
      TRUE ~ -3
    ),
    ## Detailed versions (S8, S9 only)
    ecoactadu25 = case_when(
      !is.na(ecoactadu25) ~ ecoactadu25,
      is.na(ecoactadu25) ~ -3
    ),
    ecoactadu32 = case_when(
      !is.na(ecoactadu32) ~ ecoactadu32,
      is.na(ecoactadu32) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
        labels = c(
          "In paid work",
          "Apprenticeship/government training scheme/training",
          "Education",
          "Unemployed",
          "Looking after home",
          "Other",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(ecoactadu25, ecoactadu32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1, -2, -3, -8, -9),
        labels = c(
          "employee – in paid work",
          "self employed",
          "voluntary work",
          "Unemployed",
          "Education",
          "Apprenticeship",
          "government employment scheme",
          "sick/disabled",
          "Looking after home/family",
          "Something else",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
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
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(ecoact_all, output_data_path, row.names = FALSE)