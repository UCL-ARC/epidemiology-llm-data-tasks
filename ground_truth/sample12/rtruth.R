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

S1 <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID)
S4 <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, nssec17 = W4nsseccatYP)
S5 <- read_delim(file.path(data_path, "wave_five_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, nssec18 = W5nsseccatYP)
S6 <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, nssec19 = w6nsseccatYP)
S7 <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, nssec20 = W7NSSECCat)
S8 <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE) %>% select(NSID, nssec25 = W8DNSSEC17, ecoactadu25 = W8DACTIVITYC)
S9 <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE) %>% select(NSID, nssec32 = W9NSSEC)

nssec_all <- reduce(list(S1, S4, S5, S6, S7, S8, S9), full_join, by = "NSID")

# Harmonise NS-SEC values and derive categories
nssec_all <- nssec_all %>%
  mutate(
    ## Sweep 4 (age 17)
    nssec17 = case_when(
      is.na(nssec17) ~ -3,
      floor(nssec17) %in% 1:17 ~ floor(nssec17),
      nssec17 == -91 ~ -1,
      nssec17 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 5 (age 18)
    nssec18 = case_when(
      is.na(nssec18) ~ -3,
      floor(nssec18) %in% 1:17 ~ floor(nssec18),
      nssec18 == -91 ~ -1,
      nssec18 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 6 (age 19)
    nssec19 = case_when(
      is.na(nssec19) ~ -3,
      floor(nssec19) %in% 1:17 ~ floor(nssec19),
      nssec19 == -91 ~ -1,
      nssec19 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 7 (age 20)
    nssec20 = case_when(
      is.na(nssec20) ~ -3,
      floor(nssec20) %in% 1:17 ~ floor(nssec20),
      nssec20 == -91 ~ -1,
      nssec20 == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 8 (age 25)
    nssec25 = case_when(
      is.na(nssec25) ~ -3,
      floor(nssec25) %in% 1:14 ~ floor(nssec25),
      ecoactadu25 == 5 ~ 15, # full-time student
      nssec25 == -9 ~ -9,
      nssec25 == -8 ~ -8,
      nssec25 == -1 ~ -1,
    ),
    ## Sweep 9 (age 32)
    nssec32 = case_when(
      is.na(nssec32) ~ -3,
      nssec32 %in% 1:17 ~ nssec32,
      nssec32 == -9 ~ -9,
      nssec32 == -8 ~ -8,
      nssec32 == -1 ~ -1
    )
  ) %>%
  mutate(across(
    starts_with("nssec"),
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
        17,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "Employers in large organisations",
        "Higher managerial and administrative occupations",
        "Higher professional occupations",
        "Lower professional and higher technical occupations",
        "Lower managerial and administrative occupations",
        "Higher supervisory occupations",
        "Intermediate occupations",
        "Employers in small establishments",
        "Own account workers",
        "Lower supervisory occupations",
        "Lower technical occupations",
        "Semi-routine occupations",
        "Routine occupations",
        "Never worked and long-term unemployed",
        "Full-time student",
        "Not classified or inadequately stated",
        "Not classifiable for other reasons",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(nssec_all, output_data_path, row.names = FALSE)