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


# Merge all NS-SEC variables by NSID
nssec_all <- reduce(list(S1, S4, S5, S6, S7, S8, S9), full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'nssec*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ paste0(.x, "_raw"),
    contains("nssec")
  )

## Fix source variable labels --------------------------------------------------------------------

# This is done for simpler validation.
nssec_s4_s5_s6_s7_missing <- c(
  `YP Not interviewed` = -99,
  `Not applicable` = -91
)

nssec_s8_missing <- c(
  `Refused` = -9,
  `Insufficient information` = -8,
  `Not applicable` = -1
)

nssec_labels_core <- c(
  `Employers in large organisations` = 1,
  `Higher managerial occupations` = 2,
  `Higher professional traditional employee` = 3.1,
  `Higher professional new employee` = 3.2,
  `Higher professional traditional self emp` = 3.3,
  `Higher professional new self emp` = 3.4,
  `Lower professional traditional employee` = 4.1,
  `Lower professional new employee` = 4.2,
  `Lower professional traditional self emp` = 4.3,
  `Lower professional new self emp` = 4.4,
  `Lower managerial occupations` = 5,
  `Higher supervisory occupations` = 6,
  `Intermediate clerical and administrative` = 7.1,
  `Intermediate sales and service` = 7.2,
  `Intermediate technical and auxiliary` = 7.3,
  `Intermediate engineering` = 7.4,
  `Employers in small orgs non-professional` = 8.1,
  `Employers in small orgs agriculture` = 8.2,
  `Own account workers non professional` = 9.1,
  `Own account workers agriculture` = 9.2,
  `Lower supervisory occupations` = 10,
  `Lower technical craft` = 11.1,
  `Lower technical process operative` = 11.2,
  `Semi routine sales` = 12.1,
  `Semi routine services` = 12.2,
  `Semi routine technical` = 12.3,
  `Semi routine operative` = 12.4,
  `Semi routine agricultural` = 12.5,
  `Semi routine clerical` = 12.6,
  `Semi routine childcare` = 12.7,
  `Routine sales and service` = 13.1,
  `Routine production` = 13.2,
  `Routine technical` = 13.3,
  `Routine operative` = 13.4,
  `Routine agricultural` = 13.5,
  `Never worked` = 14.1,
  `Long-term unemployed` = 14.2,
  `Not working` = 14.3,
  `Full-time students` = 15,
  `Not classified or inadequately stated` = 16,
  `Not classifiable for other reasons` = 17
)

# Apply common labels & sweep-specific features
nssec_all <- nssec_all %>%
  mutate(
    # Sweeps with common labels
    across(
      c(nssec17_raw, nssec18_raw, nssec19_raw, nssec20_raw),
      ~ labelled(.x, labels = c(nssec_s4_s5_s6_s7_missing, nssec_labels_core))
    ),
    # S8: Same occupation labels & values, different missing values
    nssec25_raw = labelled(
      nssec25_raw,
      labels = c(nssec_s8_missing, nssec_labels_core)
    )
  )

## Recode --------------------------------------------------------------------

common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)

# Harmonise NS-SEC values and derive categories
nssec_rec <- nssec_all %>%
  mutate(
    ## Sweep 4 (age 17)
    nssec17 = case_when(
      is.na(nssec17_raw) ~ -3,
      floor(nssec17_raw) %in% 1:17 ~ floor(nssec17_raw),
      nssec17_raw == -91 ~ -1,
      nssec17_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 5 (age 18)
    nssec18 = case_when(
      is.na(nssec18_raw) ~ -3,
      floor(nssec18_raw) %in% 1:17 ~ floor(nssec18_raw),
      nssec18_raw == -91 ~ -1,
      nssec18_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 6 (age 19)
    nssec19 = case_when(
      is.na(nssec19_raw) ~ -3,
      floor(nssec19_raw) %in% 1:17 ~ floor(nssec19_raw),
      nssec19_raw == -91 ~ -1,
      nssec19_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 7 (age 20)
    nssec20 = case_when(
      is.na(nssec20_raw) ~ -3,
      floor(nssec20_raw) %in% 1:17 ~ floor(nssec20_raw),
      nssec20_raw == -91 ~ -1,
      nssec20_raw == -99 ~ -3,
      TRUE ~ -3
    ),
    ## Sweep 8 (age 25)
    nssec25 = case_when(
      is.na(nssec25_raw) ~ -3,
      floor(nssec25_raw) %in% 1:14 ~ floor(nssec25_raw),
      ecoactadu25 == 5 ~ 15, # full-time student
      nssec25_raw == -9 ~ -9,
      nssec25_raw == -8 ~ -8,
      nssec25_raw == -1 ~ -1,
    ),
    ## Sweep 9 (age 32)
    nssec32 = case_when(
      is.na(nssec32_raw) ~ -3,
      nssec32_raw %in% 1:17 ~ nssec32_raw,
      nssec32_raw == -9 ~ -9,
      nssec32_raw == -8 ~ -8,
      nssec32_raw == -1 ~ -1
    )
  ) %>%
  mutate(
    across(
      starts_with("nssec") & !ends_with("raw"),
      ~ labelled(
        .x,
        labels = c(
          "Employers in large organisations" = 1,
          "Higher managerial and administrative occupations" = 2,
          "Higher professional occupations" = 3,
          "Lower professional and higher technical occupations" = 4,
          "Lower managerial and administrative occupations" = 5,
          "Higher supervisory occupations" = 6,
          "Intermediate occupations" = 7,
          "Employers in small establishments" = 8,
          "Own account workers" = 9,
          "Lower supervisory occupations" = 10,
          "Lower technical occupations" = 11,
          "Semi-routine occupations" = 12,
          "Routine occupations" = 13,
          "Never worked and long-term unemployed" = 14,
          "Full-time student" = 15,
          "Not classified or inadequately stated" = 16,
          "Not classifiable for other reasons" = 17,
          common_missing_labels
        )
      )
    )
  )

nssec_all <- nssec_rec %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(nssec_all, output_data_path, row.names = FALSE)