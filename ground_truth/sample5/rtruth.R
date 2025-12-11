# This script is used to run the Next Steps MSEU 
# Set CRAN mirror before installing packages
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

# Set folder path (change as needed)
DATA_PATH <- 'data/input/' # it's a constant so all caps


sweeps <- list(
  S1youngperson = "wave_one_lsype_young_person_2020.tab",
  S4youngperson = "wave_four_lsype_young_person_2020.tab",
  S5youngperson = "wave_five_lsype_young_person_2020.tab",
  S6youngperson = "wave_six_lsype_young_person_2020.tab",
  S8derivedvariable = "ns8_2015_derived.tab",
  S9derivedvariable = "ns9_2022_derived_variables.tab"
)


#### partnership ####
# Load partnership variables from relevant sweeps
partnr_vars <- list(
  S1 = read_delim(file.path(DATA_PATH, sweeps$S1youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID),
  S4 = read_delim(file.path(DATA_PATH, sweeps$S4youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID),
  S5 = read_delim(file.path(DATA_PATH, sweeps$S5youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, partnr18 = W5Marstat2YP),
  S6 = read_delim(file.path(DATA_PATH, sweeps$S6youngperson), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, partnr19 = W6MarStatYP),
  S8 = read_delim(file.path(DATA_PATH, sweeps$S8derivedvariable), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, partnradu25 = W8DMARSTAT),
  S9 = read_delim(file.path(DATA_PATH, sweeps$S9derivedvariable), delim = "\t", show_col_types = FALSE) %>% 
    select(NSID, partnradu32 = W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "NSID")

# recode missing values and response categories
partnr_all <- partnr_all %>%
  mutate(
    partnr18 = case_when(
      partnr18 == 1 ~ 1,         # married/civil partner
      partnr18 == 2 ~ 0, # engaged or neither → single
      partnr18 == 3 ~ 2, #  neither → other
      partnr18 == -92 ~ -9,
      partnr18 == -91 ~ -1,
      partnr18 == -1  ~ -8,
      TRUE ~ -3
    ),
    partnr19 = case_when(
      partnr19 == 2 ~ 1,
      partnr19 == 1 ~ 0,
      partnr19 %in% c(3, 4, 5) ~ 2,
      partnr19 == -92 ~ -9,
      partnr19 == -91 ~ -1,
      partnr19 == -1 ~ -8,
      partnr19 %in% c(-997, -97) ~ -3,
      TRUE ~ -3
    ),
    partnr25 = case_when(
      partnradu25 %in% c(2, 6) ~ 1,
      partnradu25 == 1 ~ 0,
      partnradu25 %in% c(3,4,5,7,8,9) ~ 2,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      partnradu32 %in% c(2,6) ~ 1,
      partnradu32 == 1 ~ 0,
      partnradu32 %in% c(3,4,5,7,8,9) ~ 2,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Derive adult partnership variables detailed response categories (age 25 and 32)
partnr_all <- partnr_all %>%
  mutate(
    partnradu25 = case_when(
      partnradu25 %in% 1:9 ~ partnradu25-1,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      partnradu32 %in% 1:9 ~ partnradu32-1,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      partnradu32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  select(NSID, partnr18, partnr19, partnr25, partnr32,
         partnradu25, partnradu32)


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(partnr_all, output_data_path, row.names = FALSE)