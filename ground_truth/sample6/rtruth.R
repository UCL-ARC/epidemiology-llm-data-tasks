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


# Required datasets
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types = FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S2fb <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE)
S3fb <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE)
S8dv <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE)
S9dv <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE)
S9mi <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE)

region_vars <- list(
  S1 = S1yp %>% select(NSID),
  S4 = S4yp %>% select(NSID),
  S2 = S2fb %>% select(NSID, regub15 = urbind, regov15 = gor),
  S3 = S3fb %>% select(NSID, regub16 = urbind, regov16 = gor),
  S8 = S8dv %>% select(NSID, regor25 = W8DGOR),
  S9 = S9dv %>% select(NSID, regor32 = W9DRGN),
  S9_2 = S9mi %>% select(NSID, regint32 = W9NATIONRES)
)

region_all <- reduce(region_vars, full_join, by = "NSID")

region_all <- region_all %>%
  mutate(
    regint32 = case_when(
      regint32 %in% 1:4 ~ 1,
      regint32 == 5 ~ 2,
      regint32 == -9 ~ -9,
      regint32 == -8 ~ -8,
      regint32 == -1 ~ -1,
      TRUE ~ -3
    ),
    regint32 = factor(
      regint32,
      levels = c(1, 2, -1, -3, -8, -9),
      labels = c(
        "In the UK",
        "Abroad",
        "Item not applicable",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(region_all, output_data_path, row.names = FALSE)