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

# Merge all region variables by NSID
region_all <- reduce(region_vars, full_join, by = "NSID")

# Recode missing valuse and response categories
region_all <- region_all %>%
  mutate(across(
    c(regub15, regub16),
    ~ case_when(
      .x %in% 1:8 ~ .x,
      .x == -94 ~ -8,
      TRUE ~ -3
    )
  )) %>%

  mutate(across(
    c(regov15, regov16),
    ~ case_when(
      .x %in% 1:9 ~ .x,
      .x == -94 ~ -8,
      TRUE ~ -3
    )
  )) %>%

  mutate(
    across(
      c(regor25, regor32),
      ~ case_when(
        .x %in% 1:12 ~ .x,
        .x == 13 ~ -2, # faulty location
        .x == -9 ~ -9, # refused
        .x == -8 ~ -8, # don't know
        .x == -1 ~ -1, # not applicable
        TRUE ~ -3 # not participated
      )
    )
  ) %>%

  mutate(
    regint32 = case_when(
      regint32 %in% 1:4 ~ 1, # in the UK
      regint32 == 5 ~ 2, # abroad
      regint32 == -9 ~ -9,
      regint32 == -8 ~ -8,
      regint32 == -3 ~ -3,
      regint32 == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    across(
      c(regub15, regub16),
      ~ labelled(
        x = .x,
        labels = c(
          "Urban >=10k – sparse" = 1,
          "Town & Fringe – sparse" = 2,
          "Village – sparse" = 3,
          "Hamlet and Isolated Dwelling – sparse" = 4,
          "Urban >= 10k - less sparse" = 5,
          "Town & Fringe - less sparse" = 6,
          "Village - less sparse" = 7,
          "Hamlet and Isolated Dwelling - less sparse" = 8,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Prefer not to say" = -7,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    across(
      c(regov15, regov16),
      ~ labelled(
        x = .x,
        labels = c(
          "North East" = 1,
          "North West" = 2,
          "Yorkshire and The Humber" = 3,
          "East Midlands" = 4,
          "West Midlands" = 5,
          "East of England" = 6,
          "London" = 7,
          "South East" = 8,
          "South West" = 9,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Prefer not to say" = -7,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    across(
      c(regor25, regor32),
      ~ labelled(
        x = .x,
        labels = c(
          "North East" = 1,
          "North West" = 2,
          "Yorkshire and The Humber" = 3,
          "East Midlands" = 4,
          "West Midlands" = 5,
          "East of England" = 6,
          "London" = 7,
          "South East" = 8,
          "South West" = 9,
          "Wales" = 10,
          "Scotland" = 11,
          "Northern Ireland" = 12,
          "Item not applicable" = -1,
          "Script error/information lost" = -2,
          "Not asked at the fieldwork stage/participated/interviewed" = -3,
          "Don’t know/insufficient information" = -8,
          "Refusal" = -9
        )
      )
    ),
    regint32 = labelled(
      x = regint32,
      labels = c(
        "In the UK" = 1,
        "Abroad" = 2,
        "Item not applicable" = -1,
        "Not asked at the fieldwork stage/participated/interviewed" = -3,
        "Don’t know/insufficient information" = -8,
        "Refusal" = -9
      )
    )
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(region_all, output_data_path, row.names = FALSE)