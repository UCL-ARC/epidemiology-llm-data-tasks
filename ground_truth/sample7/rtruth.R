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

# Load only required datasets
S1youngperson <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types = FALSE)
S4youngperson <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"), show_col_types = FALSE)
S6youngperson <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE)
S7youngperson <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE)
S8main <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"), show_col_types = FALSE)
S9main <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE)

# Select needed variables
educaim_vars <- list(
  S1 = S1youngperson %>% select(NSID),
  S4 = S4youngperson %>% select(NSID, educaim17_raw = w4saim),
  S6 = S6youngperson %>% select(NSID, educaim19_raw = W6Saim),
  S7 = S7youngperson %>% select(NSID, educaim20_raw = W7SAim),
  S8 = S8main %>% select(NSID, W8ACTIVITY05, starts_with("W8ACQUC"), starts_with("W8VCQUC")),
  S9 = S9main %>% select(NSID, W9ECONACT2, starts_with("W9ACQUC"), starts_with("W9VCQUC"))
)

educaim_all <- reduce(educaim_vars, full_join, by = "NSID")

# recode missing valuse and response categories
educaim_all <- educaim_all %>%
  mutate(
    # Sweep 4
    educaim17 = case_when(
      educaim17_raw %in% c(1:9, 10, 11) ~ 1, # NVQ 1-3
      educaim17_raw == 14 ~ 5, # not studying
      educaim17_raw %in% 12 ~ 3, # other
      educaim17_raw == 13 ~ 4, # none of these
      educaim17_raw == -94 ~ -2,
      educaim17_raw == -91 ~ -1,
      TRUE ~ -3 # Not interviewed/present
    ),

    # Sweep 6
    educaim19 = case_when(
      educaim19_raw %in% 1:4 ~ 0,
      educaim19_raw %in% 5:12 ~ 1,
      educaim19_raw == 14 ~ 3,
      educaim19_raw == 15 ~ 4,
      educaim19_raw == 16 ~ 5,
      educaim19_raw == -94 ~ -2,
      educaim19_raw == -91 ~ -1,
      TRUE ~ -3
    ),

    # Sweep 7
    educaim20 = case_when(
      educaim20_raw %in% 10:13 ~ 0,
      educaim20_raw %in% 1:9 ~ 1,
      educaim20_raw == 14 ~ 3,
      educaim20_raw == -94 ~ -2,
      educaim20_raw == -91 ~ 5,
      TRUE ~ -3
    )
  ) %>%

  # Sweep 8
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5, #not studying
      W8ACQUC0A == 1 |
        W8ACQUC0B == 1 |
        W8ACQUC0C == 1 |
        W8ACQUC0D == 1 |
        W8ACQUC0E == 1 |
        W8VCQUC0J == 1 |
        W8VCQUC0K == 1 ~ 0,
      W8ACQUC0F == 1 |
        W8ACQUC0G == 1 |
        W8ACQUC0H == 1 |
        W8ACQUC0I == 1 |
        W8ACQUC0J == 1 |
        W8ACQUC0K == 1 |
        W8ACQUC0L == 1 |
        W8ACQUC0M == 1 |
        W8VCQUC0A == 1 |
        W8VCQUC0B == 1 |
        W8VCQUC0C == 1 |
        W8VCQUC0E == 1 |
        W8VCQUC0F == 1 |
        W8VCQUC0G == 1 |
        W8VCQUC0H == 1 |
        W8VCQUC0I == 1 |
        W8VCQUC0L == 1 |
        W8VCQUC0M == 1 |
        W8VCQUC0N == 1 ~ 1,
      W8VCQUC0D == 1 | W8VCQUC0P == 1 ~ 2,
      W8ACQUC0N == 1 |
        W8VCQUC0O == 1 ~ 3,
      W8ACQUC0O == 1 |
        W8ACQUC0P == 1 ~ 4,
      W8ACQUC0Q == 1 |
        W8VCQUC0R == 1 ~ -9,
      W8ACQUC0P == 1 |
        W8VCQUC0Q == 1 ~ -8,
      TRUE ~ -3
    ),

    # Sweep 9
    educaim32 = case_when(
      W9ECONACT2 == -91 ~ -8,
      W9ECONACT2 != 6 & W9ECONACT2 != 7 ~ 5, # not studying
      W9ACQUC0A == 1 |
        W9ACQUC0B == 1 |
        W9ACQUC0C == 1 |
        W9ACQUC0D == 1 |
        W9ACQUC0E == 1 |
        W9ACQUC0F == 1 |
        W9VCQUC0A == 1 |
        W9VCQUC0B == 1 |
        W9VCQUC0C == 1 |
        W9VCQUC0S == 1 |
        W9VCQUC0V == 1 |
        W9VCQUCAC == 1 ~ 0,
      W9ACQUC0G == 1 |
        W9ACQUC0H == 1 |
        W9ACQUC0I == 1 |
        W9ACQUC0J == 1 |
        W9ACQUC0K == 1 |
        W9ACQUC0L == 1 |
        W9ACQUC0M == 1 |
        W9ACQUC0O == 1 |
        W9ACQUC0P == 1 |
        W9ACQUC0Q == 1 |
        W9VCQUC0D == 1 |
        W9VCQUC0E == 1 |
        W9VCQUC0F == 1 |
        W9VCQUC0G == 1 |
        W9VCQUC0H == 1 |
        W9VCQUC0I == 1 |
        W9VCQUC0L == 1 |
        W9VCQUC0M == 1 |
        W9VCQUC0N == 1 |
        W9VCQUC0O == 1 |
        W9VCQUC0P == 1 |
        W9VCQUC0Q == 1 |
        W9VCQUC0R == 1 |
        W9VCQUC0T == 1 |
        W9VCQUC0U == 1 |
        W9VCQUC0W == 1 |
        W9VCQUC0X == 1 |
        W9VCQUC0Y == 1 |
        W9VCQUC0Z == 1 |
        W9VCQUCAA == 1 |
        W9VCQUCAB == 1 |
        W9VCQUCAD == 1 |
        W9VCQUCAE == 1 ~ 1,
      W9ACQUC0N == 1 ~ 2,
      W9ACQUC0R == 1 |
        W9VCQUCAF == 1 ~ 3,
      W9ACQUC0S == 1 |
        W9VCQUCAG == 1 ~ 4,
      W9ACQUC0T == 1 |
        W9VCQUCAH == 1 ~ -8,
      W9ACQUC0U == 1 |
        W9VCQUCAI == 1 ~ -9,
      TRUE ~ -3
    )
  ) %>%
  mutate(across(
    c(educaim17, educaim19, educaim20, educaim25, educaim32),
    ~ factor(
      .x,
      levels = c(0, 1, 2, 3, 4, 5, -1, -2, -3, -8, -9),
      labels = c(
        "NVQ 4-5",
        "NVQ 1-3",
        "None/entry",
        "Other",
        "None of these qualifications",
        "Not studying",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(educaim_all, output_data_path, row.names = FALSE)