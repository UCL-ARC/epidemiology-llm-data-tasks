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

# Read required datasets only
S1yp <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types=FALSE)
S2yp <- read_delim(file.path(data_path, "wave_two_lsype_young_person_2020.tab"),show_col_types=FALSE)
S3yp <- read_delim(file.path(data_path, "wave_three_lsype_young_person_2020.tab"),show_col_types=FALSE)
S4yp <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"),show_col_types=FALSE)
S5yp <- read_delim(file.path(data_path, "wave_five_lsype_young_person_2020.tab"),show_col_types=FALSE)
S6yp <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"),show_col_types=FALSE)
S7yp <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"),show_col_types=FALSE)
S8mi <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"),show_col_types=FALSE)
S9mi <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"),show_col_types=FALSE)

sex_vars <- list(
  S1 = S1yp %>% select(NSID, sex_S1 = W1sexYP),
  S2 = S2yp %>% select(NSID, sex_S2 = W2SexYP),
  S3 = S3yp %>% select(NSID, sex_S3 = W3sexYP),
  S4 = S4yp %>% select(NSID, W4Boost, sex_S4 = W4SexYP),
  S5 = S5yp %>% select(NSID, sex_S5 = W5SexYP),
  S6 = S6yp %>% select(NSID, sex_S6 = W6Sex),
  S7 = S7yp %>% select(NSID, sex_S7 = W7Sex),
  S8 = S8mi %>% select(NSID, sex_S8 = W8CMSEX),
  S9 = S9mi %>% select(NSID, sex_S9 = W9DSEX)
)

sex_all <- reduce(sex_vars, full_join, by = "NSID")

sex_all <- sex_all %>%
  mutate(across(
    paste0("sex_S", 1:7),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      TRUE ~ .x
    )
  )) %>%
  mutate(
    sex_final_main = case_when(
      !is.na(sex_S9) & sex_S9 > 0 ~ sex_S9,
      !is.na(sex_S1) & sex_S1 > 0 ~ sex_S1,
      !is.na(sex_S2) & sex_S2 > 0 ~ sex_S2,
      !is.na(sex_S3) & sex_S3 > 0 ~ sex_S3,
      !is.na(sex_S4) & sex_S4 > 0 ~ sex_S4,
      !is.na(sex_S5) & sex_S5 > 0 ~ sex_S5,
      !is.na(sex_S6) & sex_S6 > 0 ~ sex_S6,
      !is.na(sex_S7) & sex_S7 > 0 ~ sex_S7,
      !is.na(sex_S8) & sex_S8 > 0 ~ sex_S8,
      TRUE ~ NA_real_
    ),
    sex_final = case_when(
      !is.na(sex_final_main) ~ sex_final_main,
      !is.na(sex_S1) ~ sex_S1,
      !is.na(sex_S2) ~ sex_S2,
      !is.na(sex_S3) ~ sex_S3,
      !is.na(sex_S4) ~ sex_S4,
      !is.na(sex_S5) ~ sex_S5,
      !is.na(sex_S6) ~ sex_S6,
      !is.na(sex_S7) ~ sex_S7,
      !is.na(sex_S8) ~ sex_S8,
      TRUE ~ NA_real_
    ),
    sex = case_when(
      sex_final == 1 ~ 0,
      sex_final == 2 ~ 1,
      TRUE ~ sex_final
    ),
    sex = factor(
      sex,
      levels = c(0, 1, -1, -3, -9),
      labels = c(
        "male",
        "female",
        "Item not applicable",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, sex)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(sex_all, output_data_path, row.names = FALSE)