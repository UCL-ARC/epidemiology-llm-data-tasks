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

# Load required datasets
S1youngperson <- read_delim(file.path(data_path, "wave_one_lsype_young_person_2020.tab"),show_col_types = FALSE)
S4youngperson <- read_delim(file.path(data_path, "wave_four_lsype_young_person_2020.tab"),show_col_types = FALSE)
S8main <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"), show_col_types = FALSE)
S8derived <- read_delim(file.path(data_path, "ns8_2015_derived.tab"), show_col_types = FALSE)
S9main <- read_delim(file.path(data_path, "ns9_2022_main_interview.tab"), show_col_types = FALSE)
S9derived <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE)

educ_vars <- list(
  S1 = S1youngperson %>% select(NSID),
  S4 = S4youngperson %>% select(NSID),
  S8dv = S8derived %>% select(NSID, W8DHANVQH),
  S8 = S8main %>% select(NSID, starts_with("W8VCQU")),
  S9dv = S9derived %>% select(NSID, W9DANVQH, W9DVNVQH),
  S9 = S9main %>% select(NSID, starts_with("W9ACQU"), starts_with("W9VCQU"))
)

# Education Own --------------------------------------------------------------------
# Merge by ID
educ_all <- reduce(educ_vars, full_join, by = "NSID")

# recode missing valuse and response categories
educ_all <- educ_all %>%
  # Sweep 8
  mutate(
    educ25 = case_when(
      W8DHANVQH %in% c(4, 5) ~ 0,
      W8VCQU0J == 1 | W8VCQU0K == 1 ~ 0,
      W8DHANVQH %in% c(1, 2, 3) ~ 1,
      W8VCQU0A == 1 |
        W8VCQU0B == 1 |
        W8VCQU0C == 1 |
        W8VCQU0E == 1 |
        W8VCQU0F == 1 |
        W8VCQU0G == 1 |
        W8VCQU0H == 1 |
        W8VCQU0I == 1 |
        W8VCQU0L == 1 |
        W8VCQU0M == 1 |
        W8VCQU0N == 1 ~ 1,
      W8VCQU0D == 1 | W8VCQU0P == 1 ~ 2,
      W8DHANVQH == 95 ~ 3,
      W8VCQU0O == 1 ~ 3,
      W8DHANVQH == 96 ~ 4,
      W8DHANVQH < 0 ~ W8DHANVQH,
      W8VCQU0R == 1 ~ -9,
      W8VCQU0Q == 1 ~ -8,
      TRUE ~ -3
    ),

    # Sweep 9
    educ32 = case_when(
      W9DANVQH %in% c(4, 5) | W9DVNVQH %in% c(4, 5) ~ 0,
      W9DANVQH %in% c(1, 2, 3) | W9DVNVQH %in% c(1, 2, 3) ~ 1,
      W9DANVQH == 0 | W9DVNVQH == 0 ~ 2,
      W9DANVQH == 95 | W9DVNVQH == 95 ~ 3,
      W9DANVQH == 96 | W9DVNVQH == 96 ~ 4,
      W9DANVQH < 0 ~ W9DANVQH,
      W9DVNVQH < 0 ~ W9DVNVQH,
      TRUE ~ -3
    ),
    educadtl32 = case_when(
      W9ACQU0A == 1 ~ 1,
      W9ACQU0B == 1 ~ 2,
      W9ACQU0C == 1 ~ 3,
      W9ACQU0D == 1 ~ 4,
      W9ACQU0E == 1 ~ 5,
      W9ACQU0F == 1 ~ 6,
      W9ACQU0G == 1 ~ 7,
      W9ACQU0H == 1 ~ 8,
      W9ACQU0I == 1 ~ 9,
      W9ACQU0J == 1 ~ 10,
      W9ACQU0K == 1 ~ 11,
      W9ACQU0L == 1 ~ 12,
      W9ACQU0M == 1 ~ 13,
      W9ACQU0N == 1 ~ 14,
      W9ACQU0O == 1 ~ 15,
      W9ACQU0P == 1 ~ 16,
      W9ACQU0Q == 1 ~ 17,
      W9ACQU0R == 1 ~ 18,
      W9ACQU0S == 1 ~ 19,
      W9ACQU0T == 1 ~ -8,
      W9ACQU0U == 1 ~ -9,
      W9ACQU0V == 1 ~ -2,
      if_all(starts_with("W9ACQU0"), ~ .x == -1) ~ -1,
      if_all(starts_with("W9ACQU0"), ~ .x == 2) ~ 19,
      if_all(starts_with("W9ACQU0"), ~ is.na(.x) | .x == -3) ~ -3
    ),
    educvdtl32 = case_when(
      W9VCQU0A == 1 ~ 1,
      W9VCQU0B == 1 ~ 2,
      W9VCQU0C == 1 ~ 3,
      W9VCQU0D == 1 ~ 4,
      W9VCQU0E == 1 ~ 5,
      W9VCQU0F == 1 ~ 6,
      W9VCQU0G == 1 ~ 7,
      W9VCQU0H == 1 ~ 8,
      W9VCQU0I == 1 ~ 9,
      W9VCQU0J == 1 ~ 10,
      W9VCQU0K == 1 ~ 11,
      W9VCQU0L == 1 ~ 12,
      W9VCQU0M == 1 ~ 13,
      W9VCQU0N == 1 ~ 14,
      W9VCQU0O == 1 ~ 15,
      W9VCQU0P == 1 ~ 16,
      W9VCQU0Q == 1 ~ 17,
      W9VCQU0R == 1 ~ 18,
      W9VCQU0S == 1 ~ 19,
      W9VCQU0T == 1 ~ 20,
      W9VCQU0U == 1 ~ 21,
      W9VCQU0V == 1 ~ 22,
      W9VCQU0W == 1 ~ 23,
      W9VCQU0X == 1 ~ 24,
      W9VCQU0Y == 1 ~ 25,
      W9VCQU0Z == 1 ~ 26,
      W9VCQUAA == 1 ~ 27,
      W9VCQUAB == 1 ~ 28,
      W9VCQUAC == 1 ~ 29,
      W9VCQUAD == 1 ~ 30,
      W9VCQUAE == 1 ~ 31,
      W9VCQUAF == 1 ~ 32,
      W9VCQUAG == 1 ~ 33,
      W9VCQUAH == 1 ~ -8,
      W9VCQUAI == 1 ~ -9,
      if_all(starts_with("W9VCQU"), ~ .x == -1) ~ -1,
      if_all(starts_with("W9VCQU"), ~ .x == 2) ~ 33,
      if_all(starts_with("W9VCQU"), ~ is.na(.x) | .x == -3) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(educ25, educ32),
      ~ factor(
        .x,
        levels = c(0, 1, 2, 3, 4, -1, -2, -3, -8, -9),
        labels = c(
          "NVQ 4-5",
          "NVQ 1-3",
          "None/entry",
          "Other",
          "None of these qualifications",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    educadtl32 = factor(
      educadtl32,
      levels = c(1:19, -1, -2, -3, -8, -9),
      labels = c(
        "Doctorate or equivalent",
        "Masters or equivalent",
        "Undergraduate or equivalent",
        "Post-graduate Diplomas and Certificates",
        "Diplomas in higher education and other higher education qualifications",
        "Teaching qualifications for schools or further education",
        "A/AS Levels or equivalent",
        "GCSE - Grade A-C, Level 4-9",
        "GCSE - Grade D-G, Level 1-3",
        "Scottish Qualifications - SCE Higher",
        "Scottish Qualifications - Scottish Certificate Sixth Year Studies",
        "Scottish Qualifications - SCE Standard",
        "Scottish Qualifications - National 4 and 5",
        "Scottish Qualifications - National 2 and 3",
        "Irish Qualifications - Leaving Certificate",
        "Irish Qualifications - Junior Certificate grade A-C",
        "Irish Qualifications - Junior Certificate grade D and below",
        "Other academic qualifications (including overseas)",
        "None of these qualifications",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    ),
    educvdtl32 = factor(
      educvdtl32,
      levels = c(1:33, -1, -2, -3, -8, -9),
      labels = c(
        "Professional qualifications at degree level",
        "Nursing or other medical qualifications",
        "NVQ or SVQ - Level 4 or 5",
        "NVQ or SVQ -  Level 3",
        "NVQ or SVQ - Level 2",
        "NVQ or SVQ - Level 1",
        "GNVQ - GNVQ Advanced",
        "GNVQ - GNVQ Intermediate",
        "GNVQ - Level 3",
        "GNVQ - Level 2",
        "GNVQ - Level Foundation",
        "City & Guilds - advanced craft, Part III",
        "City & Guilds - craft, Part II",
        "City & Guilds - craft, Part I",
        "City & Guilds - Level 3",
        "City & Guilds - Level 2",
        "City & Guilds - Level 1",
        "RSA - Advanced Diploma",
        "RSA - Higher Diploma",
        "RSA - Diploma",
        "RSA - RSA Stage I, II,III",
        "BTEC - Higher Level BTEC",
        "BTEC - BTEC National",
        "BTEC - BTEC First",
        "SCOTVEC-  SCOTVEC National Certificate",
        "SCOTVEC-  SCOTVEC first or general diploma",
        "SCOTVEC - SCOTVEC general diploma",
        "SCOTVEC - SCOTVEC modules",
        "HND or HNC",
        "OND or ONCM",
        "Junior certificate",
        "Other vocational qualifications (including some overseas)",
        "None of these qualifications",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  ) %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)


# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(educ_all, output_data_path, row.names = FALSE)