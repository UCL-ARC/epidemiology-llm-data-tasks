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
S1family <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE)
S2family <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE)
S4family <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE)

parent_edu_vars <- list(
  S1 = S1family %>% select(NSID, educma_S1 = W1hiqualmum, educpa_S1 = W1hiqualdad),
  S2 = S2family %>% select(NSID, educma_S2 = W2hiqualmum, educpa_S2 = W2hiqualdad),
  S4 = S4family %>% select(NSID, educma_S4 = w4hiqualmum, educpa_S4 = w4hiqualdad)
)

parent_edu_all <- reduce(parent_edu_vars, full_join, by = "NSID")

# Step 4: Derive full education and transform to simple education
parent_edu_all <- parent_edu_all %>%
  mutate(
    #mother full education (aggregate the information from sweeps 1-4)
    educdtlma = case_when(
      !is.na(educma_S4) & educma_S4 > 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 > 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 > 0 ~ educma_S1,
      !is.na(educma_S4) & educma_S4 < 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 < 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 < 0 ~ educma_S1,
      TRUE ~ -3 # Not interviewed / present
    ),
    #transform to 3-level education (mother)
    educma = case_when(
      educdtlma %in% 1:4 ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3, # other
      educdtlma == 20 ~ 4, # none of these qualifications
      TRUE ~ educdtlma # keep negatives as-is
    ),
    #father full education (aggregate the information from sweeps 1-4)
    educdtlpa = case_when(
      !is.na(educpa_S1) & educpa_S1 > 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 > 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 > 0 ~ educpa_S4,
      !is.na(educpa_S1) & educpa_S1 < 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 < 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 < 0 ~ educpa_S4,
      TRUE ~ -3
    ),
    #transform to 3-level education (father)
    educpa = case_when(
      educdtlpa %in% 1:4 ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa # keep negatives as-is
    )
  ) %>%
  mutate(
    across(
      c(educma, educpa),
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
    across(
      c(educdtlma, educdtlpa),
      ~ factor(
        .x,
        levels = c(1:20, -1, -2, -3, -8, -9),
        labels = c(
          "Higher Degree",
          "First degree",
          "HE Diploma",
          "HNC/HND/NVQ4",
          "Teaching qualification, non-degree",
          "Nursing qualification, non-degree",
          "A Levels",
          "OND/ONC",
          "City and guilds part III, NVQ3",
          "CSYS",
          "Scottish Higher Grade",
          "AS Level",
          "Trade apprenticeship",
          "City and guilds part II, NVQ2",
          "GCSE grade A-C and equivalent",
          "GCSE grade D-E and equivalent",
          "City and guilds part I, NVQ1",
          "Youth training, skill seekers",
          "Qualification, level unspecified",
          "No qualification mentioned",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(parent_edu_all, output_data_path, row.names = FALSE)
