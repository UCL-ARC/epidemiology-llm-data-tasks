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

#  Merge all sweeps by NSID
parent_edu_all <- reduce(parent_edu_vars, full_join, by = "NSID")

# Recode missing values and response categories
parent_edu_all <- parent_edu_all %>%
  mutate(
    across(
      matches("educ(ma|pa)_S[1-4]"),
      ~ case_when(
        .x == -92 ~ -9,
        .x == -91 ~ -1,
        .x == -94 ~ -8,
        .x %in% c(-98,-99) ~ -3,
        .x == -999 ~ -2,
        TRUE ~ .x
      )
    )
  )

parent_edu_detailed_labels <- c(
  "Higher Degree" = 1L,
  "First degree" = 2L,
  "HE Diploma" = 3L,
  "HNC/HND/NVQ4" = 4L,
  "Teaching qualification, non-degree" = 5L,
  "Nursing qualification, non-degree" = 6L,
  "A Levels" = 7L,
  "OND/ONC" = 8L,
  "City and guilds part III, NVQ3" = 9L,
  "CSYS" = 10L,
  "Scottish Higher Grade" = 11L,
  "AS Level" = 12L,
  "Trade apprenticeship" = 13L,
  "City and guilds part II, NVQ2" = 14L,
  "GCSE grade A-C and equivalent" = 15L,
  "GCSE grade D-E and equivalent" = 16L,
  "City and guilds part I, NVQ1" = 17L,
  "Youth training, skill seekers" = 18L,
  "Qualification, level unspecified" = 19L,
  "No qualification mentioned" = 20L,
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/participated/interviewed" = -3L,
  "Don't know/insufficient information" = -8L,
  "Refusal" = -9L
)

parent_edu_simple_labels <- c(
  "NVQ 4-5" = 0L,
  "NVQ 1-3" = 1L,
  "None/entry" = 2L,
  "Other" = 3L,
  "None of these qualifications" = 4L,
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/participated/interviewed" = -3L,
  "Don't know/insufficient information" = -8L,
  "Refusal" = -9L
)

# Derive full education and transform to simple education
parent_edu_rec <- parent_edu_all %>%
  mutate(
    # mother full education (aggregate the information from sweeps 1-4)
    educdtlma = case_when(
      educma_S1 > 0 ~ educma_S1,
      educma_S2 > 0 ~ educma_S2,
      educma_S4 > 0 ~ educma_S4,
      educma_S1 < 0 ~ educma_S1,
      educma_S2 < 0 ~ educma_S2,
      educma_S4 < 0 ~ educma_S4,
      .default = -3 # Not interviewed / present
    ),
    # transform to 3-level education (mother)
    educma = case_when(
      educdtlma %in% 1:4 ~ 0,
      educdtlma %in% 5:17 ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3, # other
      educdtlma == 20 ~ 4, # none of these qualifications
      .default = educdtlma # keep negatives as-is
    ),
    # father full education (aggregate the information from sweeps 1-4)
    educdtlpa = case_when(
      educpa_S1 > 0 ~ educpa_S1,
      educpa_S2 > 0 ~ educpa_S2,
      educpa_S4 > 0 ~ educpa_S4,
      educpa_S1 < 0 ~ educpa_S1,
      educpa_S2 < 0 ~ educpa_S2,
      educpa_S4 < 0 ~ educpa_S4,
      .default = -3
    ),
    # transform to 3-level education (father)
    educpa = case_when(
      educdtlpa %in% 1:4 ~ 0,
      educdtlpa %in% 5:17 ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      .default = educdtlpa # keep negatives as-is
    )
  ) %>%
  mutate(
    across(
      c(educma, educpa),
      ~ labelled(
        .x,
        labels = parent_edu_simple_labels
      )
    ),
    across(
      c(educdtlma, educdtlpa),
      ~ labelled(
        .x,
        labels = parent_edu_detailed_labels
      )
    )
  )

parent_edu_all <- parent_edu_rec %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa) %>%
  mutate(
    across(
      c(educma, educpa, educdtlma, educdtlpa),
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(parent_edu_all, output_data_path, row.names = FALSE)
