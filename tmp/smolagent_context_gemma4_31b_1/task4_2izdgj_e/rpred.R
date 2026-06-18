library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File mapping
files <- list(
  w1 = 'wave_one_lsype_young_person_2020.tab',
  w4 = 'wave_four_lsype_young_person_2020.tab',
  w6 = 'wave_six_lsype_young_person_2020.tab',
  w7 = 'wave_seven_lsype_young_person_2020.tab',
  w8 = 'ns8_2015_self_completion.tab',
  w9 = 'ns9_2022_main_interview.tab'
)

# Load datasets
load_data <- function(filename) {
  readr::read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols())
}

df1 <- load_data(files$w1)
df4 <- load_data(files$w4)
df6 <- load_data(files$w6)
df7 <- load_data(files$w7)
df8 <- load_data(files$w8)
df9 <- load_data(files$w9)

# Merge datasets
cohort <- df1 %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Process sori19 (from W6SexualityYP)
cohort <- cohort %>%
  mutate(sori19 = case_when(
    W6SexualityYP == 1 ~ 1,
    W6SexualityYP == 2 ~ 2,
    W6SexualityYP == 3 ~ 3,
    W6SexualityYP == 4 ~ 4,
    W6SexualityYP == -97 ~ -9,
    W6SexualityYP == -92 ~ -9,
    W6SexualityYP == -91 ~ -1,
    W6SexualityYP == -1 ~ -8,
    is.na(W6SexualityYP) ~ -3,
    TRUE ~ -3
  ))

# Process sori20 (from W7SexualityYP)
cohort <- cohort %>%
  mutate(sori20 = case_when(
    W7SexualityYP == 1 ~ 1,
    W7SexualityYP == 2 ~ 2,
    W7SexualityYP == 3 ~ 3,
    W7SexualityYP == 4 ~ 4,
    W7SexualityYP == -100 ~ -9,
    W7SexualityYP == -97 ~ -9,
    W7SexualityYP == -92 ~ -9,
    W7SexualityYP == -91 ~ -1,
    W7SexualityYP == -1 ~ -8,
    is.na(W7SexualityYP) ~ -3,
    TRUE ~ -3
  ))

# Process sori25 (from W8SEXUALITY)
cohort <- cohort %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == 1 ~ 1,
    W8SEXUALITY == 2 ~ 2,
    W8SEXUALITY == 3 ~ 3,
    W8SEXUALITY == 4 ~ 4,
    W8SEXUALITY == -9 ~ -9,
    W8SEXUALITY == -8 ~ -8,
    W8SEXUALITY == -1 ~ -1,
    is.na(W8SEXUALITY) ~ -3,
    TRUE ~ -3
  ))

# Process sori32 (from W9SORI)
cohort <- cohort %>%
  mutate(sori32 = case_when(
    W9SORI == 1 ~ 1,
    W9SORI == 2 ~ 2,
    W9SORI == 3 ~ 3,
    W9SORI == 4 ~ 4,
    W9SORI == 5 ~ -7,
    W9SORI == -9 ~ -9,
    W9SORI == -8 ~ -8,
    W9SORI == -3 ~ -3,
    W9SORI == -1 ~ -1,
    is.na(W9SORI) ~ -3,
    TRUE ~ -3
  ))

# Factor labels for all sori variables
sori_labels <- c(
  "1" = "Heterosexual/straight",
  "2" = "Gay/lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply labels as factors
apply_sori_labels <- function(x) {
  f <- factor(x, levels = as.numeric(names(sori_labels)), labels = sori_labels)
  return(f)
}

cohort <- cohort %>%
  mutate(across(c(sori19, sori20, sori25, sori32), apply_sori_labels))

# Select final columns
final_data <- cohort %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')
