library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# The join error suggests NSID might be read as numeric with precision issues or contain NAs
# leading to a cartesian product. We will load NSID as character.
files_vars <- list(
  'wave_one_lsype_young_person_2020.tab' = c('NSID'),
  'wave_four_lsype_young_person_2020.tab' = c('NSID'),
  'wave_six_lsype_young_person_2020.tab' = c('NSID', 'W6SexualityYP'),
  'wave_seven_lsype_young_person_2020.tab' = c('NSID', 'W7SexualityYP'),
  'ns8_2015_self_completion.tab' = c('NSID', 'W8SEXUALITY'),
  'ns9_2022_main_interview.tab' = c('NSID', 'W9SORI')
)

load_subset_data <- function(file, vars) {
  # Load NSID as character to ensure stable joining
  df <- read_delim(paste0('data/input/', file), delim = '\t', show_col_types = FALSE)
  # Ensure NSID is character and remove rows where NSID is NA to prevent join explosion
  df %>% 
    mutate(NSID = as.character(NSID)) %>%
    filter(!is.na(NSID)) %>%
    select(all_of(vars))
}

data_list <- map2(names(files_vars), files_vars, load_subset_data)

# Merge using full_join by NSID
merged_data <- data_list %>% reduce(full_join, by = 'NSID')

# Helper function to harmonize missing values and categories for sexuality
harmonize_sori <- function(val, wave_id) {
  if (is.na(val)) return(-3)
  
  if (wave_id == 'w6') {
    if (val == -97 || val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  } else if (wave_id == 'w7') {
    if (val == -100 || val == -97 || val == -92) return(-9)
    if (val == -91) return(-1)
    if (val == -1) return(-8)
  } else if (wave_id == 'w8') {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -1) return(-1)
  } else if (wave_id == 'w9') {
    if (val == -9) return(-9)
    if (val == -8) return(-8)
    if (val == -3) return(-3)
    if (val == -1) return(-1)
    if (val == 5) return(-7)
  }
  return(val)
}

# Vectorized harmonization
vectorize_sori <- function(vec, wave_id) {
  # Ensure input is numeric for the helper
  vec_num <- as.numeric(vec)
  sapply(vec_num, harmonize_sori, wave_id = wave_id)
}

merged_data <- merged_data %>%
  mutate(
    sori19 = vectorize_sori(W6SexualityYP, 'w6'),
    sori20 = vectorize_sori(W7SexualityYP, 'w7'),
    sori_w8 = vectorize_sori(W8SEXUALITY, 'w8'),
    sori32 = vectorize_sori(W9SORI, 'w9')
  )

# Define labels for the factor
sori_labels <- c(
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked"
)

apply_sori_factor <- function(x) {
  factor(x, levels = as.numeric(names(sori_labels)), labels = sori_labels)
}

merged_data <- merged_data %>%
  mutate(
    sori19 = apply_sori_factor(sori19),
    sori20 = apply_sori_factor(sori20),
    sori_w8 = apply_sori_factor(sori_w8),
    sori32 = apply_sori_factor(sori32)
  )

# Final selection
final_data <- merged_data %>%
  select(NSID, sori19, sori20, sori_w8, sori32)

write_csv(final_data, 'data/output/cleaned_data.csv')
