library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# The previous error (join overflow) suggests NSID columns might be read as numeric
# or contain unexpected values causing Cartesian products. We will read NSID as character
# and ensure we only join unique IDs.

wave_meta <- list(
  'wave_one_lsype_young_person_2020.tab' = c('NSID'),
  'wave_four_lsype_young_person_2020.tab' = c('NSID'),
  'wave_six_lsype_young_person_2020.tab' = c('NSID', 'W6SexualityYP'),
  'wave_seven_lsype_young_person_2020.tab' = c('NSID', 'W7SexualityYP'),
  'ns8_2015_self_completion.tab' = c('NSID', 'W8SEXUALITY'),
  'ns9_2022_main_interview.tab' = c('NSID', 'W9SORI')
)

load_subset <- function(filename, cols) {
  # Force NSID to character to avoid precision/joining issues
  # Use a custom col_types for NSID specifically
  df <- readr::read_delim(
    paste0('data/input/', filename),
    delim = '\t',
    col_types = readr::cols(NSID = readr::col_character(), .default = readr::col_double()),
    col_select = all_of(cols)
  )
  # Ensure unique NSIDs per file to prevent Cartesian product during join
  df %>% distinct(NSID, .keep_all = TRUE)
}

data_list <- map2(names(wave_meta), wave_meta, load_subset)

# Merge using full_join by NSID
merged_data <- data_list %>% reduce(full_join, by = 'NSID')

# Standard Missing-Value Codes Mapping Function
harmonize_missing <- function(x, wave_mapping = NULL) {
  # Handle Nulls first as -3
  x[is.na(x)] <- -3
  
  if (!is.null(wave_mapping)) {
    for (old_val in names(wave_mapping)) {
      x[x == as.numeric(old_val)] <- as.numeric(wave_mapping[[old_val]])
    }
  }
  return(x)
}

# Harmonization for 'sori'
map_w6 <- c("-97" = -9, "-92" = -9, "-91" = -1, "-1" = -8)
merged_data$sori19 <- harmonize_missing(merged_data$W6SexualityYP, map_w6)

map_w7 <- c("-100" = -7, "-97" = -9, "-92" = -9, "-91" = -1, "-1" = -8)
merged_data$sori20 <- harmonize_missing(merged_data$W7SexualityYP, map_w7)

map_w8 <- c("-9" = -9, "-8" = -8, "-1" = -1)
merged_data$sori_w8 <- harmonize_missing(merged_data$W8SEXUALITY, map_w8)

map_w9 <- c("-9" = -9, "-8" = -8, "-3" = -3, "-1" = -1, "5" = -7)
merged_data$sori32 <- harmonize_missing(merged_data$W9SORI, map_w9)

# Define labels for sori factor
sori_labels <- c(
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable"
)

make_sori_factor <- function(x) {
  factor(x, levels = as.numeric(names(sori_labels)), labels = sori_labels)
}

merged_data$sori19 <- make_sori_factor(merged_data$sori19)
merged_data$sori20 <- make_sori_factor(merged_data$sori20)
merged_data$sori_w8 <- make_sori_factor(merged_data$sori_w8)
merged_data$sori32 <- make_sori_factor(merged_data$sori32)

# Output derived variables
final_df <- merged_data %>%
  select(NSID, sori19, sori20, sori_w8, sori32)

write_csv(final_df, 'data/output/cleaned_data.csv')
