library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files and source variables
files_meta <- list(
  'wave_one_lsype_young_person_2020.tab' = 'W1sexYP',
  'wave_two_lsype_young_person_2020.tab' = 'W2SexYP',
  'wave_three_lsype_young_person_2020.tab' = 'W3sexYP',
  'wave_four_lsype_young_person_2020.tab' = 'W4SexYP',
  'wave_five_lsype_young_person_2020.tab' = 'W5SexYP',
  'wave_six_lsype_young_person_2020.tab' = 'W6Sex',
  'wave_seven_lsype_young_person_2020.tab' = 'W7Sex',
  'ns8_2015_main_interview.tab' = 'W8CMSEX',
  'ns9_2022_main_interview.tab' = 'W9DSEX'
)

# Load and merge datasets
all_data <- list()
for (filename in names(files_meta)) {
  path <- paste0('data/input/', filename)
  df <- readr::read_delim(path, delim = "\t", col_types = readr::cols(.default = 'c'))
  # Convert the sex variable to numeric for processing
  sex_var <- files_meta[[filename]]
  if (sex_var %in% names(df)) {
    df[[sex_var]] <- as.numeric(df[[sex_var]])
  }
  all_data[[filename]] <- df %>% select(NSID, all_of(sex_var))
}

# Merge all
final_df <- all_data[[1]]
for (i in 2:length(all_data)) {
  final_df <- full_join(final_df, all_data[[i]], by = 'NSID')
}

# Process each wave's sex variable to standard missing codes
# W1
final_df <- final_df %>% mutate(sex1 = case_when(
  W1sexYP == 1 ~ 1, W1sexYP == 2 ~ 2,
  W1sexYP == -92 ~ -9, W1sexYP == -91 ~ -1, W1sexYP == -99 ~ -3,
  TRUE ~ -3
))
# W2
final_df <- final_df %>% mutate(sex2 = case_when(
  W2SexYP == 1 ~ 1, W2SexYP == 2 ~ 2,
  W2SexYP == -92 ~ -9, W2SexYP == -91 ~ -1, W2SexYP == -1 ~ -8, W2SexYP == -99 ~ -3,
  W2SexYP %in% c(-998, -997, -995) ~ -2,
  TRUE ~ -3
))
# W3
final_df <- final_df %>% mutate(sex3 = case_when(
  W3sexYP == 1 ~ 1, W3sexYP == 2 ~ 2,
  W3sexYP == -92 ~ -9, W3sexYP == -91 ~ -1, W3sexYP == -99 ~ -3,
  TRUE ~ -3
))
# W4
final_df <- final_df %>% mutate(sex4 = case_when(
  W4SexYP == 1 ~ 1, W4SexYP == 2 ~ 2,
  W4SexYP == -92 ~ -9, W4SexYP == -91 ~ -1, W4SexYP == -1 ~ -8, W4SexYP == -99 ~ -3,
  TRUE ~ -3
))
# W5
final_df <- final_df %>% mutate(sex5 = case_when(
  W5SexYP == 1 ~ 1, W5SexYP == 2 ~ 2,
  W5SexYP == -1 ~ -8,
  TRUE ~ -3
))
# W6
final_df <- final_df %>% mutate(sex6 = case_when(
  W6Sex == 1 ~ 1, W6Sex == 2 ~ 2,
  W6Sex == -92 ~ -9, W6Sex == -91 ~ -1,
  TRUE ~ -3
))
# W7
final_df <- final_df %>% mutate(sex7 = case_when(
  W7Sex == 1 ~ 1, W7Sex == 2 ~ 2,
  W7Sex == -91 ~ -1,
  TRUE ~ -3
))
# W8
final_df <- final_df %>% mutate(sex8 = case_when(
  W8CMSEX == 1 ~ 1, W8CMSEX == 2 ~ 2,
  W8CMSEX == -9 ~ -9, W8CMSEX == -8 ~ -8, W8CMSEX == -1 ~ -1,
  TRUE ~ -3
))
# W9
final_df <- final_df %>% mutate(sex9 = case_when(
  W9DSEX == 1 ~ 1, W9DSEX == 2 ~ 2,
  TRUE ~ -3
))

# Consolidate sex: most-recent-valid-first
sex_cols <- c('sex9', 'sex8', 'sex7', 'sex6', 'sex5', 'sex4', 'sex3', 'sex2', 'sex1')

get_valid_sex <- function(row) {
  for (col in sex_cols) {
    val <- as.numeric(row[[col]])
    if (!is.na(val) && val > 0) return(val)
  }
  for (col in sex_cols) {
    val <- as.numeric(row[[col]])
    if (!is.na(val)) return(val)
  }
  return(-3)
}

final_df$sex_val <- apply(final_df[, sex_cols], 1, get_valid_sex)

# Define labels for the consolidated variable
sex_labels <- c("1" = "Male", "2" = "Female", "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Not applicable")

# Map numeric to factor using the labels
final_df$sex <- factor(final_df$sex_val, levels = as.numeric(names(sex_labels)), labels = sex_labels)

# Final selection
output_df <- final_df %>% select(NSID, sex)

# Write output
readr::write_csv(output_df, 'data/output/cleaned_data.csv')
