library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

# Load datasets
load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = 'c'))
}

# Use a list to store dataframes then merge
data_list <- map(files, load_data)

# Merge all datasets using full_join on NSID
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Helper function to map missing values based on the standard scheme
# -9: Refusal, -8: DK, -7: Prefer not to say, -3: Not asked, -2: Schedule/Script/Lost, -1: N/A
map_missing <- function(val, wave_labels) {
  # Convert value to numeric for comparison
  v <- as.numeric(val)
  if (is.na(v)) return(-3)
  
  # Mapping based on metadata labels provided in the prompt
  # This logic is applied per-wave since codes change
  return(v)
}

# Process sex variables for each wave
# Wave 1: W1sexYP
# -99: not interviewed (-3), -92: Refused (-9), -91: N/A (-1)
# Wave 2: W2SexYP
# -998: missed (-2), -997: script (-2), -995: missing history (-2), -99: not interviewed (-3), -92: Refused (-9), -91: N/A (-1), -1: DK (-8)
# Wave 3: W3sexYP
# -99: not interviewed (-3), -92: Refused (-9), -91: N/A (-1)
# Wave 4: W4SexYP
# -99: not interviewed (-3), -92: Refused (-9), -91: N/A (-1), -1: DK (-8)
# Wave 5: W5SexYP
# -1: DK (-8)
# Wave 6: W6Sex
# -92: Refused (-9), -91: N/A (-1)
# Wave 7: W7Sex
# -91: N/A (-1)
# Wave 8: W8CMSEX
# -9: Refused (-9), -8: DK (-8), -1: N/A (-1)
# Wave 9: W9DSEX
# (No missing codes provided in labels, just 1 and 2)

clean_sex <- function(x, wave) {
  v <- as.numeric(x)
  res <- v
  
  # Common mapping based on label meaning
  if (wave == 1) {
    res[v == -99] <- -3
    res[v == -92] <- -9
    res[v == -91] <- -1
  } else if (wave == 2) {
    res[v %in% c(-998, -997, -995)] <- -2
    res[v == -99] <- -3
    res[v == -92] <- -9
    res[v == -91] <- -1
    res[v == -1] <- -8
  } else if (wave == 3) {
    res[v == -99] <- -3
    res[v == -92] <- -9
    res[v == -91] <- -1
  } else if (wave == 4) {
    res[v == -99] <- -3
    res[v == -92] <- -9
    res[v == -91] <- -1
    res[v == -1] <- -8
  } else if (wave == 5) {
    res[v == -1] <- -8
  } else if (wave == 6) {
    res[v == -92] <- -9
    res[v == -91] <- -1
  } else if (wave == 7) {
    res[v == -91] <- -1
  } else if (wave == 8) {
    res[v == -9] <- -9
    res[v == -8] <- -8
    res[v == -1] <- -1
  } else if (wave == 9) {
    # No specific codes in metadata
  }
  
  # Final NA to -3 conversion
  res[is.na(res)] <- -3
  return(res)
}

# Create cleaned wave-specific sex variables
full_df <- full_df %>%
  mutate(
    s1 = clean_sex(W1sexYP, 1),
    s2 = clean_sex(W2SexYP, 2),
    s3 = clean_sex(W3sexYP, 3),
    s4 = clean_sex(W4SexYP, 4),
    s5 = clean_sex(W5SexYP, 5),
    s6 = clean_sex(W6Sex, 6),
    s7 = clean_sex(W7Sex, 7),
    s8 = clean_sex(W8CMSEX, 8),
    s9 = clean_sex(W9DSEX, 9)
  )

# Consolidate sex: Most recent valid first, then oldest to newest
# Valid values are 1 (Male) and 2 (Female)
consolidate_sex <- function(row) {
  # Sequence from most recent to oldest
  vals <- row[c('s9', 's8', 's7', 's6', 's5', 's4', 's3', 's2', 's1')]
  
  # 1. Look for first substantive response (1 or 2)
  for (v in vals) {
    if (!is.na(v) && v == 1) return(1)
    if (!is.na(v) && v == 2) return(2)
  }
  
  # 2. Fall back to most recent missing code if no substantive found
  for (v in vals) {
    if (!is.na(v)) return(v)
  }
  
  return(-3)
}

# Apply consolidation
full_df$sex <- apply(full_df[, c('s1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9')], 1, consolidate_sex)

# Create labels for the final sex variable
sex_labels <- c(
  '1' = 'Male',
  '2' = 'Female',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

full_df$sex <- factor(full_df$sex, levels = names(sex_labels), labels = sex_labels)

# Final selection: ID and consolidated sex
final_df <- full_df %>% select(NSID, sex)

# Write to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')