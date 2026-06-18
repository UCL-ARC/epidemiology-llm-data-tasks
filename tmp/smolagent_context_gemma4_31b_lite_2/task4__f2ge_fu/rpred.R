library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols(), show_col_types = FALSE)

# Merge datasets
full_frame <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave6, by = 'NSID') %>%
  full_join(wave7, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

# Function to harmonise missing values based on provided mapping
harmonise_missing <- function(x, mapping = list()) {
  # Start with NA -> -3
  res <- as.numeric(x)
  
  # Apply specific label-based mappings provided in metadata
  for (val in names(mapping)) {
    res[x == as.numeric(val)] <- mapping[[val]]
  }
  
  # Final NA check
  res[is.na(res)] <- -3
  return(res)
}

# Wave 6 (Age 19)
# Labels: -97: Respondent declined self completion, -92: Refused, -91: Not applicable, -1: Don't know
sori19_raw <- full_frame$W6SexualityYP
sori19 <- harmonise_missing(sori19_raw, list(
  '-97' = -2, # declined self completion (info lost/script error)
  '-92' = -9, # Refused
  '-91' = -1, # Not applicable
  '-1' = -8   # Don't know
))

# Wave 7 (Age 20)
# Labels: -100: Respondent declined sexual experience questions, -97: Refused self completion, -92: Refused, -91: Not applicable, -1: Don't know
sori20_raw <- full_frame$W7SexualityYP
sori20 <- harmonise_missing(sori20_raw, list(
  '-100' = -2, # declined sexual experience questions
  '-97' = -2,  # Refused self completion
  '-92' = -9,  # Refused
  '-91' = -1,  # Not applicable
  '-1' = -8    # Don't know
))

# Wave 8 (Age 25)
# Labels: -9: Refused, -8: Don't know, -1: Not applicable
sori25_raw <- full_frame$W8SEXUALITY
sori25 <- harmonise_missing(sori25_raw, list(
  '-9' = -9, # Refused
  '-8' = -8, # Don't know
  '-1' = -1  # Not applicable
))

# Wave 9 (Age 32)
# Labels: -9: Refused, -8: Don't know, -3: Not asked, -1: Not applicable, 5: Prefer not to say
sori32_raw <- full_frame$W9SORI
sori32 <- harmonise_missing(sori32_raw, list(
  '-9' = -9, # Refused
  '-8' = -8, # Don't know
  '-3' = -3, # Not asked
  '-1' = -1, # Not applicable
  '5' = -7   # Prefer not to say
))

# Create final dataframe
final_df <- data.frame(
  NSID = full_frame$NSID,
  sori19 = sori19,
  sori20 = sori20,
  sori25 = sori25,
  sori32 = sori32
)

# Apply labels for categories and missing values
val_labels <- c(
  '1' = 'Heterosexual / Straight',
  '2' = 'Gay / Lesbian',
  '3' = 'Bisexual',
  '4' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Convert to factors with labels
apply_labels <- function(vec) {
  # Find which labels from val_labels are actually present in the data
  present_labels <- val_labels[as.character(vec) %in% names(val_labels)]
  # To maintain order of metadata (1, 2, 3, 4 then missing codes), we sort the levels
  levels_order <- c('1', '2', '3', '4', '-9', '-8', '-7', '-3', '-2', '-1')
  levels_order <- levels_order[levels_order %in% as.character(vec)]
  
  f <- factor(vec, levels = levels_order, labels = val_labels[levels_order])
  return(f)
}

final_df$sori19 <- apply_labels(final_df$sori19)
final_df$sori20 <- apply_labels(final_df$sori20)
final_df$sori25 <- apply_labels(final_df$sori25)
final_df$sori32 <- apply_labels(final_df$sori32)

write_csv(final_df, 'data/output/cleaned_data.csv')