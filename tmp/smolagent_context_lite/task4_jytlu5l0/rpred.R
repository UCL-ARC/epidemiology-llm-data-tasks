library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  w1 = 'data/input/wave_one_lsype_young_person_2020.tab',
  w4 = 'data/input/wave_four_lsype_young_person_2020.tab',
  w6 = 'data/input/wave_six_lsype_young_person_2020.tab',
  w7 = 'data/input/wave_seven_lsype_young_person_2020.tab',
  w8 = 'data/input/ns8_2015_self_completion.tab',
  w9 = 'data/input/ns9_2022_main_interview.tab'
)

load_data <- function(path) {
  read_delim(path, delim = '\t', col_types = cols(.default = 'c'))
}

# Load all files explicitly
data_w1 <- load_data(files$w1)
data_w4 <- load_data(files$w4)
data_w6 <- load_data(files$w6)
data_w7 <- load_data(files$w7)
data_w8 <- load_data(files$w8)
data_w9 <- load_data(files$w9)

# Merge datasets using full_join by NSID
full_df <- data_w1 %>%
  full_join(data_w4, by = 'NSID') %>%
  full_join(data_w6, by = 'NSID') %>%
  full_join(data_w7, by = 'NSID') %>%
  full_join(data_w8, by = 'NSID') %>%
  full_join(data_w9, by = 'NSID')

# Helper function for missing value mapping
# -9 Refusal, -8 DK, -7 Prefer not to say, -3 Not asked, -2 Schedule/Lost, -1 Not applicable
map_missing <- function(val, mapping) {
  if (is.na(val)) return(-3)
  val_num <- as.numeric(val)
  if (is.na(val_num)) return(-3)
  if (val_num %in% names(mapping)) {
    return(as.numeric(mapping[[as.character(val_num)]]))
  }
  return(val_num)
}

# Define value labels for the harmonised variables
sori_labels <- c(
  '1' = 'Heterosexual / Straight',
  '2' = 'Gay / Lesbian',
  '3' = 'Bisexual',
  '4' = 'Other',
  '-9' = 'Refusal',
  '-8' = "Don't know",
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Process W6 (Age 19)
# W6SexualityYP: -97 Refused self comp, -92 Refused, -91 Not app, -1 DK
full_df <- full_df %>%
  mutate(sori19 = case_when(
    W6SexualityYP == '1' ~ 1,
    W6SexualityYP == '2' ~ 2,
    W6SexualityYP == '3' ~ 3,
    W6SexualityYP == '4' ~ 4,
    W6SexualityYP == '-92' ~ -9,
    W6SexualityYP == '-1' ~ -8,
    W6SexualityYP == '-91' ~ -1,
    W6SexualityYP == '-97' ~ -9, # Refused self completion treated as refusal
    TRUE ~ -3
  ))

# Process W7 (Age 20)
# W7SexualityYP: -100 declined exp, -97 refused self comp, -92 Refused, -91 Not app, -1 DK
full_df <- full_df %>%
  mutate(sori20 = case_when(
    W7SexualityYP == '1' ~ 1,
    W7SexualityYP == '2' ~ 2,
    W7SexualityYP == '3' ~ 3,
    W7SexualityYP == '4' ~ 4,
    W7SexualityYP == '-92' ~ -9,
    W7SexualityYP == '-1' ~ -8,
    W7SexualityYP == '-91' ~ -1,
    W7SexualityYP == '-97' ~ -9,
    W7SexualityYP == '-100' ~ -2, # declined experience questions -> schedule not app/info lost
    TRUE ~ -3
  ))

# Process W8 (Age 25)
# W8SEXUALITY: -9 Refused, -8 DK, -1 Not app
full_df <- full_df %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == '1' ~ 1,
    W8SEXUALITY == '2' ~ 2,
    W8SEXUALITY == '3' ~ 3,
    W8SEXUALITY == '4' ~ 4,
    W8SEXUALITY == '-9' ~ -9,
    W8SEXUALITY == '-8' ~ -8,
    W8SEXUALITY == '-1' ~ -1,
    TRUE ~ -3
  ))

# Process W9 (Age 32)
# W9SORI: -9 Refused, -8 DK, -3 Not asked, -1 Not app, 5 Prefer not to say
full_df <- full_df %>%
  mutate(sori32 = case_when(
    W9SORI == '1' ~ 1,
    W9SORI == '2' ~ 2,
    W9SORI == '3' ~ 3,
    W9SORI == '4' ~ 4,
    W9SORI == '5' ~ -7,
    W9SORI == '-9' ~ -9,
    W9SORI == '-8' ~ -8,
    W9SORI == '-3' ~ -3,
    W9SORI == '-1' ~ -1,
    TRUE ~ -3
  ))

# Apply factor labels
final_vars <- c('sori19', 'sori20', 'sori25', 'sori32')
full_df <- full_df %>%
  mutate(across(all_of(final_vars), ~ { 
    v <- .x
    # Convert to factor with defined levels
    factor(v, levels = as.numeric(names(sori_labels)), labels = sori_labels)
  }))

# Final selection
output_df <- full_df %>%
  select(NSID, all_of(final_vars))

# Write to CSV
write_csv(output_df, 'data/output/cleaned_data.csv')
