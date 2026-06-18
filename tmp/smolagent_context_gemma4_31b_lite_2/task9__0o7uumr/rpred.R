library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file_w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
file_w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
file_w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'c'))

# Convert specific variables to numeric for processing
vars_to_num <- c('W1hiqualmum', 'W1hiqualdad', 'W2hiqualmum', 'W2hiqualdad', 'w4hiqualmum', 'w4hiqualdad')

file_w1 <- file_w1 %>% mutate(across(any_of(vars_to_num), as.numeric))
file_w2 <- file_w2 %>% mutate(across(any_of(vars_to_num), as.numeric))
file_w4 <- file_w4 %>% mutate(across(any_of(vars_to_num), as.numeric))

# Merge datasets
df <- file_w1 %>%
  full_join(file_w2, by = 'NSID') %>%
  full_join(file_w4, by = 'NSID')

# 2. Harmonisation Logic
# Standard missing-value codes:
# -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not applicable/loss, -1 = Item not applicable

map_missing <- function(x) {
  case_when(
    x == -999.0 ~ -2,
    x == -99.0 ~ -3,
    x == -98.0 ~ -3, # Parent not present usually means not interviewed
    x == -94.0 ~ -8,
    x == -92.0 ~ -9,
    x == -91.0 ~ -1,
    x == -1.0 ~ -8,  # Don't know
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Process Detailed Variables
# Mother's detailed
mu_det <- c('W1hiqualmum', 'W2hiqualmum', 'w4hiqualmum')
fa_det <- c('W1hiqualdad', 'W2hiqualdad', 'w4hiqualdad')

# Clean each wave
for(v in mu_det) df[[v]] <- map_missing(df[[v]])
for(v in fa_det) df[[v]] <- map_missing(df[[v]])

# Consolidation: Earliest-valid-first
consolidate_earliest <- function(vars, data) {
  res <- rep(NA, nrow(data))
  for(v in vars) {
    val <- data[[v]]
    # Valid substantive responses are >= 1
    valid_idx <- which(is.na(res) & val >= 1)
    res[valid_idx] <- val[valid_idx]
  }
  # Fill remaining with the first available missing code if no substantive response
  for(v in vars) {
    val <- data[[v]]
    fill_idx <- which(is.na(res) & !is.na(val))
    res[fill_idx] <- val[fill_idx]
  }
  res[is.na(res)] <- -3
  return(res)
}

df$educdtlma <- consolidate_earliest(mu_det, df)
df$educdtlpa <- consolidate_earliest(fa_det, df)

# 3. NVQ Harmonisation (5-level)
# Based on typical NVQ levels from detailed categories:
# Level 4+: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4 (1,2,3,4)
# Level 3: A Levels, OND/ONC, NVQ3, CSYS, Scottish Higher (7,8,9,10,11)
# Level 2: AS Level, Trade apprentice, NVQ2, GCSE A-C (12,13,14,15)
# Level 1: GCSE D-E, NVQ1, Youth training (16,17,18)
# Level 0: No qualification (20)
# Unspecified/Others (19, 5, 6) -> map to closest or a separate 'Other' but task asks for 5-level NVQ

map_to_nvq <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 1, # Level 4+
    x == 7 | x == 8 | x == 9 | x == 10 | x == 11 ~ 2, # Level 3
    x == 12 | x == 13 | x == 14 | x == 15 ~ 3, # Level 2
    x == 16 | x == 17 | x == 18 ~ 4, # Level 1
    x == 20 ~ 5, # No qual
    x == 19 | x == 5 | x == 6 ~ 5, # Treat unspecified/non-degree as no qual/low for 5-level
    x < 1 ~ x, # Preserve missing codes
    TRUE ~ 5
  )
}

df$educma <- map_to_nvq(df$educdtlma)
df$educpa <- map_to_nvq(df$educdtlpa)

# Apply labels
# Detailed labels (1-20)
det_labels <- c(
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree",
  "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3", "10" = "CSYS",
  "11" = "Scottish Higher Grade", "12" = "AS Level", "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2", "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent", "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers", "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned", "-9" = "Refusal", "-8" = "Don't know",
  "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Not applicable"
)

nvq_labels <- c(
  "1" = "Level 4+", "2" = "Level 3", "3" = "Level 2", "4" = "Level 1", "5" = "No qualification",
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked",
  "-2" = "Schedule not applicable", "-1" = "Not applicable"
)

df$educdtlma <- factor(df$educdtlma, levels = as.numeric(names(det_labels)), labels = det_labels)
df$educdtlpa <- factor(df$educdtlpa, levels = as.numeric(names(det_labels)), labels = det_labels)
df$educma <- factor(df$educma, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
df$educpa <- factor(df$educpa, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)

# Final selection
final_df <- df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_df, 'data/output/cleaned_data.csv')
