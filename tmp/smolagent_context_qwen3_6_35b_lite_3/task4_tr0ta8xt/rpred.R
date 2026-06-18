library(dplyr)
library(readr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

# Load all files from metadata
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
w8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', show_col_types = FALSE)
w9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Merge all files by NSID using full_join
merged <- w1 %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w6, by = 'NSID') %>%
  full_join(w7, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9, by = 'NSID')

# Create harmonized sexual orientation variables with standard missing-value codes
# Standard codes: -9=Refusal, -8=Don't know, -7=Prefer not to say,
#                -3=Not asked, -2=Schedule not applicable, -1=Not applicable

# sori19 (Wave 6, Age 19) - from W6SexualityYP
merged <- merged %>%
  mutate(sori19 = case_when(
    W6SexualityYP == 1 ~ 1,
    W6SexualityYP == 2 ~ 2,
    W6SexualityYP == 3 ~ 3,
    W6SexualityYP == 4 ~ 4,
    W6SexualityYP == -97 ~ -2,   # Respondent declined self completion -> schedule not applicable
    W6SexualityYP == -92 ~ -9,   # Refused
    W6SexualityYP == -91 ~ -1,   # Not applicable
    W6SexualityYP == -1 ~ -8,    # Don't know
    is.na(W6SexualityYP) ~ -3,   # Not asked / not interviewed
    TRUE ~ -3
  ))

# sori20 (Wave 7, Age 20) - from W7SexualityYP
merged <- merged %>%
  mutate(sori20 = case_when(
    W7SexualityYP == 1 ~ 1,
    W7SexualityYP == 2 ~ 2,
    W7SexualityYP == 3 ~ 3,
    W7SexualityYP == 4 ~ 4,
    W7SexualityYP == -100 ~ -2,  # Respondent declined sexual experience questions -> schedule not applicable
    W7SexualityYP == -97 ~ -9,   # Refused self completion -> Refused
    W7SexualityYP == -92 ~ -9,   # Refused
    W7SexualityYP == -91 ~ -1,   # Not applicable
    W7SexualityYP == -1 ~ -8,    # Don't know
    is.na(W7SexualityYP) ~ -3,   # Not asked / not interviewed
    TRUE ~ -3
  ))

# sori25 (Wave 8, Age 25) - from W8SEXUALITY (already has standard codes)
merged <- merged %>%
  mutate(sori25 = case_when(
    W8SEXUALITY == 1 ~ 1,
    W8SEXUALITY == 2 ~ 2,
    W8SEXUALITY == 3 ~ 3,
    W8SEXUALITY == 4 ~ 4,
    W8SEXUALITY == -9 ~ -9,      # Refused
    W8SEXUALITY == -8 ~ -8,      # Don't know
    W8SEXUALITY == -1 ~ -1,      # Not applicable
    is.na(W8SEXUALITY) ~ -3,     # Not asked / not interviewed
    TRUE ~ -3
  ))

# sori32 (Wave 9, Age 32) - from W9SORI
merged <- merged %>%
  mutate(sori32 = case_when(
    W9SORI == 1 ~ 1,
    W9SORI == 2 ~ 2,
    W9SORI == 3 ~ 3,
    W9SORI == 4 ~ 4,
    W9SORI == 5 ~ -7,            # Prefer not to say -> -7
    W9SORI == -9 ~ -9,           # Refused
    W9SORI == -8 ~ -8,           # Don't know
    W9SORI == -3 ~ -3,           # Not asked at fieldwork stage
    W9SORI == -1 ~ -1,           # Not applicable
    is.na(W9SORI) ~ -3,          # Not asked / not interviewed
    TRUE ~ -3
  ))

# Define value labels for all variables
labels_valid <- c('Heterosexual/Straight' = 1L, 'Gay/Lesbian' = 2L, 'Bisexual' = 3L, 'Other' = 4L)
labels_missing <- c('Not applicable' = -1L, 'Schedule not applicable' = -2L, 
                    'Not asked at fieldwork stage' = -3L, 'Prefer not to say' = -7L,
                    "Don't know" = -8L, 'Refused' = -9L)
all_labels <- c(labels_valid, labels_missing)

# Apply labels to each derived variable
for (var_name in c('sori19', 'sori20', 'sori25', 'sori32')) {
  merged[[var_name]] <- haven::labelled(merged[[var_name]], labels = all_labels)
}

# Select only NSID and final derived variables
output <- merged %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully.\n')
cat('Number of rows:', nrow(output), '\n')
cat('Columns:', paste(names(output), collapse = ', '), '\n')

# Print summary
str(output)