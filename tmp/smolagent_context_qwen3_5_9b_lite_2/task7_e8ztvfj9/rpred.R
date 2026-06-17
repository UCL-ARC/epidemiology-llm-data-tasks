library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
data <- full_join(wave1, wave4, by = 'NSID')
data <- full_join(data, wave6, by = 'NSID')
data <- full_join(data, wave7, by = 'NSID')
data <- full_join(data, wave8, by = 'NSID')
data <- full_join(data, wave9, by = 'NSID')

# Define 6-category NVQ scheme
category_labels <- c('NVQ 5 / First Degree', 'NVQ 4', 'NVQ 3', 'NVQ 2', 'NVQ 1', 'Other / Lower Levels')
all_labels <- c('Refused', "Don't know", 'Prefer not to say', 'Not asked', 'Not applicable', 'Item not applicable', category_labels)

# Apply wave-specific recoding in sequence on data
data <- data %>%
  mutate(
    educaim17 = case_when(
      is.na(w4saim) | w4saim %in% c(-999, -998, -997, -995, -94, -92, -91, -100, -97, -1) ~ -3,
      w4saim == 1 ~ 3,  # NVQ 3
      w4saim == 5 ~ 4,  # NVQ 2
      w4saim == 9 ~ 5,  # NVQ 1
      w4saim %in% c(2, 3, 6, 7, 8, 10, 11, 12, 13, 14) ~ 6,  # Other/lower/not studying
      TRUE ~ NA_integer_
    ),
    educaim19 = case_when(
      is.na(W6Saim) | W6Saim %in% c(-999, -998, -997, -995, -94, -92, -91, -100, -97, -1) ~ -3,
      W6Saim == 1 | W6Saim == 2 ~ 1,  # NVQ 5 or Degree (highest)
      W6Saim == 3 ~ 2,  # NVQ 4
      W6Saim == 5 ~ 3,  # NVQ 3
      W6Saim == 9 ~ 4,  # NVQ 2
      W6Saim == 12 ~ 5,  # NVQ 1
      W6Saim %in% c(6, 7, 8, 10, 11, 13, 14, 15, 16) ~ 6,  # Other/AVCE/A/AS/GCSE/Not studying
      TRUE ~ NA_integer_
    ),
    educaim20 = case_when(
      is.na(W7SAim) | W7SAim %in% c(-999, -998, -997, -995, -94, -92, -91, -100, -97, -1) ~ -3,
      W7SAim %in% c(11, 12, 13) ~ 1,  # Degree or NVQ 5
      W7SAim == 10 ~ 2,  # NVQ 4
      W7SAim == 6 ~ 3,  # NVQ 3
      W7SAim == 3 ~ 4,  # NVQ 2
      W7SAim == 1 ~ 5,  # NVQ 1
      W7SAim %in% c(2, 4, 5, 7, 8, 9, 14) ~ 6,  # Other/GCSE/AVCE/A/AS/etc.
      TRUE ~ NA_integer_
    ),
    educaim25 = case_when(
      is.na(W8ACTIVITY05) | W8ACTIVITY05 %in% c(-9, -8, -1, -3) ~ -3,
      W8ACTIVITY05 == 1 ~ 6,  # Studying
      TRUE ~ NA_integer_
    ),
    educaim32 = case_when(
      is.na(W9ECONACT2) | W9ECONACT2 %in% c(-9, -8, -3, -1) ~ -3,
      W9ECONACT2 %in% c(6, 7) ~ 6,  # In education
      TRUE ~ NA_integer_
    )
  )

# Convert to labelled factors
data <- data %>%
  mutate(
    educaim17 = factor(educaim17, levels = c(-9, -8, -7, -3, -2, -1, 1:6), labels = all_labels),
    educaim19 = factor(educaim19, levels = c(-9, -8, -7, -3, -2, -1, 1:6), labels = all_labels),
    educaim20 = factor(educaim20, levels = c(-9, -8, -7, -3, -2, -1, 1:6), labels = all_labels),
    educaim25 = factor(educaim25, levels = c(-9, -8, -7, -3, -2, -1, 1:6), labels = all_labels),
    educaim32 = factor(educaim32, levels = c(-9, -8, -7, -3, -2, -1, 1:6), labels = all_labels)
  )

# Create final output with NSID and five educaim variables
output <- data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write to CSV
write_csv(output, 'data/output/cleaned_data.csv')

cat('Done!\n')
cat('Rows:', nrow(output), '\n')
cat('Columns:', colnames(output), '\n')
print(head(output))