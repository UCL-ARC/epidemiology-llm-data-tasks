library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
# Note: We load all files listed in metadata to preserve the full cohort frame
file_list <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- lapply(file_list, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'c'))
})

names(data_list) <- file_list

# Merge datasets
full_df <- data_list[[1]] %>% 
  mutate(NSID = as.character(NSID))

for(i in 2:length(data_list)) {
  temp_df <- data_list[[i]] %>% mutate(NSID = as.character(NSID))
  full_df <- full_join(full_df, temp_df, by = 'NSID')
}

# Function to map missing values based on general guidance and specific requirements
# Standard missing codes: -9 Refusal, -8 DK, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable, -1 Not applicable

# Processing Wave 6 (sori19)
full_df <- full_df %>%
  mutate(
    W6SexualityYP = as.numeric(W6SexualityYP),
    sori19 = case_when(
      W6SexualityYP == 1 ~ 1,
      W6SexualityYP == 2 ~ 2,
      W6SexualityYP == 3 ~ 3,
      W6SexualityYP == 4 ~ 4,
      W6SexualityYP == -97 ~ -9, # Specific requirement
      W6SexualityYP == -92 ~ -9, # Refused
      W6SexualityYP == -91 ~ -1, # Not applicable
      W6SexualityYP == -1 ~ -8,  # Don't know (General guidance says -8 for DK)
      is.na(W6SexualityYP) ~ -3,
      TRUE ~ -3
    )
  )

# Processing Wave 7 (sori20)
full_df <- full_df %>%
  mutate(
    W7SexualityYP = as.numeric(W7SexualityYP),
    sori20 = case_when(
      W7SexualityYP == 1 ~ 1,
      W7SexualityYP == 2 ~ 2,
      W7SexualityYP == 3 ~ 3,
      W7SexualityYP == 4 ~ 4,
      W7SexualityYP == -100 ~ -9, # Specific requirement
      W7SexualityYP == -97 ~ -9,  # Specific requirement
      W7SexualityYP == -92 ~ -9,   # Refused
      W7SexualityYP == -91 ~ -1,   # Not applicable
      W7SexualityYP == -1 ~ -8,    # Don't know
      is.na(W7SexualityYP) ~ -3,
      TRUE ~ -3
    )
  )

# Processing Wave 8 (sori25)
full_df <- full_df %>%
  mutate(
    W8SEXUALITY = as.numeric(W8SEXUALITY),
    sori25 = case_when(
      W8SEXUALITY == 1 ~ 1,
      W8SEXUALITY == 2 ~ 2,
      W8SEXUALITY == 3 ~ 3,
      W8SEXUALITY == 4 ~ 4,
      W8SEXUALITY == -9 ~ -9, # Refused
      W8SEXUALITY == -8 ~ -8, # Don't know
      W8SEXUALITY == -1 ~ -1, # Not applicable
      is.na(W8SEXUALITY) ~ -3,
      TRUE ~ -3
    )
  )

# Processing Wave 9 (sori32)
full_df <- full_df %>%
  mutate(
    W9SORI = as.numeric(W9SORI),
    sori32 = case_when(
      W9SORI == 1 ~ 1,
      W9SORI == 2 ~ 2,
      W9SORI == 3 ~ 3,
      W9SORI == 4 ~ 4,
      W9SORI == 5 ~ -7, # Specific requirement: Prefer not to say
      W9SORI == -9 ~ -9, # Refused
      W9SORI == -8 ~ -8, # Don't know
      W9SORI == -3 ~ -3, # Not asked
      W9SORI == -1 ~ -1, # Not applicable
      is.na(W9SORI) ~ -3,
      TRUE ~ -3
    )
  )

# Apply labels to derived variables
val_labels <- c(
  '1' = 'Heterosexual/straight',
  '2' = 'Gay/lesbian',
  '3' = 'Bisexual',
  '4' = 'Other',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know / insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Item not applicable'
)

# Create factors for sori variables
vars_to_label <- c('sori19', 'sori20', 'sori25', 'sori32')

for(v in vars_to_label) {
  full_df[[v]] <- factor(full_df[[v]], levels = names(val_labels), labels = val_labels)
}

# Select only the required variables
final_output <- full_df %>% 
  select(NSID, all_of(vars_to_label))

# Write to CSV
write_csv(final_output, 'data/output/cleaned_data.csv')
