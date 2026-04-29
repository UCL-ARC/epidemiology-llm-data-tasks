library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define paths based on metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

# Load each file explicitly
# Using a list to keep them separate before merging
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- files

# Convert numeric columns that should be numeric (specifically the ones we need for analysis)
# Note: read_delim with col_types = 'c' reads everything as character to avoid type issues during merge
# We will convert specific variables to numeric during preprocessing

# Merge datasets using full_join by NSID
cohort_df <- data_list[[1]] %>% 
  select(NSID) %>% 
  full_join(data_list[[2]] %>% select(NSID), by = 'NSID') %>% 
  full_join(data_list[[3]] %>% select(NSID, W6SexualityYP), by = 'NSID') %>% 
  full_join(data_list[[4]] %>% select(NSID, W7SexualityYP), by = 'NSID') %>% 
  full_join(data_list[[5]] %>% select(NSID, W8SEXUALITY), by = 'NSID') %>% 
  full_join(data_list[[6]] %>% select(NSID, W9SORI), by = 'NSID')

# Preprocessing helper for sexual orientation
# Standard missing codes:
# -9 = Refusal
# -8 = Don't know
# -7 = Prefer not to say
# -3 = Not asked
# -2 = Schedule not applicable/info lost
# -1 = Item not applicable

process_sexuality <- function(vec, wave_name) {
  # Convert to numeric
  v <- as.numeric(vec)
  
  # Wave specific mappings based on metadata and additional requirements
  if (wave_name == 'W6') {
    v <- case_when(
      v == 1 ~ 1,
      v == 2 ~ 2,
      v == 3 ~ 3,
      v == 4 ~ 4,
      v == -97 ~ -9, # Refused self-completion
      v == -92 ~ -9, # Refused
      v == -91 ~ -1, # Not applicable
      v == -1  ~ -8, # Don't know
      TRUE ~ -3
    )
  } else if (wave_name == 'W7') {
    v <- case_when(
      v == 1 ~ 1,
      v == 2 ~ 2,
      v == 3 ~ 3,
      v == 4 ~ 4,
      v == -100 ~ -9, # Declined sexual experience
      v == -97  ~ -9, # Refused self-completion
      v == -92  ~ -9, # Refused
      v == -91  ~ -1, # Not applicable
      v == -1   ~ -8, # Don't know
      TRUE ~ -3
    )
  } else if (wave_name == 'W8') {
    v <- case_when(
      v == 1 ~ 1,
      v == 2 ~ 2,
      v == 3 ~ 3,
      v == 4 ~ 4,
      v == -9 ~ -9, # Refused
      v == -8 ~ -8, # Don't know
      v == -1 ~ -1, # Not applicable
      TRUE ~ -3
    )
  } else if (wave_name == 'W9') {
    v <- case_when(
      v == 1 ~ 1,
      v == 2 ~ 2,
      v == 3 ~ 3,
      v == 4 ~ 4,
      v == 5 ~ -7, # Prefer not to say
      v == -9 ~ -9, # Refused
      v == -8 ~ -8, # Don't know
      v == -3 ~ -3, # Not asked
      v == -1 ~ -1, # Not applicable
      TRUE ~ -3
    )
  }
  
  # Final NA conversion
  v[is.na(v)] <- -3
  return(v)
}

# Apply processing
cohort_df <- cohort_df %>%
  mutate(
    sexual_orientation_19 = process_sexuality(W6SexualityYP, 'W6'),
    sexual_orientation_20 = process_sexuality(W7SexualityYP, 'W7'),
    sexual_orientation_21 = process_sexuality(W8SEXUALITY, 'W8'), # Assuming wave 8 corresponds to a specific age/year
    sexual_orientation_32 = process_sexuality(W9SORI, 'W9')
  )

# Note on Wave 8 age: The metadata doesn't explicitly state age for W8, but W9 is 32. 
# Based on the prompt's variable naming rule "append the age as a numeric suffix".
# Since W8 age isn't given, I'll name it sexual_orientation_w8 or infer if possible.
# Looking at the flow: W6(19), W7(20), W8(?), W9(32). 
# I will use 'w8' for the wave 8 variable if age is unknown, but the prompt asks for numeric suffixes.
# Let's check if W8 is usually associated with a specific age in this cohort (e.g. 21). 
# Without explicit info, I will use sexual_orientation_w8.

# Re-evaluating: The prompt says "append the age as a numeric suffix". 
# Let's name W8 as sexual_orientation_w8 for now as age is not provided in the metadata for that specific file.

# Consolidation: Create a consolidated variable (most recent valid first)
# Valid substantive responses are 1, 2, 3, 4

cohort_df <- cohort_df %>%
  mutate(
    sexual_orientation = coalesce(
      ifelse(sexual_orientation_32 >= 1 & sexual_orientation_32 <= 4, sexual_orientation_32, NA),
      ifelse(sexual_orientation_21 >= 1 & sexual_orientation_21 <= 4, sexual_orientation_21, NA),
      ifelse(sexual_orientation_20 >= 1 & sexual_orientation_20 <= 4, sexual_orientation_20, NA),
      ifelse(sexual_orientation_19 >= 1 & sexual_orientation_19 <= 4, sexual_orientation_19, NA)
    )
  )

# If no valid substantive response, fall back to the most recent missing code
cohort_df <- cohort_df %>%
  mutate(
    sexual_orientation = coalesce(
      sexual_orientation, 
      sexual_orientation_32, 
      sexual_orientation_21, 
      sexual_orientation_20, 
      sexual_orientation_19
    )
  )

# Final cleaning of the consolidated variable NA
cohort_df$sexual_orientation[is.na(cohort_df$sexual_orientation)] <- -3

# Define labels for factors
sex_labels <- c(
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked / Not interviewed",
  "-2" = "Schedule not applicable / Information lost",
  "-1" = "Not applicable"
)

# Apply labels to all derived variables
apply_sex_labels <- function(x) {
  factor(x, levels = as.numeric(names(sex_labels)), labels = sex_labels)
}

final_vars <- c("NSID", "sexual_orientation", "sexual_orientation_19", "sexual_orientation_20", "sexual_orientation_w8", "sexual_orientation_32")

# Since I used 'sexual_orientation_w8', I'll ensure it exists
cohort_df <- cohort_df %>%
  rename(sexual_orientation_w8 = sexual_orientation_21)

final_df <- cohort_df %>%
  select(all_of(final_vars))

# Convert to factors with labels
final_df <- final_df %>%
  mutate(across(starts_with("sexual_orientation"), apply_sex_labels))

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")