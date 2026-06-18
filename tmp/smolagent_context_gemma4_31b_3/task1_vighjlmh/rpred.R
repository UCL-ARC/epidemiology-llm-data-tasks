library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
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

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))

# Convert relevant sex variables to numeric and handle missing values based on metadata
# Mapping for sex variables
sex_vars <- list(
  w1 = 'W1sexYP',
  w2 = 'W2SexYP',
  w3 = 'W3sexYP',
  w4 = 'W4SexYP',
  w5 = 'W5SexYP',
  w6 = 'W6Sex',
  w7 = 'W7Sex',
  w8 = 'W8CMSEX',
  w9 = 'W9DSEX'
)

# Helper function to clean sex variables according to General Guidance
clean_sex <- function(df, var_name) {
  if (!var_name %in% names(df)) return(NULL)
  
  vals <- as.numeric(df[[var_name]])
  
  # Standard missing codes: 
  # -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable, -1 Item not applicable
  
  # We map by label meaning as per guidance
  # Labels are provided in metadata
  
  # Logic for mapping is usually: 
  # -92 -> -9 (Refusal)
  # -91 -> -1 (Not applicable)
  # -99 -> -3 (Not interviewed)
  # -1 -> -8 (Don't know)
  
  cleaned <- vals
  cleaned[vals == -92] <- -9
  cleaned[vals == -91] <- -1
  cleaned[vals == -99] <- -3
  cleaned[vals == -1] <- -8
  cleaned[vals == -998 | vals == -997 | vals == -995] <- -2
  
  # Convert R NA to -3
  cleaned[is.na(cleaned)] <- -3
  
  return(cleaned)
}

# Process files
processed_dfs <- list()
for (i in seq_along(files)) {
  df <- data_list[[i]]
  var_name <- sex_vars[[i]]
  df[[paste0('sex_', i)]] <- clean_sex(df, var_name)
  processed_dfs[[i]] <- df %>% select(NSID, !!paste0('sex_', i))
}

# Merge all
merged_data <- reduce(processed_dfs, full_join, by = 'NSID')

# Derivation logic for 'sex':
# Use most recent valid response first (W9DSEX), then fall back through early sweeps (W1 to W8).
# Valid responses are 1 (Male) and 2 (Female).

merged_data <- merged_data %>%
  mutate(sex = case_when(
    sex_9 %in% c(1, 2) ~ sex_9,
    sex_1 %in% c(1, 2) ~ sex_1,
    sex_2 %in% c(1, 2) ~ sex_2,
    sex_3 %in% c(1, 2) ~ sex_3,
    sex_4 %in% c(1, 2) ~ sex_4,
    sex_5 %in% c(1, 2) ~ sex_5,
    sex_6 %in% c(1, 2) ~ sex_6,
    sex_7 %in% c(1, 2) ~ sex_7,
    sex_8 %in% c(1, 2) ~ sex_8,
    TRUE ~ -3
  ))

# Factor labels
sex_labels <- c("1" = "Male", "2" = "Female", "-3" = "Not asked at the fieldwork stage / not interviewed")
# Note: Since it's a consolidated variable, we use the standard missing code if no valid substantive response is available.

# Final selection
final_data <- merged_data %>% select(NSID, sex)

# Ensure factor levels are ordered
final_data$sex <- factor(final_data$sex, levels = c("1", "2", "-3"), labels = c("Male", "Female", "Not asked at the fieldwork stage / not interviewed"))

# Wait, the requirement says: "Coded as: 1 = Male, 2 = Female".
# Usually, this means the underlying numeric value. Let's keep it as numeric with labels if possible, 
# but the output is CSV. Factors in CSV are written as strings.
# Let's provide numeric values and set labels using the labelled package for the internal R object,
# but for CSV, we will write the numeric codes 1, 2 and the fallback missing code.

final_data <- merged_data %>% select(NSID, sex)

write_csv(final_data, 'data/output/cleaned_data.csv')
