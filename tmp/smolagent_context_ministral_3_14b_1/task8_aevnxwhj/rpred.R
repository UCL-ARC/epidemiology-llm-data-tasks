
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)

# Load and merge datasets
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
ns8_main <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
ns8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge datasets by NSID
merged_data <- full_join(wave_one, wave_four, by = 'NSID') %>%
  full_join(ns8_main, by = 'NSID') %>%
  full_join(ns8_derived, by = 'NSID') %>%
  full_join(ns9_main, by = 'NSID') %>%
  full_join(ns9_derived, by = 'NSID')

# Define NVQ tier mapping for W8VCQU* variables
vcqu_tier_mapping <- tibble(
  variable = c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G',
               'W8VCQU0H', 'W8VCQU0I', 'W8VCQU0J', 'W8VCQU0K', 'W8VCQU0L', 'W8VCQU0M', 'W8VCQU0N',
               'W8VCQU0O'),
  tier = c(1, 1, 1, 1, 3, 1, 1, 2, 1, 5, 4, 4, 1, 1, 1)
)

# Create tier variables for W8VCQU
for (i in 1:nrow(vcqu_tier_mapping)) {
  var <- vcqu_tier_mapping$variable[i]
  tier <- vcqu_tier_mapping$tier[i]
  merged_data[[paste0(var, '_tier')]] <- ifelse(merged_data[[var]] == 1, tier, NA_integer_)
}

# Calculate highest vocational tier at age 25
tier_vars <- grep('_tier', names(merged_data), value = TRUE)
merged_data$highest_vocational_tier <- sapply(1:nrow(merged_data), function(i) {
  tiers <- merged_data[i, tier_vars, drop = FALSE]
  if (all(is.na(tiers))) NA_integer_ else max(tiers, na.rm = TRUE)
})

# Derive educ25 (NVQ 5-level scheme at age 25)
merged_data <- merged_data %>%
  mutate(
    combined_tier = pmax(W8DHANVQH, highest_vocational_tier, na.rm = TRUE),
    educ25 = case_when(
      combined_tier %in% c(4, 5) | W8DHANVQH == 95 | highest_vocational_tier == 95 ~ 0,
      combined_tier %in% c(1, 2, 3) ~ 1,
      combined_tier == 0 | combined_tier == 96 | is.na(combined_tier) ~ 2,
      W8DHANVQH == 96 | highest_vocational_tier == 96 ~ 4,
      TRUE ~ -3
    )
  )

# Derive educ32 (NVQ 5-level scheme at age 32)
merged_data <- merged_data %>%
  mutate(
    combined_tier_32 = pmax(W9DANVQH, W9DVNVQH, na.rm = TRUE),
    educ32 = case_when(
      combined_tier_32 %in% c(4, 5) | combined_tier_32 == 95 ~ 0,
      combined_tier_32 %in% c(1, 2, 3) ~ 1,
      combined_tier_32 == 0 | combined_tier_32 == 96 | is.na(combined_tier_32) ~ 2,
      combined_tier_32 == 95 ~ 3,
      TRUE ~ -3
    )
  )

# Create factor variables for educ25 and educ32
merged_data <- merged_data %>%
  mutate(
    educ25 = factor(educ25,
                    levels = c(-3, 0, 1, 2, 3, 4),
                    labels = c('Not asked', 'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent',
                               'Entry level or no qualifications', 'Other qualifications',
                               'None of these qualifications')),
    educ32 = factor(educ32,
                    levels = c(-3, 0, 1, 2, 3, 4),
                    labels = c('Not asked', 'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent',
                               'Entry level or no qualifications', 'Other qualifications',
                               'None of these qualifications'))
  )

# Select only the required columns
final_data <- merged_data %>%
  select(NSID, educ25, educ32)

# Write the output to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

message('Cleaned data has been written to data/output/cleaned_data.csv')
