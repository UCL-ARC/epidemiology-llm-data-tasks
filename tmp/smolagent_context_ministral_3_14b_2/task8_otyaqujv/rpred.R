
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Define file paths and load data
file_paths <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/ns8_2015_main_interview.tab',
  'data/input/ns8_2015_derived.tab',
  'data/input/ns9_2022_main_interview.tab',
  'data/input/ns9_2022_derived_variables.tab'
)

# Load and merge data
data_frames <- map(file_paths, ~ read_delim(.x, delim = '\t'))
merged_data <- reduce(data_frames, full_join, by = 'NSID')

# Step 1: Create educ25 variable
nvq_mapping_w8 <- tibble(
  var = c('W8VCQU0I', 'W8VCQU0J', 'W8VCQU0K', 'W8VCQU0L', 'W8VCQU0M', 'W8VCQU0N', 'W8VCQU0O'),
  level = c(1, 3, 4, 4, 3, 3, 3)
)

# Map vocational qualifications to NVQ levels
voc_vars <- merged_data %>% select(matches('^W8VCQU0[A-Z]')) %>% names()
relevant_vars <- voc_vars[voc_vars %in% nvq_mapping_w8$var]

# Create a temporary dataframe for mapping
temp_mapping <- merged_data %>%
  select(NSID, all_of(relevant_vars)) %>%
  mutate(across(all_of(relevant_vars), ~ case_when(
    .x == 1 ~ nvq_mapping_w8$level[nvq_mapping_w8$var == cur_column()],
    TRUE ~ -3  # Use -3 for all other cases instead of NA
  )))

# Calculate highest vocational NVQ level
voc_nvq_levels <- temp_mapping %>%
  mutate(highest_voc_nvq = rowSums(select(., all_of(relevant_vars)) > 0) > 0) %>%
  mutate(highest_voc_nvq = ifelse(highest_voc_nvq,
                                 max(select(., all_of(relevant_vars)), na.rm = TRUE),
                                 -3))

# Create educ25
merged_data <- merged_data %>%
  left_join(voc_nvq_levels %>% select(NSID, highest_voc_nvq), by = 'NSID') %>%
  mutate(
    educ25 = case_when(
      !is.na(W8DHANVQH) & highest_voc_nvq != -3 ~ pmax(W8DHANVQH, highest_voc_nvq),
      !is.na(W8DHANVQH) ~ W8DHANVQH,
      highest_voc_nvq != -3 ~ highest_voc_nvq,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    educ25 = case_when(
      educ25 %in% c(1, 2, 3, 4, 5) ~ educ25,
      educ25 == 95 ~ 3,
      educ25 == 96 ~ 4,
      TRUE ~ -3
    )
  )

# Step 2: Create educ32 variable
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      !is.na(W9DANVQH) & !is.na(W9DVNVQH) ~ pmax(W9DANVQH, W9DVNVQH),
      !is.na(W9DANVQH) ~ W9DANVQH,
      !is.na(W9DVNVQH) ~ W9DVNVQH,
      TRUE ~ -3
    )
  ) %>%
  mutate(
    educ32 = case_when(
      educ32 %in% c(0, 1, 2, 3, 4, 5) ~ educ32,
      educ32 == 95 ~ 3,
      educ32 == 96 ~ 4,
      TRUE ~ -3
    )
  )

# Step 3: Create educadtl32 variable
ac_vars <- merged_data %>% select(matches('^W9ACQU0[A-Z]')) %>% names()
substantive_vars <- ac_vars[!ac_vars %in% c('W9ACQU0T', 'W9ACQU0U', 'W9ACQU0V')]

# Create a copy to avoid modifying the original
temp_data <- merged_data

# Assign integer codes
int_codes <- rep(-3, nrow(temp_data))
for (i in seq_along(substantive_vars)) {
  var <- substantive_vars[i]
  temp_data[[var]][temp_data[[var]] == 1] <- i
  temp_data[[var]][temp_data[[var]] == 2] <- -1
  temp_data[[var]][is.na(temp_data[[var]])] <- -3
}

for (i in seq_len(nrow(temp_data))) {
  valid_responses <- temp_data[i, substantive_vars]
  valid_indices <- which(valid_responses > 0 & valid_responses != -1)

  if (length(valid_indices) > 0) {
    int_codes[i] <- min(valid_indices)
  } else if (all(valid_responses == 2)) {
    int_codes[i] <- length(substantive_vars) + 1
  }
}

temp_data$educadtl32 <- int_codes

# Step 4: Create educvdtl32 variable
vc_vars <- merged_data %>% select(matches('^W9VCQU0[A-Z]')) %>% names()
substantive_vars_voc <- vc_vars[!vc_vars %in% c('W9VCQUAH', 'W9VCQUAI')]

# Create a copy to avoid modifying the original
temp_data_voc <- merged_data

# Assign integer codes
int_codes_voc <- rep(-3, nrow(temp_data_voc))
for (i in seq_along(substantive_vars_voc)) {
  var <- substantive_vars_voc[i]
  temp_data_voc[[var]][temp_data_voc[[var]] == 1] <- i
  temp_data_voc[[var]][temp_data_voc[[var]] == 2] <- -1
  temp_data_voc[[var]][is.na(temp_data_voc[[var]])] <- -3
}

for (i in seq_len(nrow(temp_data_voc))) {
  valid_responses <- temp_data_voc[i, substantive_vars_voc]
  valid_indices <- which(valid_responses > 0 & valid_responses != -1)

  if (length(valid_indices) > 0) {
    int_codes_voc[i] <- min(valid_indices)
  } else if (all(valid_responses == 2)) {
    int_codes_voc[i] <- length(substantive_vars_voc) + 1
  }
}

temp_data_voc$educvdtl32 <- int_codes_voc

# Combine all variables
final_data <- merged_data %>%
  mutate(educadtl32 = temp_data$educadtl32,
         educvdtl32 = temp_data_voc$educvdtl32)

# Define factor labels
educ25_labels <- c('NVQ 4-5 equivalent', 'NVQ 1-3 equivalent', 'Entry level or no qualifications',
                   'Other qualifications', 'None of these qualifications')
educ32_labels <- educ25_labels

final_data$educ25 <- factor(final_data$educ25, levels = 0:4, labels = educ25_labels)
final_data$educ32 <- factor(final_data$educ32, levels = 0:4, labels = educ32_labels)

# Select final variables
output_data <- final_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
readr::write_csv(output_data, 'data/output/cleaned_data.csv')

message('Cleaned data has been written to data/output/cleaned_data.csv')
