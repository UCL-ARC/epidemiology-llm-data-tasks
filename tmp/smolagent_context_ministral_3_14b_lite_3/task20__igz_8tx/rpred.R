
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Load all relevant datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all datasets by NSID
all_data <- full_join(wave1, wave2, by = 'NSID')
all_data <- full_join(all_data, wave3, by = 'NSID')
all_data <- full_join(all_data, wave4, by = 'NSID')
all_data <- full_join(all_data, wave6, by = 'NSID')
all_data <- full_join(all_data, wave7, by = 'NSID')
all_data <- full_join(all_data, wave8, by = 'NSID')
all_data <- full_join(all_data, wave9, by = 'NSID')

# Define a function to map missing values to standard codes
map_missing <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x %in% c(-999, -998, -997, -995, -96, -92, -9, -8, -3), -3, x)
    x <- ifelse(x %in% c(-91, -1), -1, x)
    x <- ifelse(x %in% c(-97, -92), -9, x)
  }
  return(x)
}

# Define a function to derive alcfst for each wave
derive_alc_var <- function(data, age, source_var, is_audit) {
  data <- data %>%
    mutate(
      !!paste0("alc_", age) := ifelse(is_audit,
                                      ifelse(!is.na(!!sym(source_var)) & !!sym(source_var) == 1, age, 99),
                                      ifelse(!is.na(!!sym(source_var)) & !!sym(source_var) == 1, age, 99))
    )

  data <- data %>%
    mutate(!!paste0("alc_", age) := map_missing(!!sym(paste0("alc_", age))))
  return(data)
}

# Apply the function to derive alcfst for each wave
ages_map <- list(
  list(source_var = 'W1alceverYP', age = 14, is_audit = FALSE),
  list(source_var = 'W2alceverYP', age = 15, is_audit = FALSE),
  list(source_var = 'W3alceverYP', age = 16, is_audit = FALSE),
  list(source_var = 'W4AlcEverYP', age = 17, is_audit = FALSE),
  list(source_var = 'W6AlcEverYP', age = 19, is_audit = FALSE),
  list(source_var = 'W7AlcEverYP', age = 20, is_audit = FALSE),
  list(source_var = 'W8AUDIT1', age = 25, is_audit = TRUE),
  list(source_var = 'W9AUDIT1', age = 32, is_audit = TRUE)
)

for (age_info in ages_map) {
  all_data <- derive_alc_var(all_data, age_info$age, age_info$source_var, age_info$is_audit)
}

# Derive alcfst: earliest age at which the cohort member is known to have consumed alcohol
all_data <- all_data %>%
  mutate(
    alcfst = case_when(
      !is.na(alc_14) & alc_14 != 99 ~ alc_14,
      !is.na(alc_15) & alc_15 != 99 ~ alc_15,
      !is.na(alc_16) & alc_16 != 99 ~ alc_16,
      !is.na(alc_17) & alc_17 != 99 ~ alc_17,
      !is.na(alc_19) & alc_19 != 99 ~ alc_19,
      !is.na(alc_20) & alc_20 != 99 ~ alc_20,
      !is.na(alc_25) & alc_25 != 99 ~ alc_25,
      !is.na(alc_32) & alc_32 != 99 ~ alc_32,
      TRUE ~ 99
    )
  )

# Ensure alcfst is numeric
all_data$alcfst <- as.numeric(all_data$alcfst)

# Apply missing value mapping
all_data$alcfst <- map_missing(all_data$alcfst)

# Remove intermediate variables
all_data <- all_data %>%
  select(NSID, alcfst)

# Write cleaned data to CSV
write_csv(all_data, 'data/output/cleaned_data.csv')
