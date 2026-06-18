library(haven)
library(dplyr)
library(readr)

# Load files
wave_four <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_five <- readr::read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave_six <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- readr::read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')

# Merge datasets
merged <- full_join(wave_four, wave_five, by = 'NSID')
merged <- full_join(merged, wave_six, by = 'NSID')
merged <- full_join(merged, wave_seven, by = 'NSID')
merged <- full_join(merged, ns8, by = 'NSID')

# Create NS-SEC processing function
process_nssec <- function(data, source_var, target_var) {
  if (!(source_var %in% colnames(data))) return(data)

  # Convert to numeric if needed
  if (!is.numeric(data[[source_var]])) {
    data[[source_var]] <- as.numeric(data[[source_var]])
  }

  # Standardize missing values
  data[[source_var]] <- ifelse(is.na(data[[source_var]]), -3, data[[source_var]])
  data[[source_var]][data[[source_var]] >= -999 & data[[source_var]] <= -995] <- -2
  data[[source_var]][data[[source_var]] == -94] <- -8
  data[[source_var]][data[[source_var]] == -92] <- -9
  data[[source_var]][data[[source_var]] == -91] <- -1
  data[[source_var]][data[[source_var]] == -99] <- -3
  data[[source_var]][data[[source_var]] == -100 | data[[source_var]] == -97] <- -2

  # Collapse categories
  data[[target_var]] <- case_when(
    data[[source_var]] %in% c(1, 2, 5, 6, 8.1, 8.2, 9.1, 9.2, 10, 1.0, 2.0, 5.0, 6.0, 8.1, 8.2, 9.1, 9.2, 10.0) ~ 1,
    data[[source_var]] %in% c(3.1, 3.2, 3.3, 3.4, 3.0) ~ 2,
    data[[source_var]] %in% c(4.1, 4.2, 4.3, 4.4, 4.0) ~ 3,
    data[[source_var]] %in% c(7.1, 7.2, 7.3, 7.4, 7.0) ~ 4,
    data[[source_var]] %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 12.0) ~ 5,
    data[[source_var]] %in% c(13.1, 13.2, 13.3, 13.4, 13.5, 13.0) ~ 6,
    data[[source_var]] %in% c(14.1, 14.2, 14.3, 15.0, 16.0, 17.0) ~ 7,
    TRUE ~ NA_integer_
  )

  # Create factor with labels
  data[[target_var]] <- factor(data[[target_var]], 
    levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7),
    labels = c('Refusal', 'Don\'t know', 'Prefer not to say', 'Not asked', 
              'Schedule error', 'Not applicable', 'Employers and managers', 
              'Higher professionals', 'Lower professionals', 'Intermediate', 
              'Semi-routine', 'Routine', 'Never worked'))

  return(data)
}

# Process each NS-SEC variable
merged <- process_nssec(merged, 'W4nsseccatYP', 'nssec17')
merged <- process_nssec(merged, 'W5nsseccatYP', 'nssec18')
merged <- process_nssec(merged, 'w6nsseccatYP', 'nssec19')
merged <- process_nssec(merged, 'W7NSSECCat', 'nssec20')
merged <- process_nssec(merged, 'W8DNSSEC17', 'nssec25')

# Select final columns
final_data <- merged %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25)

# Write output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')