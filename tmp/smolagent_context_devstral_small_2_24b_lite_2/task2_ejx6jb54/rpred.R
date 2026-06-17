library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define metadata for each wave
metadata_wave1 <- list(
  value_labels = list(
    '-999.0' = 'Missing - household data lost',
    '-94.0' = 'Insufficient information',
    '-92.0' = 'Refused',
    '-91.0' = 'Not applicable',
    '-1.0' = "Don't know",
    '1.0' = 'White - British',
    '2.0' = 'White - Irish',
    '3.0' = 'Any other White background',
    '4.0' = 'Mixed - White and Black Caribbean',
    '5.0' = 'Mixed - White and Black African',
    '6.0' = 'Mixed - White and Asian',
    '7.0' = 'Any other mixed background',
    '8.0' = 'Indian',
    '9.0' = 'Pakistani',
    '10.0' = 'Bangladeshi',
    '11.0' = 'Any other Asian background',
    '12.0' = 'Black Caribbean',
    '13.0' = 'Black African',
    '14.0' = 'Any other Black background',
    '15.0' = 'Chinese',
    '16.0' = 'Any other ethnic background'
  )
)

metadata_wave2 <- list(
  value_labels = list(
    '-998.0' = 'Interviewer missed question',
    '-997.0' = 'Script error',
    '-995.0' = 'Missing history section data - unexplained',
    '-99.0' = 'YP not interviewed',
    '-92.0' = 'Refused',
    '-91.0' = 'Not applicable',
    '-1.0' = "Don't Know",
    '1.0' = 'White - British',
    '2.0' = 'White - Irish',
    '3.0' = 'Any other White background',
    '4.0' = 'White and Black Caribbean',
    '5.0' = 'White and Black African',
    '6.0' = 'White and Asian',
    '7.0' = 'Any other mixed background',
    '8.0' = 'Indian',
    '9.0' = 'Pakistani',
    '10.0' = 'Bangladeshi',
    '11.0' = 'Any other Asian background',
    '12.0' = 'Caribbean',
    '13.0' = 'African',
    '14.0' = 'Any other Black background',
    '15.0' = 'Chinese',
    '16.0' = 'Any other'
  )
)

metadata_wave4 <- list(
  value_labels = list(
    '-94.0' = 'Insufficient information',
    '-1.0' = "Don't know",
    '1.0' = 'White - British',
    '2.0' = 'White - Irish',
    '3.0' = 'Any other White background',
    '4.0' = 'Mixed - White and Black Caribbean',
    '5.0' = 'Mixed - White and Black African',
    '6.0' = 'Mixed - White and Asian',
    '7.0' = 'Any other mixed background',
    '8.0' = 'Indian',
    '9.0' = 'Pakistani',
    '10.0' = 'Bangladeshi',
    '11.0' = 'Any other Asian background',
    '12.0' = 'Black Caribbean',
    '13.0' = 'Black African',
    '14.0' = 'Any other Black background',
    '15.0' = 'Chinese',
    '16.0' = 'Any other ethnic background'
  )
)

metadata_wave8 <- list(
  value_labels = list(
    '-9.0' = 'Refused',
    '-8.0' = 'Insufficient information',
    '-1.0' = 'Not applicable',
    '1.0' = 'White - British',
    '2.0' = 'White - Irish',
    '3.0' = 'Any other White background',
    '4.0' = 'Mixed - White and Black Caribbean',
    '5.0' = 'Mixed - White and Black African',
    '6.0' = 'Mixed - White and Asian',
    '7.0' = 'Any other mixed background',
    '8.0' = 'Asian/Asian British - Indian',
    '9.0' = 'Asian/Asian British - Pakistani',
    '10.0' = 'Asian/Asian British - Bangladeshi',
    '11.0' = 'Other other Asian background',
    '12.0' = 'Black/Black British - Caribbean',
    '13.0' = 'Black/Black British - African',
    '14.0' = 'Any other Black background',
    '15.0' = 'Chinese',
    '16.0' = 'Any other background'
  )
)

metadata_wave9 <- list(
  value_labels = list(
    '-8.0' = 'Insufficient information',
    '1.0' = 'White - British',
    '2.0' = 'White - Irish',
    '3.0' = 'Any other White background',
    '4.0' = 'Mixed - White and Black Caribbean',
    '5.0' = 'Mixed - White and Black African',
    '6.0' = 'Mixed - White and Asian',
    '7.0' = 'Any other Mixed background',
    '8.0' = 'Asian/Asian British - Indian',
    '9.0' = 'Asian/Asian British - Pakistani',
    '10.0' = 'Asian/Asian British - Bangladeshi',
    '11.0' = 'Any other Asian background',
    '12.0' = 'Black/Black British - Caribbean',
    '13.0' = 'Black/Black British - African',
    '14.0' = 'Any other Black background',
    '15.0' = 'Chinese',
    '16.0' = 'Any other background'
  )
)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, metadata) {
  var <- as.numeric(var)
  value_labels <- metadata$value_labels
  
  # Initialize output variable
  output <- var
  
  # Map missing values based on metadata labels
  for (code in names(value_labels)) {
    label <- value_labels[[code]]
    if (grepl("Refusal|Refused", label)) {
      output[var == as.numeric(code)] <- -9
    } else if (grepl("Don't know|Insufficient information", label)) {
      output[var == as.numeric(code)] <- -8
    } else if (grepl("Prefer not to say", label)) {
      output[var == as.numeric(code)] <- -7
    } else if (grepl("Not asked|Not interviewed", label)) {
      output[var == as.numeric(code)] <- -3
    } else if (grepl("Schedule not applicable|Script error|Information lost|Not applicable", label)) {
      output[var == as.numeric(code)] <- -2
    } else if (grepl("Item not applicable", label)) {
      output[var == as.numeric(code)] <- -1
    }
  }
  
  # Convert NA to -3
  output[is.na(output)] <- -3
  
  return(output)
}

# Map missing values for each ethnicity variable
wave1$W1ethnic2YP <- map_missing(wave1$W1ethnic2YP, metadata_wave1)
wave2$W2ethnicYP <- map_missing(wave2$W2ethnicYP, metadata_wave2)
wave4$w4ethnic2YP <- map_missing(wave4$w4ethnic2YP, metadata_wave4)
wave8$W8DETHN15 <- map_missing(wave8$W8DETHN15, metadata_wave8)
wave9$W9DETHN15 <- map_missing(wave9$W9DETHN15, metadata_wave9)

# Derive consolidated ethnicity variable (eth) using earliest-valid-first
merged_data <- merged_data %>%
  mutate(eth = coalesce(W1ethnic2YP, W2ethnicYP, w4ethnic2YP, W8DETHN15, W9DETHN15))

# Select only NSID and eth for output
output_data <- merged_data %>%
  select(NSID, eth)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print summary
cat("Output written to data/output/cleaned_data.csv\n")
cat("Summary of eth variable:\n")
summary(output_data$eth)