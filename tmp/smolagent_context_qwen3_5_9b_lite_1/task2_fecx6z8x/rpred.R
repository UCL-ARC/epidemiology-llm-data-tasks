library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory
setwd('/home/jovyan/rdss-volume/tmp/smolagent_context_qwen3_5_9b_1/task2_fecx6z8x')

# Read all files with correct paths
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols(.default = 'c'))
wave9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols(.default = 'c'))

# Define value labels for harmonisation
final_value_labels <- c(
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background'
)

# Function to convert missing values
convert_missing <- function(x, miss_vec) {
  x <- as.numeric(as.character(x))
  for (i in seq_along(miss_vec)) {
    code <- names(miss_vec)[i]
    new_code <- miss_vec[i]
    if (!is.na(code)) {
      x[x == as.numeric(code)] <- new_code
    }
  }
  x[is.na(x)] <- -3
  return(x)
}

# Wave 1 missing value mapping - based on metadata
# Use setNames to create named vector
wave1_miss_vec <- setNames(c(-2, -8, -9, -1, -8), c(-999, -94, -92, -91, -1))

# Wave 2 missing value mapping
wave2_miss_vec <- setNames(c(-2, -2, -2, -3, -9, -1, -8), c(-998, -997, -995, -99, -92, -91, -1))

# Wave 4 missing value mapping
wave4_miss_vec <- setNames(c(-8, -8), c(-94, -1))

# Wave 8 missing value mapping
wave8_miss_vec <- setNames(c(-9, -8, -1), c(-9, -8, -1))

# Wave 9 missing value mapping
wave9_miss_vec <- setNames(c(-8), c(-8))

# Apply missing value conversion for each wave
data_w1 <- wave1 %>% mutate(W1ethnic2YP = convert_missing(W1ethnic2YP, wave1_miss_vec))
data_w2 <- wave2 %>% mutate(W2ethnicYP = convert_missing(W2ethnicYP, wave2_miss_vec))
data_w4 <- wave4 %>% mutate(w4ethnic2YP = convert_missing(w4ethnic2YP, wave4_miss_vec))
data_w8 <- wave8 %>% mutate(W8DETHN15 = convert_missing(W8DETHN15, wave8_miss_vec))
data_w9 <- wave9 %>% mutate(W9DETHN15 = convert_missing(W9DETHN15, wave9_miss_vec))

# Merge all datasets
full_data <- data_w1 %>%
  full_join(data_w2, by = "NSID") %>%
  full_join(data_w4, by = "NSID") %>%
  full_join(data_w8, by = "NSID") %>%
  full_join(data_w9, by = "NSID")

# Function to check if value is valid (not a missing code)
is_valid_eth <- function(x) {
  return(x >= 1 && x <= 16)
}

# Create eth variable - use earliest valid value (wave 1 first, then wave 2, etc.)
full_data <- full_data %>%
  mutate(
    eth_val = case_when(
      !is.na(W1ethnic2YP) & is_valid_eth(W1ethnic2YP) 
      ~ as.character(W1ethnic2YP),
      !is.na(W2ethnicYP) & is_valid_eth(W2ethnicYP) 
      ~ as.character(W2ethnicYP),
      !is.na(w4ethnic2YP) & is_valid_eth(w4ethnic2YP) 
      ~ as.character(w4ethnic2YP),
      !is.na(W8DETHN15) & is_valid_eth(W8DETHN15) 
      ~ as.character(W8DETHN15),
      !is.na(W9DETHN15) & is_valid_eth(W9DETHN15) 
      ~ as.character(W9DETHN15),
      TRUE 
      ~ as.character(-3)
    )
  )

# Convert to factor with proper labels
full_data <- full_data %>%
  mutate(eth = factor(eth_val, 
    levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16'),
    labels = final_value_labels
  ))

# Select only NSID and eth
eth_data <- full_data %>% select(NSID, eth)

# Remove empty levels
eth_data <- eth_data %>% mutate(eth = droplevels(eth))

# Write output
write_csv(eth_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Rows:', nrow(eth_data)))
print(paste('Eth distribution:'))
print(table(eth_data$eth))
print(head(eth_data))
