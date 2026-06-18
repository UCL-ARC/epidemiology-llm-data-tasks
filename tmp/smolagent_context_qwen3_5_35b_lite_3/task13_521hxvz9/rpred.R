library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define NS-SEC major category mapping
nssec_major_mapping <- c(
  # Managers and Employers
  '1.0' = 'Managers and Employers (Large)',
  '2.0' = 'Managers and Employers (Large)',
  '3.1' = 'Professionals',
  '3.2' = 'Professionals',
  '3.3' = 'Professionals',
  '3.4' = 'Professionals',
  '4.1' = 'Professionals',
  '4.2' = 'Professionals',
  '4.3' = 'Professionals',
  '4.4' = 'Professionals',
  '5.0' = 'Managers and Employers (Small)',
  '6.0' = 'Higher Supervisory',
  '7.1' = 'Intermediate',
  '7.2' = 'Intermediate',
  '7.3' = 'Intermediate',
  '7.4' = 'Intermediate',
  '8.1' = 'Managers and Employers (Small)',
  '8.2' = 'Managers and Employers (Small)',
  '9.1' = 'Own Account',
  '9.2' = 'Own Account',
  '10.0' = 'Lower Supervisory',
  '11.1' = 'Skilled Technical',
  '11.2' = 'Skilled Technical',
  '12.1' = 'Semi-Routine',
  '12.2' = 'Semi-Routine',
  '12.3' = 'Semi-Routine',
  '12.4' = 'Semi-Routine',
  '12.5' = 'Semi-Routine',
  '12.6' = 'Semi-Routine',
  '12.7' = 'Semi-Routine',
  '13.1' = 'Routine',
  '13.2' = 'Routine',
  '13.3' = 'Routine',
  '13.4' = 'Routine',
  '13.5' = 'Routine',
  '14.1' = 'Not in Labour Force',
  '14.2' = 'Not in Labour Force',
  '14.3' = 'Not in Labour Force',
  '15.0' = 'Not in Labour Force',
  '16.0' = 'Not in Labour Force',
  '17.0' = 'Not in Labour Force'
)

# Function to convert NS-SEC to major category
nssec_to_major <- function(nssec_var) {
  # Map numeric codes to major categories
  nssec_major <- case_when(
    nssec_var %in% c(1, 2, 8) ~ 1,  # Managers and Employers
    nssec_var >= 3 & nssec_var <= 4 ~ 2,  # Professionals
    nssec_var == 5 ~ 3,  # Lower Managerial
    nssec_var == 6 ~ 4,  # Higher Supervisory
    nssec_var >= 7 & nssec_var <= 9 ~ 5,  # Intermediate
    nssec_var == 10 ~ 6,  # Lower Supervisory
    nssec_var == 11 ~ 7,  # Skilled Technical
    nssec_var >= 12 & nssec_var <= 13 ~ 8,  # Semi-Routine and Routine
    nssec_var >= 14 & nssec_var <= 17 ~ 9  # Not in Labour Force / Not Classified
  )
  return(nssec_major)
}

# Function to handle missing values
handle_missing <- function(var, missing_codes = c(-999, -99, -98, -94)) {
  # Replace user missing codes with standard codes
  var[var == -999] <- -3  # Missing - household data lost
  var[var == -99] <- -1   # Not interviewed
  var[var == -98] <- -1   # Not present
  var[var == -94] <- -8   # Insufficient information
  return(var)
}

# Function to create NS-SEC variable with major category mapping
create_nssec_var <- function(var, age) {
  # Handle missing values
  var <- handle_missing(var)
  
  # Convert to major categories
  var <- nssec_to_major(var)
  
  # Set labels
  var <- factor(var, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -3, -1, -8),
                labels = c('Managers and Employers (Large)',
                          'Professionals',
                          'Lower Managerial',
                          'Higher Supervisory',
                          'Intermediate',
                          'Lower Supervisory',
                          'Skilled Technical',
                          'Semi-Routine/Routine',
                          'Not in Labour Force / Not Classified',
                          'Not asked',
                          'Not applicable',
                          'Insufficient information'))
  
  return(var)
}

# Load data files
data_14 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
data_15 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
data_16 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
data_17 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
data_18 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')

# Merge all data files
cleaned_data <- full_join(data_14, data_15, by = 'NSID')
cleaned_data <- full_join(cleaned_data, data_16, by = 'NSID')
cleaned_data <- full_join(cleaned_data, data_17, by = 'NSID')
cleaned_data <- full_join(cleaned_data, data_18, by = 'NSID')

# Create NS-SEC variables for each wave and parent
# Mother variables
nssecma14 <- create_nssec_var(cleaned_data$W1nsseccatmum, 14)
snssecpa14 <- create_nssec_var(cleaned_data$W1nsseccatdad, 14)
snssecma15 <- create_nssec_var(cleaned_data$W2nsseccatmum, 15)
snssecpa15 <- create_nssec_var(cleaned_data$W2nsseccatdad, 15)
snssecma16 <- create_nssec_var(cleaned_data$W3cnsseccatmum, 16)
snssecpa16 <- create_nssec_var(cleaned_data$W3cnsseccatdad, 16)
snssecma17 <- create_nssec_var(cleaned_data$w4cnsseccatmum, 17)
snssecpa17 <- create_nssec_var(cleaned_data$w4cnsseccatdad, 17)
snssecma18 <- create_nssec_var(cleaned_data$w5Cnsseccatmum, 18)
snssecpa18 <- create_nssec_var(cleaned_data$w5Cnsseccatdad, 18)

# Create final output with only required variables
output_data <- tibble(
  NSID = cleaned_data$NSID,
  nssecma14 = nssecma14,
  nssecpa14 = snssecpa14,
  nssecma15 = snssecma15,
  nssecpa15 = snssecpa15,
  nssecma16 = snssecma16,
  nssecpa16 = snssecpa16,
  nssecma17 = snssecma17,
  nssecpa17 = snssecpa17,
  nssecma18 = snssecma18,
  nssecpa18 = snssecpa18
)

# Write output
write_csv(output_data, 'data/output/cleaned_data.csv')

print('Data cleaning complete!')
print(paste('Number of records:', nrow(output_data)))
print('Variables created:')
print(colnames(output_data))
