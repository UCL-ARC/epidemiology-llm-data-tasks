library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Define the files to load
files <- c(
  'data/input/wave_one_lsype_family_background_2020.tab',
  'data/input/wave_two_lsype_family_background_2020.tab',
  'data/input/wave_three_lsype_family_background_2020.tab',
  'data/input/wave_four_lsype_family_background_2020.tab',
  'data/input/wave_five_lsype_family_background_2020.tab',
  'data/input/wave_six_lsype_young_person_2020.tab',
  'data/input/wave_seven_lsype_young_person_2020.tab',
  'data/input/ns8_2015_main_interview.tab',
  'data/input/ns9_2022_derived_variables.tab'
)

# Load all files
wave1 <- read_delim(files[1], delim = '\t', show_col_types = FALSE)
wave2 <- read_delim(files[2], delim = '\t', show_col_types = FALSE)
wave3 <- read_delim(files[3], delim = '\t', show_col_types = FALSE)
wave4 <- read_delim(files[4], delim = '\t', show_col_types = FALSE)
wave5 <- read_delim(files[5], delim = '\t', show_col_types = FALSE)
wave6 <- read_delim(files[6], delim = '\t', show_col_types = FALSE)
wave7 <- read_delim(files[7], delim = '\t', show_col_types = FALSE)
wave8 <- read_delim(files[8], delim = '\t', show_col_types = FALSE)
wave9 <- read_delim(files[9], delim = '\t', show_col_types = FALSE)

# Function to recode missing values to standard codes
recode_missing <- function(x) {
  x[x == -999] <- -3  # Not asked/missing grid
  x[x == -998] <- -3  # Interviewer missed question
  x[x == -997] <- -2  # Script error
  x[x == -995] <- -3  # Missing history section data
  x[x == -92] <- -9   # Refusal
  x[x == -91] <- -1   # Not applicable
  x[x == -1] <- -8    # Don't know
  x[x == -99] <- -3   # Missing household grid
  x[x == -8] <- -8    # Don't know
  x[x == -9] <- -9    # Refusal
  x[x == -2] <- -2    # Script error
  x[is.na(x)] <- -3   # Null to Not asked
  return(x)
}

# Recode all tenure variables in original datasets
wave1$W1hous12HH <- recode_missing(wave1$W1hous12HH)
wave2$W2Hous12HH <- recode_missing(wave2$W2Hous12HH)
wave3$W3hous12HH <- recode_missing(wave3$W3hous12HH)
wave4$W4Hous12HH <- recode_missing(wave4$W4Hous12HH)
wave5$W5Hous12HH <- recode_missing(wave5$W5Hous12HH)
wave5$W5Hous12BHH <- recode_missing(wave5$W5Hous12BHH)
wave5$W5Hous12CHH <- recode_missing(wave5$W5Hous12CHH)
wave6$W6Hous12YP <- recode_missing(wave6$W6Hous12YP)
wave6$W6Hous12bYP <- recode_missing(wave6$W6Hous12bYP)
wave6$W6Hous12cYP <- recode_missing(wave6$W6Hous12cYP)
wave7$W7Hous12YP <- recode_missing(wave7$W7Hous12YP)
wave7$W7Hous12bYP <- recode_missing(wave7$W7Hous12bYP)
wave7$W7Hous12cYP <- recode_missing(wave7$W7Hous12cYP)
wave8$W8TENURE <- recode_missing(wave8$W8TENURE)
wave9$W9DTENURE <- recode_missing(wave9$W9DTENURE)

# Merge all datasets by NSID
cleaned_data <- full_join(wave1, wave2, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave3, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave4, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave5, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave6, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave7, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave8, by = 'NSID')
cleaned_data <- full_join(cleaned_data, wave9, by = 'NSID')

cat('Merged data dimensions:', nrow(cleaned_data), 'rows,', ncol(cleaned_data), 'columns\n')

# Function to combine multi-variable tenure into 8-category variable
combine_tenure <- function(type_var, owned_var, rented_var) {
  n <- length(type_var)
  result <- rep(-3, n)  # Default: Not asked
  
  for (i in 1:n) {
    type_val <- type_var[i]
    
    if (is.na(type_val)) {
      result[i] <- -3
    } else if (type_val == 1) {  # Owned
      owned_val <- owned_var[i]
      if (is.na(owned_val)) {
        result[i] <- -3
      } else if (owned_val == 1) {
        result[i] <- 1   # Owned outright
      } else if (owned_val == 2) {
        result[i] <- 2   # Being bought on a mortgage
      } else if (owned_val == 3) {
        result[i] <- 3   # Shared ownership
      } else if (owned_val == 4) {
        result[i] <- 8   # Some other arrangement
      } else if (owned_val == -1) {
        result[i] <- -1  # Not applicable
      } else if (owned_val == -2) {
        result[i] <- -2  # Script error
      } else if (owned_val == -8) {
        result[i] <- -8  # Don't know
      } else if (owned_val == -9) {
        result[i] <- -9  # Refusal
      } else {
        result[i] <- -3
      }
    } else if (type_val == 2) {  # Rented
      rented_val <- rented_var[i]
      if (is.na(rented_val)) {
        result[i] <- -3
      } else if (rented_val == 1) {
        result[i] <- 4   # Rented from Council
      } else if (rented_val == 2) {
        result[i] <- 5   # Rented from Housing Association
      } else if (rented_val == 3) {
        result[i] <- 6   # Rented privately
      } else if (rented_val == 4) {
        result[i] <- 7   # Rent free
      } else if (rented_val == 5) {
        result[i] <- 8   # Some other arrangement
      } else if (rented_val == -1) {
        result[i] <- -1  # Not applicable
      } else if (rented_val == -2) {
        result[i] <- -2  # Script error
      } else if (rented_val == -8) {
        result[i] <- -8  # Don't know
      } else if (rented_val == -9) {
        result[i] <- -9  # Refusal
      } else {
        result[i] <- -3
      }
    } else if (type_val == 3) {  # Something else
      result[i] <- 8  # Other
    } else {
      result[i] <- -3
    }
  }
  
  return(result)
}

# Create detailed adolescent variables (hownteen14-20)
# Age 14 (Wave 1): W1hous12HH - already 8 categories
hownteen14 <- cleaned_data$W1hous12HH

# Age 15 (Wave 2): W2Hous12HH - already 8 categories
hownteen15 <- cleaned_data$W2Hous12HH

# Age 16 (Wave 3): W3hous12HH - already 8 categories
hownteen16 <- cleaned_data$W3hous12HH

# Age 17 (Wave 4): W4Hous12HH - already 8 categories
hownteen17 <- cleaned_data$W4Hous12HH

# Age 18 (Wave 5): Combine W5Hous12HH, W5Hous12BHH, W5Hous12CHH
hownteen18 <- combine_tenure(cleaned_data$W5Hous12HH, cleaned_data$W5Hous12BHH, cleaned_data$W5Hous12CHH)

# Age 19 (Wave 6): Combine W6Hous12YP, W6Hous12bYP, W6Hous12cYP
hownteen19 <- combine_tenure(cleaned_data$W6Hous12YP, cleaned_data$W6Hous12bYP, cleaned_data$W6Hous12cYP)

# Age 20 (Wave 7): Combine W7Hous12YP, W7Hous12bYP, W7Hous12cYP
hownteen20 <- combine_tenure(cleaned_data$W7Hous12YP, cleaned_data$W7Hous12bYP, cleaned_data$W7Hous12cYP)

# Create collapsed variables (hown14-20, hown25, hown32)
# Collapse 8 categories to 6 main categories
collapse_tenure <- function(x) {
  n <- length(x)
  result <- rep(-3, n)
  
  for (i in 1:n) {
    val <- x[i]
    if (is.na(val)) {
      result[i] <- -3
    } else if (val == 1) {
      result[i] <- 1   # Owned outright
    } else if (val == 2) {
      result[i] <- 2   # Owned, buying with help of mortgage/loan
    } else if (val == 3) {
      result[i] <- 3   # Part rent, part mortgage
    } else if (val %in% c(4, 5, 6)) {
      result[i] <- 4   # Rent it (Council, Housing Association, Privately)
    } else if (val == 7) {
      result[i] <- 5   # Live rent-free
    } else if (val == 8) {
      result[i] <- 6   # Other
    } else if (val == -1) {
      result[i] <- -1  # Not applicable
    } else if (val == -2) {
      result[i] <- -2  # Script error
    } else if (val == -3) {
      result[i] <- -3  # Not asked
    } else if (val == -8) {
      result[i] <- -8  # Don't know
    } else if (val == -9) {
      result[i] <- -9  # Refusal
    } else {
      result[i] <- -3
    }
  }
  
  return(result)
}

hown14 <- collapse_tenure(hownteen14)
hown15 <- collapse_tenure(hownteen15)
hown16 <- collapse_tenure(hownteen16)
hown17 <- collapse_tenure(hownteen17)
hown18 <- collapse_tenure(hownteen18)
hown19 <- collapse_tenure(hownteen19)
hown20 <- collapse_tenure(hownteen20)
hown25 <- collapse_tenure(cleaned_data$W8TENURE)
hown32 <- collapse_tenure(cleaned_data$W9DTENURE)

cat('Derived variables created\n')

# Add derived variables to cleaned_data
cleaned_data$hownteen14 <- hownteen14
cleaned_data$hownteen15 <- hownteen15
cleaned_data$hownteen16 <- hownteen16
cleaned_data$hownteen17 <- hownteen17
cleaned_data$hownteen18 <- hownteen18
cleaned_data$hownteen19 <- hownteen19
cleaned_data$hownteen20 <- hownteen20
cleaned_data$hown14 <- hown14
cleaned_data$hown15 <- hown15
cleaned_data$hown16 <- hown16
cleaned_data$hown17 <- hown17
cleaned_data$hown18 <- hown18
cleaned_data$hown19 <- hown19
cleaned_data$hown20 <- hown20
cleaned_data$hown25 <- hown25
cleaned_data$hown32 <- hown32

# Create final output with only required variables
output_data <- cleaned_data[, c('NSID', 
         'hown14', 'hown15', 'hown16', 'hown17', 'hown18', 'hown19', 'hown20', 'hown25', 'hown32',
         'hownteen14', 'hownteen15', 'hownteen16', 'hownteen17', 'hownteen18', 'hownteen19', 'hownteen20')]

# Write to CSV
write.csv(output_data, 'data/output/cleaned_data.csv', row.names = FALSE)

cat('Output written to data/output/cleaned_data.csv\n')
cat('Output dimensions:', nrow(output_data), 'rows,', ncol(output_data), 'columns\n')