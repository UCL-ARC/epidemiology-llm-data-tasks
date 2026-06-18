library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Full join all datasets by NSID
cohort <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

cat('Number of rows after merge:', nrow(cohort), '\n')

# Define banded income labels (harmonised across all waves)
banded_labels <- c(
  '1' = 'Up to \u00a349',
  '2' = '\u00a350 up to \u00a399',
  '3' = '\u00a3100 up to \u00a3199',
  '4' = '\u00a3200 up to \u00a3299',
  '5' = '\u00a3300 up to \u00a3399',
  '6' = '\u00a3400 up to \u00a3499',
  '7' = '\u00a3500 up to \u00a3599',
  '8' = '\u00a3600 up to \u00a3699',
  '9' = '\u00a3700 up to \u00a3799',
  '10' = '\u00a3800 up to \u00a3899',
  '11' = '\u00a3900 up to \u00a3990',
  '12' = '\u00a31,000 or more'
)

# Helper function to map missing values for ages 14-15 (continuous variables W1GrsswkHH, W2GrsswkHH)
map_missing_14_15 <- function(x) {
  x[x == -3] <- -1   # "Not yet paid" -> -1 (Item not applicable)
  x[x == -1] <- -8   # "Don't know" -> -8 (Don't know)
  x[x == -992] <- -9 # "No information - refused" -> -9 (Refusal)
  x[x == -999] <- -2 # "Missing in error" -> -2 (Schedule not applicable)
  x[x == -99] <- -3  # "HH not interviewed" -> -3 (Not asked)
  x[x == -94] <- -8  # "Insufficient information" -> -8 (Don't know)
  x[x == -92] <- -9  # "Refused" -> -9 (Refusal)
  x[x == -91] <- -1  # "Not applicable" -> -1 (Item not applicable)
  # Convert remaining NAs to -3 (Not asked)
  x[is.na(x)] <- -3
  return(x)
}

# Helper function to map missing values for age 17 (w4IncEstW)
map_missing_17 <- function(x) {
  x[x == -1] <- -8   # "Don't know" -> -8
  x[x == -92] <- -9  # "Refused" -> -9
  x[x == -99] <- -3  # "MP not interviewed" -> -3
  x[x == -996] <- -3 # "No parent in household" -> -3
  # Convert remaining NAs to -3
  x[is.na(x)] <- -3
  return(x)
}

# Helper function to map missing values for age 16 (W3incestw)
map_missing_16 <- function(x) {
  x[x == -1] <- -8   # "Don't know" -> -8
  x[x == -99] <- -3  # "MP not interviewed" -> -3
  x[x == -92] <- -9  # "Refused" -> -9
  # Convert remaining NAs to -3
  x[is.na(x)] <- -3
  return(x)
}

# Helper function to band continuous values into categories 1-12
band_income <- function(x) {
  # Only band positive values (1-12 are valid; negative values are missing codes)
  valid <- x > 0
  if (any(valid)) {
    x[valid] <- as.integer(cut(x[valid], 
                               breaks = c(0, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
                               labels = FALSE))
  }
  return(x)
}

# Process Wave 1 (Age 14) - continuous source
cohort$incwhhcnt14 <- cohort$W1GrsswkHH
# Map missing values first
cohort$incwhhcnt14 <- map_missing_14_15(cohort$incwhhcnt14)
# Band the continuous values to create banded variable
cohort$incwhh14 <- cohort$incwhhcnt14
cohort$incwhh14 <- band_income(cohort$incwhh14)

# Process Wave 2 (Age 15) - continuous source
cohort$incwhhcnt15 <- cohort$W2GrsswkHH
# Map missing values first
cohort$incwhhcnt15 <- map_missing_14_15(cohort$incwhhcnt15)
# Band the continuous values to create banded variable
cohort$incwhh15 <- cohort$incwhhcnt15
cohort$incwhh15 <- band_income(cohort$incwhh15)

# Process Wave 3 (Age 16) - already banded
cohort$incwhh16 <- cohort$W3incestw
# Map missing values
cohort$incwhh16 <- map_missing_16(cohort$incwhh16)

# Process Wave 4 (Age 17) - already banded
cohort$incwhh17 <- cohort$w4IncEstW
# Map missing values
cohort$incwhh17 <- map_missing_17(cohort$incwhh17)

# Convert banded variables to labelled factors
# Create factors with the banded labels
for (var_name in c('incwhh14', 'incwhh15', 'incwhh16', 'incwhh17')) {
  vals <- cohort[[var_name]]
  # Create a factor with all possible labels
  cohort[[var_name]] <- factor(vals, levels = as.integer(names(banded_labels)), labels = banded_labels, ordered = FALSE)
}

# Select final output columns
output <- cohort %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(output), '\n')
cat('Columns:', names(output), '\n')

# Quick summary
cat('\nSample of output:\n')
print(head(output))

cat('\nincwhh14 levels:', levels(output$incwhh14), '\n')
