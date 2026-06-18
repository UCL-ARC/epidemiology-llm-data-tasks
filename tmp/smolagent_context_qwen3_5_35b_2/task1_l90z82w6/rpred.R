library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge all files by NSID
merged <- full_join(wave1, wave2, by = 'NSID')
merged <- full_join(merged, wave3, by = 'NSID')
merged <- full_join(merged, wave4, by = 'NSID')
merged <- full_join(merged, wave5, by = 'NSID')
merged <- full_join(merged, wave6, by = 'NSID')
merged <- full_join(merged, wave7, by = 'NSID')
merged <- full_join(merged, wave8, by = 'NSID')
merged <- full_join(merged, wave9, by = 'NSID')

# Function to standardize missing values to -3
standardize_missing <- function(x) {
  # Valid sex values are 1 and 2
  valid_values <- c(1, 2)
  # All other values (including NA, negative codes) become -3
  x[x %in% valid_values] <- x[x %in% valid_values]
  x[!(x %in% valid_values)] <- -3
  return(x)
}

# Standardize sex variables from all waves
merged$W9DSEX_std <- standardize_missing(merged$W9DSEX)
merged$W8CMSEX_std <- standardize_missing(merged$W8CMSEX)
merged$W7Sex_std <- standardize_missing(merged$W7Sex)
merged$W6Sex_std <- standardize_missing(merged$W6Sex)
merged$W5SexYP_std <- standardize_missing(merged$W5SexYP)
merged$W4SexYP_std <- standardize_missing(merged$W4SexYP)
merged$W3sexYP_std <- standardize_missing(merged$W3sexYP)
merged$W2SexYP_std <- standardize_missing(merged$W2SexYP)
merged$W1sexYP_std <- standardize_missing(merged$W1sexYP)

# Create consolidated sex variable using most recent valid response first
# Start from W9DSEX, if missing fall back to W8CMSEX, etc.
merged$sex <- case_when(
  merged$W9DSEX_std != -3 ~ merged$W9DSEX_std,
  merged$W8CMSEX_std != -3 ~ merged$W8CMSEX_std,
  merged$W7Sex_std != -3 ~ merged$W7Sex_std,
  merged$W6Sex_std != -3 ~ merged$W6Sex_std,
  merged$W5SexYP_std != -3 ~ merged$W5SexYP_std,
  merged$W4SexYP_std != -3 ~ merged$W4SexYP_std,
  merged$W3sexYP_std != -3 ~ merged$W3sexYP_std,
  merged$W2SexYP_std != -3 ~ merged$W2SexYP_std,
  merged$W1sexYP_std != -3 ~ merged$W1sexYP_std,
  TRUE ~ -3
)

# Keep only NSID and sex
result <- merged %>% select(NSID, sex)

# Add labels for sex variable
var_label(result$sex) <- 'Sex (consolidated)'

# Create factor with proper ordering
result$sex <- factor(result$sex, levels = c(1, 2, -3),
                     labels = c('Male', 'Female', 'Missing'))

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

cat('Processing complete. Output written to data/output/cleaned_data.csv\n')
