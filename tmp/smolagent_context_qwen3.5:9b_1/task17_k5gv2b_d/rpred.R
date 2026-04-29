library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Extract IMDRSCORE from wave_two (age 15) and rename to imd15
wave_two_imd <- wave_two %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15 = IMDRSCORE)

# Extract IMDRSCORE from wave_three (age 16) and rename to imd16
wave_three_imd <- wave_three %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16 = IMDRSCORE)

# Extract W9DIMDD from ns9 (age 32) and rename to imd32
derived_imd <- derived %>%
  select(NSID, W9DIMDD) %>%
  rename(imd32 = W9DIMDD)

# Merge all datasets by NSID
cleaned_data <- full_join(wave_two_imd, wave_three_imd, by = 'NSID') %>%
  full_join(derived_imd, by = 'NSID')

# Recode missing values
# -94 -> -8 (Don't know), NA -> -3
cleaned_data$imd15 <- ifelse(is.na(cleaned_data$imd15), -3, cleaned_data$imd15)
cleaned_data$imd15 <- ifelse(cleaned_data$imd15 == -94, -8, cleaned_data$imd15)

cleaned_data$imd16 <- ifelse(is.na(cleaned_data$imd16), -3, cleaned_data$imd16)
cleaned_data$imd16 <- ifelse(cleaned_data$imd16 == -94, -8, cleaned_data$imd16)

cleaned_data$imd32 <- ifelse(is.na(cleaned_data$imd32), -3, cleaned_data$imd32)
cleaned_data$imd32 <- ifelse(cleaned_data$imd32 == -8, -8, cleaned_data$imd32)

# Create labels - numeric names for the labelled package
lbl_nums <- c(-9, -8, -3, -2, -1)
lbl_chars <- c('Refusal', "Don't know / insufficient information", 'Not asked at the fieldwork stage / not interviewed', 'Script error / information lost', 'Not applicable')

# Create labelled vector with proper numeric labels
# Use integer type for labels to match numeric data
lbl_int <- as.integer(lbl_nums)
names(lbl_int) <- as.character(lbl_nums)

# Apply labels using haven::labelled
cleaned_data$imd15 <- haven::labelled(cleaned_data$imd15, labels = lbl_int)
cleaned_data$imd16 <- haven::labelled(cleaned_data$imd16, labels = lbl_int)
cleaned_data$imd32 <- haven::labelled(cleaned_data$imd32, labels = lbl_int)

# Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

cat('Script completed successfully.')
cat('Variables:', names(cleaned_data), '\n')
cat('Rows:', nrow(cleaned_data), '\n')