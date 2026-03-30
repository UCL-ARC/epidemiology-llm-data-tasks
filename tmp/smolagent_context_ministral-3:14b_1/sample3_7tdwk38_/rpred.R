
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(labelled)

# Set working directory to ensure output is saved correctly
wd <- 'data/output/'
if (!dir.exists(wd)) {
  dir.create(wd)
}

# Step 1: Load datasets
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Convert NSID to character to ensure consistent merging
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_three$NSID <- as.character(wave_three$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Step 2: Rename variables to standardized format
wave_one <- wave_one %>%
  rename(lang_S1 = W1englangYP)

wave_two <- wave_two %>%
  rename(lang_S2 = W2EnglangYP)

wave_three <- wave_three %>%
  rename(lang_S3 = W3englangHH)

wave_four <- wave_four %>%
  rename(lang_S4 = W4EngLangHH)

# Step 3: Define mapping for missing values
missing_value_mapping <- list(
  '-999.0' = -2, '-998.0' = -2, '-997.0' = -2, '-995.0' = -2, '-99.0' = -3,
  '-92.0' = -9, '-91.0' = -1, '-1.0' = -8
)

# Step 4: Function to harmonize missing values
harmonize_missing <- function(x) {
  if (is.numeric(x)) {
    x <- as.character(x)
    x <- ifelse(x %in% names(missing_value_mapping), missing_value_mapping[[x]], x)
    x <- ifelse(x == '' | x == 'NA', -3, x)
    x <- as.numeric(x)
  }
  return(x)
}

# Step 5: Apply missing value harmonization to each wave-specific variable
wave_one <- wave_one %>%
  mutate(lang_S1 = harmonize_missing(lang_S1))

wave_two <- wave_two %>%
  mutate(lang_S2 = harmonize_missing(lang_S2))

wave_three <- wave_three %>%
  mutate(lang_S3 = harmonize_missing(lang_S3))

wave_four <- wave_four %>%
  mutate(lang_S4 = harmonize_missing(lang_S4))

# Step 6: Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = 'NSID') %>%
  full_join(wave_three, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID')

# Step 7: Create consolidated variable for language
merged_data <- merged_data %>%
  mutate(lang = case_when(
    !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
    !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
    !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
    !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
    !is.na(lang_S1) ~ lang_S1,
    !is.na(lang_S2) ~ lang_S2,
    !is.na(lang_S3) ~ lang_S3,
    !is.na(lang_S4) ~ lang_S4,
    TRUE ~ -3
  ))

# Step 8: Select only the ID and consolidated variables
final_data <- merged_data %>%
  select(NSID, lang)

# Step 9: Save the final dataset to the specified directory
output_path <- file.path(wd, 'cleaned_data.csv')
write.csv(final_data, output_path, row.names = FALSE)

# Print confirmation message
message('Data successfully saved to: ', output_path)
