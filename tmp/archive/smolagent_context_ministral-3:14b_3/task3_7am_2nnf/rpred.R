
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(vroom)

# Set memory options to handle large datasets
options(Ncpus = parallel::detectCores() - 1)
options(vroom_progress_bar = FALSE)

# Step 1: Load datasets incrementally with explicit column types for NSID
cat('Loading wave_one data...\n')
wave_one <- vroom('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_integer()))
wave_one$NSID <- as.integer(wave_one$NSID)

cat('Loading wave_two data...\n')
wave_two <- vroom('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_integer()))
wave_two$NSID <- as.integer(wave_two$NSID)

cat('Loading wave_three data...\n')
wave_three <- vroom('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = cols(NSID = col_integer()))
wave_three$NSID <- as.integer(wave_three$NSID)

cat('Loading wave_four data...\n')
wave_four <- vroom('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = cols(NSID = col_integer()))
wave_four$NSID <- as.integer(wave_four$NSID)

# Step 2: Merge datasets incrementally using left_join
cat('Merging datasets...\n')
merged_data <- wave_one %>%
  left_join(wave_two, by = 'NSID', relationship = 'many-to-many') %>%
  left_join(wave_three, by = 'NSID', relationship = 'many-to-many') %>%
  left_join(wave_four, by = 'NSID', relationship = 'many-to-many')

# Step 3: Rename variables to follow naming conventions
cat('Renaming variables...\n')
merged_data <- merged_data %>%
  rename(
    lang_S1 = W1englangYP,
    lang_S2 = W2EnglangYP,
    lang_S3 = W3englangHH,
    lang_S4 = W4EngLangHH
  )

# Step 4: Standardize missing value codes
standardize_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995, -94) ~ -2,
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

cat('Standardizing missing values...\n')
merged_data <- merged_data %>%
  mutate(
    lang_S1 = ifelse(is.null(lang_S1), -3, standardize_missing(lang_S1)),
    lang_S2 = ifelse(is.null(lang_S2), -3, standardize_missing(lang_S2)),
    lang_S3 = ifelse(is.null(lang_S3), -3, standardize_missing(lang_S3)),
    lang_S4 = ifelse(is.null(lang_S4), -3, standardize_missing(lang_S4))
  )

# Step 5: Create consolidated variable for language
cat('Creating consolidated language variable...\n')
consolidated_lang <- case_when(
  !is.na(merged_data$lang_S1) & merged_data$lang_S1 > 0 ~ merged_data$lang_S1,
  !is.na(merged_data$lang_S2) & merged_data$lang_S2 > 0 ~ merged_data$lang_S2,
  !is.na(merged_data$lang_S3) & merged_data$lang_S3 > 0 ~ merged_data$lang_S3,
  !is.na(merged_data$lang_S4) & merged_data$lang_S4 > 0 ~ merged_data$lang_S4,
  !is.na(merged_data$lang_S1) ~ merged_data$lang_S1,
  !is.na(merged_data$lang_S2) ~ merged_data$lang_S2,
  !is.na(merged_data$lang_S3) ~ merged_data$lang_S3,
  !is.na(merged_data$lang_S4) ~ merged_data$lang_S4,
  TRUE ~ -3
)

merged_data$lang <- consolidated_lang

# Step 6: Select only required variables (NSID and consolidated variables)
cat('Selecting final variables...\n')
final_data <- merged_data %>%
  select(NSID, lang)

# Step 7: Write output to CSV
cat('Writing output to file...\n')
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Process completed successfully!\n')
