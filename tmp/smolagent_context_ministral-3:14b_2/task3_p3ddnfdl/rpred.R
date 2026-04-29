
# Load required packages
library(dplyr)
library(readr)

# Step 1: Load datasets with explicit column types and suppress warnings
suppressPackageStartupMessages({
  wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
  wave_two <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
  wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
  wave_four <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
})

# Ensure NSID is character
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_three$NSID <- as.character(wave_three$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Step 2: Rename variables
wave_one <- wave_one %>% rename(lang_S1 = W1englangYP)
wave_two <- wave_two %>% rename(lang_S2 = W2EnglangYP)
wave_three <- wave_three %>% rename(lang_S3 = W3englangHH)
wave_four <- wave_four %>% rename(lang_S4 = W4EngLangHH)

# Step 3: Create recoding function
recode_missing <- function(x) {
  x <- as.numeric(x)
  # Handle specific missing values
  x[x == -999] <- -2; x[x == -998] <- -2; x[x == -997] <- -2; x[x == -995] <- -2
  x[x == -99] <- -3; x[x == -92] <- -9; x[x == -91] <- -1; x[x == -1] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Step 4: Apply recoding
wave_one$lang_S1 <- recode_missing(wave_one$lang_S1)
wave_two$lang_S2 <- recode_missing(wave_two$lang_S2)
wave_three$lang_S3 <- recode_missing(wave_three$lang_S3)
wave_four$lang_S4 <- recode_missing(wave_four$lang_S4)

# Step 5: Merge datasets
merged_data <- full_join(wave_one, wave_two, by = 'NSID') %>%
  full_join(wave_three, by = 'NSID') %>%
  full_join(wave_four, by = 'NSID')

# Step 6: Create consolidated variable
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

# Step 7: Select final columns and save
final_data <- merged_data %>% select(NSID, lang)
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)

# Confirmation message
message('Data cleaning completed successfully!')
message('Output file saved to: data/output/cleaned_data.csv')
message(paste('Dataset contains', nrow(final_data), 'rows and', ncol(final_data), 'columns'))
