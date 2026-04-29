library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('../output')) {
  dir.create('../output', recursive = TRUE)
}

# Set working directory to where input files are
setwd('data/input')

# Load all datasets
wave_one <- read_delim('wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('wave_seven_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('ns8_2015_self_completion.tab', delim = '\t')
ns9 <- read_delim('ns9_2022_main_interview.tab', delim = '\t')

# Function to convert NA to -3
convert_na_to_minus3 <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- -3
    } else if (is.factor(x)) {
      levels(x)[levels(x) == 'NA'] <- '-3'
      x <- as.integer(x)
      x[is.na(x)] <- -3
      factor(x, levels = levels(x))
    } else {
      x
    }
  })
  return(df)
}

# Apply NA conversion
wave_one <- convert_na_to_minus3(wave_one)
wave_four <- convert_na_to_minus3(wave_four)
wave_six <- convert_na_to_minus3(wave_six)
wave_seven <- convert_na_to_minus3(wave_seven)
ns8 <- convert_na_to_minus3(ns8)
ns9 <- convert_na_to_minus3(ns9)

# Convert wave-specific missing codes to standard codes
wave_six <- wave_six %>%
  mutate(
    W6SexualityYP = case_when(
      W6SexualityYP == -999 | W6SexualityYP == -97 ~ -97,
      W6SexualityYP == -92 ~ -92,
      W6SexualityYP == -91 ~ -91,
      W6SexualityYP == -1 ~ -1,
      TRUE ~ W6SexualityYP
    )
  )

wave_seven <- wave_seven %>%
  mutate(
    W7SexualityYP = case_when(
      W7SexualityYP %in% c(-100, -97) ~ -97,
      W7SexualityYP == -92 ~ -92,
      W7SexualityYP == -91 ~ -91,
      W7SexualityYP == -1 ~ -1,
      TRUE ~ W7SexualityYP
    )
  )

ns8 <- ns8 %>%
  mutate(
    W8SEXUALITY = case_when(
      W8SEXUALITY == -9 ~ -9,
      W8SEXUALITY == -8 ~ -8,
      W8SEXUALITY == -1 ~ -1,
      TRUE ~ W8SEXUALITY
    )
  )

ns9 <- ns9 %>%
  mutate(
    W9SORI = case_when(
      W9SORI == -9 ~ -9,
      W9SORI == -8 ~ -8,
      W9SORI == -3 ~ -3,
      W9SORI == -1 ~ -1,
      TRUE ~ W9SORI
    )
  )

# Merge datasets by NSID
print('Merging datasets...')
merged <- full_join(wave_one, wave_four, by = 'NSID')
merged <- full_join(merged, wave_six, by = 'NSID')
merged <- full_join(merged, wave_seven, by = 'NSID')
merged <- full_join(merged, ns8, by = 'NSID')
merged <- full_join(merged, ns9, by = 'NSID')

print(paste('Merged dimensions:', nrow(merged), 'x', ncol(merged)))

# Create harmonized sexuality variable - use most recent valid response
merged <- merged %>%
  mutate(
    Sex_Harmonized = case_when(
      !is.na(W9SORI) & W9SORI %in% c(-9, -8, -3, -1, 1, 2, 3, 4, 5) ~ W9SORI,
      !is.na(W8SEXUALITY) & W8SEXUALITY %in% c(-9, -8, -1, 1, 2, 3, 4) ~ W8SEXUALITY,
      !is.na(W7SexualityYP) & W7SexualityYP %in% c(-97, -92, -91, -1, 1, 2, 3, 4) ~ W7SexualityYP,
      !is.na(W6SexualityYP) & W6SexualityYP %in% c(-97, -92, -91, -1, 1, 2, 3, 4) ~ W6SexualityYP,
      !is.na(W4SexYP) & W4SexYP %in% c(-9, -8, -1, 1, 2, 3, 4) ~ W4SexYP,
      !is.na(W1sexYP) & W1sexYP %in% c(-9, -8, -1, 1, 2, 3, 4) ~ W1sexYP,
      TRUE ~ NA_real_
    )
  )

# Convert to factor with labels
merged$Sex_Harmonized <- factor(merged$Sex_Harmonized,
  levels = c(-9, -8, -7, -3, -1, 1, 2, 3, 4, 5),
  labels = c('Refused', 'Don\'t know', 'Prefer not to say', 'Not asked',
             'Not applicable', 'Heterosexual / Straight', 'Gay / Lesbian', 
             'Bisexual', 'Other', 'Prefer not to say'))

print('Saving to output...')
# Write to output directory (relative to project root)
write_csv(merged, '../output/cleaned_data.csv')
print('Done!')
