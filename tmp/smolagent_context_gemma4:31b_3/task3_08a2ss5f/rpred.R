library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files from metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Load and rename variables based on metadata and instructions
# Wave 1
df1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t') %>% 
  select(nsid = NSID, lang_s1 = W1englangYP) %>% 
  rename_with(tolower)

# Wave 2
df2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t') %>% 
  select(nsid = NSID, lang_s2 = W2EnglangYP) %>% 
  rename_with(tolower)

# Wave 3
df3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t') %>% 
  select(nsid = NSID, lang_s3 = W3englangHH) %>% 
  rename_with(tolower)

# Wave 4
df4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t') %>% 
  select(nsid = NSID, lang_s4 = W4EngLangHH) %>% 
  rename_with(tolower)

# Merge datasets
full_data <- df1 %>%
  full_join(df2, by = 'nsid') %>%
  full_join(df3, by = 'nsid') %>%
  full_join(df4, by = 'nsid')

# 5. Missing Value Code Harmonization function
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  case_when(
    is.na(x) ~ -3,
    x %in% c(-999, -998, -997, -995, -94) ~ -2,
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

# Apply harmonization to wave-specific variables
full_data <- full_data %>%
  mutate(across(starts_with('lang_s'), harmonize_missing))

# 6. Time-Invariant Consolidation for 'lang'
full_data <- full_data %>%
  mutate(lang = case_when(
    !is.na(lang_s1) & lang_s1 > 0 ~ lang_s1,
    !is.na(lang_s2) & lang_s2 > 0 ~ lang_s2,
    !is.na(lang_s3) & lang_s3 > 0 ~ lang_s3,
    !is.na(lang_s4) & lang_s4 > 0 ~ lang_s4,
    !is.na(lang_s1) ~ lang_s1,
    !is.na(lang_s2) ~ lang_s2,
    !is.na(lang_s3) ~ lang_s3,
    !is.na(lang_s4) ~ lang_s4,
    TRUE ~ -3
  ))

# 10. Variables to Include in Output: NSID and consolidated variables
final_df <- full_data %>%
  select(nsid, lang)

# 9. Output Requirements
write_csv(final_df, 'data/output/cleaned_data.csv')