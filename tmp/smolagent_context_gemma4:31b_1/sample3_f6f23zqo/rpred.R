library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets and ensure NSID is treated as character to avoid numeric precision issues during join
# We only load the columns we need to avoid memory issues and parsing errors
load_and_clean <- function(path, var_name, wave_suffix) {
  df <- read_delim(path, delim = "\t", col_types = cols(NSID = "character", .default = "numeric"))
  df <- df %>% select(NSID, !!sym(paste0("lang_", wave_suffix)) := !!sym(var_name))
  return(df)
}

df1 <- load_and_clean(file1, "W1englangYP", "S1")
df2 <- load_and_clean(file2, "W2EnglangYP", "S2")
df3 <- load_and_clean(file3, "W3englangHH", "S3")
df4 <- load_and_clean(file4, "W4EngLangHH", "S4")

# Remove potential duplicates of NSID within each wave to prevent many-to-many join explosion
df1 <- df1 %>% distinct(NSID, .keep_all = TRUE)
df2 <- df2 %>% distinct(NSID, .keep_all = TRUE)
df3 <- df3 %>% distinct(NSID, .keep_all = TRUE)
df4 <- df4 %>% distinct(NSID, .keep_all = TRUE)

# Merge datasets
merged_df <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID")

# Function to harmonize missing value codes
harmonize_missing <- function(x) {
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

# Apply harmonization to wave variables
merged_df <- merged_df %>%
  mutate(across(starts_with("lang_S"), harmonize_missing))

# Consolidate time-invariant variable 'lang'
merged_df <- merged_df %>%
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

# Final selection: ID and consolidated variables
final_df <- merged_df %>% select(NSID, lang)

# Save to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
