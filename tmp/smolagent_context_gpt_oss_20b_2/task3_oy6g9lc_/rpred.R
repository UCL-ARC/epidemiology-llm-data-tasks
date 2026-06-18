library(readr)
library(dplyr)
library(purrr)

# Helper: map source missing codes → standard codes
clean_lang_col <- function(x) {
  case_when(
    is.na(x) ~ NA_real_,
    x %in% c(-99, -99.0) ~ -3,          # YP not interviewed
    x %in% c(-92, -92.0) ~ -9,          # Refused
    x %in% c(-91, -91.0) ~ -1,          # Not applicable
    x %in% c(-1, -1.0) ~ -8,            # Don\'t know / insufficient info
    x %in% c(-998, -998.0, -997, -997.0,
             -995, -995.0, -999, -999.0,
             -94, -94.0) ~ -2,            # schedule not applicable / script error / info lost
    TRUE ~ x
  )
}

# Read wave files (tab-delimited, all columns as character)
read_wave <- function(file_path) {
  read_delim(file_path,
             delim = "\t",
             col_types = cols(.default = col_character(),
                               NSID = col_character()))
}

file1 <- read_wave("data/input/wave_one_lsype_young_person_2020.tab")
file2 <- read_wave("data/input/wave_two_lsype_young_person_2020.tab")
file3 <- read_wave("data/input/wave_three_lsype_family_background_2020.tab")
file4 <- read_wave("data/input/wave_four_lsype_family_background_2020.tab")

# Convert target columns to numeric and clean missing codes
file1 <- file1 %>%
  mutate(
    W1englangYP = as.numeric(W1englangYP),
    W1englangYP_clean = clean_lang_col(W1englangYP)
  )

file2 <- file2 %>%
  mutate(
    W2EnglangYP = as.numeric(W2EnglangYP),
    W2EnglangYP_clean = clean_lang_col(W2EnglangYP)
  )

file3 <- file3 %>%
  mutate(
    W3englangHH = as.numeric(W3englangHH),
    W3englangHH_clean = clean_lang_col(W3englangHH)
  )

file4 <- file4 %>%
  mutate(
    W4EngLangHH = as.numeric(W4EngLangHH),
    W4EngLangHH_clean = clean_lang_col(W4EngLangHH)
  )

# Merge all waves on NSID
merged <- reduce(list(file1, file2, file3, file4), full_join, by = "NSID")

# Derive consolidated lang variable (earliest valid positive response first)
merged <- merged %>%
  mutate(lang = case_when(
    !is.na(W1englangYP_clean) & W1englangYP_clean %in% 1:4 ~ W1englangYP_clean,
    !is.na(W2EnglangYP_clean) & W2EnglangYP_clean %in% 1:4 ~ W2EnglangYP_clean,
    !is.na(W3englangHH_clean) & W3englangHH_clean %in% 1:4 ~ W3englangHH_clean,
    !is.na(W4EngLangHH_clean) & W4EngLangHH_clean %in% 1:4 ~ W4EngLangHH_clean,
    TRUE ~ NA_real_
  ))

# Keep only required output variables
final_df <- merged %>% select(NSID, lang)

# Write cleaned data to CSV
write_csv(final_df, "data/output/cleaned_data.csv")