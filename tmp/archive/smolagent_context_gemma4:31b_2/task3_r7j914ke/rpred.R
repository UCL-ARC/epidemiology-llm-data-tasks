library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define file paths
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets
# Use read_delim and let R infer types, but ensure NSID is treated consistently (likely numeric)
data1 <- read_delim(file1, delim = "\t")
data2 <- read_delim(file2, delim = "\t")
data3 <- read_delim(file3, delim = "\t")
data4 <- read_delim(file4, delim = "\t")

# 2. Variable Naming and selection
# We must ensure NSID is unique per file before joining to avoid the Cartesian product (many-to-many join)
# We select only the needed columns and remove duplicates based on NSID

data1_clean <- data1 %>% 
  select(NSID, W1englangYP) %>% 
  distinct(NSID, .keep_all = TRUE) %>% 
  rename(lang_s1 = W1englangYP)

data2_clean <- data2 %>% 
  select(NSID, W2EnglangYP) %>% 
  distinct(NSID, .keep_all = TRUE) %>% 
  rename(lang_s2 = W2EnglangYP)

data3_clean <- data3 %>% 
  select(NSID, W3englangHH) %>% 
  distinct(NSID, .keep_all = TRUE) %>% 
  rename(lang_s3 = W3englangHH)

data4_clean <- data4 %>% 
  select(NSID, W4EngLangHH) %>% 
  distinct(NSID, .keep_all = TRUE) %>% 
  rename(lang_s4 = W4EngLangHH)

# Merge datasets using full_join
df <- data1_clean %>%
  full_join(data2_clean, by = "NSID") %>%
  full_join(data3_clean, by = "NSID") %>%
  full_join(data4_clean, by = "NSID")

# 5. Missing Value Code Harmonization
harmonize_missing <- function(x) {
  # Convert NA to -3
  x <- ifelse(is.na(x), -3, x)
  # Apply specific mapping
  x <- case_when(
    x %in% c(-999, -998, -997, -995, -94) ~ -2,
    x == -99 ~ -3,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
  return(x)
}

df <- df %>%
  mutate(across(starts_with("lang_s"), harmonize_missing))

# 6. Time-Invariant consolidated variable
df <- df %>%
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

# 10. Variables to Include in Output
final_df <- df %>% select(NSID, lang)

# 9. Output Requirements
write_csv(final_df, "data/output/cleaned_data.csv")
