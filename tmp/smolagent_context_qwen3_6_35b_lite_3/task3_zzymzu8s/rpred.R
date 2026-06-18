# Load required libraries
library(dplyr)
library(readr)
library(haven)
library(labelled)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

# Load each file
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join on NSID
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

cat("Merged dataset dimensions:", dim(df), "\n")
cat("NSID values in merged data:", n_distinct(df$NSID), "\n")

# === Harmonise missing values to standard codes ===

# Wave 1 (Age 14): W1englangYP
df$w1_lang <- recode(df$W1englangYP,
  `-99` = -3,
  `-92` = -9,
  `-91` = -1,
  `-1` = -8
)
df$w1_lang[is.na(df$w1_lang)] <- -3

# Wave 2 (Age 15): W2EnglangYP
df$w2_lang <- recode(df$W2EnglangYP,
  `-998` = -2,
  `-997` = -2,
  `-995` = -2,
  `-99` = -3,
  `-92` = -9,
  `-91` = -1,
  `-1` = -8
)
df$w2_lang[is.na(df$w2_lang)] <- -3

# Wave 3 (Age 16): W3englangHH
df$w3_lang <- recode(df$W3englangHH,
  `-999` = -3,
  `-997` = -2,
  `-99` = -3,
  `-92` = -9,
  `-91` = -1,
  `-1` = -8
)
df$w3_lang[is.na(df$w3_lang)] <- -3

# Wave 4 (Age 17): W4EngLangHH
df$w4_lang <- recode(df$W4EngLangHH,
  `-999` = -3,
  `-997` = -2,
  `-92` = -9,
  `-91` = -1,
  `-1` = -8
)
df$w4_lang[is.na(df$w4_lang)] <- -3

# === Create consolidated lang variable ===
# Use earliest-valid-first for stable construct (language)
df$lang <- df$w1_lang

missing_mask <- is.na(df$lang) | df$lang %in% c(-1, -2, -3, -8, -9)
df$lang[missing_mask] <- df$w2_lang[missing_mask]

missing_mask <- is.na(df$lang) | df$lang %in% c(-1, -2, -3, -8, -9)
df$lang[missing_mask] <- df$w3_lang[missing_mask]

missing_mask <- is.na(df$lang) | df$lang %in% c(-1, -2, -3, -8, -9)
df$lang[missing_mask] <- df$w4_lang[missing_mask]

# Apply value labels using haven::labelled
# Correct format: names are the labels, values are the numeric codes
value_labels_haven <- c(
  "Yes - English only" = 1,
  "Yes - English first/main and speaks other languages" = 2,
  "No, another language is respondent's first/main language" = 3,
  "Household is bilingual" = 4,
  "Not applicable" = -1,
  "Schedule not applicable / script error / information lost" = -2,
  "Not asked / not interviewed" = -3,
  "Don't know" = -8,
  "Refused" = -9
)

df$lang <- haven::labelled(df$lang, labels = value_labels_haven)

# Keep only NSID and lang
df <- df %>% select(NSID, lang)

# Write output
write_csv(df, "data/output/cleaned_data.csv")
cat("Output written successfully\n")
cat("Output dimensions:", dim(df), "\n")
cat("Unique NSID values:", n_distinct(df$NSID), "\n")

# Print summary of lang variable
print(table(df$lang, useNA = "ifany"))
