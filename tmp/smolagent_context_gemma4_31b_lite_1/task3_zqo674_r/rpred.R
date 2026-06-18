library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Convert relevant columns to numeric
w1$W1englangYP <- as.numeric(w1$W1englangYP)
w2$W2EnglangYP <- as.numeric(w2$W2EnglangYP)
w3$W3englangHH <- as.numeric(w3$W3englangHH)
w4$W4EngLangHH <- as.numeric(w4$W4EngLangHH)

# Merge datasets
df <- w1 %>% 
  full_join(w2, by = "NSID") %>% 
  full_join(w3, by = "NSID") %>% 
  full_join(w4, by = "NSID")

# Define harmonisation function for missing values based on labels
harmonise_missing <- function(val, labels) {
  # The labels provided in metadata are keys to the value_labels dictionary
  # We map the value to the standard missing codes
  # -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule not applicable/error, -1 Not applicable
  
  case_when(
    is.na(val) ~ -3,
    # Specific mappings based on labels from metadata
    # Refused
    val == -92.0 ~ -9,
    # Don't know
    val == -1.0 ~ -8,
    # Not applicable
    val == -91.0 ~ -1,
    # Not interviewed / HH grid missing
    val == -99.0 ~ -3,
    val == -999.0 ~ -3,
    # Script error / Interviewer missed / Missing history
    val == -998.0 ~ -2,
    val == -997.0 ~ -2,
    val == -995.0 ~ -2,
    TRUE ~ val
  )
}

# Apply harmonisation to source variables
# W1
df$w1_clean <- harmonise_missing(df$W1englangYP, NULL)
# W2
df$w2_clean <- harmonise_missing(df$W2EnglangYP, NULL)
# W3
df$w3_clean <- harmonise_missing(df$W3englangHH, NULL)
# W4
df$w4_clean <- harmonise_missing(df$W4EngLangHH, NULL)

# Consolidate variable 'lang' using earliest-valid-first
# Valid substantive responses are 1, 2, 3, 4
# We prioritise substantive values over missing codes

df <- df %>% 
  mutate(lang = case_when(
    w1_clean >= 1 & w1_clean <= 4 ~ w1_clean,
    w2_clean >= 1 & w2_clean <= 4 ~ w2_clean,
    w3_clean >= 1 & w3_clean <= 4 ~ w3_clean,
    w4_clean >= 1 & w4_clean <= 4 ~ w4_clean,
    # If no substantive, use the first available missing code (earliest wave)
    !is.na(w1_clean) ~ w1_clean,
    !is.na(w2_clean) ~ w2_clean,
    !is.na(w3_clean) ~ w3_clean,
    !is.na(w4_clean) ~ w4_clean,
    TRUE ~ -3
  ))

# Create labelled factor for 'lang'
# Categories: 1: Yes - English only, 2: Yes - English first/main and speaks other languages, 
# 3: No - another language is first/main, 4: Bilingual/Household bilingual
# Missing: -9 Refusal, -8 Don't know, -1 Not applicable, -3 Not asked, -2 Schedule error

lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No - another language is first/main language",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable"
)

df$lang <- factor(df$lang, levels = as.numeric(names(lang_labels)), labels = lang_labels)

# Final selection
final_df <- df %>% select(NSID, lang)

write_csv(final_df, "data/output/cleaned_data.csv")