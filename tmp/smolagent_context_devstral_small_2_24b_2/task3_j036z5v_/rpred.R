library(readr)
library(dplyr)
library(haven)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define a function to map missing values according to the task requirements
map_missing <- function(x) {
  case_when(
    x == -94 ~ -2,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

# Apply missing value mapping to each source variable
merged_data <- merged_data %>%
  mutate(
    W1englangYP = map_missing(W1englangYP),
    W2EnglangYP = map_missing(W2EnglangYP),
    W3englangHH = map_missing(W3englangHH),
    W4EngLangHH = map_missing(W4EngLangHH)
  )

# Derive the consolidated 'lang' variable using earliest valid positive response
merged_data <- merged_data %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    lang = ifelse(is.na(lang), -3, lang)
  )

# Convert 'lang' to a labelled factor with explicit labels
lang_labels <- c(
  "Yes - English only" = 1,
  "Yes - English first/ main and speaks other languages" = 2,
  "No, another language is respondent's first or main language" = 3,
  "Respondent is bilingual" = 4,
  "Schedule not applicable / script error / information lost" = -2,
  "Don't know / insufficient information" = -8,
  "Not asked at the fieldwork stage / not interviewed" = -3
)

merged_data$lang <- factor(merged_data$lang, levels = c(1, 2, 3, 4, -2, -8, -3), labels = lang_labels)

# Select only the ID variable and the final derived variable
output_data <- merged_data %>%
  select(NSID, lang)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")