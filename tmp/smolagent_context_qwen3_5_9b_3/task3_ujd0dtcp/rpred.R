library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all 4 files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Combine into one dataset using full_join
data <- w1

for (file in list(w2, w3, w4)) {
  data <- full_join(data, file, by = "NSID", keep = FALSE)
}

# Derive lang variable using earliest valid positive response
# Order: W1englangYP -> W2EnglangYP -> W3englangHH -> W4EngLangHH
# Valid positive values are 1, 2, 3, 4
# Missing codes to consider as invalid: -3, -2, -9, -1, -8, NA

# First, harmonize missing value codes
# Convert R NA to -3 (not asked)
# Convert source -94 to -2
# Convert source -1 labelled "Don't know" to -8
# -999, -998, -997, -995, -99 map to -2 (schedule not applicable/script error)
# -92 maps to -9 (refused)
# -91 maps to -1 (not applicable)

# Apply harmonization to all language variables
data <- data %>%
  mutate(
    W1englangYP = if_else(is.na(W1englangYP), -3,
      if_else(W1englangYP == -94, -2,
        if_else(W1englangYP == -1, -8, W1englangYP)
      )
    ),
    W2EnglangYP = if_else(is.na(W2EnglangYP), -3,
      if_else(W2EnglangYP == -94, -2,
        if_else(W2EnglangYP == -1, -8, W2EnglangYP)
      )
    ),
    W3englangHH = if_else(is.na(W3englangHH), -3,
      if_else(W3englangHH == -94, -2,
        if_else(W3englangHH == -1, -8, W3englangHH)
      )
    ),
    W4EngLangHH = if_else(is.na(W4EngLangHH), -3,
      if_else(W4EngLangHH == -94, -2,
        if_else(W4EngLangHH == -1, -8, W4EngLangHH)
      )
    )
  )

# Derive lang variable using earliest valid positive response
# Valid positive values are 1, 2, 3, 4
# All negative codes and NA are considered invalid

lang_data <- data %>%
  
  # Start with W1
  mutate(lang = W1englangYP) %>%
  
  # Update with W2 where W1 is invalid (negative codes or NA)
  mutate(lang = if_else(is.na(lang) | lang < 0, W2EnglangYP, lang)) %>%
  
  # Update with W3 where W1/W2 are invalid
  mutate(lang = if_else(is.na(lang) | lang < 0, W3englangHH, lang)) %>%
  
  # Update with W4 where W1/W2/W3 are invalid
  mutate(lang = if_else(is.na(lang) | lang < 0, W4EngLangHH, lang))

# Create factor with proper levels
# Valid categories: 1, 2, 3, 4
# Missing codes: -9 (refused), -2 (not asked), -8 (don't know), -1 (not applicable), -3 (not asked)

lang_data <- lang_data %>%
  mutate(
    lang = factor(
      lang,
      levels = c(1, 2, 3, 4, -9, -2, -8, -3),
      labels = c("Yes - English only", "Yes - English first/ main and speaks other languages", "No, another language is respondent's first or main language", "Respondent is bilingual", "Refused", "Not asked", "Don't know", "Not asked")
    )
  )

# Keep only NSID and lang
cleaned <- lang_data %>%
  select(NSID, lang)

# Write to CSV
write_csv(cleaned, "data/output/cleaned_data.csv")

# Print summary
cat("Number of observations:", nrow(cleaned), "\n")
cat("Variable classes:", sapply(cleaned, class), "\n")
cat("\nLang frequency table:\n")
tabs <- table(cleaned$lang)
print(tabs)

# Verify structure
cat("\nFirst 5 rows:\n")
print(head(cleaned, 5))