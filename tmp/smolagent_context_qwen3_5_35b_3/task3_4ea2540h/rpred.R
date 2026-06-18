library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(x) {
  case_when(
    x == -999 ~ -2,
    x == -998 ~ -2,
    x == -997 ~ -2,
    x == -995 ~ -2,
    x == -99 ~ -3,
    x == -94 ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

# Apply mapping to all language variables
merged$W1englangYP <- map_missing(merged$W1englangYP)
merged$W2EnglangYP <- map_missing(merged$W2EnglangYP)
merged$W3englangHH <- map_missing(merged$W3englangHH)
merged$W4EngLangHH <- map_missing(merged$W4EngLangHH)

# Create lang variable using earliest valid response first
# Valid responses are 1-4 (English language categories)
merged$lang <- case_when(
  merged$W1englangYP %in% 1:4 ~ merged$W1englangYP,
  merged$W2EnglangYP %in% 1:4 ~ merged$W2EnglangYP,
  merged$W3englangHH %in% 1:4 ~ merged$W3englangHH,
  merged$W4EngLangHH %in% 1:4 ~ merged$W4EngLangHH,
  TRUE ~ -3  # No valid response - Not asked/not interviewed
)

# Convert lang to factor with labels
merged$lang <- factor(merged$lang, 
                      levels = c(1, 2, 3, 4, -1, -2, -3, -8, -9),
                      labels = c("Yes - English only", 
                                "Yes - English first/main and speaks other languages", 
                                "No - another language is respondent's/household's first/main language", 
                                "Respondent/household is bilingual",
                                "Not applicable", 
                                "Schedule not applicable / script error / information lost",
                                "Not asked / not interviewed", 
                                "Don't know", 
                                "Refused"))

# Create output with only NSID and lang
output <- merged %>%
  select(NSID, lang)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Language distribution:\n")
print(table(output$lang, useNA = "ifany"))
