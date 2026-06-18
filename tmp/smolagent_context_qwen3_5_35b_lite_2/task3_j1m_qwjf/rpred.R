library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge all files by NSID
cleaned <- full_join(w1, w2, by = "NSID")
cleaned <- full_join(cleaned, w3, by = "NSID")
cleaned <- full_join(cleaned, w4, by = "NSID")

# Function to convert wave-specific missing codes to standard codes
standardize_missing <- function(x, label_map) {
  # Map source codes to standard codes based on label meaning
  result <- x
  result[x == -999] <- -3   # Not asked / not interviewed
  result[x == -998] <- -2   # Schedule not applicable
  result[x == -997] <- -2   # Script error
  result[x == -995] <- -2   # Information lost
  result[x == -99] <- -3    # Not asked
  result[x == -92] <- -9    # Refusal
  result[x == -91] <- -1    # Not applicable
  result[x == -1] <- -8     # Don't know
  result[is.na(result)] <- -3  # R NA to not asked
  return(result)
}

# Standardize missing codes for each wave variable
cleaned$W1englangYP_std <- standardize_missing(cleaned$W1englangYP, NULL)
cleaned$W2EnglangYP_std <- standardize_missing(cleaned$W2EnglangYP, NULL)
cleaned$W3englangHH_std <- standardize_missing(cleaned$W3englangHH, NULL)
cleaned$W4EngLangHH_std <- standardize_missing(cleaned$W4EngLangHH, NULL)

# Create consolidated lang variable using earliest-valid-first
# Priority: W1 (age 14) > W2 (age 15) > W3 (age 16) > W4 (age 17)
cleaned$lang <- case_when(
  cleaned$W1englangYP_std >= 1 & cleaned$W1englangYP_std <= 4 ~ cleaned$W1englangYP_std,
  cleaned$W2EnglangYP_std >= 1 & cleaned$W2EnglangYP_std <= 4 ~ cleaned$W2EnglangYP_std,
  cleaned$W3englangHH_std >= 1 & cleaned$W3englangHH_std <= 4 ~ cleaned$W3englangHH_std,
  cleaned$W4EngLangHH_std >= 1 & cleaned$W4EngLangHH_std <= 4 ~ cleaned$W4EngLangHH_std,
  TRUE ~ -3  # All waves missing
)

# Create labelled factor for lang
lang_labels <- c(
  "1" = "English only",
  "2" = "English first/main and other languages",
  "3" = "Another language is first/main",
  "4" = "Bilingual"
)

lang_missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable"
)

cleaned$lang <- factor(cleaned$lang, 
                       levels = c(1, 2, 3, 4, -9, -8, -7, -3, -2, -1),
                       labels = c("English only", "English first/main and other languages", 
                                  "Another language is first/main", "Bilingual",
                                  "Refusal", "Don't know", "Prefer not to say", 
                                  "Not asked", "Schedule not applicable", "Not applicable"))

# Keep only ID and final derived variable
output <- cleaned %>% select(NSID, lang)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
print(paste("Output dimensions:", nrow(output), "rows,", ncol(output), "columns"))
print(summary(output$lang))