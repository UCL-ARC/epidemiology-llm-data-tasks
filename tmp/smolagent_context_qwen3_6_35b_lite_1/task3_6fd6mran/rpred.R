library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID using full_join
cleaned <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to harmonise missing values based on label meaning
harmonise_missing <- function(x) {
  case_when(
    x %in% c(-999, -998, -997, -995) ~ -2L,  # Schedule not applicable / script error / info lost
    x == -99 ~ -3L,  # Not interviewed / not asked
    x == -92 ~ -9L,  # Refusal
    x == -91 ~ -1L,  # Not applicable
    x == -1 ~ -8L,   # Don't know
    !is.na(x) ~ as.integer(x)
  )
}

# Map NA to -3 (not asked)
map_na_to_missing <- function(x) {
  ifelse(is.na(x), -3L, harmonise_missing(x))
}

# Harmonise each wave's variable
cleaned$W1englangYP_clean <- map_na_to_missing(cleaned$W1englangYP)
cleaned$W2EnglangYP_clean <- map_na_to_missing(cleaned$W2EnglangYP)
cleaned$W3englangHH_clean <- map_na_to_missing(cleaned$W3englangHH)
cleaned$W4EngLangHH_clean <- map_na_to_missing(cleaned$W4EngLangHH)

# Consolidation function using sequential assignment (earliest-valid-first)
consolidate_lang <- function(w1, w2, w3, w4) {
  result <- rep(-3L, length(w1))  # Default: not asked
  
  valid_vals <- c(1L, 2L, 3L, 4L)
  
  # Step 1: Check wave 1 (age 14) - earliest
  mask_w1 <- !is.na(w1) & w1 %in% valid_vals
  result[mask_w1] <- as.integer(w1[mask_w1])
  
  # Step 2: Check wave 2 (age 15) - only where wave 1 had no valid value
  mask_w2 <- !mask_w1 & !is.na(w2) & w2 %in% valid_vals
  result[mask_w2] <- as.integer(w2[mask_w2])
  
  # Step 3: Check wave 3 (age 16)
  mask_w3 <- !mask_w1 & !mask_w2 & !is.na(w3) & w3 %in% valid_vals
  result[mask_w3] <- as.integer(w3[mask_w3])
  
  # Step 4: Check wave 4 (age 17)
  mask_w4 <- !mask_w1 & !mask_w2 & !mask_w3 & !is.na(w4) & w4 %in% valid_vals
  result[mask_w4] <- as.integer(w4[mask_w4])
  
  # For remaining NA/missing, determine best missing code
  still_na <- is.na(result)
  if (any(still_na)) {
    for (i in which(still_na)) {
      vals <- c(w1[i], w2[i], w3[i], w4[i])
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        result[i] <- -3L  # Not asked
      } else if (-9L %in% vals) {
        result[i] <- -9L  # Refused
      } else if (-1L %in% vals) {
        result[i] <- -1L  # Not applicable
      } else if (-8L %in% vals) {
        result[i] <- -8L  # Don't know
      } else {
        result[i] <- -3L  # Default
      }
    }
  }
  
  as.integer(result)
}

cleaned$lang <- consolidate_lang(
  cleaned$W1englangYP_clean,
  cleaned$W2EnglangYP_clean,
  cleaned$W3englangHH_clean,
  cleaned$W4EngLangHH_clean
)

# Create labelled factor with proper labels
lang_labels <- c(
  "Yes - English only" = 1,
  "Yes - English first/main and speaks other languages" = 2,
  "No, another language is first/main language" = 3,
  "Bilingual" = 4,
  "Not asked / not interviewed" = -3,
  "Schedule not applicable / script error" = -2,
  "Not applicable" = -1,
  "Don't know" = -8,
  "Refused" = -9
)

cleaned$lang <- labelled::labelled(
  cleaned$lang,
  labels = lang_labels
)

# Select only NSID and lang for output
cleaned_output <- cleaned %>%
  select(NSID, lang)

# Write to CSV
write_csv(cleaned_output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_output), "\n")
cat("\nValue distribution:\n")
print(table(cleaned$lang, useNA = "ifany"))
