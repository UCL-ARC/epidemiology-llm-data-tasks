# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the four wave files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge all waves using full_join by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Define missing value recoding function based on metadata mappings
recode_missing <- function(x) {
  # Convert to numeric to handle any type issues
  x <- as.numeric(x)
  
  # -999, -998, -997, -995, -94 → -2 (Schedule not applicable)
  x[x == -999 | x == -998 | x == -997 | x == -995 | x == -94] <- -2
  
  # -99 → -3 (Not asked/interview not conducted)
  x[x == -99] <- -3
  
  # -92 → -9 (Refusal)
  x[x == -92] <- -9
  
  # -91 → -1 (Item not applicable)
  x[x == -91] <- -1
  
  # -1 → -8 (Don't know/insufficient information)
  x[x == -1] <- -8
  
  # Any Null value to -3 (Not asked/interview not conducted)
  x[is.na(x)] <- -3
  
  return(x)
}

# Recode missing values for each wave's language variable
merged$W1englangYP <- recode_missing(merged$W1englangYP)
merged$W2EnglangYP <- recode_missing(merged$W2EnglangYP)
merged$W3englangHH <- recode_missing(merged$W3englangHH)
merged$W4EngLangHH <- recode_missing(merged$W4EngLangHH)

# Create wave-specific variables with _S suffix (lowercase variable names)
merged$lang_S1 <- merged$W1englangYP
merged$lang_S2 <- merged$W2EnglangYP
merged$lang_S3 <- merged$W3englangHH
merged$lang_S4 <- merged$W4EngLangHH

# Create consolidated language variable using earliest valid positive value
# For time-invariant characteristics, prioritize earliest valid POSITIVE value
merged$lang <- case_when(
  !is.na(merged$lang_S1) & merged$lang_S1 > 0 ~ merged$lang_S1,
  !is.na(merged$lang_S2) & merged$lang_S2 > 0 ~ merged$lang_S2,
  !is.na(merged$lang_S3) & merged$lang_S3 > 0 ~ merged$lang_S3,
  !is.na(merged$lang_S4) & merged$lang_S4 > 0 ~ merged$lang_S4,
  !is.na(merged$lang_S1) ~ merged$lang_S1,
  !is.na(merged$lang_S2) ~ merged$lang_S2,
  !is.na(merged$lang_S3) ~ merged$lang_S3,
  !is.na(merged$lang_S4) ~ merged$lang_S4,
  TRUE ~ -3
)

# Select only ID variable and final consolidated variables
output <- merged %>%
  select(NSID, lang)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Cleaned data saved to data/output/cleaned_data.csv\n")
cat("Number of records:", nrow(output), "\n")
cat("Variables included:", names(output), "\n")