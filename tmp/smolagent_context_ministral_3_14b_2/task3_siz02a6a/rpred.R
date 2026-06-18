
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define paths and filenames
input_path <- "data/input/"
output_path <- "data/output/"

# Load metadata files into separate objects
wave1 <- read_delim(paste0(input_path, "wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave2 <- read_delim(paste0(input_path, "wave_two_lsype_young_person_2020.tab"), delim = "\t")
wave3 <- read_delim(paste0(input_path, "wave_three_lsype_family_background_2020.tab"), delim = "\t")
wave4 <- read_delim(paste0(input_path, "wave_four_lsype_family_background_2020.tab"), delim = "\t")

# Convert NSID to character to ensure consistent merging
wave1$NSID <- as.character(wave1$NSID)
wave2$NSID <- as.character(wave2$NSID)
wave3$NSID <- as.character(wave3$NSID)
wave4$NSID <- as.character(wave4$NSID)

# Ensure all relevant columns are numeric
wave1$W1englangYP <- as.numeric(wave1$W1englangYP)
wave2$W2EnglangYP <- as.numeric(wave2$W2EnglangYP)
wave3$W3englangHH <- as.numeric(wave3$W3englangHH)
wave4$W4EngLangHH <- as.numeric(wave4$W4EngLangHH)

# Merge all datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define the mapping for missing values
missing_value_mapping <- list(
  "-999" = -3,
  "-998" = -2,
  "-997" = -2,
  "-995" = -2,
  "-99" = -3,
  "-94" = -2,
  "-92" = -9,
  "-91" = -1,
  "-1" = -8
)

# Create a function to map missing values for a vector
map_missing_values <- function(x) {
  x <- as.numeric(x)
  is_na <- is.na(x)
  x[is_na] <- -3
  for (code in names(missing_value_mapping)) {
    x[x == as.numeric(code)] <- missing_value_mapping[[code]]
  }
  return(x)
}

# Derive the lang variable using the priority order
merged_data <- merged_data %>%
  mutate(
    lang = ifelse(!is.na(W1englangYP), map_missing_values(W1englangYP),
                  ifelse(!is.na(W2EnglangYP), map_missing_values(W2EnglangYP),
                         ifelse(!is.na(W3englangHH), map_missing_values(W3englangHH),
                                ifelse(!is.na(W4EngLangHH), map_missing_values(W4EngLangHH), -3))))
  )

# Remove source variables to keep only the derived lang variable and NSID
final_data <- merged_data %>%
  select(NSID, lang)

# Ensure lang is a labeled factor with appropriate labels
lang_labels <- c(
  "1" = "English only",
  "2" = "English first/main and speaks other languages",
  "3" = "Another language is first/main",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked/Not interviewed",
  "-2" = "Schedule not applicable/Script error",
  "-1" = "Item not applicable"
)

final_data$lang <- factor(final_data$lang, levels = c(1, 2, 3, 4, -9, -8, -7, -3, -2, -1),
                          labels = lang_labels)

# Write the final data to CSV
write_csv(final_data, paste0(output_path, "cleaned_data.csv"))

# Print confirmation
cat("Final cleaned data has been written to data/output/cleaned_data.csv\n")
