# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Suppress unnecessary messages
suppressPackageStartupMessages(
  library(dplyr)
)

# Load files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = col_character()))
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = col_character()))
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character()))
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = col_character()))

# Print the number of rows in each dataset to verify loading
cat("Wave 1 rows:", nrow(wave_one), "\n")
cat("Wave 2 rows:", nrow(wave_two), "\n")
cat("Wave 3 rows:", nrow(wave_three), "\n")
cat("Wave 4 rows:", nrow(wave_four), "\n")

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")

# Print the number of rows in merged data
cat("Merged data rows:", nrow(merged_data), "\n")

# Function to harmonize missing values
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -998 | x == -997 | x == -995] <- -2
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Apply mapping to each language variable
merged_data <- merged_data %>%
  mutate(
    W1englangYP = harmonize_missing(W1englangYP),
    W2EnglangYP = harmonize_missing(W2EnglangYP),
    W3englangHH = harmonize_missing(W3englangHH),
    W4EngLangHH = harmonize_missing(W4EngLangHH)
  )

# Consolidate 'lang' variable
consolidated_lang <- merged_data %>%
  group_by(NSID) %>%
  summarise(lang = coalesce(
    first(W1englangYP[!is.na(W1englangYP)]),
    first(W2EnglangYP[!is.na(W2EnglangYP)]),
    first(W3englangHH[!is.na(W3englangHH)]),
    first(W4EngLangHH[!is.na(W4EngLangHH)]),
    -3  # Default value if all are NA
  ))

# Print the number of rows in consolidated data
cat("Consolidated data rows:", nrow(consolidated_lang), "\n")

# Define labels for the 'lang' variable
lang_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4)
lang_labels <- c("Refusal", "Don't know", "Prefer not to say", "Not asked / not interviewed", 
                 "Schedule not applicable / script error", "Item not applicable", 
                 "English only", "English first/main and speaks other languages", 
                 "Another language is first/main", "Household is bilingual")

# Convert 'lang' to a labelled factor
consolidated_lang <- consolidated_lang %>%
  mutate(lang = factor(lang, levels = lang_levels, labels = lang_labels))

# Print a sample of the consolidated data
cat("Sample of consolidated data:\n")
print(head(consolidated_lang))

# Write the output to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(consolidated_lang, output_path)

# Verify the file was written
file_exists <- file.exists(output_path)
cat("Output file written:", file_exists, "\n")
if (file_exists) {
  cat("File path:", output_path, "\n")
}