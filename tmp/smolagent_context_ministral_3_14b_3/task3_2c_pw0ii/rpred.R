# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load metadata files into separate objects
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- full_join(
  full_join(wave_one, wave_two, by = "NSID"), 
  full_join(wave_three, wave_four, by = "NSID"), 
  by = "NSID"
)

# Function to map missing values based on the task requirements
map_missing_values <- function(x) {
  if (!is.numeric(x)) return(x)
  
  # Map specific missing values
  x[x == -999] <- -3
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99]  <- -3
  x[x == -92]  <- -9
  x[x == -91]  <- -1
  x[x == -1]   <- -8
  
  return(x)
}

# Apply missing value mapping to each source variable
merged_data <- merged_data %>%
  mutate(
    W1englangYP = map_missing_values(W1englangYP),
    W2EnglangYP = map_missing_values(W2EnglangYP),
    W3englangHH = map_missing_values(W3englangHH),
    W4EngLangHH = map_missing_values(W4EngLangHH)
  )

# Define the logic for deriving the consolidated 'lang' variable
merged_data <- merged_data %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP %in% 1:4 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP %in% 1:4 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH %in% 1:4 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH %in% 1:4 ~ W4EngLangHH,
      TRUE ~ -3
    )
  )

# Create labelled factor for 'lang'
lang_levels <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4)
lang_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-3" = "Not asked/interviewed",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable",
  "1" = "English only",
  "2" = "English first/main and speaks other languages",
  "3" = "No, another language is first/main",
  "4" = "Bilingual"
)

# Apply labels to the 'lang' variable
merged_data$lang <- factor(merged_data$lang, levels = lang_levels, labels = lang_labels)

# Select only the ID and the final derived variable
final_data <- merged_data %>%
  select(NSID, lang)

# Write the final cleaned data to CSV
write_csv(final_data, "data/output/cleaned_data.csv")