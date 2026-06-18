library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge datasets
merged_data <- reduce(list(wave_one, wave_two, wave_three, wave_four), 
                     full_join, by = 'NSID')

# Define missing value mapping function
missing_mapping <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x %in% c(-999, -998, -997, -995)] <- -2
  x[x == -99] <- -3
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Define language harmonization function
harmonize_lang <- function(x) {
  x <- missing_mapping(x)
  x[x %in% c(1, 2, 4)] <- 1
  x[x == 3] <- 2
  return(x)
}

# Apply harmonization to each source variable
merged_data <- merged_data %>%
  mutate(
    lang_w1 = harmonize_lang(W1englangYP),
    lang_w2 = harmonize_lang(W2EnglangYP),
    lang_w3 = harmonize_lang(W3englangHH),
    lang_w4 = harmonize_lang(W4EngLangHH)
  )

# Create consolidated variable using earliest-valid-first rule
merged_data <- merged_data %>%
  mutate(lang = coalesce(lang_w1, lang_w2, lang_w3, lang_w4))

# Create labeled factor using base R factor first, then add labels
lang_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2)
lang_labels <- c("Refusal", "Don't know", "Prefer not to say", "Not interviewed", 
                 "Schedule error", "Not applicable", "English first/main", "Other language first/main")

# Create factor with levels and labels
lang_factor <- factor(merged_data$lang, levels = lang_levels, labels = lang_labels)

# Create labelled object
merged_data$lang <- labelled(lang_factor)

# Select final variables
final_data <- merged_data %>%
  select(NSID, lang)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')
message('Data cleaning complete')