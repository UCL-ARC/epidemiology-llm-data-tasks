library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets by NSID
merged_data <- full_join(wave8, wave9, by = "NSID")

# Define standard missing value codes and labels
missing_codes <- c(-9, -8, -3, -2, -1)
missing_labels <- c("Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable")

# Define value labels for income bands
income_codes <- 1:16
income_labels <- c("less than 25", "25 to 50", "50 to 90", "90 to 140", 
                     "140 to 240", "240 to 300", "300 to 350", "350 to 400", 
                     "400 to 500", "500 to 600", "600 to 700", "700 to 800", 
                     "800 to 900", "900 to 1200", "1200 to 1400", "more than 1400")

# Combine codes and labels
all_codes <- c(missing_codes, income_codes)
all_labels <- c(missing_labels, income_labels)

# Recode wave-specific missing values to standard missing codes
merged_data <- merged_data %>%
  mutate(W8DINCB = ifelse(W8DINCB == -1, -1, ifelse(is.na(W8DINCB), -3, W8DINCB)))

merged_data <- merged_data %>%
  mutate(W9DINCB = ifelse(W9DINCB == -1, -1, ifelse(is.na(W9DINCB), -3, W9DINCB)))

# Rename variables to inc25 and inc32
merged_data <- merged_data %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Convert income variables to labelled factors
merged_data <- merged_data %>%
  mutate(inc25 = factor(inc25, levels = all_codes, labels = all_labels))

merged_data <- merged_data %>%
  mutate(inc32 = factor(inc32, levels = all_codes, labels = all_labels))

# Select only required variables
cleaned_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write output to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")