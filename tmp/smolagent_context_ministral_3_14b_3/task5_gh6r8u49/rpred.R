
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths and load datasets
files <- list(
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  ns8 = "data/input/ns8_2015_derived.tab",
  ns9 = "data/input/ns9_2022_derived_variables.tab"
)

# Load each file explicitly
wave6_data <- read_delim(files$wave6, delim = "\t", col_types = cols(NSID = col_character()))
ns8_data <- read_delim(files$ns8, delim = "\t", col_types = cols(NSID = col_character()))
ns9_data <- read_delim(files$ns9, delim = "\t", col_types = cols(NSID = col_character()))

# Print summary of loaded data
cat("Wave 6 data loaded with", nrow(wave6_data), "rows\n")
cat("Wave 8 data loaded with", nrow(ns8_data), "rows\n")
cat("Wave 9 data loaded with", nrow(ns9_data), "rows\n")

# Merge datasets by NSID
cleaned_data <- full_join(wave6_data, ns8_data, by = "NSID")
cleaned_data <- full_join(cleaned_data, ns9_data, by = "NSID")

# Print merged data summary
cat("Merged data contains", nrow(cleaned_data), "rows\n")

# Define missing value mappings
w6_missing_map <- c(
  `-997.0` = -2, `-97.0` = -7, `-92.0` = -9, `-91.0` = -1, `-1.0` = -8
)
w8_missing_map <- c(
  `-9.0` = -9, `-8.0` = -8, `-1.0` = -1
)
w9_missing_map <- c(
  `-9.0` = -9, `-8.0` = -8
)

# Create partnr19 from W6MarStatYP
cleaned_data <- cleaned_data %>%
  mutate(partnr19 = recode(W6MarStatYP, !!!w6_missing_map)) %>%
  mutate(partnr19 = ifelse(partnr19 %in% c(1, 2, 3, 4, 5), partnr19, NA_integer_))

# Create detailed adult variables
cleaned_data <- cleaned_data %>%
  mutate(partnradu25 = recode(W8DMARSTAT, !!!w8_missing_map)) %>%
  mutate(partnradu32 = recode(W9DMARSTAT, !!!w9_missing_map))

# Define labels
harmonised_labels <- data.frame(
  code = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
  label = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Item not applicable", "Single, never married", "Married", "Separated", "Divorced", "Widowed")
)

detailed_labels_25 <- data.frame(
  code = c(-9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  label = c("Refusal", "Insufficient information", "Not applicable", "Single, never married or in a CP", "Married", "Separated but still legally married", "Divorced", "Widowed", "A Civil Partner", "Separated but still legally in a CP", "A former Civil Partner", "A surviving Civil Partner")
)

detailed_labels_32 <- data.frame(
  code = c(-9, -8, 1, 2, 3, 4, 5, 6, 7, 8),
  label = c("Refusal", "Insufficient information", "Single, never married or never in a CP", "Married", "Divorced", "Legally separated", "Widowed", "A Civil Partner", "A former Civil Partner", "A surviving Civil Partner")
)

# Create labelled factors
cleaned_data <- cleaned_data %>%
  mutate(partnr19 = factor(partnr19, levels = harmonised_labels$code, labels = harmonised_labels$label)) %>%
  mutate(partnradu25 = factor(partnradu25, levels = detailed_labels_25$code, labels = detailed_labels_25$label)) %>%
  mutate(partnradu32 = factor(partnradu32, levels = detailed_labels_32$code, labels = detailed_labels_32$label))

# Collapse detailed variables into harmonized categories
cleaned_data <- cleaned_data %>%
  mutate(partnr25 = case_when(
    is.na(as.numeric(partnradu25)) ~ NA_integer_,
    as.numeric(partnradu25) %in% c(1, 6) ~ 1,
    as.numeric(partnradu25) == 2 ~ 2,
    as.numeric(partnradu25) %in% c(3, 7) ~ 3,
    as.numeric(partnradu25) %in% c(4, 8) ~ 4,
    as.numeric(partnradu25) %in% c(5, 9) ~ 5
  )) %>%
  mutate(partnr32 = case_when(
    is.na(as.numeric(partnradu32)) ~ NA_integer_,
    as.numeric(partnradu32) %in% c(1, 6) ~ 1,
    as.numeric(partnradu32) == 2 ~ 2,
    as.numeric(partnradu32) %in% c(3, 4) ~ 3,
    as.numeric(partnradu32) %in% c(7, 8) ~ 4,
    as.numeric(partnradu32) == 5 ~ 5
  )) %>%
  mutate(partnr25 = factor(partnr25, levels = harmonised_labels$code, labels = harmonised_labels$label)) %>%
  mutate(partnr32 = factor(partnr32, levels = harmonised_labels$code, labels = harmonised_labels$label))

# Select final variables
final_vars <- c("NSID", "partnr19", "partnr25", "partnr32", "partnradu25", "partnradu32")
cleaned_data <- cleaned_data %>% select(all_of(final_vars))

# Print final data summary
cat("Final dataset contains", nrow(cleaned_data), "rows and", ncol(cleaned_data), "columns\n")

# Write the cleaned data to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(cleaned_data, output_path)
cat("Successfully wrote output to", output_path, "\n")
cat("First few rows:\n")
print(head(cleaned_data))
