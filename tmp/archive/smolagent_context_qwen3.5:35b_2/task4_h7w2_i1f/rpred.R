# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing value codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -7 = Prefer not to say
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# Null values should be coded as -3

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x, wave_specific_codes) {
  # wave_specific_codes is a named list where names are wave-specific codes
  # and values are standard codes
  for (code in names(wave_specific_codes)) {
    x[x == as.numeric(code)] <- as.numeric(wave_specific_codes[code])
  }
  return(x)
}

# Load all wave files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", 
                         delim = "\t", show_col_types = FALSE)
wave_eight <- read_delim("data/input/ns8_2015_self_completion.tab", 
                         delim = "\t", show_col_types = FALSE)
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", 
                        delim = "\t", show_col_types = FALSE)

# Merge all datasets using full_join by NSID
cleaned_data <- full_join(wave_one, wave_four, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_six, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_seven, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_eight, by = "NSID")
cleaned_data <- full_join(cleaned_data, wave_nine, by = "NSID")

# Convert all Null values to -3 (Not asked at fieldwork stage)
cleaned_data <- cleaned_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), -3, .)))

# Harmonize sexuality variables across waves
# Standard codes: 1=Heterosexual/Straight, 2=Gay/Lesbian, 3=Bisexual, 4=Other
# Missing codes: -9=Refusal, -8=Don't know, -7=Prefer not to say, -1=Not applicable, -3=Not asked

# W6SexualityYP (Age 19): -999 to -1 are missing
# Map: -999=-3, -97=-9, -92=-9, -91=-1, -1=-8
w6_missing_map <- c("-999" = "-3", "-97" = "-9", "-92" = "-9", "-91" = "-1", "-1" = "-8")
cleaned_data$sori19 <- convert_missing_codes(cleaned_data$W6SexualityYP, w6_missing_map)

# W7SexualityYP (Age 20): -100 to -1 are missing
# Map: -100=-3, -97=-9, -92=-9, -91=-1, -1=-8
w7_missing_map <- c("-100" = "-3", "-97" = "-9", "-92" = "-9", "-91" = "-1", "-1" = "-8")
cleaned_data$sori20 <- convert_missing_codes(cleaned_data$W7SexualityYP, w7_missing_map)

# W8SEXUALITY (Wave 8): -9 to -1 are missing
# Map: -9=-9, -8=-8, -1=-1
w8_missing_map <- c("-9" = "-9", "-8" = "-8", "-1" = "-1")
cleaned_data$sori22 <- convert_missing_codes(cleaned_data$W8SEXUALITY, w8_missing_map)

# W9SORI (Age 32): -9 to -1 are missing
# Map: -9=-9, -8=-8, -3=-3, -1=-1, 5=-7 (Prefer not to say)
w9_missing_map <- c("-9" = "-9", "-8" = "-8", "-3" = "-3", "-1" = "-1", "5" = "-7")
cleaned_data$sori32 <- convert_missing_codes(cleaned_data$W9SORI, w9_missing_map)

# Create factor variables with labels for sexuality
sori_labels <- c(
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked at fieldwork stage"
)

cleaned_data$sori19 <- factor(cleaned_data$sori19, levels = c(1, 2, 3, 4, -9, -8, -7, -1, -3),
                              labels = sori_labels)
cleaned_data$sori20 <- factor(cleaned_data$sori20, levels = c(1, 2, 3, 4, -9, -8, -7, -1, -3),
                              labels = sori_labels)
cleaned_data$sori22 <- factor(cleaned_data$sori22, levels = c(1, 2, 3, 4, -9, -8, -7, -1, -3),
                              labels = sori_labels)
cleaned_data$sori32 <- factor(cleaned_data$sori32, levels = c(1, 2, 3, 4, -9, -8, -7, -1, -3),
                              labels = sori_labels)

# Select only the ID and derived variables for output
output_data <- cleaned_data %>%
  select(NSID, sori19, sori20, sori22, sori32)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of records:", nrow(output_data), "\n")
cat("Number of variables:", ncol(output_data), "\n")
