# Load required libraries
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing value codes
standard_missing <- c(-9, -8, -1, -3, -2, -7)

# Function to standardize missing values
standardize_missing <- function(x, wave_specific_codes = list()) {
  # Replace wave-specific codes with standard codes
  for (code in names(wave_specific_codes)) {
    x[x == as.numeric(code)] <- wave_specific_codes[[code]]
  }
  # Convert any remaining NA to -3
  x[is.na(x)] <- -3
  return(x)
}

# Load all data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

cat("Loaded all files successfully\n")
cat("Wave 1:", nrow(wave1), "rows,", ncol(wave1), "columns\n")
cat("Wave 2:", nrow(wave2), "rows,", ncol(wave2), "columns\n")
cat("Wave 3:", nrow(wave3), "rows,", ncol(wave3), "columns\n")
cat("Wave 4:", nrow(wave4), "rows,", ncol(wave4), "columns\n")
cat("Wave 8:", nrow(wave8), "rows,", ncol(wave8), "columns\n")
cat("Wave 9 derived:", nrow(wave9_derived), "rows,", ncol(wave9_derived), "columns\n")
cat("Wave 9 main:", nrow(wave9_main), "rows,", ncol(wave9_main), "columns\n")

# Standardize missing values for wave2 and wave3 urbind and gor BEFORE merging
# Wave 2 and 3: -94 -> -8 (Insufficient information)
wave2$urbind <- standardize_missing(wave2$urbind, list("-94" = -8))
wave2$gor <- standardize_missing(wave2$gor, list("-94" = -8))
wave3$urbind <- standardize_missing(wave3$urbind, list("-94" = -8))
wave3$gor <- standardize_missing(wave3$gor, list("-94" = -8))

# Wave 8: -9 -> -9 (Refused), -8 -> -8 (Insufficient information), -1 -> -1 (Not applicable)
wave8$W8DGOR <- standardize_missing(wave8$W8DGOR, list("-9" = -9, "-8" = -8, "-1" = -1))

# Wave 9 derived: -9 -> -9 (Refused), -8 -> -8 (Insufficient information), -1 -> -1 (Not applicable)
wave9_derived$W9DRGN <- standardize_missing(wave9_derived$W9DRGN, list("-9" = -9, "-8" = -8, "-1" = -1))

# Wave 9 main: -9 -> -9 (Refused), -8 -> -8 (Don't know), -3 -> -3 (Not asked), -1 -> -1 (Not applicable)
wave9_main$W9NATIONRES <- standardize_missing(wave9_main$W9NATIONRES, list("-9" = -9, "-8" = -8, "-3" = -3, "-1" = -1))

# Rename wave2 and wave3 columns to include age suffix before merging
wave2 <- wave2 %>% rename(urbind15 = urbind, gor15 = gor)
wave3 <- wave3 %>% rename(urbind16 = urbind, gor16 = gor)

cat("\nStandardized missing values and renamed columns\n")

# Merge all datasets using full_join by NSID
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID")

cat("\nMerged dataset:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "columns\n")

# Create age-specific variables with standardized names
# Age 15 (Wave 2) - urbind15
cleaned_data$urbind15 <- cleaned_data$urbind15

# Age 15 (Wave 2) - gor15
cleaned_data$gor15 <- cleaned_data$gor15

# Age 16 (Wave 3) - urbind16
cleaned_data$urbind16 <- cleaned_data$urbind16

# Age 16 (Wave 3) - gor16
cleaned_data$gor16 <- cleaned_data$gor16

# Age 23 (Wave 8) - Government Office Region
cleaned_data$gor23 <- cleaned_data$W8DGOR

# Age 32 (Wave 9) - Government Office Region
cleaned_data$gor32 <- cleaned_data$W9DRGN

# Age 32 (Wave 9) - Nation of UK
cleaned_data$nationres32 <- cleaned_data$W9NATIONRES

# Create collapsed UK/abroad variable for nationres32
cleaned_data$uk_abroad32 <- ifelse(cleaned_data$nationres32 %in% c(1, 2, 3, 4), 1, 
                                   ifelse(cleaned_data$nationres32 %in% c(-9, -8, -3, -1, 5), cleaned_data$nationres32, -3))

cat("\nCreated age-specific geographical variables\n")

# Select only the final output variables
final_output <- cleaned_data %>%
  select(NSID, urbind15, gor15, urbind16, gor16, gor23, gor32, nationres32, uk_abroad32)

cat("\nFinal output variables:", names(final_output), "\n")

# Write to CSV
write_csv(final_output, "data/output/cleaned_data.csv")
cat("\nCleaned data written to data/output/cleaned_data.csv\n")

cat("\nFinal dataset dimensions:", nrow(final_output), "rows,", ncol(final_output), "columns\n")