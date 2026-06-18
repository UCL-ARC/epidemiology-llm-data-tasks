library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Function to recode employment status variables
recode_employment <- function(var) {
  var <- case_when(
    var == -999 ~ -2,   # Missing household information - lost -> schedule not applicable
    var == -99 ~ -3,    # Mother/Father not interviewed -> not interviewed
    var == -98 ~ -1,    # Mother/Father not present -> not applicable
    var == -94 ~ -8,    # Insufficient information -> don't know
    var == -92 ~ -9,    # Refusal -> refusal
    TRUE ~ var          # Keep valid responses unchanged
  )
  return(var)
}

# Create ecoactma14 and ecoactpa14 from wave1
merged$ecoactma14 <- recode_employment(merged$W1empsmum)
merged$ecoactpa14 <- recode_employment(merged$W1empsdad)

# Create ecoactma15 and ecoactpa15 from wave2
merged$ecoactma15 <- recode_employment(merged$W2empsmum)
merged$ecoactpa15 <- recode_employment(merged$W2empsdad)

# Create ecoactma16 and ecoactpa16 from wave3
merged$ecoactma16 <- recode_employment(merged$W3empsmum)
merged$ecoactpa16 <- recode_employment(merged$W3empsdad)

# Create ecoactma17 and ecoactpa17 from wave4
merged$ecoactma17 <- recode_employment(merged$w4empsmum)
merged$ecoactpa17 <- recode_employment(merged$w4empsdad)

# Keep only NSID and the 8 derived variables
output <- merged %>% select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output
cat("Number of rows in output:", nrow(output), "\n")
cat("Number of columns in output:", ncol(output), "\n")
write_csv(output, "data/output/cleaned_data.csv")
cat("Output written successfully\n")