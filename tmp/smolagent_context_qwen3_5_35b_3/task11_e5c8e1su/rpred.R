library(readr)
library(dplyr)
library(haven)
library(labelled)

# Load all wave files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID using full_join
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Define recoding function for economic activity
# Maps all missing value codes to standard scheme
recode_econ_activity <- function(x) {
  result <- x
  # Map specific codes to standard missing values
  result[x == -999] <- -2    # Missing household information - lost -> Schedule not applicable
  result[x == -99] <- -3     # Not interviewed -> Not asked at fieldwork stage
  result[x == -98] <- -3     # Not present -> Not asked at fieldwork stage
  result[x == -94] <- -8     # Insufficient information -> Don't know
  result[x == -92] <- -9     # Refusal -> Refusal
  result[x == -996] <- -3    # No parent in household -> Not asked at fieldwork stage
  return(result)
}

# Create economic activity variables for mother (ecoactma*) and father (ecoactpa*)
# Wave 1 (Age 14): W1empsmum, W1empsdad
merged$ecoactma14 <- recode_econ_activity(merged$W1empsmum)
merged$ecoactpa14 <- recode_econ_activity(merged$W1empsdad)

# Wave 2 (Age 15): W2empsmum, W2empsdad
merged$ecoactma15 <- recode_econ_activity(merged$W2empsmum)
merged$ecoactpa15 <- recode_econ_activity(merged$W2empsdad)

# Wave 3 (Age 16): W3empsmum, W3empsdad
merged$ecoactma16 <- recode_econ_activity(merged$W3empsmum)
merged$ecoactpa16 <- recode_econ_activity(merged$W3empsdad)

# Wave 4 (Age 17): w4empsmum, w4empsdad
merged$ecoactma17 <- recode_econ_activity(merged$w4empsmum)
merged$ecoactpa17 <- recode_econ_activity(merged$w4empsdad)

# Set variable labels
var_label(merged$ecoactma14) <- "Mother's economic activity (Age 14)"
var_label(merged$ecoactpa14) <- "Father's economic activity (Age 14)"
var_label(merged$ecoactma15) <- "Mother's economic activity (Age 15)"
var_label(merged$ecoactpa15) <- "Father's economic activity (Age 15)"
var_label(merged$ecoactma16) <- "Mother's economic activity (Age 16)"
var_label(merged$ecoactpa16) <- "Father's economic activity (Age 16)"
var_label(merged$ecoactma17) <- "Mother's economic activity (Age 17)"
var_label(merged$ecoactpa17) <- "Father's economic activity (Age 17)"

# Keep only NSID and the 8 economic activity variables
final_data <- merged %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to output CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Successfully created data/output/cleaned_data.csv\n")
cat("Output variables: NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17\n")