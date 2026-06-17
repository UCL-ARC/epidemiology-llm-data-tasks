library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(labelled)

# Load all four wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
all_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define a function to harmonize economic activity variables
harmonize_ecoact <- function(x) {
  x[is.na(x)] <- -3  # Any remaining NA -> -3
  x[x == -99] <- -3   # not interviewed -> not asked
  x[x == -98] <- -3   # not present -> not asked
  x[x == -996] <- -3  # no parent in household -> not asked
  x[x == -999] <- -2  # missing household information -> schedule not applicable
  x[x == -94] <- -8   # insufficient information -> don't know
  x[x == -92] <- -9   # refusal -> refusal
  
  x
}

# Apply harmonization to each variable in the merged dataframe
all_data$W1empsmum <- harmonize_ecoact(all_data$W1empsmum)
all_data$W1empsdad <- harmonize_ecoact(all_data$W1empsdad)
all_data$W2empsmum <- harmonize_ecoact(all_data$W2empsmum)
all_data$W2empsdad <- harmonize_ecoact(all_data$W2empsdad)
all_data$W3empsmum <- harmonize_ecoact(all_data$W3empsmum)
all_data$W3empsdad <- harmonize_ecoact(all_data$W3empsdad)
all_data$w4empsmum <- harmonize_ecoact(all_data$w4empsmum)
all_data$w4empsdad <- harmonize_ecoact(all_data$w4empsdad)

# Create the final output dataframe
cleaned <- all_data %>%
  mutate(
    ecoactma14 = W1empsmum,
    ecoactpa14 = W1empsdad,
    ecoactma15 = W2empsmum,
    ecoactpa15 = W2empsdad,
    ecoactma16 = W3empsmum,
    ecoactpa16 = W3empsdad,
    ecoactma17 = w4empsmum,
    ecoactpa17 = w4empsdad
  ) %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Create a named numeric vector for labels where:
# - names are the label strings
# - values are the numeric codes
labels_9cat <- c(
  "Doing paid work for 30 or more hours a week" = 1,
  "Doing paid work for fewer than 30 hours a week" = 2,
  "Unemployed/ Looking for a job" = 3,
  "On a training course or scheme" = 4,
  "In full-time education/ at school" = 5,
  "Looking after the family/ household" = 6,
  "Retired from work altogether" = 7,
  "Sick/ disabled" = 8,
  "Other" = 9,
  "Refusal" = -9,
  "Don't know" = -8,
  "Not asked at the fieldwork stage" = -3,
  "Schedule not applicable" = -2
)

# Apply labels using haven::labelled
for (var in names(cleaned[-1])) {
  cleaned[[var]] <- haven::labelled(cleaned[[var]], labels = labels_9cat)
}

# Print summary to check
cat("Number of rows:", nrow(cleaned), "\n")
cat("Variables:", paste(names(cleaned), collapse=", "), "\n")
cat("\nFirst few rows:\n")
print(head(cleaned))

# Write to CSV
write_csv(cleaned, "data/output/cleaned_data.csv")
cat("\nDone. Output written to data/output/cleaned_data.csv\n")
