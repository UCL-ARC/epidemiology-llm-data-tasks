library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all four waves from data/input/
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Select relevant variables
w1_sel <- w1 %>% select(NSID, W1GrsswkHH)
w2_sel <- w2 %>% select(NSID, W2GrsswkHH)
w3_sel <- w3 %>% select(NSID, W3incestw)
w4_sel <- w4 %>% select(NSID, w4IncEstW)

# Merge all waves by NSID using full_join
merged <- w1_sel %>%
  full_join(w2_sel, by = "NSID") %>%
  full_join(w3_sel, by = "NSID") %>%
  full_join(w4_sel, by = "NSID")

# Function to recode missing values for waves 1-2 (ages 14-15, continuous income)
# Standard codes: -9=Refusal, -8=Don't know/insufficient, -1=Not applicable, -3=Not asked, -2=Script error
recode_missing_w1w2 <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -999 ~ -2,  # Missing in error
    x == -992 ~ -9,  # No information - work status questions refused
    x == -99 ~ -3,   # HH not interviewed
    x == -94 ~ -8,   # Insufficient information
    x == -92 ~ -9,   # Refused
    x == -91 ~ -1,   # Not applicable
    x == -3 ~ -3,    # Not yet paid
    x == -1 ~ -8,    # Don't know
    TRUE ~ as.numeric(x)
  )
}

# Function to recode missing values for waves 3-4 (ages 16-17, banded income)
recode_missing_w3w4 <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -99 ~ -3,   # MP not interviewed
    x == -92 ~ -9,   # Refused
    x == -1 ~ -8,    # Don't know
    x < 0 ~ -3,      # Other negative codes (from -999 to -1 range)
    TRUE ~ as.numeric(x)
  )
}

# Apply recoding to continuous variables (ages 14-15)
merged <- merged %>%
  mutate(
    incwhhcnt14 = recode_missing_w1w2(W1GrsswkHH),
    incwhhcnt15 = recode_missing_w1w2(W2GrsswkHH)
  )

# Apply recoding to banded variables (ages 16-17)
merged <- merged %>%
  mutate(
    incwhh16 = recode_missing_w3w4(W3incestw),
    incwhh17 = recode_missing_w3w4(w4IncEstW)
  )

# Create income bands for ages 14-15 based on continuous income
# Banding scheme matches ages 16-17: 1=Up to £49, 2=£50-99, ..., 12=£1000+
create_income_band <- function(x) {
  case_when(
    x < 0 ~ as.numeric(x),  # Preserve missing codes
    x <= 49 ~ 1,
    x <= 99 ~ 2,
    x <= 199 ~ 3,
    x <= 299 ~ 4,
    x <= 399 ~ 5,
    x <= 499 ~ 6,
    x <= 599 ~ 7,
    x <= 699 ~ 8,
    x <= 799 ~ 9,
    x <= 899 ~ 10,
    x <= 999 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ as.numeric(x)
  )
}

# Apply banding to ages 14-15
merged <- merged %>%
  mutate(
    incwhh14 = create_income_band(incwhhcnt14),
    incwhh15 = create_income_band(incwhhcnt15)
  )

# Define factor levels for banded variables (1-12 for income bands, plus missing codes)
band_levels <- c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
band_labels <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed",
  "Script error/lost", "Item not applicable",
  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
  "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699",
  "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more"
)

# Convert banded variables to factors with proper labels
merged <- merged %>%
  mutate(
    incwhh14 = factor(as.character(incwhh14), levels = band_levels, labels = band_labels),
    incwhh15 = factor(as.character(incwhh15), levels = band_levels, labels = band_labels),
    incwhh16 = factor(as.character(incwhh16), levels = band_levels, labels = band_labels),
    incwhh17 = factor(as.character(incwhh17), levels = band_levels, labels = band_labels)
  )

# Select final output variables
final <- merged %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write to CSV
write_csv(final, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Output written to: data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final), "\n")
cat("Variables:", paste(names(final), collapse = ", "), "\n")