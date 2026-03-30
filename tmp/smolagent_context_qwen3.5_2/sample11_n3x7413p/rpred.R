# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all four waves from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge all waves by NSID using full_join
merged_data <- wave1 %>%
  select(NSID, W1empsmum, W1empsdad) %>%
  full_join(wave2 %>% select(NSID, W2empsmum, W2empsdad), by = "NSID") %>%
  full_join(wave3 %>% select(NSID, W3empsmum, W3empsdad), by = "NSID") %>%
  full_join(wave4 %>% select(NSID, w4empsmum, w4empsdad), by = "NSID")

# Function to recode wave-specific missing values to standard codes
# Standard codes: -9=Refusal, -8=Don't know/insufficient info, -1=Not applicable,
# -3=Not asked, -2=Information lost, -7=Prefer not to say
recode_missing <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x == -999 ~ -2,  # Missing household information - lost -> Information lost
    x == -99 ~ -3,   # Not interviewed -> Not asked
    x == -98 ~ -3,   # Not present -> Not asked
    x == -94 ~ -8,   # Insufficient information -> Don't know/insufficient info
    x == -96 ~ -1,   # No parent in household -> Item not applicable
    x == -92 ~ -9,   # Refusal -> Refusal
    is.na(x) ~ -3,   # Null values -> Not asked
    TRUE ~ x
  )
  return(x)
}

# Apply missing value recoding to all employment variables
merged_data <- merged_data %>%
  mutate(
    W1empsmum = recode_missing(W1empsmum),
    W1empsdad = recode_missing(W1empsdad),
    W2empsmum = recode_missing(W2empsmum),
    W2empsdad = recode_missing(W2empsdad),
    W3empsmum = recode_missing(W3empsmum),
    W3empsdad = recode_missing(W3empsdad),
    w4empsmum = recode_missing(w4empsmum),
    w4empsdad = recode_missing(w4empsdad)
  )

# Create labeled factors for employment status with explicit labels for all categories
# Including both valid categories (1-9) and all missing value codes (-1, -2, -3, -8, -9)
create_emp_factor <- function(x) {
  x <- as.numeric(x)
  factor(x, 
         levels = c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
         labels = c("Refusal", "Insufficient information", "Not asked", 
                   "Information lost", "Item not applicable",
                   "Full-time work (30+ hrs)", "Part-time work (<30 hrs)",
                   "Unemployed", "Training", "Education",
                   "Looking after home", "Retired", "Sick/disabled", "Other"))
}

# Create age-specific parental economic activity variables
# Following naming convention: ecoactdtma = economic activity detailed mother
# ecoactdtpa = economic activity detailed father (parent)
# Suffix = age (14, 15, 16, 17)
merged_data <- merged_data %>%
  mutate(
    ecoactdtma14 = create_emp_factor(W1empsmum),
    ecoactdtpa14 = create_emp_factor(W1empsdad),
    ecoactdtma15 = create_emp_factor(W2empsmum),
    ecoactdtpa15 = create_emp_factor(W2empsdad),
    ecoactdtma16 = create_emp_factor(W3empsmum),
    ecoactdtpa16 = create_emp_factor(W3empsdad),
    ecoactdtma17 = create_emp_factor(w4empsmum),
    ecoactdtpa17 = create_emp_factor(w4empsdad)
  )

# Select only the derived variables for output (ID + cleaned parental economic activity variables)
output_data <- merged_data %>%
  select(NSID, ecoactdtma14, ecoactdtpa14, ecoactdtma15, ecoactdtpa15, 
         ecoactdtma16, ecoactdtpa16, ecoactdtma17, ecoactdtpa17)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Cleaned data written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Number of columns:", ncol(output_data), "\n")
cat("Column names:", paste(names(output_data), collapse = ", "), "\n")