library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load Wave 1 (Age 14)
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = "cic")

# Load Wave 2 (Age 15)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = "cic")

# Load Wave 3 (Age 16)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = "cic")

# Load Wave 4 (Age 17)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = "cic")

# Define value labels for employment status
emp_labels <- c(
  "1" = "Paid work (30+ hours)",
  "2" = "Part-time paid work (<30 hours)",
  "3" = "Unemployed/Looking for work",
  "4" = "On training course",
  "5" = "In full-time education",
  "6" = "Looking after family/household",
  "7" = "Retired",
  "8" = "Sick/disabled",
  "9" = "Other"
)

# Function to recode missing values and apply labels
recode_and_label <- function(x, missing_map, valid_labels) {
  # Map missing values to standard codes
  x <- case_when(
    x %in% as.numeric(names(missing_map)) ~ as.integer(missing_map[x]),
    TRUE ~ x
  )
  # Apply labels
  x <- factor(x, levels = unique(c(as.numeric(names(valid_labels)), x)), 
              labels = c(valid_labels, as.character(x)))
  return(x)
}

# Process Wave 1 - Maternal employment
wave1 <- wave1 %>%
  mutate(
    W1empsmum = case_when(
      W1empsmum %in% c(-999, -99, -98) ~ -99,
      W1empsmum == -94 ~ -8,
      TRUE ~ W1empsmum
    ),
    W1empsmum = factor(W1empsmum, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 1 - Paternal employment
wave1 <- wave1 %>%
  mutate(
    W1empsdad = case_when(
      W1empsdad %in% c(-999, -99, -98) ~ -99,
      W1empsdad == -94 ~ -8,
      TRUE ~ W1empsdad
    ),
    W1empsdad = factor(W1empsdad, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 2 - Maternal employment
wave2 <- wave2 %>%
  mutate(
    W2empsmum = case_when(
      W2empsmum %in% c(-999, -99, -98) ~ -99,
      W2empsmum == -94 ~ -8,
      TRUE ~ W2empsmum
    ),
    W2empsmum = factor(W2empsmum, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 2 - Paternal employment
wave2 <- wave2 %>%
  mutate(
    W2empsdad = case_when(
      W2empsdad %in% c(-999, -99, -98) ~ -99,
      W2empsdad == -94 ~ -8,
      TRUE ~ W2empsdad
    ),
    W2empsdad = factor(W2empsdad, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 3 - Maternal employment
wave3 <- wave3 %>%
  mutate(
    W3empsmum = case_when(
      W3empsmum %in% c(-999, -99, -98) ~ -99,
      W3empsmum == -94 ~ -8,
      TRUE ~ W3empsmum
    ),
    W3empsmum = factor(W3empsmum, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 3 - Paternal employment
wave3 <- wave3 %>%
  mutate(
    W3empsdad = case_when(
      W3empsdad %in% c(-999, -99, -98) ~ -99,
      W3empsdad == -94 ~ -8,
      TRUE ~ W3empsdad
    ),
    W3empsdad = factor(W3empsdad, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 4 - Maternal employment
wave4 <- wave4 %>%
  mutate(
    w4empsmum = case_when(
      w4empsmum %in% c(-999, -99, -98) ~ -99,
      w4empsmum == -94 ~ -8,
      TRUE ~ w4empsmum
    ),
    w4empsmum = factor(w4empsmum, levels = c(-99, -8, 1:9), 
                       labels = c("Missing household information", "Insufficient info", emp_labels))
  )

# Process Wave 4 - Paternal employment
wave4 <- wave4 %>%
  mutate(
    w4empsdad = case_when(
      w4empsdad %in% c(-999, -996, -99, -98, -94, -92) ~ case_when(
        w4empsdad == -996 ~ -8,
        w4empsdad == -99 ~ -99,
        w4empsdad == -98 ~ -98,
        w4empsdad == -94 ~ -8,
        w4empsdad == -92 ~ -9,
        TRUE ~ -99
      ),
      TRUE ~ w4empsdad
    ),
    w4empsdad = factor(w4empsdad, levels = c(-996, -99, -98, -94, -92, -99, -8, 1:9), 
                       labels = c("No parent in household", "Missing household information", "Father not present", "Insufficient info", "Refusal", "Father not interviewed", "Insufficient info", emp_labels))
  )

# Merge all waves
result <- full_join(wave1, wave2, by = "NSID")
result <- full_join(result, wave3, by = "NSID")
result <- full_join(result, wave4, by = "NSID")

# Write output
write_csv(result, "data/output/cleaned_data.csv")

print("Data cleaning complete. Output written to data/output/cleaned_data.csv")
print(paste("Number of rows:", nrow(result)))
print(paste("Number of columns:", ncol(result)))