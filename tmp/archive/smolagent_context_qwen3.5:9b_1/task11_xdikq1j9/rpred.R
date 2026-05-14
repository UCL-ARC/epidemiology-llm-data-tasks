library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define input and output directories
input_dir <- "data/input/"
output_dir <- "data/output/"

# Load wave 1 (Age 14)
wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_family_background_2020.tab"), delim = "\t")
cat("Wave 1 loaded:", nrow(wave1), "rows\n")

# Load wave 2 (Age 15)
wave2 <- read_delim(paste0(input_dir, "wave_two_lsype_family_background_2020.tab"), delim = "\t")
cat("Wave 2 loaded:", nrow(wave2), "rows\n")

# Load wave 3 (Age 16)
wave3 <- read_delim(paste0(input_dir, "wave_three_lsype_family_background_2020.tab"), delim = "\t")
cat("Wave 3 loaded:", nrow(wave3), "rows\n")

# Load wave 4 (Age 17)
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_family_background_2020.tab"), delim = "\t")
cat("Wave 4 loaded:", nrow(wave4), "rows\n")

# Recode missing values for wave 1 mother's employment
wave1 <- wave1 %>%
  mutate(
    empsmum14_hrm = case_when(
      W1empsmum %in% c(-999, -99, -98) ~ -9,  # Missing household info, not interviewed, not present -> Refusal
      W1empsmum == -94 ~ -8,                   # Insufficient information -> Don't know
      W1empsmum >= 1 & W1empsmum <= 9 ~ W1empsmum,
      TRUE ~ -3  # Not asked
    )
  )

# Recode missing values for wave 1 father's employment
wave1 <- wave1 %>%
  mutate(
    empsdad14_hrm = case_when(
      W1empsdad %in% c(-999, -99, -98) ~ -9,
      W1empsdad == -94 ~ -8,
      W1empsdad >= 1 & W1empsdad <= 9 ~ W1empsdad,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 2 mother's employment
wave2 <- wave2 %>%
  mutate(
    empsmum15_hrm = case_when(
      W2empsmum %in% c(-999, -99, -98) ~ -9,
      W2empsmum == -94 ~ -8,
      W2empsmum >= 1 & W2empsmum <= 9 ~ W2empsmum,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 2 father's employment
wave2 <- wave2 %>%
  mutate(
    empsdad15_hrm = case_when(
      W2empsdad %in% c(-999, -99, -98) ~ -9,
      W2empsdad == -94 ~ -8,
      W2empsdad >= 1 & W2empsdad <= 9 ~ W2empsdad,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 3 mother's employment
wave3 <- wave3 %>%
  mutate(
    empsmum16_hrm = case_when(
      W3empsmum %in% c(-999, -99, -98) ~ -9,
      W3empsmum == -94 ~ -8,
      W3empsmum >= 1 & W3empsmum <= 9 ~ W3empsmum,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 3 father's employment
wave3 <- wave3 %>%
  mutate(
    empsdad16_hrm = case_when(
      W3empsdad %in% c(-999, -99, -98) ~ -9,
      W3empsdad == -94 ~ -8,
      W3empsdad >= 1 & W3empsdad <= 9 ~ W3empsdad,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 4 mother's employment
wave4 <- wave4 %>%
  mutate(
    empsmum17_hrm = case_when(
      w4empsmum %in% c(-999, -99, -98) ~ -9,
      w4empsmum == -94 ~ -8,
      w4empsmum >= 1 & w4empsmum <= 9 ~ w4empsmum,
      TRUE ~ -3
    )
  )

# Recode missing values for wave 4 father's employment
wave4 <- wave4 %>%
  mutate(
    empsdad17_hrm = case_when(
      w4empsdad %in% c(-999, -996, -99, -98) ~ -9,
      w4empsdad == -94 ~ -8,
      w4empsdad == -92 ~ -9,  # Refusal -> Refusal
      w4empsdad >= 1 & w4empsdad <= 9 ~ w4empsdad,
      TRUE ~ -3
    )
  )

# Merge all waves using full_join by NSID
merged <- full_join(wave1, wave2, by = "NSID")
cat("After wave1-wave2 join:", nrow(merged), "rows\n")
merged <- full_join(merged, wave3, by = "NSID")
cat("After wave1-3 join:", nrow(merged), "rows\n")
merged <- full_join(merged, wave4, by = "NSID")
cat("After all join:", nrow(merged), "rows\n")

# Check column names
cat("Column names in merged:", paste(names(merged), collapse = ", "), "\n")

# Create age-specific harmonized variables with labels
# Age 14 (wave1)
merged <- merged %>%
  mutate(
    empsmum14 = factor(empsmum14_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    ),
    empsdad14 = factor(empsdad14_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    )
  )

# Age 15 (wave2)
merged <- merged %>%
  mutate(
    empsmum15 = factor(empsmum15_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    ),
    empsdad15 = factor(empsdad15_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    )
  )

# Age 16 (wave3)
merged <- merged %>%
  mutate(
    empsmum16 = factor(empsmum16_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    ),
    empsdad16 = factor(empsdad16_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    )
  )

# Age 17 (wave4)
merged <- merged %>%
  mutate(
    empsmum17 = factor(empsmum17_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    ),
    empsdad17 = factor(empsdad17_hrm, 
      levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c("Refusal", "Don't know", "Not asked", 
                 "Paid work 30+ hrs/week", 
                 "Paid work <30 hrs/week",
                 "Unemployed/Looking for work",
                 "Training course/scheme",
                 "Full-time education/school",
                 "Looking after family/household",
                 "Retired",
                 "Sick/disabled",
                 "Other")
    )
  )

# Select only the derived variables to include in output
merged <- merged %>%
  select(NSID, empsmum14, empsdad14, empsmum15, empsdad15, empsmum16, empsdad16, empsmum17, empsdad17)

# Write output
csv_path <- paste0(output_dir, "cleaned_data.csv")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(merged, csv_path)
cat("Output written to:", csv_path, "\n")
cat("Output dimensions:", nrow(merged), "rows", ncol(merged), "columns\n")

# Print summary of variables
print(names(merged))
print(table(merged$empsmum14, useNA = "always"))
