# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# 1. FILE LOADING
# ============================================================================

# Load each wave using read_delim with tab delimiter
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", 
                    delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                    delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                    delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", 
                    delim = "\t", show_col_types = FALSE)

# ============================================================================
# 2. MERGE DATASETS
# ============================================================================

# Full join all waves by NSID (the ID variable from source data)
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# ============================================================================
# 3. MISSING VALUE CODE HARMONIZATION FUNCTION
# ============================================================================

# Function to recode wave-specific missing codes to standard codes
# Standard codes:
# -9 = Refusal
# -8 = Don't know/insufficient information  
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable/Script error/information lost
# -7 = Prefer not to say
# Null -> -3

recode_missing <- function(x, var_name) {
  # First convert any NA/NULL to -3
  x <- ifelse(is.na(x), -3, x)
  
  # Map wave-specific codes to standard codes
  # -999 (Missing household information - lost) -> -2
  # -99 (Not interviewed) -> -3
  # -98 (Not present) -> -1
  # -94 (Insufficient information) -> -8
  # -996 (No parent in household) -> -1
  # -92 (Refusal) -> -9
  
  x <- case_when(
    x == -999 ~ -2,
    x == -996 ~ -1,
    x == -99 ~ -3,
    x == -98 ~ -1,
    x == -94 ~ -8,
    x == -92 ~ -9,
    TRUE ~ x
  )
  
  return(x)
}

# ============================================================================
# 4. PARENTAL ECONOMIC ACTIVITY VARIABLES
# ============================================================================

# Create age-specific variables for mother's and father's employment status
# Wave 1 (Age 14): W1empsmum, W1empsdad
# Wave 2 (Age 15): W2empsmum, W2empsdad
# Wave 3 (Age 16): W3empsmum, W3empsdad
# Wave 4 (Age 17): w4empsmum, w4empsdad

# Apply missing value recoding
merged_data <- merged_data %>%
  mutate(
    # Age 14 variables
    empsmum14 = recode_missing(W1empsmum, "W1empsmum"),
    empsdad14 = recode_missing(W1empsdad, "W1empsdad"),
    # Age 15 variables
    empsmum15 = recode_missing(W2empsmum, "W2empsmum"),
    empsdad15 = recode_missing(W2empsdad, "W2empsdad"),
    # Age 16 variables
    empsmum16 = recode_missing(W3empsmum, "W3empsmum"),
    empsdad16 = recode_missing(W3empsdad, "W3empsdad"),
    # Age 17 variables
    empsmum17 = recode_missing(w4empsmum, "w4empsmum"),
    empsdad17 = recode_missing(w4empsdad, "w4empsdad")
  )

# ============================================================================
# 5. CREATE DERIVED CATEGORICAL VARIABLES (COLLAPSED) - BEFORE FACTOR CONVERSION
# ============================================================================

# Create collapsed 6-category scheme for economic activity:
# 1 = Paid work (full-time or part-time)
# 2 = Training/Education
# 3 = Unemployed
# 4 = Looking after home
# 5 = Retired/Sick/Disabled
# 6 = Other
# Plus missing codes preserved

collapse_employment <- function(x) {
  # x is already numeric at this point
  collapsed <- case_when(
    x %in% c(1, 2) ~ 1,  # Paid work
    x %in% c(4, 5) ~ 2,  # Training/Education
    x == 3 ~ 3,          # Unemployed
    x == 6 ~ 4,          # Looking after home
    x %in% c(7, 8) ~ 5,  # Retired/Sick/Disabled
    x == 9 ~ 6,          # Other
    x %in% c(-1, -2, -3, -8, -9) ~ x,  # Missing codes preserved
    TRUE ~ -3
  )
  
  return(collapsed)
}

# Apply collapsed categories (BEFORE converting to factors)
merged_data <- merged_data %>%
  mutate(
    empsmum14_coll = collapse_employment(empsmum14),
    empsdad14_coll = collapse_employment(empsdad14),
    empsmum15_coll = collapse_employment(empsmum15),
    empsdad15_coll = collapse_employment(empsdad15),
    empsmum16_coll = collapse_employment(empsmum16),
    empsdad16_coll = collapse_employment(empsdad16),
    empsmum17_coll = collapse_employment(empsmum17),
    empsdad17_coll = collapse_employment(empsdad17)
  )

# ============================================================================
# 6. CREATE FACTOR VARIABLES WITH LABELS
# ============================================================================

# Define common employment status categories (1-9 are consistent across waves)
# 1 = Full-time paid work (30+ hours)
# 2 = Part-time paid work (<30 hours)
# 3 = Unemployed/Looking for job
# 4 = Training course/scheme
# 5 = Full-time education/school
# 6 = Looking after family/household
# 7 = Retired
# 8 = Sick/disabled
# 9 = Other

# Missing codes:
# -1 = Item not applicable
# -2 = Schedule not applicable/Script error/information lost
# -3 = Not asked at the fieldwork stage
# -8 = Don't know/insufficient information
# -9 = Refusal

employment_labels <- c(
  "1" = "Full-time paid work (30+ hrs)",
  "2" = "Part-time paid work (<30 hrs)",
  "3" = "Unemployed/Looking for job",
  "4" = "Training course/scheme",
  "5" = "Full-time education/school",
  "6" = "Looking after family/household",
  "7" = "Retired",
  "8" = "Sick/disabled",
  "9" = "Other",
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable/Information lost",
  "-3" = "Not asked at fieldwork stage",
  "-8" = "Don't know/Insufficient information",
  "-9" = "Refusal"
)

# Convert detailed employment variables to factors with labels
employment_vars <- c("empsmum14", "empsdad14", "empsmum15", "empsdad15", 
                     "empsmum16", "empsdad16", "empsmum17", "empsdad17")

for (var in employment_vars) {
  merged_data[[var]] <- factor(
    as.character(merged_data[[var]]),
    levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    labels = employment_labels,
    ordered = FALSE
  )
}

# Create labels for collapsed categories
collapsed_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/Insufficient information",
  "-3" = "Not asked at fieldwork stage",
  "-2" = "Schedule not applicable/Information lost",
  "-1" = "Item not applicable",
  "1" = "Paid work",
  "2" = "Training/Education",
  "3" = "Unemployed",
  "4" = "Looking after home",
  "5" = "Retired/Sick/Disabled",
  "6" = "Other"
)

# Convert collapsed variables to factors
collapsed_vars <- c("empsmum14_coll", "empsdad14_coll", "empsmum15_coll", "empsdad15_coll",
                    "empsmum16_coll", "empsdad16_coll", "empsmum17_coll", "empsdad17_coll")

for (var in collapsed_vars) {
  merged_data[[var]] <- factor(
    as.character(merged_data[[var]]),
    levels = c("-9", "-8", "-3", "-2", "-1", "1", "2", "3", "4", "5", "6"),
    labels = collapsed_labels,
    ordered = FALSE
  )
}

# ============================================================================
# 7. SELECT FINAL OUTPUT VARIABLES
# ============================================================================

# Select only the ID and derived variables (not intermediate wave-specific vars)
final_data <- merged_data %>%
  select(
    NSID,
    # Mother's employment - detailed
    empsmum14, empsmum15, empsmum16, empsmum17,
    # Father's employment - detailed
    empsdad14, empsdad15, empsdad16, empsdad17,
    # Mother's employment - collapsed
    empsmum14_coll, empsmum15_coll, empsmum16_coll, empsmum17_coll,
    # Father's employment - collapsed
    empsdad14_coll, empsdad15_coll, empsdad16_coll, empsdad17_coll
  )

# ============================================================================
# 8. OUTPUT CLEANED DATA
# ============================================================================

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("\n=== CLEANING COMPLETE ===\n")
cat("Output file: data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
cat("\nVariables included:\n")
print(names(final_data))
cat("\n")
