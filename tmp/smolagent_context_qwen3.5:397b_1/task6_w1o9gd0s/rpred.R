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

# Load all tab-delimited files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# ============================================================================
# 2. MERGE DATASETS BY NSID
# ============================================================================

# Start with wave1 and progressively join other waves
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID")

# ============================================================================
# 3. STANDARD MISSING VALUE CODE HARMONIZATION
# ============================================================================

# Function to convert wave-specific missing codes to standard codes
# Standard codes:
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable/Script error/information lost
# -7 = Prefer not to say
# Any Null value to be coded as -3

convert_missing_codes <- function(x) {
  if (!is.numeric(x)) return(x)
  
  # Convert various wave-specific codes to standard codes
  x <- case_when(
    x == -999 ~ -3,  # Not asked/missing
    x == -94 ~ -8,   # Insufficient information -> Don't know
    x == -92 ~ -3,   # Not asked
    x == -91 ~ -3,   # Not asked
    x == -100 ~ -3,  # Not asked
    x == -97 ~ -3,   # Not asked
    x == -998 ~ -8,  # Don't know
    x == -997 ~ -9,  # Refused
    x == -995 ~ -1,  # Not applicable
    x == -9 ~ -9,    # Already standard: Refusal
    x == -8 ~ -8,    # Already standard: Don't know
    x == -7 ~ -7,    # Already standard: Prefer not to say
    x == -3 ~ -3,    # Already standard: Not asked
    x == -2 ~ -2,    # Already standard: Schedule not applicable
    x == -1 ~ -1,    # Already standard: Not applicable
    TRUE ~ as.numeric(x)
  )
  
  return(x)
}

# Convert NULL/NA values to -3 for numeric columns
convert_null_to_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- -3
  }
  return(x)
}

# ============================================================================
# 4. CREATE AGE-SPECIFIC VARIABLES WITH PROPER NAMING
# ============================================================================

# Check what columns exist after merge
cat("Columns in merged dataset:\n")
cat(paste(names(cleaned_data), collapse = ", "), "\n\n")

# Government Office Region variables (time-varying - retain age-specific)
# Wave 2 (Age 15): gor - check for gor, gor.x, or gor..y etc.
gor_w2_col <- grep("^gor", names(cleaned_data), value = TRUE)[1]
if (!is.na(gor_w2_col) && gor_w2_col != "") {
  cat("Found GOR column for wave 2:", gor_w2_col, "\n")
  cleaned_data <- cleaned_data %>%
    mutate(
      regov15 = convert_missing_codes(.data[[gor_w2_col]]),
      regov15 = convert_null_to_missing(regov15)
    )
}

# Wave 3 (Age 16): gor - need to find the second gor column
gor_cols <- grep("^gor", names(cleaned_data), value = TRUE)
if (length(gor_cols) >= 2) {
  gor_w3_col <- gor_cols[2]
  cat("Found GOR column for wave 3:", gor_w3_col, "\n")
  cleaned_data <- cleaned_data %>%
    mutate(
      regov16 = convert_missing_codes(.data[[gor_w3_col]]),
      regov16 = convert_null_to_missing(regov16)
    )
}

# Wave 8: W8DGOR
if ("W8DGOR" %in% names(cleaned_data)) {
  cleaned_data <- cleaned_data %>%
    mutate(
      regov_w8 = convert_missing_codes(W8DGOR),
      regov_w8 = convert_null_to_missing(regov_w8)
    )
}

# Wave 9: W9DRGN
if ("W9DRGN" %in% names(cleaned_data)) {
  cleaned_data <- cleaned_data %>%
    mutate(
      regov32 = convert_missing_codes(W9DRGN),
      regov32 = convert_null_to_missing(regov32)
    )
}

# Urban/Rural Indicator (time-varying - retain age-specific)
# Wave 2 (Age 15): urbind
urb_cols <- grep("^urbind", names(cleaned_data), value = TRUE)
if (length(urb_cols) >= 1) {
  urb_w2_col <- urb_cols[1]
  cat("Found URBIND column for wave 2:", urb_w2_col, "\n")
  cleaned_data <- cleaned_data %>%
    mutate(
      regub15 = convert_missing_codes(.data[[urb_w2_col]]),
      regub15 = convert_null_to_missing(regub15)
    )
}

# Wave 3 (Age 16): urbind - second column
if (length(urb_cols) >= 2) {
  urb_w3_col <- urb_cols[2]
  cat("Found URBIND column for wave 3:", urb_w3_col, "\n")
  cleaned_data <- cleaned_data %>%
    mutate(
      regub16 = convert_missing_codes(.data[[urb_w3_col]]),
      regub16 = convert_null_to_missing(regub16)
    )
}

# Nation of Residence (Wave 9, Age 32)
if ("W9NATIONRES" %in% names(cleaned_data)) {
  cleaned_data <- cleaned_data %>%
    mutate(
      regnat32 = convert_missing_codes(W9NATIONRES),
      regnat32 = convert_null_to_missing(regnat32)
    )
}

# ============================================================================
# 5. CREATE FACTOR VARIABLES WITH LABELS
# ============================================================================

# Government Office Region labels (harmonized across waves)
gor_labels <- c(
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-3" = "Not asked at fieldwork stage",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable",
  "1" = "North East",
  "2" = "North West",
  "3" = "Yorkshire and the Humber",
  "4" = "East Midlands",
  "5" = "West Midlands",
  "6" = "East of England",
  "7" = "London",
  "8" = "South East",
  "9" = "South West",
  "10" = "Wales",
  "11" = "Scotland",
  "12" = "Northern Ireland",
  "13" = "Unknown due to faulty/missing postcode"
)

# Urban/Rural labels
urb_labels <- c(
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-3" = "Not asked at fieldwork stage",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable",
  "1" = "Urban >= 10k - sparse",
  "2" = "Town & Fringe - sparse",
  "3" = "Village - sparse",
  "4" = "Hamlet and Isolated Dwelling - sparse",
  "5" = "Urban >= 10k - less sparse",
  "6" = "Town & Fringe - less sparse",
  "7" = "Village - less sparse",
  "8" = "Hamlet & Isolated Dwelling"
)

# Nation labels
nation_labels <- c(
  "-9" = "Refused",
  "-8" = "Don't know",
  "-3" = "Not asked at fieldwork stage",
  "-1" = "Not applicable",
  "1" = "England",
  "2" = "Scotland",
  "3" = "Wales",
  "4" = "Northern Ireland",
  "5" = "Outside of UK or unknown"
)

# Apply factor labels to created variables
if ("regov15" %in% names(cleaned_data)) {
  cleaned_data$regov15 <- factor(cleaned_data$regov15, 
                                  levels = as.numeric(names(gor_labels)),
                                  labels = gor_labels)
}

if ("regov16" %in% names(cleaned_data)) {
  cleaned_data$regov16 <- factor(cleaned_data$regov16,
                                  levels = as.numeric(names(gor_labels)),
                                  labels = gor_labels)
}

if ("regov_w8" %in% names(cleaned_data)) {
  cleaned_data$regov_w8 <- factor(cleaned_data$regov_w8,
                                   levels = as.numeric(names(gor_labels)),
                                   labels = gor_labels)
}

if ("regov32" %in% names(cleaned_data)) {
  cleaned_data$regov32 <- factor(cleaned_data$regov32,
                                  levels = as.numeric(names(gor_labels)),
                                  labels = gor_labels)
}

if ("regub15" %in% names(cleaned_data)) {
  cleaned_data$regub15 <- factor(cleaned_data$regub15,
                                  levels = as.numeric(names(urb_labels)),
                                  labels = urb_labels)
}

if ("regub16" %in% names(cleaned_data)) {
  cleaned_data$regub16 <- factor(cleaned_data$regub16,
                                  levels = as.numeric(names(urb_labels)),
                                  labels = urb_labels)
}

if ("regnat32" %in% names(cleaned_data)) {
  cleaned_data$regnat32 <- factor(cleaned_data$regnat32,
                                   levels = as.numeric(names(nation_labels)),
                                   labels = nation_labels)
}

# ============================================================================
# 6. SELECT FINAL OUTPUT VARIABLES
# ============================================================================

# Select only the ID and derived variables for output
output_vars <- c("NSID")

# Add all created age-specific variables
created_vars <- c("regov15", "regov16", "regov_w8", "regov32",
                  "regub15", "regub16", "regnat32")

# Only include variables that exist in the dataset
output_vars <- c(output_vars, created_vars[created_vars %in% names(cleaned_data)])

cat("\nOutput variables:", paste(output_vars, collapse = ", "), "\n")

# Create final output dataset
final_output <- cleaned_data %>% select(all_of(output_vars))

# ============================================================================
# 7. WRITE OUTPUT
# ============================================================================

write_csv(final_output, "data/output/cleaned_data.csv")

# Print summary
cat("\nData cleaning complete.\n")
cat("Number of observations:", nrow(final_output), "\n")
cat("Number of variables:", ncol(final_output), "\n")
cat("Variables included:", paste(names(final_output), collapse = ", "), "\n")
cat("Output written to: data/output/cleaned_data.csv\n")
