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
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# ============================================================================
# 2. MERGE DATASETS BY NSID
# ============================================================================

# Start with wave 1 and progressively full_join all other waves
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID")

# ============================================================================
# 3. MISSING VALUE CODE HARMONIZATION
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
  # First convert NULL/NA to -3
  x[is.na(x)] <- -3
  
  # Convert wave-specific codes to standard codes
  # -94, -999 -> -8 (Don't know/insufficient information)
  x[x %in% c(-94, -999, -998, -997, -995, -92, -91)] <- -8
  
  # -100, -97 -> -3 (Not asked at fieldwork stage)
  x[x %in% c(-100, -97)] <- -3
  
  # -99 -> -2 (Schedule not applicable)
  x[x == -99] <- -2
  
  return(x)
}

# ============================================================================
# 4. VARIABLE RENAMING AND CREATION
# ============================================================================

# Rename variables according to naming conventions
# Age-specific variables: append age as numeric suffix

# Wave 2 (Age 15) variables
cleaned_data <- cleaned_data %>%
  mutate(
    # Urban/Rural at age 15
    regub15 = urbind.x,  # from wave2
    # Government Office Region at age 15
    regov15 = gor.x      # from wave2
  )

# Wave 3 (Age 16) variables
cleaned_data <- cleaned_data %>%
  mutate(
    # Urban/Rural at age 16
    regub16 = urbind.y,  # from wave3
    # Government Office Region at age 16
    regov16 = gor.y      # from wave3
  )

# Wave 8 variables
cleaned_data <- cleaned_data %>%
  mutate(
    # Government Office Region at wave 8 (approximately age 26-27)
    regov26 = W8DGOR
  )

# Wave 9 variables
cleaned_data <- cleaned_data %>%
  mutate(
    # Government Office Region at age 32
    regov32 = W9DRGN,
    # Nation of residence at age 32
    regnat32 = W9NATIONRES
  )

# ============================================================================
# 5. APPLY MISSING VALUE CONVERSION
# ============================================================================

# Apply missing value conversion to key variables
cleaned_data <- cleaned_data %>%
  mutate(
    regub15 = convert_missing_codes(regub15),
    regov15 = convert_missing_codes(regov15),
    regub16 = convert_missing_codes(regub16),
    regov16 = convert_missing_codes(regov16),
    regov26 = convert_missing_codes(regov26),
    regov32 = convert_missing_codes(regov32),
    regnat32 = convert_missing_codes(regnat32)
  )

# ============================================================================
# 6. CREATE FACTOR VARIABLES WITH LABELS
# ============================================================================

# Urban/Rural classification labels
urbind_labels <- c(
  "1" = "Urban >= 10k - sparse",
  "2" = "Town & Fringe - sparse",
  "3" = "Village - sparse",
  "4" = "Hamlet and Isolated Dwelling - sparse",
  "5" = "Urban >= 10k - less sparse",
  "6" = "Town & Fringe - less sparse",
  "7" = "Village - less sparse",
  "8" = "Hamlet & Isolated Dwelling",
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable",
  "-3" = "Not asked at fieldwork stage",
  "-7" = "Prefer not to say",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# Government Office Region labels (expanded for waves 8-9)
gor_labels <- c(
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
  "13" = "Unknown due to faulty/missing postcode",
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable",
  "-3" = "Not asked at fieldwork stage",
  "-7" = "Prefer not to say",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# Nation of residence labels
nation_labels <- c(
  "1" = "England",
  "2" = "Scotland",
  "3" = "Wales",
  "4" = "Northern Ireland",
  "5" = "Outside of UK or unknown",
  "-1" = "Item not applicable",
  "-2" = "Schedule not applicable",
  "-3" = "Not asked at fieldwork stage",
  "-7" = "Prefer not to say",
  "-8" = "Don't know",
  "-9" = "Refusal"
)

# Convert to factors with labels
cleaned_data <- cleaned_data %>%
  mutate(
    regub15 = factor(regub15, levels = c(-9:-3, -1, 1:8), labels = urbind_labels[as.character(c(-9:-3, -1, 1:8))]),
    regub16 = factor(regub16, levels = c(-9:-3, -1, 1:8), labels = urbind_labels[as.character(c(-9:-3, -1, 1:8))]),
    regov15 = factor(regov15, levels = c(-9:-3, -1, 1:9), labels = gor_labels[as.character(c(-9:-3, -1, 1:9))]),
    regov16 = factor(regov16, levels = c(-9:-3, -1, 1:9), labels = gor_labels[as.character(c(-9:-3, -1, 1:9))]),
    regov26 = factor(regov26, levels = c(-9:-3, -1, 1:13), labels = gor_labels[as.character(c(-9:-3, -1, 1:13))]),
    regov32 = factor(regov32, levels = c(-9:-3, -1, 1:13), labels = gor_labels[as.character(c(-9:-3, -1, 1:13))]),
    regnat32 = factor(regnat32, levels = c(-9:-3, -1, 1:5), labels = nation_labels[as.character(c(-9:-3, -1, 1:5))])
  )

# ============================================================================
# 7. CREATE DERIVED CATEGORICAL VARIABLES
# ============================================================================

# Create UK/abroad indicator from nation of residence
cleaned_data <- cleaned_data %>%
  mutate(
    reguk32 = case_when(
      regnat32 %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "UK",
      regnat32 == "Outside of UK or unknown" ~ "Abroad/Unknown",
      regnat32 %in% c("Item not applicable", "Schedule not applicable", "Not asked at fieldwork stage", 
                      "Prefer not to say", "Don't know", "Refusal") ~ as.character(regnat32),
      TRUE ~ "Unknown"
    ),
    reguk32 = factor(reguk32, levels = c("UK", "Abroad/Unknown", "Item not applicable", 
                                          "Schedule not applicable", "Not asked at fieldwork stage",
                                          "Prefer not to say", "Don't know", "Refusal"))
  )

# ============================================================================
# 8. SELECT FINAL OUTPUT VARIABLES
# ============================================================================

# Select only the ID and derived variables for output
final_data <- cleaned_data %>%
  select(
    NSID,
    regub15, regub16,
    regov15, regov16, regov26, regov32,
    regnat32,
    reguk32
  )

# ============================================================================
# 9. WRITE OUTPUT
# ============================================================================

write_csv(final_data, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete.\n")
cat("Number of observations:", nrow(final_data), "\n")
cat("Number of variables:", ncol(final_data), "\n")
cat("Output written to: data/output/cleaned_data.csv\n")
