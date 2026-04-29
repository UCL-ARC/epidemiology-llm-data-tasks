library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the three wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge by NSID using full_join
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Select only relevant variables
selected <- merged %>%
  select(NSID, W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad)

# Consolidate detailed education variables
# Priority: valid positive responses (1-20) in wave order (1, 2, 4), then fall back to negative codes
consolidate_detailed <- function(w1, w2, w4) {
  # First try to get positive values (1-20) in wave order
  result <- case_when(
    !is.na(w1) & w1 >= 1 & w1 <= 20 ~ w1,
    !is.na(w2) & w2 >= 1 & w2 <= 20 ~ w2,
    !is.na(w4) & w4 >= 1 & w4 <= 20 ~ w4,
    TRUE ~ NA_real_
  )
  
  # If no positive values exist, fall back to negative codes
  if (all(is.na(result) | (result >= 1 & result <= 20) == FALSE)) {
    result <- case_when(
      !is.na(w1) & w1 < 0 ~ w1,
      !is.na(w2) & w2 < 0 ~ w2,
      !is.na(w4) & w4 < 0 ~ w4,
      TRUE ~ NA_real_
    )
  }
  
  # Replace any remaining NA with -3 (Not asked at the fieldwork stage)
  result[is.na(result)] <- -3
  
  return(result)
}

# Consolidate maternal education
dtlma_raw <- consolidate_detailed(selected$W1hiqualmum, selected$W2hiqualmum, selected$w4hiqualmum)

# Consolidate paternal education
dtlpa_raw <- consolidate_detailed(selected$W1hiqualdad, selected$W2hiqualdad, selected$w4hiqualdad)

# Harmonize missing codes to standard scheme
harmonize_missing <- function(x) {
  case_when(
    x == -999 ~ -2,   # Missing - household data lost -> Schedule not applicable
    x == -99 ~ -3,    # Not interviewed -> Not asked at fieldwork
    x == -98 ~ -3,    # Not present -> Not asked at fieldwork
    x == -94 ~ -8,    # Insufficient information -> Don't know
    x == -92 ~ -9,    # Refused -> Refusal
    x == -91 ~ -1,    # Not applicable -> Item not applicable
    x == -1 ~ -8,     # Don't know -> Don't know
    TRUE ~ x          # Keep other values unchanged
  )
}

# Harmonize missing codes
dtlma <- harmonize_missing(dtlma_raw)
dtlpa <- harmonize_missing(dtlpa_raw)

# Create collapsed variables (mapping to NVQ scheme)
collapse_detailed <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 0,              # NVQ 4-5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4
    x >= 5 & x <= 17 ~ 1,             # NVQ 1-3: Teaching/Nursing non-degree through to City/Guilds NVQ2 (includes 10,11,12)
    x == 18 ~ 2,                      # Youth training
    x == 19 ~ 3,                      # Qualification level unspecified
    x == 20 ~ 4,                      # No qualification mentioned
    TRUE ~ x                          # Keep negative codes unchanged
  )
}

# Create collapsed variables
ma <- collapse_detailed(dtlma)
pa <- collapse_detailed(dtlpa)

# Create final dataset
final_data <- tibble(
  NSID = selected$NSID,
  educma = ma,
  educpa = pa,
  educdtlma = dtlma,
  educdtlpa = dtlpa
)

# Apply variable labels using labelled::set_variable_labels
final_data <- labelled::set_variable_labels(
  final_data,
  educma = "Maternal education (collapsed)",
  educpa = "Paternal education (collapsed)",
  educdtlma = "Maternal education (detailed)",
  educdtlpa = "Paternal education (detailed)"
)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Cleaned data saved to data/output/cleaned_data.csv\n")

# Check output
print(head(final_data))
print(summary(final_data))
