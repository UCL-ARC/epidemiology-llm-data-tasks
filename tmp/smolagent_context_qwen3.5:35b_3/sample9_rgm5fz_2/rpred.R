library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the three wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab",
                     delim = "\t",
                     show_col_types = FALSE)

wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab",
                     delim = "\t",
                     show_col_types = FALSE)

wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab",
                     delim = "\t",
                     show_col_types = FALSE)

# Merge using full_join by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Select only parental education variables + NSID
selected <- merged %>%
  select(NSID, W1hiqualmum, W1hiqualdad,
         W2hiqualmum, W2hiqualdad,
         w4hiqualmum, w4hiqualdad)

# Function to harmonize missing codes to standard scheme
harmonize_missing <- function(x) {
  x <- case_when(
    x == -999 ~ -3,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    is.na(x) ~ -3,
    TRUE ~ x
  )
  return(x)
}

# Consolidate detailed maternal education (educdtlma)
selected$educdtlma <- case_when(
  !is.na(selected$W1hiqualmum) && selected$W1hiqualmum %in% 1:20 ~ selected$W1hiqualmum,
  !is.na(selected$W2hiqualmum) && selected$W2hiqualmum %in% 1:20 ~ selected$W2hiqualmum,
  !is.na(selected$w4hiqualmum) && selected$w4hiqualmum %in% 1:20 ~ selected$w4hiqualmum,
  !is.na(selected$W1hiqualmum) ~ selected$W1hiqualmum,
  !is.na(selected$W2hiqualmum) ~ selected$W2hiqualmum,
  !is.na(selected$w4hiqualmum) ~ selected$w4hiqualmum,
  TRUE ~ NA_real_
)

# Consolidate detailed paternal education (educdtlpa)
selected$educdtlpa <- case_when(
  !is.na(selected$W1hiqualdad) && selected$W1hiqualdad %in% 1:20 ~ selected$W1hiqualdad,
  !is.na(selected$W2hiqualdad) && selected$W2hiqualdad %in% 1:20 ~ selected$W2hiqualdad,
  !is.na(selected$w4hiqualdad) && selected$w4hiqualdad %in% 1:20 ~ selected$w4hiqualdad,
  !is.na(selected$W1hiqualdad) ~ selected$W1hiqualdad,
  !is.na(selected$W2hiqualdad) ~ selected$W2hiqualdad,
  !is.na(selected$w4hiqualdad) ~ selected$w4hiqualdad,
  TRUE ~ NA_real_
)

# Harmonize missing codes
selected$educdtlma <- harmonize_missing(selected$educdtlma)
selected$educdtlpa <- harmonize_missing(selected$educdtlpa)

# Collapse detailed to 5-level NVQ scheme
collapse_nqv <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 0,
    x %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
    x == 18 ~ 2,
    x == 19 ~ 3,
    x == 20 ~ 4,
    TRUE ~ x
  )
}

selected$educma <- collapse_nqv(selected$educdtlma)
selected$educpa <- collapse_nqv(selected$educdtlpa)

# Define labels as named vectors with character keys
detailed_labels_vec <- c(
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal",
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned"
)

collapsed_labels_vec <- c(
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal",
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: qualifications where level is unspecified or cannot be determined",
  "4" = "No qualifications mentioned"
)

# Convert to labelled factors using factor() with labels argument
# First convert to character to handle all values as strings
selected$educdtlma <- factor(as.character(selected$educdtlma), 
                              levels = names(detailed_labels_vec),
                              labels = detailed_labels_vec)
selected$educdtlpa <- factor(as.character(selected$educdtlpa), 
                              levels = names(detailed_labels_vec),
                              labels = detailed_labels_vec)
selected$educma <- factor(as.character(selected$educma), 
                          levels = names(collapsed_labels_vec),
                          labels = collapsed_labels_vec)
selected$educpa <- factor(as.character(selected$educpa), 
                          levels = names(collapsed_labels_vec),
                          labels = collapsed_labels_vec)

# Select final 5 variables
final_data <- selected %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Summary statistics:\n")
print(table(final_data$educma, useNA = "ifany"))
print(table(final_data$educpa, useNA = "ifany"))
print(table(final_data$educdtlma, useNA = "ifany"))
print(table(final_data$educdtlpa, useNA = "ifany"))
