library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Suppress warnings for cleaner output
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Step 1: Load datasets with explicit column types
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Convert NSID to character for consistent joining
wave1$NSID <- as.character(wave1$NSID)
wave2$NSID <- as.character(wave2$NSID)
wave4$NSID <- as.character(wave4$NSID)

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Step 3: Select and rename variables
selected_vars <- merged_data %>%
  select(NSID, 
         W1hiqualmum, W2hiqualmum, w4hiqualmum, 
         W1hiqualdad, W2hiqualdad, w4hiqualdad) %>%
  rename(
    educdtlma_w1 = W1hiqualmum,
    educdtlma_w2 = W2hiqualmum,
    educdtlma_w4 = w4hiqualmum,
    educdtlpa_w1 = W1hiqualdad,
    educdtlpa_w2 = W2hiqualdad,
    educdtlpa_w4 = w4hiqualdad
  )

# Step 4: Consolidate education variables
consolidated <- selected_vars %>%
  mutate(
    educdtlma = coalesce(
      educdtlma_w1,
      educdtlma_w2,
      educdtlma_w4
    ),
    educdtlpa = coalesce(
      educdtlpa_w1,
      educdtlpa_w2,
      educdtlpa_w4
    )
  )

# Step 5: Define mapping for collapsed variables
map_collapsed <- function(detailed) {
  case_when(
    detailed %in% c(1, 2, 3, 4) ~ 0, 
    detailed %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1, 
    detailed == 18 ~ 2, 
    detailed == 19 ~ 3, 
    detailed == 20 ~ 4, 
    TRUE ~ detailed
  )
}

# Step 6: Harmonize missing value codes
harmonize_missing <- function(x) {
  x <- ifelse(x == -999, -3, x)  # Household data lost -> Not asked
  x <- ifelse(x == -99, -1, x)   # Not interviewed -> Not applicable
  x <- ifelse(x == -98, -1, x)   # Not present -> Not applicable
  x <- ifelse(x == -94, -8, x)   # Insufficient information -> Don't know/insufficient
  x <- ifelse(x == -92, -9, x)   # Refused -> Refusal
  x <- ifelse(x == -1, -8, x)    # Don't know -> Don't know/insufficient
  return(x)
}

# Apply harmonization and derive collapsed variables
consolidated <- consolidated %>%
  mutate(
    educdtlma = harmonize_missing(educdtlma),
    educdtlpa = harmonize_missing(educdtlpa),
    educma = map_collapsed(educdtlma),
    educpa = map_collapsed(educdtlpa)
  )

# Step 7: Define labels for detailed variables
detailed_labels <- setNames(
  c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed",
    "Schedule not applicable/Script error/information lost", "Item not applicable",
    "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", "Teaching qualification, non-degree",
    "Nursing qualification, non-degree", "A Levels", "OND/ONC", "City and guilds part III, NVQ3",
    "CSYS", "Scottish Higher Grade", "AS Level", "Trade apprenticeship",
    "City and guilds part II, NVQ2", "GCSE grade A-C and equivalent",
    "GCSE grade D-E and equivalent", "City and guilds part I, NVQ1",
    "Youth training, skill seekers", "Qualification, level unspecified",
    "No qualification mentioned"),
  c(-9, -8, -3, -2, -1, 1:20)
)

# Define labels for collapsed variables
collapsed_labels <- setNames(
  c("NVQ 4-5: degree-level qualifications and above", "NVQ 1-3: sub-degree qualifications",
    "None/entry: training programmes below NVQ level", "Other: qualifications where the level is unspecified",
    "No qualifications mentioned", "Refusal", "Don't know/insufficient information",
    "Not asked at the fieldwork stage/participated/interviewed",
    "Schedule not applicable/Script error/information lost", "Item not applicable"),
  c(0:4, -9, -8, -3, -2, -1)
)

# Step 8: Convert to labelled factors
final_data <- consolidated %>%
  mutate(
    educdtlma = factor(educdtlma, levels = names(detailed_labels), labels = detailed_labels),
    educdtlpa = factor(educdtlpa, levels = names(detailed_labels), labels = detailed_labels),
    educma = factor(educma, levels = names(collapsed_labels), labels = collapsed_labels),
    educpa = factor(educpa, levels = names(collapsed_labels), labels = collapsed_labels)
  ) %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Step 9: Write output
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Confirm successful execution
cat("Data cleaning and consolidation completed successfully!")
cat("Output file saved to: data/output/cleaned_data.csv")