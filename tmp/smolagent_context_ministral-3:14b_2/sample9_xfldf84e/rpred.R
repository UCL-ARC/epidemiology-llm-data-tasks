
# Load required libraries
library(haven)
library(dplyr)
library(forcats)
library(readr)

# Step 1: Load datasets from specified directory
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Convert NSID to character to ensure proper merging
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Step 3: Select relevant variables
selected_vars <- merged_data %>%
  select(NSID,
         W1hiqualmum, W2hiqualmum, w4hiqualmum,
         W1hiqualdad, W2hiqualdad, w4hiqualdad)

# Step 4: Consolidate detailed parental education variables
selected_vars <- selected_vars %>%
  mutate(
    educdtlma = coalesce(w4hiqualmum, W2hiqualmum, W1hiqualmum, -3),
    educdtlpa = coalesce(w4hiqualdad, W2hiqualdad, W1hiqualdad, -3)
  )

# Step 5: Define mapping for collapsed variables
collapse_mapping <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 0,
    x %in% c(5:16) ~ 1,
    x %in% c(17, 18) ~ 2,
    x == 19 ~ 3,
    x == 20 ~ 4,
    TRUE ~ NA_integer_
  )
}

# Step 6: Create collapsed variables
selected_vars <- selected_vars %>%
  mutate(
    educma = collapse_mapping(educdtlma),
    educpa = collapse_mapping(educdtlpa)
  )

# Step 7: Define labels for detailed variables
detailed_labels <- c(
  "Missing - household data lost", "Parent not interviewed", "Parent not present",
  "Insufficient information", "Refused", "Not applicable",
  "Not asked at the fieldwork stage/participated/interviewed",
  "Schedule not applicable/Script error/information lost", "Don't know/insufficient information",
  "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4",
  "Teaching qualification, non-degree", "Nursing qualification, non-degree",
  "A Levels", "OND/ONC", "City and Guilds Part III, NVQ3", "CSYS",
  "Scottish Higher Grade", "AS Level", "Trade apprenticeship",
  "City and Guilds Part II, NVQ2", "GCSE grade A-C and equivalent",
  "GCSE grade D-E and equivalent", "City and Guilds Part I, NVQ1",
  "Youth training, skill seekers", "Qualification, level unspecified",
  "No qualification mentioned"
)

# Define levels for detailed variables
detailed_levels <- c(-999, -99, -98, -94, -92, -91, -3, -2, -1, 1:20)

# Apply labels to detailed variables
selected_vars <- selected_vars %>%
  mutate(
    educdtlma = factor(educdtlma, levels = detailed_levels, labels = detailed_labels),
    educdtlpa = factor(educdtlpa, levels = detailed_levels, labels = detailed_labels)
  )

# Define labels for collapsed variables
collapse_labels <- c(
  "NVQ 4–5: degree-level qualifications and above",
  "NVQ 1–3: sub-degree qualifications",
  "None/entry: training programmes below NVQ level",
  "Other: qualifications where the level is unspecified or cannot be determined",
  "No qualifications mentioned"
)

# Apply labels to collapsed variables
selected_vars <- selected_vars %>%
  mutate(
    educma = factor(educma, levels = 0:4, labels = collapse_labels),
    educpa = factor(educpa, levels = 0:4, labels = collapse_labels)
  )

# Step 8: Select final variables
final_vars <- selected_vars %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Step 9: Write output
write.csv(final_vars, "data/output/cleaned_data.csv", row.names = FALSE)
