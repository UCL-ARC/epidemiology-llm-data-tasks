library(haven)
library(dplyr)
library(readr)
library(labelled)

# Vectorized function to map detailed to collapsed education
vectorized_detailed_to_collapsed <- function(detailed) {
  result <- numeric(length(detailed))
  
  # NVQ 4-5: degree-level qualifications and above
  result[detailed %in% c(1, 2, 3, 4)] <- 0
  # NVQ 1-3: sub-degree qualifications
  result[detailed %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)] <- 1
  # None/entry: training programmes below NVQ level
  result[detailed == 18] <- 2
  # Other: qualifications where the level is unspecified
  result[detailed == 19] <- 3
  # No qualifications mentioned
  result[detailed == 20] <- 4
  
  # Missing codes remain unchanged
  result[is.na(detailed)] <- NA_integer_
  return(result)
}

# Define the mapping for missing codes
missing_code_map <- setNames(
  c(-2, -1, -1, -8, -9, -1, -8),
  c(-999, -99, -98, -94, -92, -91, -1)
)

# Load the datasets
wave_one <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Ensure NSID is treated as a character variable
wave_one <- wave_one %>% mutate(NSID = as.character(NSID))
wave_two <- wave_two %>% mutate(NSID = as.character(NSID))
wave_four <- wave_four %>% mutate(NSID = as.character(NSID))

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Function to map missing codes
map_missing <- function(x) {
  ifelse(is.na(x), -3, 
         ifelse(x %in% names(missing_code_map), 
                missing_code_map[as.character(x)], 
                x))
}

# Consolidate maternal education
merged_data <- merged_data %>%
  mutate(
    educdtlma = coalesce(
      map_missing(w4hiqualmum),
      map_missing(W2hiqualmum),
      map_missing(W1hiqualmum)
    )
  )

# Consolidate paternal education
merged_data <- merged_data %>%
  mutate(
    educdtlpa = coalesce(
      map_missing(w4hiqualdad),
      map_missing(W2hiqualdad),
      map_missing(W1hiqualdad)
    )
  )

# Create collapsed variables
merged_data <- merged_data %>%
  mutate(
    educma = vectorized_detailed_to_collapsed(educdtlma),
    educpa = vectorized_detailed_to_collapsed(educdtlpa)
  )

# Create labeled factors for detailed variables
levels_detailed <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
labels_detailed <- c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", 
                     "Schedule not applicable/Script error/information lost", "Item not applicable", 
                     "Higher Degree", "First Degree", "HE Diploma", "HNC/HND/NVQ4", 
                     "Teaching qualification, non-degree", "Nursing qualification, non-degree", "A Levels", 
                     "OND/ONC", "City and guilds part III, NVQ3", "CSYS", "Scottish Higher Grade", 
                     "AS Level", "Trade apprenticeship", "City and guilds part II, NVQ2", 
                     "GCSE grade A-C and equivalent", "GCSE grade D-E and equivalent", 
                     "City and guilds part I, NVQ1", "Youth training, skill seekers", 
                     "Qualification, level unspecified", "No qualification mentioned")

# Create labeled factors for collapsed variables
levels_collapsed <- c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4)
labels_collapsed <- c("Refusal", "Don't know/insufficient information", "Not asked at the fieldwork stage/participated/interviewed", 
                      "Schedule not applicable/Script error/information lost", "Item not applicable", 
                      "NVQ 4–5: degree-level qualifications and above", 
                      "NVQ 1–3: sub-degree qualifications", 
                      "None/entry: training programmes below NVQ level", 
                      "Other: qualifications where the level is unspecified", 
                      "No qualifications mentioned")

# Create the final dataset with only required variables
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa) %>%
  mutate(
    educdtlma = factor(educdtlma, levels = levels_detailed, labels = labels_detailed),
    educdtlpa = factor(educdtlpa, levels = levels_detailed, labels = labels_detailed),
    educma = factor(educma, levels = levels_collapsed, labels = labels_collapsed),
    educpa = factor(educpa, levels = levels_collapsed, labels = labels_collapsed)
  )

# Write the final dataset to CSV
write_csv(final_data, "data/output/cleaned_data.csv")