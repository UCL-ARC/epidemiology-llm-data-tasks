library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Convert numeric columns to numeric
convert_to_num <- function(df, cols) {
  df %>% mutate(across(all_of(cols), as.numeric))
}

file1 <- convert_to_num(file1, c("W1hiqualmum", "W1hiqualdad"))
file2 <- convert_to_num(file2, c("W2hiqualmum", "W2hiqualdad"))
file4 <- convert_to_num(file4, c("w4hiqualmum", "w4hiqualdad"))

# Merge datasets
merged_data <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file4, by = "NSID")

# 6. Missing Value Code Harmonization mapping
# Mapping based on provided metadata labels
# -9 = Refusal, -8 = Don't know, -3 = Not asked/participated, -2 = Schedule not applicable/Script error, -1 = Item not applicable
# Null -> -3

harmonize_codes <- function(val) {
  if (is.na(val)) return(-3)
  
  # Logic based on metadata value_labels across waves
  # W1/W2/W4 common patterns:
  # -999: Missing household data lost -> -2 (information lost)
  # -99: Not interviewed -> -3 (Not asked/participated)
  # -98: Not present -> -3 (Not asked/participated)
  # -94: Insufficient information -> -8 (Don't know/insufficient info)
  # -92: Refused -> -9 (Refusal)
  # -91: Not applicable -> -1 (Item not applicable)
  # -1: Don't know (specifically in Father's W1/W2) -> -8
  
  if (val == -999) return(-2)
  if (val == -99) return(-3)
  if (val == -98) return(-3)
  if (val == -94) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -1) return(-8)
  
  return(val)
}

# Apply harmonization to all possible education variables
edu_vars <- c("W1hiqualmum", "W1hiqualdad", "W2hiqualmum", "W2hiqualdad", "w4hiqualmum", "w4hiqualdad")
merged_data <- merged_data %>%
  mutate(across(all_of(edu_vars), ~ sapply(.x, harmonize_codes)))

# 4. Consolidation Logic
consolidate_edu <- function(w1, w2, w4) {
  # Prioritize positive values (>= 1)
  res <- case_when(
    w1 >= 1 ~ w1,
    w2 >= 1 ~ w2,
    w4 >= 1 ~ w4,
    # Fall back to negative codes in wave order
    TRUE ~ coalesce(w1, w2, w4)
  )
  return(res)
}

merged_data <- merged_data %>%
  mutate(
    educdtlma = consolidate_edu(W1hiqualmum, W2hiqualmum, w4hiqualmum),
    educdtlpa = consolidate_edu(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  )

# 5. Derived Collapsed Variables
collapse_edu <- function(val) {
  if (is.na(val) || val < 1) return(val)
  if (val >= 1 && val <= 4) return(0) # NVQ 4-5
  if (val >= 5 && val <= 17) return(1) # NVQ 1-3
  if (val == 18) return(2) # None/entry
  if (val == 19) return(3) # Other
  if (val == 20) return(4) # No qualifications
  return(val)
}

merged_data <- merged_data %>%
  mutate(
    educma = sapply(educdtlma, collapse_edu),
    educpa = sapply(educdtlpa, collapse_edu)
  )

# 7. Factor Variables and Labels
# Detailed labels
detailed_labels <- c(
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree",
  "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level",
  "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified", "20" = "No qualification mentioned",
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked/participated", "-2" = "Schedule not applicable/Script error",
  "-1" = "Item not applicable"
)

# Collapsed labels
collapsed_labels <- c(
  "0" = "NVQ 4-5", "1" = "NVQ 1-3", "2" = "None/entry",
  "3" = "Other", "4" = "No qualifications",
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked/participated", "-2" = "Schedule not applicable/Script error",
  "-1" = "Item not applicable"
)

# Apply labels and convert to factors
final_df <- merged_data %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa) %>%
  mutate(across(c(educdtlma, educdtlpa), ~ factor(.x, levels = as.numeric(names(detailed_labels)), labels = detailed_labels))) %>%
  mutate(across(c(educma, educpa), ~ factor(.x, levels = as.numeric(names(collapsed_labels)), labels = collapsed_labels)))

# 10. Output Requirements
write_csv(final_df, "data/output/cleaned_data.csv")