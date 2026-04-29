library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load files explicitly
df1 <- read_delim(paste0(path, "wave_one_lsype_young_person_2020.tab"), delim = "\t")
df4 <- read_delim(paste0(path, "wave_four_lsype_young_person_2020.tab"), delim = "\t")
df5 <- read_delim(paste0(path, "wave_five_lsype_young_person_2020.tab"), delim = "\t")
df6 <- read_delim(paste0(path, "wave_six_lsype_young_person_2020.tab"), delim = "\t")
df7 <- read_delim(paste0(path, "wave_seven_lsype_young_person_2020.tab"), delim = "\t")
df8 <- read_delim(paste0(path, "ns8_2015_derived.tab"), delim = "\t")
df9 <- read_delim(paste0(path, "ns9_2022_derived_variables.tab"), delim = "\t")

# Merge datasets
merged_df <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

# Harmonisation Scheme for collapsed 6-category economic activity:
# 1: Paid work
# 2: Education/Apprenticeship
# 3: Unemployed/Looking for work
# 4: Training course/scheme
# 5: Looking after family/home
# 6: Other (Sick, Disabled, Retired, Voluntary, etc.)

# Helper function for missing values
map_missing <- function(val, label = NULL) {
  if (is.na(val)) return(-3)
  # Based on rules: label is primary, numeric is sanity check
  # -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked, -2 Schedule/Script error, -1 Not applicable
  # We'll handle specific mappings inside each wave logic
  return(val)
}

# Process Wave 4 (Age 17)
merged_df <- merged_df %>%
  mutate(
    ecoact17 = case_when(
      W4empsYP == 1 | W4empsYP == 2 ~ 1,
      W4empsYP == 5 ~ 2,
      W4empsYP == 3 ~ 3,
      W4empsYP == 4 ~ 4,
      W4empsYP == 6 ~ 5,
      W4empsYP == 7 | W4empsYP == 8 | W4empsYP == 9 ~ 6,
      W4empsYP == -92 ~ -9, # Refused
      W4empsYP == -94 ~ -8, # Insufficient info
      W4empsYP == -91 ~ -1, # Not applicable
      W4empsYP == -999 ~ -2, # Lost
      TRUE ~ -3
    )
  )

# Process Wave 5 (Age 18)
merged_df <- merged_df %>%
  mutate(
    ecoact18 = case_when(
      W5mainactYP == 3 ~ 1,
      W5mainactYP == 1 | W5mainactYP == 2 | W5mainactYP == 4 ~ 2,
      W5mainactYP == 7 ~ 3,
      W5mainactYP == 5 | W5mainactYP == 6 ~ 4,
      W5mainactYP == 8 ~ 5,
      W5mainactYP == 9 | W5mainactYP == 10 | W5mainactYP == 11 ~ 6,
      W5mainactYP == -94 ~ -8,
      TRUE ~ -3
    )
  )

# Process Wave 6 (Age 19)
merged_df <- merged_df %>%
  mutate(
    ecoact19 = case_when(
      W6TCurrentAct == 3 ~ 1,
      W6TCurrentAct == 1 | W6TCurrentAct == 2 | W6TCurrentAct == 5 ~ 2,
      W6TCurrentAct == 8 ~ 3,
      W6TCurrentAct == 4 ~ 4,
      W6TCurrentAct == 7 ~ 5,
      W6TCurrentAct == 6 | W6TCurrentAct == 9 | W6TCurrentAct == 10 | W6TCurrentAct == 11 ~ 6,
      W6TCurrentAct == -91 ~ -1,
      TRUE ~ -3
    )
  )

# Process Wave 7 (Age 20)
merged_df <- merged_df %>%
  mutate(
    ecoact20 = case_when(
      W7TCurrentAct == 3 | W7TCurrentAct == 9 ~ 1,
      W7TCurrentAct == 1 | W7TCurrentAct == 2 | W7TCurrentAct == 5 ~ 2,
      W7TCurrentAct == 8 ~ 3,
      W7TCurrentAct == 4 | W7TCurrentAct == 11 ~ 4,
      W7TCurrentAct == 7 ~ 5,
      W7TCurrentAct == 6 | W7TCurrentAct == 10 | W7TCurrentAct == 12 | W7TCurrentAct == 13 | W7TCurrentAct == 14 | W7TCurrentAct == 15 ~ 6,
      W7TCurrentAct == -91 ~ -1,
      TRUE ~ -3
    )
  )

# Process Wave 8 (Age 25)
merged_df <- merged_df %>%
  mutate(
    ecoact25 = case_when(
      W8DACTIVITYC == 1 | W8DACTIVITYC == 2 ~ 1,
      W8DACTIVITYC == 5 | W8DACTIVITYC == 6 ~ 2,
      W8DACTIVITYC == 4 ~ 3,
      W8DACTIVITYC == 7 ~ 4,
      W8DACTIVITYC == 9 ~ 5,
      W8DACTIVITYC == 3 | W8DACTIVITYC == 8 | W8DACTIVITYC == 10 ~ 6,
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    ),
    ecoactadu25 = case_when(
      W8DACTIVITYC == -9 ~ -9,
      W8DACTIVITYC == -8 ~ -8,
      W8DACTIVITYC == -1 ~ -1,
      TRUE ~ coalesce(W8DACTIVITYC, -3)
    )
  )

# Process Wave 9 (Age 32)
merged_df <- merged_df %>%
  mutate(
    ecoact32 = case_when(
      W9DACTIVITYC == 1 | W9DACTIVITYC == 2 ~ 1,
      W9DACTIVITYC == 5 | W9DACTIVITYC == 6 ~ 2,
      W9DACTIVITYC == 4 ~ 3,
      W9DACTIVITYC == 7 ~ 4,
      W9DACTIVITYC == 9 ~ 5,
      W9DACTIVITYC == 3 | W9DACTIVITYC == 8 | W9DACTIVITYC == 10 ~ 6,
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      TRUE ~ -3
    ),
    ecoactadu32 = case_when(
      W9DACTIVITYC == -9 ~ -9,
      W9DACTIVITYC == -8 ~ -8,
      W9DACTIVITYC == -1 ~ -1,
      TRUE ~ coalesce(W9DACTIVITYC, -3)
    )
  )

# Define Labels for collapsed variables
ecoact_labels <- c(
  "1" = "Paid work",
  "2" = "Education/Apprenticeship",
  "3" = "Unemployed/Looking for work",
  "4" = "Training course/scheme",
  "5" = "Looking after family/home",
  "6" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule/Script error",
  "-1" = "Not applicable"
)

# Apply labels as factors
final_vars <- c("NSID", "ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32", "ecoactadu25", "ecoactadu32")
output_df <- merged_df %>%
  select(all_of(final_vars)) %>%
  mutate(across(starts_with("ecoact") & !contains("adu"), ~ factor(.x, levels = as.numeric(names(ecoact_labels)), labels = ecoact_labels)))

# Since the task requires CSV output, we should keep the underlying numeric codes or strings. 
# To preserve factor labels in CSV, we can't easily. Usually, we keep them numeric and provide the labels if needed. 
# But the instruction says "create labelled factors". In readr::write_csv, factors are written as labels.
# However, a safer way for "analysis-ready" is often numeric. Let's stick to the request: "labelled factors".

write_csv(output_df, "data/output/cleaned_data.csv")