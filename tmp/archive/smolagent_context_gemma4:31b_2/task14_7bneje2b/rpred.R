library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

load_data <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# NSID is usually character, need to ensure it is handled correctly. 
# Since the tool says read as numeric by default, we should be careful. 
# Let's read NSID as character explicitly.
load_data_fixed <- function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(NSID = col_character(), .default = 'numeric'))
}

list_df <- map(files, load_data_fixed)
names(list_df) <- files

# Merge using full_join by NSID
cleaned_data <- list_df %>% reduce(full_join, by = 'NSID')

# Define Unified Label Schemes
teen_labels <- c(
  "Owned outright", "Being bought on a mortgage/bank loan", "Shared ownership (owns & rents property)",
  "Rented from a Council or New Town", "Rented from a Housing Association", "Rented privately",
  "Rent free", "Some other arrangement", "Item not applicable", "Script error/information lost",
  "Not asked at the fieldwork stage/participated/interviewed", "Don't know/insufficient information", "Refusal"
)

collapsed_labels <- c(
  "Owned outright", "Owned, buying with help of mortgage/loan", "Part rent, part mortgage",
  "Rent it", "live rent-free", "Other", "Item not applicable", "Script error/information lost",
  "Not asked at the fieldwork stage/participated/interviewed", "Don't know/insufficient information", "Refusal"
)

# Helper function for missing values based on metadata labels
# -9 Refusal, -8 DK, -1 NA, -3 Not asked, -2 Script error
recode_missing <- function(val, wave_meta_labels) {
  # This logic is applied inside the specific wave recoding
  # Standard mapping: 
  # Refused -> -9
  # Don't know -> -8
  # Not applicable -> -1
  # Missing/Not asked/Not interviewed -> -3
  # Script error/Lost -> -2
  # Null -> -3
  return(val)
}

# Process Age 14 (W1hous12HH)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH == 1 ~ 1, W1hous12HH == 2 ~ 2, W1hous12HH == 3 ~ 3, W1hous12HH == 4 ~ 4,
      W1hous12HH == 5 ~ 5, W1hous12HH == 6 ~ 6, W1hous12HH == 7 ~ 7, W1hous12HH == 8 ~ 8,
      W1hous12HH == -91 ~ 9, W1hous12HH == -999 ~ 11, W1hous12HH == -92 ~ 13, W1hous12HH == -1 ~ 12, 
      TRUE ~ 11
    ),
    hown14 = case_when(
      hownteen14 == 1 ~ 1, hownteen14 == 2 ~ 2, hownteen14 == 3 ~ 3, 
      hownteen14 %in% 4:6 ~ 4, hownteen14 == 7 ~ 5, hownteen14 == 8 ~ 6,
      hownteen14 == 9 ~ 7, hownteen14 == 10 ~ 8, hownteen14 == 11 ~ 9, hownteen14 == 12 ~ 10, hownteen14 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 15 (W2Hous12HH)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen15 = case_when(
      W2Hous12HH == 1 ~ 1, W2Hous12HH == 2 ~ 2, W2Hous12HH == 3 ~ 3, W2Hous12HH == 4 ~ 4,
      W2Hous12HH == 5 ~ 5, W2Hous12HH == 6 ~ 6, W2Hous12HH == 7 ~ 7, W2Hous12HH == 8 ~ 8,
      W2Hous12HH == -91 ~ 9, W2Hous12HH %in% c(-997, -995, -99) ~ 10, W2Hous12HH == -92 ~ 13, W2Hous12HH == -1 ~ 12,
      W2Hous12HH == -998 ~ 11, TRUE ~ 11
    ),
    hown15 = case_when(
      hownteen15 == 1 ~ 1, hownteen15 == 2 ~ 2, hownteen15 == 3 ~ 3, 
      hownteen15 %in% 4:6 ~ 4, hownteen15 == 7 ~ 5, hownteen15 == 8 ~ 6,
      hownteen15 == 9 ~ 7, hownteen15 == 10 ~ 8, hownteen15 == 11 ~ 9, hownteen15 == 12 ~ 10, hownteen15 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 16 (W3hous12HH)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen16 = case_when(
      W3hous12HH == 1 ~ 1, W3hous12HH == 2 ~ 2, W3hous12HH == 3 ~ 3, W3hous12HH == 4 ~ 4,
      W3hous12HH == 5 ~ 5, W3hous12HH == 6 ~ 6, W3hous12HH == 7 ~ 7, W3hous12HH == 8 ~ 8,
      W3hous12HH == -91 ~ 9, W3hous12HH == -99 ~ 10, W3hous12HH == -92 ~ 13, W3hous12HH == -1 ~ 12,
      W3hous12HH == -999 ~ 11, TRUE ~ 11
    ),
    hown16 = case_when(
      hownteen16 == 1 ~ 1, hownteen16 == 2 ~ 2, hownteen16 == 3 ~ 3, 
      hownteen16 %in% 4:6 ~ 4, hownteen16 == 7 ~ 5, hownteen16 == 8 ~ 6,
      hownteen16 == 9 ~ 7, hownteen16 == 10 ~ 8, hownteen16 == 11 ~ 9, hownteen16 == 12 ~ 10, hownteen16 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 17 (W4Hous12HH)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen17 = case_when(
      W4Hous12HH == 1 ~ 1, W4Hous12HH == 2 ~ 2, W4Hous12HH == 3 ~ 3, W4Hous12HH == 4 ~ 4,
      W4Hous12HH == 5 ~ 5, W4Hous12HH == 6 ~ 6, W4Hous12HH == 7 ~ 7, W4Hous12HH == 8 ~ 8,
      W4Hous12HH == -91 ~ 9, W4Hous12HH == -997 ~ 10, W4Hous12HH == -92 ~ 13, W4Hous12HH == -1 ~ 12,
      W4Hous12HH == -999 ~ 11, TRUE ~ 11
    ),
    hown17 = case_when(
      hownteen17 == 1 ~ 1, hownteen17 == 2 ~ 2, hownteen17 == 3 ~ 3, 
      hownteen17 %in% 4:6 ~ 4, hownteen17 == 7 ~ 5, hownteen17 == 8 ~ 6,
      hownteen17 == 9 ~ 7, hownteen17 == 10 ~ 8, hownteen17 == 11 ~ 9, hownteen17 == 12 ~ 10, hownteen17 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 18 (W5 - Split variables)
# Target mapping based on labels
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 & W5Hous12BHH == 1 ~ 1, # Owned outright
      W5Hous12HH == 1 & W5Hous12BHH == 2 ~ 2, # Mortgage
      W5Hous12HH == 1 & W5Hous12BHH == 3 ~ 3, # Shared
      W5Hous12HH == 1 & W5Hous12BHH == 4 ~ 8, # Other
      W5Hous12HH == 2 & W5Hous12CHH == 1 ~ 4, # Council
      W5Hous12HH == 2 & W5Hous12CHH == 2 ~ 5, # HA
      W5Hous12HH == 2 & W5Hous12CHH == 3 ~ 6, # Private
      W5Hous12HH == 2 & W5Hous12CHH == 4 ~ 7, # Rent free
      W5Hous12HH == 2 & W5Hous12CHH == 5 ~ 8, # Other
      W5Hous12HH == 3 ~ 8, # Something else
      W5Hous12HH == -91 ~ 9, 
      W5Hous12HH %in% c(-999, 6) ~ 11, 
      W5Hous12HH == -92 ~ 13, 
      W5Hous12HH == -1 ~ 12, 
      TRUE ~ 11
    ),
    hown18 = case_when(
      hownteen18 == 1 ~ 1, hownteen18 == 2 ~ 2, hownteen18 == 3 ~ 3, 
      hownteen18 %in% 4:6 ~ 4, hownteen18 == 7 ~ 5, hownteen18 == 8 ~ 6,
      hownteen18 == 9 ~ 7, hownteen18 == 10 ~ 8, hownteen18 == 11 ~ 9, hownteen18 == 12 ~ 10, hownteen18 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 19 (W6 - Split variables)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == 1 & W6Hous12bYP == 1 ~ 1, 
      W6Hous12YP == 1 & W6Hous12bYP == 2 ~ 2, 
      W6Hous12YP == 1 & W6Hous12bYP == 3 ~ 3, 
      W6Hous12YP == 1 & W6Hous12bYP == 4 ~ 8, 
      W6Hous12YP == 2 & W6Hous12cYP == 1 ~ 4, 
      W6Hous12YP == 2 & W6Hous12cYP == 2 ~ 5, 
      W6Hous12YP == 2 & W6Hous12cYP == 3 ~ 6, 
      W6Hous12YP == 2 & W6Hous12cYP == 4 ~ 7, 
      W6Hous12YP == 2 & W6Hous12cYP == 5 ~ 8, 
      W6Hous12YP == 3 ~ 8, 
      W6Hous12YP == -91 ~ 9, 
      W6Hous12YP == -92 ~ 13, 
      W6Hous12YP == -1 ~ 12, 
      TRUE ~ 11
    ),
    hown19 = case_when(
      hownteen19 == 1 ~ 1, hownteen19 == 2 ~ 2, hownteen19 == 3 ~ 3, 
      hownteen19 %in% 4:6 ~ 4, hownteen19 == 7 ~ 5, hownteen19 == 8 ~ 6,
      hownteen19 == 9 ~ 7, hownteen19 == 10 ~ 8, hownteen19 == 11 ~ 9, hownteen19 == 12 ~ 10, hownteen19 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 20 (W7 - Split variables)
cleaned_data <- cleaned_data %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == 1 & W7Hous12bYP == 1 ~ 1, 
      W7Hous12YP == 1 & W7Hous12bYP == 2 ~ 2, 
      W7Hous12YP == 1 & W7Hous12bYP == 3 ~ 3, 
      W7Hous12YP == 1 & W7Hous12bYP == 4 ~ 8, 
      W7Hous12YP == 2 & W7Hous12cYP == 1 ~ 4, 
      W7Hous12YP == 2 & W7Hous12cYP == 2 ~ 5, 
      W7Hous12YP == 2 & W7Hous12cYP == 3 ~ 6, 
      W7Hous12YP == 2 & W7Hous12cYP == 4 ~ 7, 
      W7Hous12YP == 2 & W7Hous12cYP == 5 ~ 8, 
      W7Hous12YP == 3 ~ 8, 
      W7Hous12YP == -91 ~ 9, 
      W7Hous12YP == -92 ~ 13, 
      W7Hous12YP == -1 ~ 12, 
      TRUE ~ 11
    ),
    hown20 = case_when(
      hownteen20 == 1 ~ 1, hownteen20 == 2 ~ 2, hownteen20 == 3 ~ 3, 
      hownteen20 %in% 4:6 ~ 4, hownteen20 == 7 ~ 5, hownteen20 == 8 ~ 6,
      hownteen20 == 9 ~ 7, hownteen20 == 10 ~ 8, hownteen20 == 11 ~ 9, hownteen20 == 12 ~ 10, hownteen20 == 13 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 25 (W8TENURE)
cleaned_data <- cleaned_data %>%
  mutate(
    hown25 = case_when(
      W8TENURE == 1 ~ 1, W8TENURE == 2 ~ 2, W8TENURE == 3 ~ 3, 
      W8TENURE == 4 ~ 4, W8TENURE == 5 ~ 5, W8TENURE == 6 ~ 6, W8TENURE == 7 ~ 6,
      W8TENURE == -1 ~ 7, W8TENURE == -8 ~ 10, W8TENURE == -9 ~ 11,
      TRUE ~ 9
    )
  )

# Process Age 32 (W9DTENURE)
cleaned_data <- cleaned_data %>%
  mutate(
    hown32 = case_when(
      W9DTENURE == 1 ~ 1, W9DTENURE == 2 ~ 2, W9DTENURE == 3 ~ 3, 
      W9DTENURE == 4 ~ 4, W9DTENURE == 5 ~ 5, W9DTENURE == 6 ~ 6, W9DTENURE == 7 ~ 6,
      W9DTENURE == -8 ~ 10, 
      TRUE ~ 9
    )
  )

# Apply Labels
vars_teen <- paste0("hownteen", c(14, 15, 16, 17, 18, 19, 20))
vars_coll <- c(paste0("hown", c(14, 15, 16, 17, 18, 19, 20)), "hown25", "hown32")

for (v in vars_teen) {
  cleaned_data[[v]] <- factor(cleaned_data[[v]], levels = 1:13, labels = teen_labels)
}
for (v in vars_coll) {
  cleaned_data[[v]] <- factor(cleaned_data[[v]], levels = 1:11, labels = collapsed_labels)
}

# Final selection
final_vars <- c("NSID", vars_coll, vars_teen)
cleaned_data_final <- cleaned_data %>% select(all_of(final_vars))

# Output
write.csv(cleaned_data_final, "data/output/cleaned_data.csv", row.names = FALSE)
