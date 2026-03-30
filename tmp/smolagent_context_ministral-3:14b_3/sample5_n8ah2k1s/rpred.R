
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Step 1: Load datasets
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- readr::read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8_derived <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9_derived <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Step 2: Merge datasets by NSID
merged_data <- full_join(
  full_join(wave_one, wave_four, by = 'NSID'),
  full_join(wave_six, ns8_derived, by = 'NSID'),
  by = 'NSID'
) %>%
  full_join(ns9_derived, by = 'NSID')

# Step 3: Define missing value harmonization function for numeric columns
harmonize_missing <- function(x) {
  x %>%
    mutate(across(
      where(is.numeric),
      ~ case_when(
        . %in% c(-999, -997, -97, -92, -91, -99, -94, -995, -998) ~
          case_when(
            . %in% c(-92, -9) ~ -9,  # Refusal
            . %in% c(-91, -8, -97) ~ -8,  # Don't know/insufficient info
            . %in% c(-1, -995) ~ -1,  # Not applicable
            TRUE ~ -3  # Not asked/fieldwork error
          ),
        . == -7 ~ -7,  # Prefer not to say
        . == -2 ~ -2,  # Schedule not applicable
        TRUE ~ .  # Keep other values
      )
    ))
}

# Step 4: Apply missing value harmonization
merged_data <- harmonize_missing(merged_data)

# Step 5: Create harmonized marital status variables
merged_data <- merged_data %>%
  mutate(
    # Marital status at age 19
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,  # Single
      W6MarStatYP == 2 ~ 2,  # Married
      W6MarStatYP %in% c(3, 4) ~ 3,  # Separated/Divorced
      W6MarStatYP == 5 ~ 4,  # Widowed
      W6MarStatYP %in% c(-997, -97, -92, -91, -1, -999, -998, -995, -99, -94) ~
        case_when(
          W6MarStatYP %in% c(-92, -9) ~ -9,  # Refusal
          W6MarStatYP %in% c(-91, -8, -97) ~ -8,  # Don't know/insufficient info
          W6MarStatYP %in% c(-1, -995) ~ -1,  # Not applicable
          TRUE ~ -3  # Not asked/fieldwork error
        ),
      TRUE ~ as.numeric(W6MarStatYP)
    ),

    # Marital status at age 25 (collapsed)
    partnr25 = case_when(
      W8DMARSTAT %in% c(1, 6) ~ 1,  # Single or Civil Partner
      W8DMARSTAT == 2 ~ 2,  # Married
      W8DMARSTAT %in% c(3, 7) ~ 3,  # Separated
      W8DMARSTAT %in% c(4, 8) ~ 4,  # Divorced/former Civil Partner
      W8DMARSTAT %in% c(5, 9) ~ 5,  # Widowed/surviving Civil Partner
      W8DMARSTAT %in% c(-9, -8, -1) ~
        case_when(
          W8DMARSTAT == -9 ~ -9,  # Refusal
          W8DMARSTAT == -8 ~ -8,  # Don't know/insufficient info
          W8DMARSTAT == -1 ~ -1,  # Not applicable
          TRUE ~ -3  # Not asked/fieldwork error
        ),
      TRUE ~ as.numeric(W8DMARSTAT)
    ),

    # Marital status at age 32 (collapsed)
    partnr32 = case_when(
      W9DMARSTAT %in% c(1, 6) ~ 1,  # Single or Civil Partner
      W9DMARSTAT == 2 ~ 2,  # Married
      W9DMARSTAT == 4 ~ 3,  # Legally separated
      W9DMARSTAT %in% c(3, 7) ~ 4,  # Divorced/former Civil Partner
      W9DMARSTAT %in% c(5, 8) ~ 5,  # Widowed/surviving Civil Partner
      W9DMARSTAT %in% c(-9, -8) ~
        case_when(
          W9DMARSTAT == -9 ~ -9,  # Refusal
          W9DMARSTAT == -8 ~ -8,  # Don't know/insufficient info
          TRUE ~ -3  # Not asked/fieldwork error
        ),
      TRUE ~ as.numeric(W9DMARSTAT)
    ),

    # Detailed marital status variables
    partnradu25 = as.numeric(W8DMARSTAT),
    partnradu32 = as.numeric(W9DMARSTAT)
  )

# Step 6: Select only the ID and derived variables for output
final_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Step 7: Write output to CSV
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)

# Print success message
message('Data cleaning completed successfully. Output saved to data/output/cleaned_data.csv')
