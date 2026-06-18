
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths and load datasets with explicit column types
file_paths <- list(
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  ns8_derived = "data/input/ns8_2015_derived.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets with explicit column types for critical variables
wave_six_data <- read_delim(
  file_paths$wave_six,
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    W6MarStatYP = col_double()
  )
)

ns8_data <- read_delim(
  file_paths$ns8_derived,
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    W8DMARSTAT = col_double()
  )
)

ns9_data <- read_delim(
  file_paths$ns9_derived,
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    W9DMARSTAT = col_double()
  )
)

# Merge datasets on NSID
merged_data <- full_join(wave_six_data, ns8_data, by = "NSID") %>%
  full_join(ns9_data, by = "NSID")

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(
      x %in% c(-999, -997, -97, -92, -91, -9, -8, -1, -99),
      case_when(
        x %in% c(-999, -997, -97, -9, -8) ~ -2,
        x == -92 ~ -9,
        x == -91 ~ -1,
        x == -1 ~ -3,
        TRUE ~ x
      ),
      x
    )
  }
  return(x)
}

# Create partnr19 from W6MarStatYP
merged_data <- merged_data %>%
  mutate(
    W6MarStatYP = map_missing_values(W6MarStatYP),
    partnr19 = case_when(
      W6MarStatYP == 1 ~ "Single, never married",
      W6MarStatYP == 2 ~ "Married",
      W6MarStatYP == 3 ~ "Separated",
      W6MarStatYP == 4 ~ "Divorced",
      W6MarStatYP == 5 ~ "Widowed",
      W6MarStatYP %in% c(-3, -2, -1, -9) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Create partnradu25 from W8DMARSTAT
merged_data <- merged_data %>%
  mutate(
    W8DMARSTAT = map_missing_values(W8DMARSTAT),
    partnradu25 = case_when(
      W8DMARSTAT == 1 ~ "Single, never married or in a CP",
      W8DMARSTAT == 2 ~ "Married",
      W8DMARSTAT == 3 ~ "Separated but still legally married",
      W8DMARSTAT == 4 ~ "Divorced",
      W8DMARSTAT == 5 ~ "Widowed",
      W8DMARSTAT == 6 ~ "A Civil Partner",
      W8DMARSTAT == 7 ~ "Separated but still legally in a CP",
      W8DMARSTAT == 8 ~ "A former Civil Partner",
      W8DMARSTAT == 9 ~ "A surviving Civil Partner",
      W8DMARSTAT %in% c(-3, -2, -1, -9) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Create partnradu32 from W9DMARSTAT
merged_data <- merged_data %>%
  mutate(
    W9DMARSTAT = map_missing_values(W9DMARSTAT),
    partnradu32 = case_when(
      W9DMARSTAT == 1 ~ "Single, never married or never in a CP",
      W9DMARSTAT == 2 ~ "Married",
      W9DMARSTAT == 3 ~ "Divorced",
      W9DMARSTAT == 4 ~ "Legally separated",
      W9DMARSTAT == 5 ~ "Widowed",
      W9DMARSTAT == 6 ~ "A Civil Partner",
      W9DMARSTAT == 7 ~ "A former Civil Partner",
      W9DMARSTAT == 8 ~ "A surviving Civil Partner",
      W9DMARSTAT %in% c(-3, -2, -1, -9) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Define mapping for detailed to collapsed categories for partnr25 and partnr32
partnr_collapsed_map <- c(
  "Single, never married or in a CP" = "Single, never married",
  "Married" = "Married",
  "Separated but still legally married" = "Separated",
  "Separated but still legally in a CP" = "Separated",
  "Divorced" = "Divorced",
  "Widowed" = "Widowed",
  "A Civil Partner" = "Civil Partner",
  "A former Civil Partner" = "Former Civil Partner",
  "A surviving Civil Partner" = "Surviving Civil Partner"
)

# Collapse detailed variables into partnr25 and partnr32
merged_data <- merged_data %>%
  mutate(
    partnr25 = partnr_collapsed_map[partnradu25],
    partnr32 = partnr_collapsed_map[partnradu32]
  )

# Replace NA values with -3 for missing values
merged_data <- merged_data %>%
  mutate(
    partnr19 = ifelse(is.na(partnr19), -3, partnr19),
    partnr25 = ifelse(is.na(partnr25), -3, partnr25),
    partnr32 = ifelse(is.na(partnr32), -3, partnr32),
    partnradu25 = ifelse(is.na(partnradu25), -3, partnradu25),
    partnradu32 = ifelse(is.na(partnradu32), -3, partnradu32)
  )

# Convert variables to factors with appropriate labels
partnr19_labels <- c(
  "Single, never married" = 1,
  "Married" = 2,
  "Separated" = 3,
  "Divorced" = 4,
  "Widowed" = 5,
  "-3" = -3
)

partnr25_labels <- c(
  "Single, never married" = 1,
  "Married" = 2,
  "Separated" = 3,
  "Divorced" = 4,
  "Widowed" = 5,
  "Civil Partner" = 6,
  "Former Civil Partner" = 7,
  "Surviving Civil Partner" = 8,
  "-3" = -3
)

partnradu25_labels <- c(
  "Single, never married or in a CP" = 1,
  "Married" = 2,
  "Separated but still legally married" = 3,
  "Divorced" = 4,
  "Widowed" = 5,
  "A Civil Partner" = 6,
  "Separated but still legally in a CP" = 7,
  "A former Civil Partner" = 8,
  "A surviving Civil Partner" = 9,
  "-3" = -3
)

partnradu32_labels <- c(
  "Single, never married or never in a CP" = 1,
  "Married" = 2,
  "Divorced" = 3,
  "Legally separated" = 4,
  "Widowed" = 5,
  "A Civil Partner" = 6,
  "A former Civil Partner" = 7,
  "A surviving Civil Partner" = 8,
  "-3" = -3
)

partnr32_labels <- c(
  "Single, never married" = 1,
  "Married" = 2,
  "Divorced" = 3,
  "Separated" = 4,
  "Widowed" = 5,
  "Civil Partner" = 6,
  "Former Civil Partner" = 7,
  "Surviving Civil Partner" = 8,
  "-3" = -3
)

# Apply factors to variables
merged_data <- merged_data %>%
  mutate(
    partnr19 = factor(partnr19, levels = names(partnr19_labels), labels = names(partnr19_labels)),
    partnr25 = factor(partnr25, levels = names(partnr25_labels), labels = names(partnr25_labels)),
    partnr32 = factor(partnr32, levels = names(partnr32_labels), labels = names(partnr32_labels)),
    partnradu25 = factor(partnradu25, levels = names(partnradu25_labels), labels = names(partnradu25_labels)),
    partnradu32 = factor(partnradu32, levels = names(partnradu32_labels), labels = names(partnradu32_labels))
  )

# Select only the ID and derived variables for output
output_data <- merged_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Print the structure of the output data to verify
print(str(output_data))

# Write output to CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Print confirmation message
message("Cleaned dataset has been written to data/output/cleaned_data.csv")
