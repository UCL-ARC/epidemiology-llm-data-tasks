# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Define missing value mapping
missing_value_mapping <- setNames(
  c(-2, -3, -1, -8, -9, -1, -8),
  c('-999.0', '-99.0', '-98.0', '-94.0', '-92.0', '-91.0', '-1.0')
)

# Define NVQ categories mapping
nvq_mapping <- setNames(
  c(1, 1, 2, 3, 3, 3, 4, 4, 5, 5, 5, 4, 4, 4, 5, 5, 4, 4, 5, 5),
  c('1.0', '2.0', '3.0', '4.0', '5.0', '6.0', '7.0', '8.0', '9.0', '10.0', '11.0', 
   '12.0', '13.0', '14.0', '15.0', '16.0', '17.0', '18.0', '19.0', '20.0')
)

# Vectorized function to map missing values
map_missing <- function(x) {
  if (is.numeric(x)) {
    x <- as.character(x)
    x[x %in% names(missing_value_mapping)] <- missing_value_mapping[x[x %in% names(missing_value_mapping)]]
    as.numeric(x)
  } else {
    x
  }
}

# Vectorized function to map NVQ categories
map_nvq <- function(x) {
  if (is.numeric(x)) {
    x <- as.character(x)
    x[x %in% names(nvq_mapping)] <- nvq_mapping[x[x %in% names(nvq_mapping)]]
    as.numeric(x)
  } else {
    x
  }
}

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', 
                    col_types = cols(NSID = col_character()))
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', 
                    col_types = cols(NSID = col_character()))
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', 
                    col_types = cols(NSID = col_character()))

# Merge datasets by NSID
merged_data <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Create detailed education variables for mother and father
merged_data <- merged_data %>%
  mutate(
    educdtlma_w1 = map_missing(coalesce(W1hiqualmum, -3)),
    educdtlma_w2 = map_missing(coalesce(W2hiqualmum, -3)),
    educdtlma_w4 = map_missing(coalesce(w4hiqualmum, -3)),
    educdtlpa_w1 = map_missing(coalesce(W1hiqualdad, -3)),
    educdtlpa_w2 = map_missing(coalesce(W2hiqualdad, -3)),
    educdtlpa_w4 = map_missing(coalesce(w4hiqualdad, -3))
  )

# Consolidate detailed education variables (earliest-valid-first)
merged_data <- merged_data %>%
  mutate(
    educdtlma = coalesce(
      educdtlma_w1,
      educdtlma_w2,
      educdtlma_w4,
      -3
    ),
    educdtlpa = coalesce(
      educdtlpa_w1,
      educdtlpa_w2,
      educdtlpa_w4,
      -3
    )
  )

# Create NVQ education variables for mother and father
merged_data <- merged_data %>%
  mutate(
    educma = map_nvq(educdtlma),
    educpa = map_nvq(educdtlpa)
  )

# Select only the ID and final derived variables
final_data <- merged_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output')
}

# Write the final cleaned data to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation message
cat('Data cleaning completed successfully!\n')
cat('Output file created at: data/output/cleaned_data.csv\n')
cat('Number of rows in output:', nrow(final_data), '\n')
cat('Variables included:', paste(names(final_data), collapse = ', '), '\n')