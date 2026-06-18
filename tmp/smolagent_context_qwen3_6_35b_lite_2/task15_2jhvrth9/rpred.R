library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Define file paths
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load all files
raw_data <- purrr::map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', show_col_types = FALSE))
names(raw_data) <- files

# Merge all datasets by NSID using full_join
cleaned <- raw_data[[1]]
for (i in 2:length(raw_data)) {
  cleaned <- full_join(cleaned, raw_data[[i]], by = 'NSID')
}

# Create inc25 from W8DINCB (Wave 8, Age 25)
# W8DINCB has values 1-16 for income bands, -1 for Not applicable
# Convert -1 to -1 (Not applicable) and NA to -3 (Not asked)
cleaned <- cleaned %>%
  mutate(
    inc25 = case_when(
      is.na(W8DINCB) ~ -3,  # Not asked
      W8DINCB == -1 ~ -1,   # Not applicable
      TRUE ~ as.numeric(W8DINCB)
    )
  )

# Create inc32 from W9DINCB (Wave 9, Age 32)
# W9DINCB has values 1-16 for income bands, -1 for Not applicable
# Convert -1 to -1 (Not applicable) and NA to -3 (Not asked)
cleaned <- cleaned %>%
  mutate(
    inc32 = case_when(
      is.na(W9DINCB) ~ -3,  # Not asked
      W9DINCB == -1 ~ -1,   # Not applicable
      TRUE ~ as.numeric(W9DINCB)
    )
  )

# Create labelled factors for inc25
inc25_labels <- c(
  'less than 25' = 1,
  '25 to 50' = 2,
  '50 to 90' = 3,
  '90 to 140' = 4,
  '140 to 240' = 5,
  '240 to 300' = 6,
  '300 to 350' = 7,
  '350 to 400' = 8,
  '400 to 500' = 9,
  '500 to 600' = 10,
  '600 to 700' = 11,
  '700 to 800' = 12,
  '800 to 900' = 13,
  '900 to 1200' = 14,
  '1200 to 1400' = 15,
  'more than 1400' = 16
)

cleaned$inc25 <- factor(cleaned$inc25, levels = c(1:16, -1, -3),
                        labels = c(names(inc25_labels), 'Not applicable', 'Not asked'))

# Create labelled factors for inc32
cleaned$inc32 <- factor(cleaned$inc32, levels = c(1:16, -1, -3),
                        labels = c(names(inc25_labels), 'Not applicable', 'Not asked'))

# Select only NSID and final derived variables
final_data <- cleaned %>% select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(final_data), '\n')
cat('Columns:', paste(names(final_data), collapse = ', '), '\n')

# Print summary
glimpse(final_data)
