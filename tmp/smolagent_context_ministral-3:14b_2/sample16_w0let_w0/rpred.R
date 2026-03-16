
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Step 1: Load datasets with explicit column types
wave_one <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave_two <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Step 2: Define missing value harmonization
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -992] <- -9
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -91] <- -1
  x[x == -3] <- -3
  x[x == -1] <- -8
  x
}

# Step 3: Process each dataset individually
wave_one_processed <- wave_one %>%
  mutate(incwhhcnt14 = harmonize_missing(W1GrsswkHH))

wave_two_processed <- wave_two %>%
  mutate(incwhhcnt15 = harmonize_missing(W2GrsswkHH))

wave_three_processed <- wave_three %>%
  mutate(incwhh16 = harmonize_missing(W3incestw))

wave_four_processed <- wave_four %>%
  mutate(incwhh17 = harmonize_missing(w4IncEstW))

# Step 4: Merge datasets by NSID
merged_data <- full_join(wave_one_processed, wave_two_processed, by = 'NSID')
merged_data <- full_join(merged_data, wave_three_processed, by = 'NSID')
merged_data <- full_join(merged_data, wave_four_processed, by = 'NSID')

# Step 5: Create factor variables with matching levels and labels
# For ages 14-15 (12 income bands + 5 missing codes = 17 levels)
levels_14_15 <- c(-9, -8, -3, -2, -1, 1:12)
labels_14_15 <- c('Refusal', 'DK/Insuff Info', 'Not Asked',
                  'Script Error', 'Not Applicable',
                  'Up to £49', '£50-£99', '£100-£149',
                  '£150-£199', '£200-£299', '£300-£399',
                  '£400-£499', '£500-£599', '£600-£699',
                  '£700-£799', '£800-£899', '£900-£999')

merged_data$incwhh14 <- factor(merged_data$incwhhcnt14,
                             levels = levels_14_15,
                             labels = labels_14_15)

merged_data$incwhh15 <- factor(merged_data$incwhhcnt15,
                             levels = levels_14_15,
                             labels = labels_14_15)

# For ages 16-17 (12 income bands + 5 missing codes = 17 levels)
levels_16_17 <- c(-9, -8, -3, -2, -1, 1:12)
labels_16_17 <- c('Refusal', 'DK/Insuff Info', 'Not Asked',
                  'Script Error', 'Not Applicable',
                  'Up to £49', '£50-£99', '£100-£199',
                  '£200-£299', '£300-£399', '£400-£499',
                  '£500-£599', '£600-£699', '£700-£799',
                  '£800-£899', '£900-£990', '£1000+')

merged_data$incwhh16 <- factor(merged_data$incwhh16,
                             levels = levels_16_17,
                             labels = labels_16_17)

merged_data$incwhh17 <- factor(merged_data$incwhh17,
                             levels = levels_16_17,
                             labels = labels_16_17)

# Step 6: Select required variables
cleaned_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Step 7: Write output
write.csv(cleaned_data, 'data/output/cleaned_data.csv', row.names = FALSE)
