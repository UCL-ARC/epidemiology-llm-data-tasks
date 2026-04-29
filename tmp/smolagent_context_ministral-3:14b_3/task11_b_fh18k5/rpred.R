# Load required packages
library(haven)
library(dplyr)
library(readr)

# Function to recode missing values
recode_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -3
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -1] <- -1
  return(x)
}

# Load datasets
wave_one <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave_two <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_four <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Rename variables
wave_one <- wave_one %>% rename(ecoactmum14 = W1empsmum, ecoactdad14 = W1empsdad)
wave_two <- wave_two %>% rename(ecoactmum15 = W2empsmum, ecoactdad15 = W2empsdad)
wave_three <- wave_three %>% rename(ecoactmum16 = W3empsmum, ecoactdad16 = W3empsdad)
wave_four <- wave_four %>% rename(ecoactmum17 = w4empsmum, ecoactdad17 = w4empsdad)

# Apply missing value recoding
wave_one <- wave_one %>% mutate(ecoactmum14 = recode_missing(ecoactmum14), ecoactdad14 = recode_missing(ecoactdad14))
wave_two <- wave_two %>% mutate(ecoactmum15 = recode_missing(ecoactmum15), ecoactdad15 = recode_missing(ecoactdad15))
wave_three <- wave_three %>% mutate(ecoactmum16 = recode_missing(ecoactmum16), ecoactdad16 = recode_missing(ecoactdad16))
wave_four <- wave_four %>% mutate(ecoactmum17 = recode_missing(ecoactmum17), ecoactdad17 = recode_missing(ecoactdad17))

# Merge datasets
merged_data <- full_join(wave_one, wave_two, by = 'NSID') %>% 
  full_join(wave_three, by = 'NSID') %>% 
  full_join(wave_four, by = 'NSID')

# Select required variables
output_data <- merged_data %>% select(NSID, ecoactmum14, ecoactmum15, ecoactmum16, ecoactmum17, ecoactdad14, ecoactdad15, ecoactdad16, ecoactdad17)

# Write output
write_csv(output_data, 'data/output/cleaned_data.csv')
cat('Data cleaning complete. Output saved to data/output/cleaned_data.csv\n')