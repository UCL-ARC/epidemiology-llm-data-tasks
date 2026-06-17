library(readr);library(dplyr);

# Function to collapse NS-SEC to major categories
collapse_nssec <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- NA
  
  # Create a copy to work with
  result <- x
  
  # Handle specific cases with proper vectorized operations
  result[result >= 1.0 & result <= 11.2] <- as.numeric(gsub("\\.\\d+", "", as.character(result[result >= 1.0 & result <= 11.2])))
  result[result >= 12.1 & result <= 13.5] <- 12
  result[result == 14.1 | result == 14.2 | result == 14.3 | result == 15.0] <- 14
  result[result == 16.0 | result == 17.0] <- 16
  
  return(result)
}

# Define mapping for missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -1
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  return(x)
}

# Load and process wave 1
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave1_processed <- wave1 %>% mutate(nssecma14 = collapse_nssec(map_missing(W1nsseccatmum)), nssecpa14 = collapse_nssec(map_missing(W1nsseccatdad)))

# Load and process wave 2
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave2_processed <- wave2 %>% mutate(nssecma15 = collapse_nssec(map_missing(W2nsseccatmum)), nssecpa15 = collapse_nssec(map_missing(W2nsseccatdad)))

# Load and process wave 3
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave3_processed <- wave3 %>% mutate(nssecma16 = collapse_nssec(map_missing(W3cnsseccatmum)), nssecpa16 = collapse_nssec(map_missing(W3cnsseccatdad)))

# Load and process wave 4
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')
wave4_processed <- wave4 %>% mutate(nssecma17 = collapse_nssec(map_missing(w4cnsseccatmum)), nssecpa17 = collapse_nssec(map_missing(w4cnsseccatdad)))

# Load and process wave 5
wave5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t')
wave5_processed <- wave5 %>% mutate(nssecma18 = collapse_nssec(map_missing(w5Cnsseccatmum)), nssecpa18 = collapse_nssec(map_missing(w5Cnsseccatdad)))

# Merge all waves
merged_data <- full_join(wave1_processed, wave2_processed, by = 'NSID')
merged_data <- full_join(merged_data, wave3_processed, by = 'NSID')
merged_data <- full_join(merged_data, wave4_processed, by = 'NSID')
merged_data <- full_join(merged_data, wave5_processed, by = 'NSID')

# Select only required variables
final_data <- merged_data %>% select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write the final cleaned data to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')