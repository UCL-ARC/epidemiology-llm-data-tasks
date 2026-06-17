library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
base_path <- 'data/input'
files <- list(
  wave1 = file.path(base_path, 'wave_one_lsype_family_background_2020.tab'),
  wave2 = file.path(base_path, 'wave_two_lsype_family_background_2020.tab'),
  wave3 = file.path(base_path, 'wave_three_lsype_family_background_2020.tab'),
  wave4 = file.path(base_path, 'wave_four_lsype_family_background_2020.tab')
)

# Create a function to map missing values based on label meaning
map_missing_codes <- function(x, wave, is_father = FALSE) {
  result <- as.numeric(x)
  
  # Replace all NA values with -3 (not asked)
  result[is.na(result)] <- -3
  
  # Map specific missing codes by meaning
  # -999: Missing household information - lost -> -2
  result[result == -999] <- -2
  # -99: Not interviewed -> -3 (already set above)
  # -98: Not present -> -3 (already set above)
  # -94: Insufficient information -> -8
  result[result == -94] <- -8
  # -92: Refusal -> -9 (wave 4 father only)
  if (wave == 4 && is_father) {
    result[result == -92] <- -9
  }
  # -996: No parent in household -> -1 (wave 4 father only)
  if (wave == 4 && is_father) {
    result[result == -996] <- -1
  }
  
  return(result)
}

# Load all files
w1 <- read_delim(files$wave1, delim = '\t')
w2 <- read_delim(files$wave2, delim = '\t')
w3 <- read_delim(files$wave3, delim = '\t')
w4 <- read_delim(files$wave4, delim = '\t')

# Extract and map the employment status variables for each wave
# Wave 1 (Age 14)
w1_empsmum <- w1$W1empsmum
w1_empsdad <- w1$W1empsdad
echoactma14 <- map_missing_codes(w1_empsmum, 1, is_father = FALSE)
echoactpa14 <- map_missing_codes(w1_empsdad, 1, is_father = TRUE)

# Wave 2 (Age 15)
w2_empsmum <- w2$W2empsmum
w2_empsdad <- w2$W2empsdad
echoactma15 <- map_missing_codes(w2_empsmum, 2, is_father = FALSE)
echoactpa15 <- map_missing_codes(w2_empsdad, 2, is_father = TRUE)

# Wave 3 (Age 16)
w3_empsmum <- w3$W3empsmum
w3_empsdad <- w3$W3empsdad
echoactma16 <- map_missing_codes(w3_empsmum, 3, is_father = FALSE)
echoactpa16 <- map_missing_codes(w3_empsdad, 3, is_father = TRUE)

# Wave 4 (Age 17)
w4_empsmum <- w4$w4empsmum
w4_empsdad <- w4$w4empsdad
echoactma17 <- map_missing_codes(w4_empsmum, 4, is_father = FALSE)
echoactpa17 <- map_missing_codes(w4_empsdad, 4, is_father = TRUE)

# Create the output dataframe with all data from wave 1 as base
output <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Map the harmonised employment status variables to each row
# Use match to find the correct values based on NSID
output$ecoactma14 <- echoactma14[match(output$NSID, names(echoactma14))]
output$ecoactpa14 <- echoactpa14[match(output$NSID, names(echoactpa14))]
output$ecoactma15 <- echoactma15[match(output$NSID, names(echoactma15))]
output$ecoactpa15 <- echoactpa15[match(output$NSID, names(echoactpa15))]
output$ecoactma16 <- echoactma16[match(output$NSID, names(echoactma16))]
output$ecoactpa16 <- echoactpa16[match(output$NSID, names(echoactpa16))]
output$ecoactma17 <- echoactma17[match(output$NSID, names(echoactma17))]
output$ecoactpa17 <- echoactpa17[match(output$NSID, names(echoactpa17))]

# Select only the ID and the derived output variables
output <- output %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Script completed successfully. Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(output), '\n')
cat('Number of columns:', ncol(output), '\n')
