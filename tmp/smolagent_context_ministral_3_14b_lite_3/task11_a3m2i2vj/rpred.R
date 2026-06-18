# Load required libraries
library(readr)
library(dplyr)

# Function to map missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -2  # Missing household information
  x[x == -99]  <- -3  # Not interviewed
  x[x == -98]  <- -1  # Not present
  x[x == -94]  <- -8  # Insufficient information
  x[x == -92]  <- -9  # Refusal
  x[x == -996] <- -1  # No parent in household
  return(x)
}

# Load and process wave 1
wave1 <- tryCatch({
  read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t') %>%
    select(NSID, W1empsmum, W1empsdad) %>%
    mutate(
      ecoactma14 = map_missing(W1empsmum),
      ecoactpa14 = map_missing(W1empsdad)
    )
}, error = function(e) {
  message("Error processing wave 1: ", e$message)
  return(NULL)
})

# Load and process wave 2
wave2 <- tryCatch({
  read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t') %>%
    select(NSID, W2empsmum, W2empsdad) %>%
    mutate(
      ecoactma15 = map_missing(W2empsmum),
      ecoactpa15 = map_missing(W2empsdad)
    )
}, error = function(e) {
  message("Error processing wave 2: ", e$message)
  return(NULL)
})

# Load and process wave 3
wave3 <- tryCatch({
  read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t') %>%
    select(NSID, W3empsmum, W3empsdad) %>%
    mutate(
      ecoactma16 = map_missing(W3empsmum),
      ecoactpa16 = map_missing(W3empsdad)
    )
}, error = function(e) {
  message("Error processing wave 3: ", e$message)
  return(NULL)
})

# Load and process wave 4
wave4 <- tryCatch({
  read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t') %>%
    select(NSID, w4empsmum, w4empsdad) %>%
    mutate(
      ecoactma17 = map_missing(w4empsmum),
      ecoactpa17 = map_missing(w4empsdad)
    )
}, error = function(e) {
  message("Error processing wave 4: ", e$message)
  return(NULL)
})

# Combine datasets if all waves were processed successfully
if (!any(sapply(list(wave1, wave2, wave3, wave4), is.null))) {
  combined_data <- full_join(wave1, wave2, by = 'NSID')
  combined_data <- full_join(combined_data, wave3, by = 'NSID')
  combined_data <- full_join(combined_data, wave4, by = 'NSID')
  
  # Select final variables
  final_vars <- c('NSID', 'ecoactma14', 'ecoactpa14', 'ecoactma15', 'ecoactpa15', 
                 'ecoactma16', 'ecoactpa16', 'ecoactma17', 'ecoactpa17')
  
  # Check which variables exist
  existing_vars <- intersect(final_vars, names(combined_data))
  
  # Select only existing variables
  final_data <- combined_data %>% select(all_of(existing_vars))
  
  # Write output with verification
  output_path <- 'data/output/cleaned_data.csv'
  write_csv(final_data, output_path)
  
  # Verify output was created
  if (file.exists(output_path)) {
    message("Successfully created output file: ", output_path)
    message("Number of rows in output: ", nrow(final_data))
  } else {
    message("Failed to create output file")
  }
} else {
  message("Could not process all waves - check error messages above")
}