library(haven)
library(dplyr)
library(readr)

# Check the output file
output_path <- 'data/output/cleaned_data.csv'

if (file.exists(output_path)) {
  cat('Output file exists. Checking contents...\n')
  
  # Read the file to verify
  cleaned <- read_csv(output_path, show_col_types = FALSE)
  
  cat('Dimensions:', nrow(cleaned), 'rows,', ncol(cleaned), 'columns\n')
  cat('Column names:\n')
  print(names(cleaned))
  
  # Check the type of sori_harmonized
  if ('sori_harmonized' %in% names(cleaned)) {
    cat('\nType of sori_harmonized:', class(cleaned$sori_harmonized), '\n')
    cat('Unique values in sori_harmonized:\n')
    print(unique(cleaned$sori_harmonized))
    cat('Count of each value:\n')
    print(table(cleaned$sori_harmonized))
  }
  
  # Save a modified version with proper labels
  cleaned_cleaned <- cleaned
  
  # Convert sori_harmonized to factor with proper labels
  unique_vals <- unique(cleaned_cleaned$sori_harmonized)
  
  # Get valid codes (1,2,3,4,5)
  valid_codes <- unique_vals[unique_vals %in% c('1', '2', '3', '4', '5')]
  
  # Create factor
  cleaned_cleaned$sori_harmonized <- factor(cleaned_cleaned$sori_harmonized, 
                                            levels = c('1', '2', '3', '4', '5'),
                                            labels = c('Heterosexual / Straight', 'Gay / Lesbian', 'Bisexual', 'Other', 'Prefer not to say'))
  
  # Write updated file
  write_csv(cleaned_cleaned, output_path)
  
  cat('\nUpdated output file with labeled factors\n')
  cat(paste('Output saved to', output_path, 'with', nrow(cleaned_cleaned), 'rows\n'))
} else {
  cat('Output file does not exist. Cannot verify.\n')
}

print('Verification complete')