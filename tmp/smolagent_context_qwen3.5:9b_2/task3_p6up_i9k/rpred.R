library(haven)
library(dplyr)
library(purrr)
library(readr)

# Check if output file exists
if (file.exists('data/output/cleaned_data.csv')) {
  cat('Output file exists\n')
  
  # Read and display summary of output
  output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
  
  cat('Output summary:\n')
  cat('Dimensions:', dim(output), '\n')
  cat('Column names:', paste(names(output), collapse = ', '), '\n')
  
  # Show some values
  cat('\nFirst 10 rows:\n')
  print(head(output, 10))
  
  # Check unique values in lang
  cat('\nUnique values in lang:\n')
  print(table(output$lang))
  
  # Check for missing
  cat('\nNA values in lang:', sum(is.na(output$lang)), '\n')
  
  cat('\nScript completed successfully\n')
}
