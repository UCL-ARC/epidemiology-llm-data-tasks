library(haven)
library(dplyr)
library(readr)

# Check if output file exists
if (file.exists('data/output/cleaned_data.csv')) {
  cat('File exists. Reading first 10 rows:\n')
  df <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
  print(head(df, 10))
  cat('\nTotal rows:', nrow(df), '\n')
  cat('Total columns:', ncol(df), '\n')
  cat('Column names:', paste(names(df), collapse=', '), '\n')
} else {
  cat('File does not exist\n')
}