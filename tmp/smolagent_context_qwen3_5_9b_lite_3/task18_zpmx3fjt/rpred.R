library(readr)

# Check if output file exists and show summary
if (file.exists('data/output/cleaned_data.csv')) {
  df <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
  cat('File exists with', nrow(df), 'rows and', ncol(df), 'columns\n')
  cat('Column names:', paste(names(df), collapse = ', '), '\n')
  cat('\nFirst few rows:\n')
  print(head(df))
} else {
  cat('Output file does not exist\n')
}