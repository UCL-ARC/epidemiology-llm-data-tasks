library(readr)
library(dplyr)

# Check output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
cat('Output file dimensions:', nrow(output), 'rows,', ncol(output), 'columns\n')
cat('Columns:', paste(names(output), collapse = ', '), '\n\n')

# Check all summary statistics
for (var in names(output)) {
  cat('Summary of', var, ':\n')
  print(table(output[[var]], useNA = 'ifany'))
  cat('\n')
}
