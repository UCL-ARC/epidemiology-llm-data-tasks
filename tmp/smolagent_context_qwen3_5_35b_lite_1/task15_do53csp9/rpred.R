library(readr)

# Read and display first few rows of the output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Number of rows:', nrow(output), '\n')
cat('Number of columns:', ncol(output), '\n')
cat('Column names:', paste(names(output), collapse = ', '), '\n')
cat('\nFirst 10 rows:\n')
print(head(output, 10))

cat('\nMissing value counts (NA):\n')
cat('NSID:', sum(is.na(output$NSID)), '\n')
cat('inc25:', sum(is.na(output$inc25)), '\n')
cat('inc32:', sum(is.na(output$inc32)), '\n')

cat('\nValue counts for inc25:\n')
print(table(output$inc25, useNA = 'ifany'))

cat('\nValue counts for inc32:\n')
print(table(output$inc32, useNA = 'ifany'))
