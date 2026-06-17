library(readr)

# Check the output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Output dimensions:', nrow(output), 'rows,', ncol(output), 'columns\n')
cat('Column names:', paste(names(output), collapse = ', '), '\n')
cat('Summary of eth variable:\n')
print(table(output$eth, useNA = 'ifany'))

cat('\nFirst 10 rows of output:\n')
print(head(output, 10))
