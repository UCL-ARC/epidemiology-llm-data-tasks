library(readr)

# Read the output file to verify
df <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Number of rows:', nrow(df), '\n')
cat('Number of columns:', ncol(df), '\n')
cat('Column names:', paste(names(df), collapse = ', '), '\n')
cat('First 5 rows:\n')
print(head(df, 5))
