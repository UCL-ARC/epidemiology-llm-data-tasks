library(readr)

# Check the output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

# Display structure
cat('Output file structure:\n')
str(output)

cat('\nFirst 10 rows:\n')
head(output, 10)

cat('\nColumn names:\n')
colnames(output)

cat('\nNumber of rows:', nrow(output), '\n')
cat('Number of columns:', ncol(output), '\n')