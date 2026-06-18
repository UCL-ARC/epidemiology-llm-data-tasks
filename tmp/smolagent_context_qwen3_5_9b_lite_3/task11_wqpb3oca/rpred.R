library(readr)

# Read back the output to verify
output <- read_csv('data/output/cleaned_data.csv')

cat('Output file verified:\n')
cat('Number of rows:', nrow(output), '\n')
cat('Number of columns:', ncol(output), '\n')
cat('Column names:\n')
print(names(output))
cat('\nFirst few rows:\n')
print(head(output))
cat('\nData types:\n')
str(output)
