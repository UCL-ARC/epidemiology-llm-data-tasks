library(readr)

# Read and verify output
output <- read_csv('data/output/cleaned_data.csv')
cat('Output file read successfully\n')
cat('Dimensions:', dim(output), '\n')
cat('Columns:', names(output), '\n')
cat('First 10 rows:\n')
print(head(output, 10))

cat('\nUnique alcfst values:', sort(unique(output$alcfst)), '\n')
cat('Distribution of alcfst:\n')
print(table(output$alcfst, useNA = 'ifany'))

cat('\nMissing values in NSID:', sum(is.na(output$NSID)), '\n')
cat('Missing values in alcfst:', sum(is.na(output$alcfst)), '\n')