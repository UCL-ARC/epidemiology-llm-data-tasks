# Verify the output file
library(dplyr)
library(readr)

# Read the output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Output file contents:\n')
print(head(output))

cat('\nTotal observations:', nrow(output), '\n')
cat('\nSummary of alcfst variable:\n')
print(table(output$alcfst, useNA = 'ifany'))

cat('\nSample NSIDs with their alcfst values:\n')
print(head(output))
