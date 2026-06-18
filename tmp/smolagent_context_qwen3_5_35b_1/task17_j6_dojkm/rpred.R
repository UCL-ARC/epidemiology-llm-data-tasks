library(readr)

# Read the output file to verify
cleaned_data <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Output file structure:\n')
str(cleaned_data)

cat('\nFirst 10 rows:\n')
print(head(cleaned_data, 10))

cat('\nSummary statistics:\n')
summary(cleaned_data)

cat('\nMissing value counts:\n')
cat('NSID:', sum(is.na(cleaned_data$NSID)), '\n')
cat('imd15:', sum(is.na(cleaned_data$imd15)), '\n')
cat('imd16:', sum(is.na(cleaned_data$imd16)), '\n')
cat('imd32:', sum(is.na(cleaned_data$imd32)), '\n')