library(readr)

# Read the output file
output <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

cat('Output file structure:\n')
print(str(output))

cat('\nSummary statistics for educaim variables:\n')
print(summary(output))

cat('\nUnique values per variable:\n')
for(col in names(output)){
  cat(paste(col, ':', paste(unique(output[[col]]), collapse=', '), '\n'))
}

cat('\nRow count:', nrow(output), '\n')
