library(readr)
library(dplyr)

# Read and inspect the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types=FALSE)

cat("Output file dimensions:", dim(output), "\n")
cat("Columns:", names(output), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))

cat("\nFactor levels for inc25:\n")
print(levels(output$inc25))

cat("\nFactor levels for inc32:\n")
print(levels(output$inc32))

cat("\nValue counts for inc25 (valid categories only):\n")
print(table(output$inc25, useNA="ifany"))

cat("\nValue counts for inc32 (valid categories only):\n")
print(table(output$inc32, useNA="ifany"))
