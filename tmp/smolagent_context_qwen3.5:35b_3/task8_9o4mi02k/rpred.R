library(readr)
library(dplyr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check for any issues
print("Column names:")
print(colnames(output))

print("\nRow count:")
print(nrow(output))

print("\neduc25 distribution:")
print(table(output$educ25, useNA = "ifany"))

print("\neduc32 distribution:")
print(table(output$educ32, useNA = "ifany"))

print("\neducadtl32 unique values (first 20):")
print(head(unique(output$educadtl32), 20))

print("\neducvdtl32 unique values (first 20):")
print(head(unique(output$educvdtl32), 20))

print("\nSample of data:")
print(head(output, 5))
