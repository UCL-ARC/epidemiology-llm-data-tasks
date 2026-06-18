library(readr)
library(dplyr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check dimensions and structure
cat("Dimensions:", dim(output), "\n")
cat("Columns:", names(output), "\n")

# Check factor levels for each educational variable
cat("\n--- educ25 levels ---\n")
print(table(output$educ25, useNA = "ifany"))

cat("\n--- educ32 levels ---\n")
print(table(output$educ32, useNA = "ifany"))

cat("\n--- educadtl32 levels ---\n")
print(table(output$educadtl32, useNA = "ifany"))

cat("\n--- educvdtl32 levels ---\n")
print(table(output$educvdtl32, useNA = "ifany"))

# Show first 5 rows
cat("\n--- First 5 rows ---\n")
print(head(output, 5))
