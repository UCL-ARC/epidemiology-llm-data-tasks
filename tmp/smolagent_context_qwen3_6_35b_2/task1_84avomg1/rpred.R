library(readr)

# Read the output file to verify
result <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Dimensions:", dim(result), "\n")
cat("Columns:", names(result), "\n")
cat("First 10 rows:\n")
print(head(result, 10))
cat("\nSex distribution:\n")
print(table(result$sex, useNA = "ifany"))