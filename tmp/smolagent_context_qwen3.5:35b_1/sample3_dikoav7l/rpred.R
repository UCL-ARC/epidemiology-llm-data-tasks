library(readr)

# Read and display the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Display summary
cat("Output file summary:\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("\nColumn names:", names(output), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))
cat("\nValue distribution for lang:\n")
print(table(output$lang, useNA = "ifany"))