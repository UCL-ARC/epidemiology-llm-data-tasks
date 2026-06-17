library(readr)

# Verify the final output file
output <- read_csv("data/output/cleaned_data.csv")
print(head(output, 10))
cat("\nTotal rows:", nrow(output), "\n")
cat("Total columns:", ncol(output), "\n")
cat("Column names:", paste(names(output), collapse=", "), "\n")
cat("\nData types:\n")
str(output)