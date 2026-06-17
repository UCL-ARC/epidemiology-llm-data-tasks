library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv")
print(head(output))
print(str(output))
cat("\nTotal rows:", nrow(output), "\n")
cat("Total columns:", ncol(output), "\n")
cat("Column names:", names(output), "\n")