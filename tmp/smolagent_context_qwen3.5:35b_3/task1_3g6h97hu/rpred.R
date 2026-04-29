library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv")
print(head(output))
print(summary(output))
cat("\nNumber of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("Column names:", names(output), "\n")