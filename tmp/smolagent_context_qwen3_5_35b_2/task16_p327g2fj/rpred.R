library(readr)

# Verify the output file
result <- read_csv("data/output/cleaned_data.csv")
cat("File loaded successfully\n")
cat("Dimensions:", nrow(result), "rows,", ncol(result), "columns\n")
cat("Column names:", names(result), "\n")
cat("\nFirst 5 rows:\n")
head(result)