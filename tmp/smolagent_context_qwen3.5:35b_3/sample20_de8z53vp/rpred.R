library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Output file dimensions:", nrow(output), "x", ncol(output), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))

cat("\nSummary of alcfst:\n")
print(summary(output$alcfst))

cat("\nUnique values in alcfst:\n")
print(unique(output$alcfst))