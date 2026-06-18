library(readr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv")

cat("Output file dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("\nColumn names:\n")
print(names(output))

cat("\nFirst 10 rows:\n")
print(head(output, 10))

cat("\nSummary statistics:\n")
summary(output)

# Check for any issues
cat("\nMissing value counts per column:\n")
for (col in names(output)) {
  cat(col, ":", sum(is.na(output[[col]])), "\n")
}

# Verify the file exists
cat("\nFile exists:", file.exists("data/output/cleaned_data.csv"), "\n")

# Show first few lines of the raw file
cat("\nFirst 5 lines of raw CSV:\n")
system("head -5 data/output/cleaned_data.csv")
