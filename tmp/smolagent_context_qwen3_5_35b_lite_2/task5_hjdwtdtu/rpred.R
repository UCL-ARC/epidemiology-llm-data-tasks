library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv")
cat("Output file structure:\n")
print(str(output))
cat("\nFirst 10 rows:\n")
print(head(output, 10))

# Check for any issues with the variables
cat("\nVariable summary:\n")
for (col in names(output)) {
  cat(paste0(col, ": ", length(unique(output[[col]])), " unique values\n"))
}