# Verify the output file exists and check its contents
cat("Checking output file...\n")
if (file.exists("data/output/cleaned_data.csv")) {
  cat("Output file exists!\n")
  data <- read.csv("data/output/cleaned_data.csv")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", paste(names(data), collapse = ", "), "\n")
  cat("\nFirst 10 rows:\n")
  print(head(data, 10))
  cat("\nSummary of bmi25:\n")
  print(summary(data$bmi25))
  cat("\nSummary of bmi32:\n")
  print(summary(data$bmi32))
} else {
  cat("Output file not found!\n")
}