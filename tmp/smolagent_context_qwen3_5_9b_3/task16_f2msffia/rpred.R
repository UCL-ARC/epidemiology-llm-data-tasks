library(readr)
output <- read_csv("data/output/cleaned_data.csv")
cat("Columns:", names(output), "\n")
cat("Row count:", nrow(output), "\n")
cat("Column types:\n")
for (col in names(output)) {
  cat(sprintf("  %s: %s\n", col, class(output[[col]])))
}