library(readr)

# Check the output file
data <- read_csv("data/output/cleaned_data.csv")
cat("Output file structure:\n")
str(data)
cat("\nFirst 10 rows:\n")
print(head(data, 10))
cat("\nSummary of variables:\n")
summary(data)
cat("\nUnique values in each educaim variable:\n")
for (col in names(data)[-1]) {
  cat(col, ":", paste(sort(unique(data[[col]])), collapse=", "), "\n")
}