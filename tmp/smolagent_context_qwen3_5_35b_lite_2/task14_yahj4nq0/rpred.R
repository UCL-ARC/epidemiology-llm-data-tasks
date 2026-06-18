# Verify the output file using base R
data <- read.csv("data/output/cleaned_data.csv")
cat("File loaded successfully.\n")
cat("Dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("\nVariable names:\n")
print(names(data))
cat("\nFirst few rows:\n")
print(head(data))