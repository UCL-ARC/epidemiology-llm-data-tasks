library(readr)

# Read the output file to verify
cleaned <- read_csv("data/output/cleaned_data.csv")
cat("Output file verified:\n")
cat("Dimensions:", dim(cleaned), "\n")
cat("Columns:", names(cleaned), "\n")
cat("\nFirst 15 rows:\n")
print(head(cleaned, 15))

cat("\nSummary of imd15:")
print(summary(cleaned$imd15))
cat("\nSummary of imd16:")
print(summary(cleaned$imd16))
cat("\nSummary of imd32:")
print(summary(cleaned$imd32))

cat("\nNumber of non-missing values per variable:\n")
cat("imd15:", sum(!is.na(cleaned$imd15)), "\n")
cat("imd16:", sum(!is.na(cleaned$imd16)), "\n")
cat("imd32:", sum(!is.na(cleaned$imd32)), "\n")