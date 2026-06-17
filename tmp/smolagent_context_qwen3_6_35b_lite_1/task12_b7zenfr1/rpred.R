library(readr)
df <- read_csv("data/output/cleaned_data.csv")
cat("Dimensions:", dim(df), "\n")
cat("Columns:", names(df), "\n")
cat("\nFirst 5 rows:\n")
print(head(df, 5))
cat("\nValue ranges for each nssec variable:\n")
for (col in names(df)[-1]) {
  cat(col, ": min=", min(df[[col]], na.rm=TRUE), "max=", max(df[[col]], na.rm=TRUE), "\n")
}