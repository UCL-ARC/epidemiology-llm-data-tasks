library(readr)

# Read the output file to verify
df <- read_csv("data/output/cleaned_data.csv")

cat("Column names:\n")
print(names(df))
cat("\nTotal rows:", nrow(df), "\n")
cat("\nFirst 10 rows:\n")
print(head(df, 10))

cat("\nSummary statistics for collapsed variables:\n")
for (col in c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")) {
  cat("\n", col, ":\n")
  print(table(df[[col]], useNA = "ifany"))
}

cat("\nSummary statistics for detailed variables:\n")
for (col in c("ecoactadu25", "ecoactadu32")) {
  cat("\n", col, ":\n")
  print(table(df[[col]], useNA = "ifany"))
}