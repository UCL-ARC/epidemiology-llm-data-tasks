library(readr)
library(dplyr)

# Read the output file
df <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check for missing value distributions
cat("Missing value distributions:\n")
for (col in names(df)) {
  if (col != "NSID") {
    vals <- table(df[[col]], useNA = "ifany")
    cat(col, ":\n")
    print(vals)
    cat("\n")
  }
}

# Verify no NAs remain
cat("\nAny NAs?\n")
print(any(is.na(df)))

# Check sample of data
cat("\nSample data:\n")
print(head(df, 5))
