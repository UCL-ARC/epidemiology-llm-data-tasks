library(readr)
library(dplyr)

# Read the output
df <- read_csv("data/output/cleaned_data.csv")

# Check first few rows
print(head(df, 10))

# Check factor levels and distribution
for (col in c("educ25", "educ32", "educadtl32", "educvdtl32")) {
  cat("\n", col, ":", sep = "")
  print(table(df[[col]], useNA = "ifany"))
}

# Check total rows
cat("\nTotal rows:", nrow(df), "\n")

# Check that NSID is preserved
cat("\nNSID sample:", head(df$NSID, 5), "\n")
