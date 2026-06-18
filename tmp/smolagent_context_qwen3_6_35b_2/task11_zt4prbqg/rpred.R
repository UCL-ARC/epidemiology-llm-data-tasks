library(readr)

# Read the output file to verify
cleaned <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Dimensions:", dim(cleaned), "\n")
cat("Columns:", names(cleaned), "\n")

# Check for any issues
print(head(cleaned, 5))

# Check factor levels for one variable
cat("\necoactma14 levels:\n")
print(levels(cleaned$ecoactma14))

# Check for NAs
cat("\nNumber of NAs per column:\n")
print(sapply(cleaned, function(x) sum(is.na(x))))