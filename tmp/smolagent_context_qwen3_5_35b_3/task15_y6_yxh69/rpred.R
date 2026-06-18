library(readr)

# Read the output file to verify it was created correctly
final <- read_csv("data/output/cleaned_data.csv")

# Check the structure
str(final)

# Check first few rows
head(final)

# Check variable types
cat("\nVariable types:\n")
str(final$inc25)
str(final$inc32)

# Check unique values
cat("\nUnique values in inc25 (first 20):\n")
print(sort(unique(final$inc25)))

cat("\nUnique values in inc32 (first 20):\n")
print(sort(unique(final$inc32)))

cat("\nTotal records:", nrow(final), "\n")