# Verify the output file
library(readr)

cleaned_data <- read_csv("data/output/cleaned_data.csv")

# Check structure and first few rows
print(str(cleaned_data))
print(head(cleaned_data, 10))

# Check variable counts
print(paste("Number of rows:", nrow(cleaned_data)))
print(paste("Number of columns:", ncol(cleaned_data)))

# Check for any issues
print(summary(cleaned_data))