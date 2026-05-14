library(readr)

# Read the output file to verify it was created correctly
cleaned_data <- read_csv("data/output/cleaned_data.csv")

print("File contents:")
print(str(cleaned_data))
print(paste("Number of rows:", nrow(cleaned_data)))
print(paste("Number of columns:", ncol(cleaned_data)))
print("Column names:")
print(names(cleaned_data))
print("Summary of variables:")
print(summary(cleaned_data))