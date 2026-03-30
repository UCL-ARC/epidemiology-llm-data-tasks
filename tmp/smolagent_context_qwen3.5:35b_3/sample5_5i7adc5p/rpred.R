library(readr)

# Read and display the cleaned data
cleaned_data <- read_csv("data/output/cleaned_data.csv")

print("Cleaned data summary:")
print(str(cleaned_data))

print("\nFirst 10 rows:")
print(head(cleaned_data, 10))

print("\nMissing value distribution:")
print(summary(cleaned_data))

print("\nFile saved successfully!")