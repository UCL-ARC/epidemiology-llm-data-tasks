# Load the output file to verify its contents
output_data <- read_csv("data/output/cleaned_data.csv")

# Display the first few rows and structure
cat("First few rows of the output file:\n")
print(head(output_data))

cat("\nStructure of the output file:\n")
print(str(output_data))

cat("\nSummary of the derived variables:\n")
print(summary(output_data))