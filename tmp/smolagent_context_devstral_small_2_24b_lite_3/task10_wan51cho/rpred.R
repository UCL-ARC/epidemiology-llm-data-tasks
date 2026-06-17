# Check if the output file exists and inspect its structure
if (file.exists("data/output/cleaned_data.csv")) {
  output_data <- read_csv("data/output/cleaned_data.csv")
  cat("Output file exists. Structure:")
  str(output_data)
  cat("\nFirst few rows:")
  head(output_data)
} else {
  cat("Output file does not exist.")
}
