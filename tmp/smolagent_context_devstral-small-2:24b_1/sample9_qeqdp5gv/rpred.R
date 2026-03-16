library(dplyr)
library(readr)

# Check if the output file exists and read it
if (file.exists("data/output/cleaned_data.csv")) {
  output_data <- read.csv("data/output/cleaned_data.csv")
  
  # Check the structure
  str(output_data)
  
  # Check the first few rows
  head(output_data)
  
  # Check the number of variables
  ncol(output_data)
  
  # Check the variable names
  names(output_data)
  
  # Check for any NA values
  sum(is.na(output_data))
  
  # Check the distribution of the collapsed variables
  table(output_data$educma)
  table(output_data$educpa)
  
  # Check the distribution of the detailed variables
  table(output_data$educdtlma)
  table(output_data$educdtlpa)
  
  # Verify the distributions are different
  cat("\nDetailed vs Collapsed distributions are different:", 
      !identical(table(output_data$educdtlma), table(output_data$educma)))
} else {
  cat("Output file not found!")
}