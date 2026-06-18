# Check if file exists and print first few rows
library(readr)
if(file.exists("data/output/cleaned_data.csv")){
  df <- read_csv("data/output/cleaned_data.csv")
  print(head(df))
} else {
  cat("File not found\n")
}