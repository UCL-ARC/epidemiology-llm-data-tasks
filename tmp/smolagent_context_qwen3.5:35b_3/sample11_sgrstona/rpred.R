library(readr)

# Check the output file
cleaned_data <- read_csv("data/output/cleaned_data.csv")
print(head(cleaned_data))
print(summary(cleaned_data))