library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv")
print(head(output))
print(str(output))
print(summary(output))