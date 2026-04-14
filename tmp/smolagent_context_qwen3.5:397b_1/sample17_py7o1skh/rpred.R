# Verify the output file
library(readr)
output <- read_csv("data/output/cleaned_data.csv")
print(head(output))
print(str(output))
print(paste("Number of rows:", nrow(output)))
print(paste("Number of columns:", ncol(output)))
print(names(output))