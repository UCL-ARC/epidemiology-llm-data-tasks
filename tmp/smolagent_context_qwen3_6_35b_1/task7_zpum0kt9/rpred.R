library(readr)
library(dplyr)

# Read and check the output file
data <- read_csv("data/output/cleaned_data.csv")
print(head(data))
print(str(data))
print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))
print(names(data))