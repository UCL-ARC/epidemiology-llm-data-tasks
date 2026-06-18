library(readr)

# Verify the output file exists and show a summary
df <- read_csv("data/output/cleaned_data.csv")
print(paste("Number of rows:", nrow(df)))
print(paste("Number of columns:", ncol(df)))
print(names(df))
print(head(df, 10))
