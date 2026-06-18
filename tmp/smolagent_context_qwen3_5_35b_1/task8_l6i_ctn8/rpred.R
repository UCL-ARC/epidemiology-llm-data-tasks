library(readr)

# Read the output file
df <- read_csv("data/output/cleaned_data.csv")

# Check structure
str(df)

# Check for any issues
summary(df)

# Check the first few rows
head(df)

# Count unique values in each column
table(df$educ25, useNA = "ifany")
table(df$educ32, useNA = "ifany")
table(df$educadtl32, useNA = "ifany")
table(df$educvdtl32, useNA = "ifany")