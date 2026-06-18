# Load the output data to verify
output_data <- read_csv("data/output/cleaned_data.csv")

# Check the structure and summary
str(output_data)
summary(output_data)

# Check the distribution of the derived variables
table(output_data$educdtlma, useNA = "always")
table(output_data$educdtlpa, useNA = "always")
table(output_data$educma, useNA = "always")
table(output_data$educpa, useNA = "always")