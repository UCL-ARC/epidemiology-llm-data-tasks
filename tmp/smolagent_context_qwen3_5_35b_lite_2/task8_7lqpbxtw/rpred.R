# Check the output file
library(readr)

# Read and check the output file
final_data <- read_csv("data/output/cleaned_data.csv")
cat("Output file read successfully.\n")
cat("Dimensions:", nrow(final_data), "rows,", ncol(final_data), "columns\n")
cat("\nColumn names:\n")
cat(names(final_data), "\n")
cat("\nFirst few rows:\n")
print(head(final_data))
cat("\nVariable types:\n")
str(final_data)

# Check for any issues
if(any(is.na(final_data$educ25))){
  cat("\neduc25 has", sum(is.na(final_data$educ25)), "NA values\n")
}
if(any(is.na(final_data$educ32))){
  cat("educ32 has", sum(is.na(final_data$educ32)), "NA values\n")
}
if(any(is.na(final_data$educadtl32))){
  cat("educadtl32 has", sum(is.na(final_data$educadtl32)), "NA values\n")
}
if(any(is.na(final_data$educvdtl32))){
  cat("educvdtl32 has", sum(is.na(final_data$educvdtl32)), "NA values\n")
}

# Summary statistics
cat("\n\neduc25 summary:\n")
table(final_data$educ25, useNA = "ifany")

cat("\neduc32 summary:\n")
table(final_data$educ32, useNA = "ifany")

cat("\neducadtl32 summary:\n")
table(final_data$educadtl32, useNA = "ifany")

cat("\neducvdtl32 summary:\n")
table(final_data$educvdtl32, useNA = "ifany")
