library(readr)

# Verify the final output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check the structure
cat("File structure:\n")
print(str(output))

# Verify counts
cat("\nFinal counts:\n")
print(table(output$alcfst, useNA = "ifany"))

# Verify NSID is preserved
cat("\nNSID sample:\n")
print(head(output$NSID))

# Verify all required levels are present
cat("\nAll expected categories present:\n")
cat("Age 14:", sum(output$alcfst == "Age 14"), "\n")
cat("Age 15:", sum(output$alcfst == "Age 15"), "\n")
cat("Age 16:", sum(output$alcfst == "Age 16"), "\n")
cat("Age 17:", sum(output$alcfst == "Age 17"), "\n")
cat("Age 19:", sum(output$alcfst == "Age 19"), "\n")
cat("Age 20:", sum(output$alcfst == "Age 20"), "\n")
cat("Age 25:", sum(output$alcfst == "Age 25"), "\n")
cat("Age 32:", sum(output$alcfst == "Age 32"), "\n")
cat("Never had alcohol:", sum(output$alcfst == "Never had alcohol"), "\n")
cat("Don't know/insufficient information:", sum(output$alcfst == "Don't know/insufficient information"), "\n")

cat("\nTotal:", nrow(output), "\n")

# Verify file exists and has content
file_info <- file.info("data/output/cleaned_data.csv")
cat("\nFile size:", file_info$size, "bytes\n")
cat("File exists:", file_info$exists, "\n")