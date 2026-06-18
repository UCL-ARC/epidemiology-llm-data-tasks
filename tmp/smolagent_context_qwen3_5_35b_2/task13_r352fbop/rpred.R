library(readr)
library(dplyr)
library(haven)
library(labelled)

# Define the 17-category labels from metadata
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional occupations",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small organisations",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked/Long-term unemployed/Full-time students/Not currently working",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons"
)

# Function to convert missing codes
convert_missing <- function(x) {
  x[x == -98] <- -3
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -999] <- -2
  x[x == -100] <- -3
  x[x == -97] <- -3
  x
}

# Function to extract major category and apply labels
extract_major_cat <- function(x) {
  # Take integer part of fractional codes
  major <- as.integer(x)
  # Handle NA values
  major[is.na(x)] <- NA
  
  # Create a labelled vector
  result <- major
  attr(result, "label") <- "NS-SEC major category"
  attr(result, "labels") <- nssec_labels
  class(result) <- c("labelled", "integer")
  result
}

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", 
                     delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
cleaned <- full_join(wave1, wave2, by = "NSID")
cleaned <- full_join(cleaned, wave3, by = "NSID")
cleaned <- full_join(cleaned, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave5, by = "NSID")

# Process Wave 1 (age 14) - Mother's NS-SEC: W1nsseccatmum
cleaned$nssecma14 <- extract_major_cat(cleaned$W1nsseccatmum)
cleaned$nssecma14 <- convert_missing(cleaned$nssecma14)

# Father's NS-SEC: W1nsseccatdad
cleaned$nssecpa14 <- extract_major_cat(cleaned$W1nsseccatdad)
cleaned$nssecpa14 <- convert_missing(cleaned$nssecpa14)

# Process Wave 2 (age 15)
# Mother's NS-SEC: W2nsseccatmum
cleaned$nssecma15 <- extract_major_cat(cleaned$W2nsseccatmum)
cleaned$nssecma15 <- convert_missing(cleaned$nssecma15)

# Father's NS-SEC: W2nsseccatdad
cleaned$nssecpa15 <- extract_major_cat(cleaned$W2nsseccatdad)
cleaned$nssecpa15 <- convert_missing(cleaned$nssecpa15)

# Process Wave 3 (age 16)
# Mother's NS-SEC: W3cnsseccatmum
cleaned$nssecma16 <- extract_major_cat(cleaned$W3cnsseccatmum)
cleaned$nssecma16 <- convert_missing(cleaned$nssecma16)

# Father's NS-SEC: W3cnsseccatdad
cleaned$nssecpa16 <- extract_major_cat(cleaned$W3cnsseccatdad)
cleaned$nssecpa16 <- convert_missing(cleaned$nssecpa16)

# Process Wave 4 (age 17)
# Mother's NS-SEC: w4cnsseccatmum
cleaned$nssecma17 <- extract_major_cat(cleaned$w4cnsseccatmum)
cleaned$nssecma17 <- convert_missing(cleaned$nssecma17)

# Father's NS-SEC: w4cnsseccatdad
cleaned$nssecpa17 <- extract_major_cat(cleaned$w4cnsseccatdad)
cleaned$nssecpa17 <- convert_missing(cleaned$nssecpa17)

# Process Wave 5 (age 18)
# Note: w5Cnsseccatmum measures mother's partner, w5Cnsseccatdad measures father's partner
# But still assign to nssecma18 and nssecpa18 respectively per requirements
cleaned$nssecma18 <- extract_major_cat(cleaned$w5Cnsseccatmum)
cleaned$nssecma18 <- convert_missing(cleaned$nssecma18)

cleaned$nssecpa18 <- extract_major_cat(cleaned$w5Cnsseccatdad)
cleaned$nssecpa18 <- convert_missing(cleaned$nssecpa18)

# Keep only NSID and the 10 output variables
output <- cleaned %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, 
         nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Successfully created data/output/cleaned_data.csv\n")
cat("Dimensions:", dim(output), "\n")
cat("Variables:", names(output), "\n")
