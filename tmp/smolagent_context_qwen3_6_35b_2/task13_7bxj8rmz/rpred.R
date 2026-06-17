# Load required libraries
library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Define file names
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab",
  "wave_five_lsype_family_background_2020.tab"
)

# Load each file into a named list
datasets <- list()
for (f in files) {
  datasets[[f]] <- read_delim(paste0("data/input/", f), delim = "\t", show_col_types = FALSE)
}

# Define the 17 major NS-SEC category labels
nssec_labels <- c(
  "Employers in large organisations" = 1,
  "Higher managerial occupations" = 2,
  "Higher professional occupations" = 3,
  "Lower professional occupations" = 4,
  "Lower managerial occupations" = 5,
  "Higher supervisory occupations" = 6,
  "Intermediate occupations" = 7,
  "Employers in small organisations" = 8,
  "Own account workers" = 9,
  "Lower supervisory occupations" = 10,
  "Lower technical occupations" = 11,
  "Semi-routine occupations" = 12,
  "Routine occupations" = 13,
  "Never worked or long-term unemployed" = 14,
  "Full-time students" = 15,
  "Not classified or inadequately stated" = 16,
  "Not classifiable for other reasons" = 17
)

# Function to collapse fractional codes to major category and handle missing values
collapse_nssec <- function(x) {
  result <- x
  
  # Convert -98 (parent not present) to -3
  result[result == -98] <- -3
  
  # Convert -99 (not interviewed) to -3
  result[result == -99] <- -3
  
  # Convert -94 (insufficient information) to -8
  result[result == -94] <- -8
  
  # Convert -999 (data lost) to -2
  result[result == -999] <- -2
  
  # Convert remaining NA to -3
  result[is.na(result)] <- -3
  
  # For valid values (positive integers or fractional codes), take integer part
  valid_mask <- result >= 1 & result <= 17.9
  result[valid_mask] <- as.integer(result[valid_mask])
  
  return(result)
}

# Merge all datasets
merged <- datasets[[1]]
for (i in 2:length(datasets)) {
  merged <- full_join(merged, datasets[[i]], by = "NSID")
}

# Derive mother's NS-SEC variables
merged$nssecma14 <- collapse_nssec(merged$W1nsseccatmum)
merged$nssecpa14 <- collapse_nssec(merged$W1nsseccatdad)

merged$nssecma15 <- collapse_nssec(merged$W2nsseccatmum)
merged$nssecpa15 <- collapse_nssec(merged$W2nsseccatdad)

merged$nssecma16 <- collapse_nssec(merged$W3cnsseccatmum)
merged$nssecpa16 <- collapse_nssec(merged$W3cnsseccatdad)

merged$nssecma17 <- collapse_nssec(merged$w4cnsseccatmum)
merged$nssecpa17 <- collapse_nssec(merged$w4cnsseccatdad)

merged$nssecma18 <- collapse_nssec(merged$w5Cnsseccatmum)
merged$nssecpa18 <- collapse_nssec(merged$w5Cnsseccatdad)

# Create labeled factors for each variable using haven::labelled
for (var in c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15",
              "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
              "nssecma18", "nssecpa18")) {
  # Use haven::labelled to set value labels for valid categories
  merged[[var]] <- haven::labelled(merged[[var]], labels = nssec_labels)
  # Convert to labelled factor
  merged[[var]] <- haven::as_factor(merged[[var]])
}

# Select only NSID and the 10 derived variables
output <- merged %>% select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15,
                            nssecma16, nssecpa16, nssecma17, nssecpa17,
                            nssecma18, nssecpa18)

# Write output
dir.create("data/output", showWarnings = FALSE)
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
print(head(output, 10))
