library(dplyr)
library(readr)
library(labelled)
library(haven)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID
df <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Define NS-SEC 17 major categories with labels as a named list
nssec_codes <- c(
  `Employers in large organisations` = 1,
  `Higher managerial occupations` = 2,
  `Higher professional occupations` = 3,
  `Lower professional occupations` = 4,
  `Lower managerial occupations` = 5,
  `Higher supervisory occupations` = 6,
  `Intermediate occupations` = 7,
  `Employers in small organisations` = 8,
  `Own account workers` = 9,
  `Lower supervisory occupations` = 10,
  `Lower technical occupations` = 11,
  `Semi-routine occupations` = 12,
  `Routine occupations` = 13,
  `Never worked / Long-term unemployed` = 14,
  `Full-time students` = 15,
  `Not classified or inadequately stated` = 16,
  `Not classifiable for other reasons` = 17,
  `Schedule not applicable / script error / information lost` = -2,
  `Not asked at the fieldwork stage / not interviewed` = -3,
  `Don't know / insufficient information` = -8
)

# Function to process NS-SEC variable
process_nssec <- function(x, missing_map) {
  # Replace NA with -3 (default)
  x[is.na(x)] <- -3
  
  # Apply missing value mappings
  for (code in names(missing_map)) {
    x[x == as.numeric(code)] <- missing_map[[code]]
  }
  
  # For valid codes (1 to 17.9), extract major category
  valid_idx <- !is.na(x) & x >= 1 & x <= 17.9
  x[valid_idx] <- floor(x[valid_idx])
  
  # Set value labels on numeric vector using haven::labelled
  x <- haven::labelled(x, labels = nssec_codes)
  
  x
}

# Define missing value mappings
common_missing <- c("-999" = -2, "-99" = -3, "-98" = -3, "-94" = -8)
wave5_missing <- c("-98" = -3)

# Process each variable
nssecma14 <- process_nssec(df$W1nsseccatmum, common_missing)
nssecpa14 <- process_nssec(df$W1nsseccatdad, common_missing)

nssecma15 <- process_nssec(df$W2nsseccatmum, common_missing)
nssecpa15 <- process_nssec(df$W2nsseccatdad, common_missing)

nssecma16 <- process_nssec(df$W3cnsseccatmum, common_missing)
nssecpa16 <- process_nssec(df$W3cnsseccatdad, common_missing)

nssecma17 <- process_nssec(df$w4cnsseccatmum, common_missing)
nssecpa17 <- process_nssec(df$w4cnsseccatdad, common_missing)

nssecma18 <- process_nssec(df$w5Cnsseccatmum, wave5_missing)
nssecpa18 <- process_nssec(df$w5Cnsseccatdad, wave5_missing)

# Create output as a tibble/data frame
output <- tibble::tibble(
  NSID = df$NSID,
  nssecma14 = nssecma14,
  nssecpa14 = nssecpa14,
  nssecma15 = nssecma15,
  nssecpa15 = nssecpa15,
  nssecma16 = nssecma16,
  nssecpa16 = nssecpa16,
  nssecma17 = nssecma17,
  nssecpa17 = nssecpa17,
  nssecma18 = nssecma18,
  nssecpa18 = nssecpa18
)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output file written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")
cat("\nSample of nssecma14:\n")
print(head(output$nssecma14, 5))
cat("\nSample of nssecpa14:\n")
print(head(output$nssecpa14, 5))