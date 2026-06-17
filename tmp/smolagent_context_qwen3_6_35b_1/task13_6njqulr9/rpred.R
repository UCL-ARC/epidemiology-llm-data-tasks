library(dplyr)
library(readr)
library(labelled)

# Define the 17-category NS-SEC labels
nssec_labels <- c(
  `1` = "Employers in large organisations",
  `2` = "Higher managerial occupations",
  `3` = "Higher professional occupations",
  `4` = "Lower professional occupations",
  `5` = "Lower managerial occupations",
  `6` = "Higher supervisory occupations",
  `7` = "Intermediate occupations",
  `8` = "Employers in small organisations",
  `9` = "Own account workers",
  `10` = "Lower supervisory occupations",
  `11` = "Lower technical occupations",
  `12` = "Semi-routine occupations",
  `13` = "Routine occupations",
  `14` = "Never worked or long-term unemployed",
  `15` = "Full-time students",
  `16` = "Not classified or inadequately stated",
  `17` = "Not classifiable for other reasons"
)

# Function to process NS-SEC variables
process_nssec <- function(raw_var) {
  out <- raw_var
  
  # Map missing codes
  out[raw_var == -98] <- -3  # Parent not present -> not asked
  out[raw_var == -99] <- -3  # Not interviewed -> not asked  
  out[raw_var == -94] <- -8  # Insufficient information -> don't know
  out[raw_var == -999] <- -2 # Missing data lost -> schedule not applicable
  
  # For valid codes (positive), collapse to integer part
  valid_mask <- !is.na(out) & out > 0
  out[valid_mask] <- floor(out[valid_mask])
  
  # Create factor
  f <- factor(out, levels = 1:17, labels = nssec_labels)
  
  return(f)
}

# Load all 5 files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files
full_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Process mother's NS-SEC for each wave
nssecma14 <- process_nssec(full_data$W1nsseccatmum)
nssecpa14 <- process_nssec(full_data$W1nsseccatdad)

nssecma15 <- process_nssec(full_data$W2nsseccatmum)
nssecpa15 <- process_nssec(full_data$W2nsseccatdad)

nssecma16 <- process_nssec(full_data$W3cnsseccatmum)
nssecpa16 <- process_nssec(full_data$W3cnsseccatdad)

nssecma17 <- process_nssec(full_data$w4cnsseccatmum)
nssecpa17 <- process_nssec(full_data$w4cnsseccatdad)

nssecma18 <- process_nssec(full_data$w5Cnsseccatmum)
nssecpa18 <- process_nssec(full_data$w5Cnsseccatdad)

# Create output dataframe
output <- full_data %>%
  select(NSID) %>%
  mutate(
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

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output
write_csv(output, "data/output/cleaned_data.csv")

# Verify
print(head(output))
cat("\nNumber of rows:", nrow(output), "\n")
cat("Variables:", paste(names(output), collapse = ", "), "\n")

# Check distributions
for (v in names(output)[-1]) {
  cat(v, ":\n")
  print(table(output[[v]], useNA = "ifany"))
  cat("\n")
}
