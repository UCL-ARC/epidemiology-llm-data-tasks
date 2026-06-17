library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the 17-category labels for NS-SEC
# Using character keys as the value names
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional occupations",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small orgs non-professional",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Never worked/Long-term unemployed/Not currently working",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons",
  "-2" = "Schedule not applicable / script error / information lost",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-8" = "Don't know / insufficient information",
  "-9" = "Refusal",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable"
)

# Function to harmonize NS-SEC with proper missing code handling
harmonize_nssec_full <- function(x, missing_code_map) {
  result <- rep(NA_real_, length(x))
  
  for (val in unique(x)) {
    idx <- which(x == val)
    val_str <- as.character(val)
    if (val_str %in% names(missing_code_map)) {
      result[idx] <- missing_code_map[[val_str]]
    } else if (is.na(val)) {
      result[idx] <- -3
    } else {
      result[idx] <- floor(abs(val))
    }
  }
  
  result
}

# Load all wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all waves by NSID (full join to preserve full cohort)
data <- wave1 %>%
  select(NSID, W1nsseccatmum, W1nsseccatdad) %>%
  full_join(wave2 %>% select(NSID, W2nsseccatmum, W2nsseccatdad), by = "NSID") %>%
  full_join(wave3 %>% select(NSID, W3cnsseccatmum, W3cnsseccatdad), by = "NSID") %>%
  full_join(wave4 %>% select(NSID, w4cnsseccatmum, w4cnsseccatdad), by = "NSID") %>%
  full_join(wave5 %>% select(NSID, w5Cnsseccatmum, w5Cnsseccatdad), by = "NSID")

# Define missing code maps
missing_map_early <- setNames(c(-2, -3, -3, -8), c("-999", "-99", "-98", "-94"))
missing_map_wave5 <- setNames(c(-3), c("-98"))

# Derive NS-SEC variables for mother and father at each wave
data$nssecma14 <- harmonize_nssec_full(data$W1nsseccatmum, missing_map_early)
data$nssecpa14 <- harmonize_nssec_full(data$W1nsseccatdad, missing_map_early)
data$nssecma15 <- harmonize_nssec_full(data$W2nsseccatmum, missing_map_early)
data$nssecpa15 <- harmonize_nssec_full(data$W2nsseccatdad, missing_map_early)
data$nssecma16 <- harmonize_nssec_full(data$W3cnsseccatmum, missing_map_early)
data$nssecpa16 <- harmonize_nssec_full(data$W3cnsseccatdad, missing_map_early)
data$nssecma17 <- harmonize_nssec_full(data$w4cnsseccatmum, missing_map_early)
data$nssecpa17 <- harmonize_nssec_full(data$w4cnsseccatdad, missing_map_early)
data$nssecma18 <- harmonize_nssec_full(data$w5Cnsseccatmum, missing_map_wave5)
data$nssecpa18 <- harmonize_nssec_full(data$w5Cnsseccatdad, missing_map_wave5)

# Select only ID and final derived variables
output <- data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, 
         nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to output file
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")

# Verify the output
cat("\nVerifying output file...\n")
output_check <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Output verification - rows:", nrow(output_check), "\n")
cat("Output verification - columns:", ncol(output_check), "\n")
cat("Column names:", paste(names(output_check), collapse = ", "), "\n")
cat("\nSample data (first 10 rows):\n")
print(head(output_check, 10))