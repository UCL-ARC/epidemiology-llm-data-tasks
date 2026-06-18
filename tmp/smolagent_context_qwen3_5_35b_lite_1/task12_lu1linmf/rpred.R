# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave5 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
wave5 <- read_delim(files$wave5, delim = "\t", show_col_types = FALSE)
wave6 <- read_delim(files$wave6, delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(files$wave7, delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(files$wave8, delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(files$wave9, delim = "\t", show_col_types = FALSE)

# Function to convert missing values to standard codes
convert_missing <- function(x) {
  x <- as.numeric(x)
  x[which(x == -9)] <- -9
  x[which(x == -8)] <- -8
  x[which(x == -7)] <- -7
  x[which(x == -99 | x == -91)] <- -2
  x[which(x == -1)] <- -1
  x[is.na(x)] <- -3
  return(x)
}

# Function to collapse NS-SEC to major categories (handles both integer and decimal codes)
collapse_nssec <- function(x) {
  result <- rep(-3, length(x))
  
  # Major category 1: Employers (large and small)
  idx <- which(x %in% c(1, 1.0, 8.1, 8.2, 8))
  result[idx] <- 1
  
  # Major category 2: Higher managerial and administrative (2, 5, 6, 10)
  idx <- which(x %in% c(2, 2.0, 5, 5.0, 6, 6.0, 10, 10.0))
  result[idx] <- 2
  
  # Major category 3: Higher professional (3.1, 3.2, 3.3, 3.4)
  idx <- which(x %in% c(3.1, 3.2, 3.3, 3.4))
  result[idx] <- 3
  
  # Major category 4: Lower professional (4.1, 4.2, 4.3, 4.4)
  idx <- which(x %in% c(4.1, 4.2, 4.3, 4.4))
  result[idx] <- 4
  
  # Major category 5: Intermediate occupations (7.1, 7.2, 7.3, 7.4)
  idx <- which(x %in% c(7.1, 7.2, 7.3, 7.4))
  result[idx] <- 5
  
  # Major category 6: Own account workers (9.1, 9.2)
  idx <- which(x %in% c(9.1, 9.2))
  result[idx] <- 6
  
  # Major category 7: Lower technical (11.1, 11.2)
  idx <- which(x %in% c(11.1, 11.2))
  result[idx] <- 7
  
  # Major category 8: Semi-routine occupations (12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7)
  idx <- which(x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7))
  result[idx] <- 8
  
  # Major category 9: Routine occupations (13.0, 13.1, 13.2, 13.3, 13.4, 13.5)
  idx <- which(x %in% c(13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13))
  result[idx] <- 9
  
  # Major category 10: Never worked and long-term unemployed (14.0, 14.1, 14.2, 14.3)
  idx <- which(x %in% c(14.0, 14.1, 14.2, 14.3, 14))
  result[idx] <- 10
  
  # Major category 11: Full-time students (15.0, 15)
  idx <- which(x %in% c(15.0, 15))
  result[idx] <- 11
  
  # Major category 12: Not stated/classified (16.0, 17.0, 16, 17)
  idx <- which(x %in% c(16.0, 16, 17.0, 17))
  result[idx] <- 12
  
  return(result)
}

# Merge all datasets by NSID
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create derived variables from the merged data
data$nssec17 <- collapse_nssec(convert_missing(data$W4nsseccatYP))
data$nssec18 <- collapse_nssec(convert_missing(data$W5nsseccatYP))
data$nssec19 <- collapse_nssec(convert_missing(data$w6nsseccatYP))
data$nssec20 <- collapse_nssec(convert_missing(data$W7NSSECCat))
data$nssec25 <- collapse_nssec(convert_missing(data$W8DNSSEC17))
data$nssec32 <- collapse_nssec(convert_missing(data$W9NSSEC))

# Create final dataset with only ID and derived variables
result <- data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(result, "data/output/cleaned_data.csv")

cat("Output file created successfully\n")
cat("Dimensions:", dim(result), "\n")
cat("\nSummary statistics:\n")
for (v in names(result)) {
  if (v != "NSID") {
    cat("\n", v, ":\n")
    cat("  Min:", min(result[[v]]), "\n")
    cat("  Max:", max(result[[v]]), "\n")
    cat("  Unique values:", paste(sort(unique(result[[v]])), collapse=", "), "\n")
    cat("  Counts:\n")
    print(table(result[[v]]))
  }
}
