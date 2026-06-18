library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File paths
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_four_lsype_young_person_2020.tab"
file3 <- "data/input/ns8_2015_derived.tab"
file4 <- "data/input/ns9_2022_derived_variables.tab"

# Load datasets
data1 <- readr::read_delim(file1, delim = "\t", col_types = readr::cols())
data2 <- readr::read_delim(file2, delim = "\t", col_types = readr::cols())
data3 <- readr::read_delim(file3, delim = "\t", col_types = readr::cols())
data4 <- readr::read_delim(file4, delim = "\t", col_types = readr::cols())

# Merge datasets
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Define common labels for income
income_labels <- c(
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

# Function to clean income variables
clean_income <- function(var, labels) {
  # Convert NA to -3 (Not asked/Not interviewed)
  res <- ifelse(is.na(var), -3, var)
  
  # Map -1.0 (Not applicable) to -1
  res[res == -1] <- -1
  
  # Create factor with labels
  # Valid categories are 1 to 16
  # Missing codes: -1 (NA), -3 (NA in original)
  
  # We use factor for categorical derived variables
  # The values in the data are numeric, we convert to character to match the label names
  
  levels_all <- c("-1", "-3", as.character(1:16))
  names_all <- c("-1" = "Not applicable", "-3" = "Not asked at the fieldwork stage / not interviewed", income_labels)
  
  # Convert to character for factor mapping
  char_res <- as.character(round(res))
  
  # Ensure all values are in the allowed levels
  char_res[!(char_res %in% levels_all)] <- "-3"
  
  f <- factor(char_res, levels = levels_all, labels = names_all)
  return(f)
}

# Process inc25 and inc32
# Wave 8 is age 25, Wave 9 is age 32
merged_data <- merged_data %>%
  mutate(
    inc25 = clean_income(W8DINCB, income_labels),
    inc32 = clean_income(W9DINCB, income_labels)
  )

# Final selection: NSID and target variables
final_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write to CSV
readr::write_csv(final_data, "data/output/cleaned_data.csv")