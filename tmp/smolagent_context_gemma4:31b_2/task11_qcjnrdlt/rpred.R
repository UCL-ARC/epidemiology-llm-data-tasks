library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Standard Missing-Value Codes mapping function
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked at the fieldwork stage/participated/interviewed
# -2 = Schedule not applicable/Script error/information lost
# -7 = Prefer not to say
# Null -> -3

harmonize_missing <- function(x) {
  x <- as.numeric(x)
  # Specific mappings based on metadata
  # -999.0 (Missing household info/lost) -> -2 (Schedule not applicable/info lost)
  # -99.0 (Not interviewed) -> -3 (Not asked/participated)
  # -98.0 (Not present) -> -3 (Not asked/participated)
  # -94.0 (Insufficient information) -> -8 (Don't know/insufficient info)
  # -92.0 (Refusal) -> -9 (Refusal)
  # -996.0 (No parent in household) -> -1 (Item not applicable)
  
  x[x == -999.0] <- -2
  x[x == -99.0] <- -3
  x[x == -98.0] <- -3
  x[x == -94.0] <- -8
  x[x == -92.0] <- -9
  x[x == -996.0] <- -1
  
  x[is.na(x)] <- -3
  return(x)
}

# Load datasets
data1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
data2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
data3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
data4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Define common categories for parental economic activity
# full-time, part-time, unemployed, training, education, home, retired, sick/disabled, other
# 1: Paid >= 30h (full-time)
# 2: Paid < 30h (part-time)
# 3: Unemployed
# 4: Training
# 5: Education
# 6: Home
# 7: Retired
# 8: Sick/Disabled
# 9: Other

parental_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say",
  "1" = "full-time",
  "2" = "part-time",
  "3" = "unemployed",
  "4" = "training",
  "5" = "education",
  "6" = "home",
  "7" = "retired",
  "8" = "sick/disabled",
  "9" = "other"
)

# Processing variables
process_parent_var <- function(var_name, age) {
  vals <- harmonize_missing(merged_data[[var_name]])
  # Convert to factor with labels
  f <- factor(vals, levels = as.numeric(names(parental_labels)), labels = parental_labels)
  return(f)
}

# Apply for each wave
# Wave 1 (Age 14)
merged_data$ecoactdtma14 <- process_parent_var("W1empsmum", 14)
merged_data$ecoactdtpa14 <- process_parent_var("W1empsdad", 14)

# Wave 2 (Age 15)
merged_data$ecoactdtma15 <- process_parent_var("W2empsmum", 15)
merged_data$ecoactdtpa15 <- process_parent_var("W2empsdad", 15)

# Wave 3 (Age 16)
merged_data$ecoactdtma16 <- process_parent_var("W3empsmum", 16)
merged_data$ecoactdtpa16 <- process_parent_var("W3empsdad", 16)

# Wave 4 (Age 17)
merged_data$ecoactdtma17 <- process_parent_var("w4empsmum", 17)
merged_data$ecoactdtpa17 <- process_parent_var("w4empsdad", 17)

# Select final columns
final_df <- merged_data %>%
  select(NSID, ecoactdtma14, ecoactdtpa14, ecoactdtma15, ecoactdtpa15, ecoactdtma16, ecoactdtpa16, ecoactdtma17, ecoactdtpa17)

# Output to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
