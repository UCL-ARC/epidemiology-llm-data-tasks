# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the data files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")
merged_data <- full_join(merged_data, ns9_derived, by = "NSID")

# Select and rename deprivation variables
# IMDRSCORE.x from wave_two (Age 15) -> imd15
# IMDRSCORE.y from wave_three (Age 16) -> imd16
# W9DIMDD from ns9_derived (Age 32) -> imd32
cleaned_data <- merged_data %>%
  select(NSID, IMDRSCORE.x, IMDRSCORE.y, W9DIMDD) %>%
  rename(
    imd15 = IMDRSCORE.x,
    imd16 = IMDRSCORE.y,
    imd32 = W9DIMDD
  )

# Recode missing values according to standard scheme
# -94 to -8 (Don't know / insufficient information)
# NA to -3 (Not asked at the fieldwork stage / not interviewed)
cleaned_data <- cleaned_data %>%
  mutate(
    imd15 = if_else(imd15 == -94, -8, imd15),
    imd16 = if_else(imd16 == -94, -8, imd16),
    imd32 = if_else(imd32 == -94, -8, imd32)
  )

# Replace NA with -3
cleaned_data <- cleaned_data %>%
  mutate(
    imd15 = if_else(is.na(imd15), -3, imd15),
    imd16 = if_else(is.na(imd16), -3, imd16),
    imd32 = if_else(is.na(imd32), -3, imd32)
  )

# Create labelled variables with labels for missing-value codes
# Use setNames to create named numeric vectors with negative number names
labelled_vars <- list(
  imd15 = setNames(c(-9, -8, -3, -2, -1), c("Refusal", "Dont know / insufficient information", "Not asked at the fieldwork stage / not interviewed", "Script error / information lost", "Not applicable")),
  imd16 = setNames(c(-9, -8, -3, -2, -1), c("Refusal", "Dont know / insufficient information", "Not asked at the fieldwork stage / not interviewed", "Script error / information lost", "Not applicable")),
  imd32 = setNames(c(-9, -8, -3, -2, -1), c("Refusal", "Dont know / insufficient information", "Not asked at the fieldwork stage / not interviewed", "Script error / information lost", "Not applicable"))
)

for (var in names(labelled_vars)) {
  var_label(cleaned_data[[var]]) <- "Area Deprivation Score"
  val_labels(cleaned_data[[var]]) <- labelled_vars[[var]]
}

# Output to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
