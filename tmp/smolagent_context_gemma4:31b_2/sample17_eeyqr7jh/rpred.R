library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file5 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols())

# Select and rename relevant variables
data2 <- file2 %>% select(NSID, imd15 = IMDRSCORE)
data3 <- file3 %>% select(NSID, imd16 = IMDRSCORE)
data5 <- file5 %>% select(NSID, imd32 = W9DIMDD)

# Merge datasets
merged_data <- data2 %>%
  full_join(data3, by = "NSID") %>%
  full_join(data5, by = "NSID")

# Function to harmonise missing values
harmonise_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -94] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Apply harmonisation
merged_data <- merged_data %>%
  mutate(across(c(imd15, imd16, imd32), harmonise_missing))

# Correct the labels: the names of the vector must be the labels, and the values must be the numeric codes
# The error was because set_value_labels expects a named vector where names are labels and values are the codes
missing_vals <- c(-9, -8, -3, -2, -1)
missing_labs <- c("Refusal", "Don’t know / insufficient information", "Not asked at the fieldwork stage / not interviewed", "Script error / information lost", "Not applicable")
names(missing_labs) <- as.character(missing_vals)

# Since set_value_labels in labelled package often expects labels as names and values as the numeric codes:
# We create a named vector: label = value
val_labels <- c(
  "Refusal" = -9,
  "Don’t know / insufficient information" = -8,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Script error / information lost" = -2,
  "Not applicable" = -1
)

merged_data <- merged_data %>%
  mutate(across(c(imd15, imd16, imd32), ~set_value_labels(.x, val_labels)))

# Ensure only requested variables are in output
final_data <- merged_data %>%
  select(NSID, imd15, imd16, imd32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
