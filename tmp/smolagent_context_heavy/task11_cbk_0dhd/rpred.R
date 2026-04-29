library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets
data1 <- readr::read_delim(file1, delim = "\t", col_types = readr::cols(.default = "c"))
data2 <- readr::read_delim(file2, delim = "\t", col_types = readr::cols(.default = "c"))
data3 <- readr::read_delim(file3, delim = "\t", col_types = readr::cols(.default = "c"))
data4 <- readr::read_delim(file4, delim = "\t", col_types = readr::cols(.default = "c"))

# Convert numeric columns to numeric for processing
# Since they were read as characters, we convert the specific variables we need
vars_to_num <- list(
  data1 = c("W1empsmum", "W1empsdad"),
  data2 = c("W2empsmum", "W2empsdad"),
  data3 = c("W3empsmum", "W3empsdad"),
  data4 = c("w4empsmum", "w4empsdad")
)

process_nums <- function(df, vars) {
  df %>% mutate(across(all_of(vars), as.numeric))
}

data1 <- process_nums(data1, vars_to_num$data1)
data2 <- process_nums(data2, vars_to_num$data2)
data3 <- process_nums(data3, vars_to_num$data3)
data4 <- process_nums(data4, vars_to_num$data4)

# Merge datasets
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Harmonisation function
# Standard Missing: -9 Refusal, -8 DK, -7 Prefer not, -3 Not asked/interviewed, -2 Schedule/Error, -1 Not applicable
# Requirement: -99, -98, -996 map to -3
# Also mapping -999.0 (Missing household information) to -2 (Information lost)
# Mapping -94 (Insufficient information) to -8 (Insufficient information)
# Mapping -92 (Refusal) to -9 (Refusal)

harmonise_eco <- function(x) {
  x <- as.numeric(x)
  res <- x
  # Specific requirements
  res[x == -99 | x == -98 | x == -996] <- -3
  # Metadata based mappings
  res[x == -999] <- -2
  res[x == -94] <- -8
  res[x == -92] <- -9
  # Default NA to -3
  res[is.na(x)] <- -3
  return(res)
}

# Apply harmonisation
final_data <- merged_data %>%
  mutate(
    ecoactma14 = harmonise_eco(W1empsmum),
    ecoactpa14 = harmonise_eco(W1empsdad),
    ecoactma15 = harmonise_eco(W2empsmum),
    ecoactpa15 = harmonise_eco(W2empsdad),
    ecoactma16 = harmonise_eco(W3empsmum),
    ecoactpa16 = harmonise_eco(W3empsdad),
    ecoactma17 = harmonise_eco(w4empsmum),
    ecoactpa17 = harmonise_eco(w4empsdad)
  )

# Define Labels
eco_labels <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply labels as factors
vars_to_label <- c("ecoactma14", "ecoactpa14", "ecoactma15", "ecoactpa15", 
                   "ecoactma16", "ecoactpa16", "ecoactma17", "ecoactpa17")

final_data <- final_data %>%
  mutate(across(all_of(vars_to_label), ~ { 
    val <- . 
    factor(val, levels = as.numeric(names(eco_labels)), labels = eco_labels) 
  }))

# Selection of final variables
final_output <- final_data %>%
  select(NSID, all_of(vars_to_label))

# Write output
readr::write_csv(final_output, "data/output/cleaned_data.csv")