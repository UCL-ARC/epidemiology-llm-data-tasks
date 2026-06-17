library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

data1 <- read_delim(file1, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data2 <- read_delim(file2, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data3 <- read_delim(file3, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data4 <- read_delim(file4, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))

# Merge datasets
df <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Define harmonisation function for economic activity
harmonise_eco <- function(x) {
  # Substantive categories (1-9) remain as they are
  # Missing data requirements: -99, -98, -996 map to -3
  # General missing guidance: NA to -3
  res <- case_when(
    x >= 1 & x <= 9 ~ x,
    x == -99 | x == -98 | x == -996 ~ -3,
    is.na(x) ~ -3,
    TRUE ~ -3 # Default for other missing values like -999, -94 based on specific task requirement to map to -3 or general guidance
  )
  return(res)
}

# Apply harmonisation for each wave and parent
df <- df %>%
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

# Define value labels for all ecoact variables
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
  "-3" = "Not asked at the fieldwork stage / not interviewed"
)

# Apply labels to the 8 variables
eco_vars <- c("ecoactma14", "ecoactpa14", "ecoactma15", "ecoactpa15", "ecoactma16", "ecoactpa16", "ecoactma17", "ecoactpa17")

for (var in eco_vars) {
  df[[var]] <- factor(df[[var]], levels = as.numeric(names(eco_labels)), labels = eco_labels)
}

# Select final columns
final_df <- df %>%
  select(NSID, all_of(eco_vars))

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")