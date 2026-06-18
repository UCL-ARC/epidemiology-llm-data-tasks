library(readr)
library(dplyr)

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = "c"))
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = cols(.default = "c"))
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(.default = "c"))

# Convert BMI columns to numeric
ns8 <- ns8 %>% mutate(W8DBMI = as.numeric(W8DBMI))
ns9 <- ns9 %>% mutate(W9DBMI = as.numeric(W9DBMI))

# Function to map missing codes to standard codes
clean_bmi <- function(x){
  x_clean <- case_when(
    is.na(x) ~ -3,
    x == -9.0 ~ -9,
    x == -8.0 ~ -8,
    x == -1.0 ~ -1,
    TRUE ~ x
  )
  return(x_clean)
}

ns8 <- ns8 %>% mutate(bmi25 = clean_bmi(W8DBMI))
ns9 <- ns9 %>% mutate(bmi32 = clean_bmi(W9DBMI))

# Merge all datasets
merged <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Select final variables
final_df <- merged %>% select(NSID, bmi25, bmi32)

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")
