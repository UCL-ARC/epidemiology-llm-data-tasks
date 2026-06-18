library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
s2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
s3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
s8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets
merged <- full_join(s1, s2, by = "NSID")
merged <- full_join(merged, s3, by = "NSID")
merged <- full_join(merged, s4, by = "NSID")
merged <- full_join(merged, s6, by = "NSID")
merged <- full_join(merged, s7, by = "NSID")
merged <- full_join(merged, s8, by = "NSID")
merged <- full_join(merged, s9, by = "NSID")

# Create drinking indicators for each wave
# Wave 1 (age 14): Both W1alceverYP = 1 AND W1alcmonYP = 1
# Wave 2-7: WxalceverYP = 1
# Wave 8-9: AUDIT1 > 1 (above "Never")

merged <- merged %>%
  mutate(
    # Wave 1 (age 14): Both variables must equal 1
    drink_14 = case_when(
      W1alceverYP == 1 & W1alcmonYP == 1 ~ 1,
      W1alceverYP == 2 | W1alcmonYP == 2 ~ 0,
      W1alceverYP %in% c(-99, -97, -92, -91, -1) | 
      W1alcmonYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 2 (age 15): W2alceverYP
    drink_15 = case_when(
      W2alceverYP == 1 ~ 1,
      W2alceverYP == 2 ~ 0,
      W2alceverYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 3 (age 16): W3alceverYP
    drink_16 = case_when(
      W3alceverYP == 1 ~ 1,
      W3alceverYP == 2 ~ 0,
      W3alceverYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 4 (age 17): W4AlcEverYP
    drink_17 = case_when(
      W4AlcEverYP == 1 ~ 1,
      W4AlcEverYP == 2 ~ 0,
      W4AlcEverYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 6 (age 19): W6AlcEverYP
    drink_19 = case_when(
      W6AlcEverYP == 1 ~ 1,
      W6AlcEverYP == 2 ~ 0,
      W6AlcEverYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 7 (age 20): W7AlcEverYP
    drink_20 = case_when(
      W7AlcEverYP == 1 ~ 1,
      W7AlcEverYP == 2 ~ 0,
      W7AlcEverYP %in% c(-99, -97, -92, -91, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 8 (age 25): W8AUDIT1 > 1 (above "Never")
    drink_25 = case_when(
      W8AUDIT1 >= 2 & W8AUDIT1 <= 5 ~ 1,
      W8AUDIT1 == 1 ~ 0,
      W8AUDIT1 %in% c(-9, -8, -3, -1) ~ -8,
      TRUE ~ NA_real_
    ),
    
    # Wave 9 (age 32): W9AUDIT1 > 1 (above "Never")
    drink_32 = case_when(
      W9AUDIT1 >= 2 & W9AUDIT1 <= 5 ~ 1,
      W9AUDIT1 == 1 ~ 0,
      W9AUDIT1 %in% c(-9, -8, -3, -1) ~ -8,
      TRUE ~ NA_real_
    )
  )

# Derive alcfst - earliest age at which drinking is recorded
# Check waves in order (earliest to latest)
merged <- merged %>%
  mutate(
    alcfst = case_when(
      # Drinking observed at each wave (in order)
      drink_14 == 1 ~ 14,
      drink_15 == 1 ~ 15,
      drink_16 == 1 ~ 16,
      drink_17 == 1 ~ 17,
      drink_19 == 1 ~ 19,
      drink_20 == 1 ~ 20,
      drink_25 == 1 ~ 25,
      drink_32 == 1 ~ 32,
      
      # No drinking observed but some indicators are missing
      TRUE ~ case_when(
        any(c(is.na(drink_14), drink_14 == -8, is.na(drink_15), drink_15 == -8, 
              is.na(drink_16), drink_16 == -8, is.na(drink_17), drink_17 == -8, 
              is.na(drink_19), drink_19 == -8, is.na(drink_20), drink_20 == -8, 
              is.na(drink_25), drink_25 == -8, is.na(drink_32), drink_32 == -8)) ~ -8,
        # All observed indicators show not-drinking, no missing
        TRUE ~ 99
      )
    )
  )

# Convert to factor with specified levels and labels
merged$alcfst <- factor(merged$alcfst, 
                        levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                        labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information"))

# Select only NSID and alcfst for output
cleaned_data <- merged %>%
  select(NSID, alcfst)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

print("Data cleaning complete. Output written to data/output/cleaned_data.csv")
print(paste("Number of records:", nrow(cleaned_data)))
print(table(cleaned_data$alcfst, useNA = "ifany"))