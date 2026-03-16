library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say"
)

# Function to harmonize missing values
harmonize_missing <- function(df, var) {
  if (var %in% colnames(df)) {
    df[[var]] <- case_when(
      df[[var]] %in% c(-999.0, -99.0, -98.0, -94.0) ~ -3,
      df[[var]] %in% c(-996.0) ~ -1,
      df[[var]] %in% c(-92.0) ~ -9,
      TRUE ~ df[[var]]
    )
  }
  return(df)
}

# Harmonize missing values for parental economic activity variables
merged_data <- harmonize_missing(merged_data, "W1empsmum")
merged_data <- harmonize_missing(merged_data, "W1empsdad")
merged_data <- harmonize_missing(merged_data, "W2empsmum")
merged_data <- harmonize_missing(merged_data, "W2empsdad")
merged_data <- harmonize_missing(merged_data, "W3empsmum")
merged_data <- harmonize_missing(merged_data, "W3empsdad")
merged_data <- harmonize_missing(merged_data, "w4empsmum")
merged_data <- harmonize_missing(merged_data, "w4empsdad")

# Create age-specific variables for parental economic activity
merged_data <- merged_data %>%
  mutate(
    ecoactdtma14 = W1empsmum,
    ecoactdtpa14 = W1empsdad,
    ecoactdtma15 = W2empsmum,
    ecoactdtpa15 = W2empsdad,
    ecoactdtma16 = W3empsmum,
    ecoactdtpa16 = W3empsdad,
    ecoactdtma17 = w4empsmum,
    ecoactdtpa17 = w4empsdad
  )

# Create collapsed harmonized variables for parental economic activity
merged_data <- merged_data %>%
  mutate(
    ecoactma14 = case_when(
      ecoactdtma14 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtma14 %in% c(3.0) ~ "unemployed",
      ecoactdtma14 %in% c(4.0) ~ "training",
      ecoactdtma14 %in% c(5.0) ~ "education",
      ecoactdtma14 %in% c(6.0) ~ "home",
      ecoactdtma14 %in% c(7.0) ~ "retired",
      ecoactdtma14 %in% c(8.0) ~ "sick_disabled",
      ecoactdtma14 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactpa14 = case_when(
      ecoactdtpa14 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtpa14 %in% c(3.0) ~ "unemployed",
      ecoactdtpa14 %in% c(4.0) ~ "training",
      ecoactdtpa14 %in% c(5.0) ~ "education",
      ecoactdtpa14 %in% c(6.0) ~ "home",
      ecoactdtpa14 %in% c(7.0) ~ "retired",
      ecoactdtpa14 %in% c(8.0) ~ "sick_disabled",
      ecoactdtpa14 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactma15 = case_when(
      ecoactdtma15 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtma15 %in% c(3.0) ~ "unemployed",
      ecoactdtma15 %in% c(4.0) ~ "training",
      ecoactdtma15 %in% c(5.0) ~ "education",
      ecoactdtma15 %in% c(6.0) ~ "home",
      ecoactdtma15 %in% c(7.0) ~ "retired",
      ecoactdtma15 %in% c(8.0) ~ "sick_disabled",
      ecoactdtma15 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactpa15 = case_when(
      ecoactdtpa15 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtpa15 %in% c(3.0) ~ "unemployed",
      ecoactdtpa15 %in% c(4.0) ~ "training",
      ecoactdtpa15 %in% c(5.0) ~ "education",
      ecoactdtpa15 %in% c(6.0) ~ "home",
      ecoactdtpa15 %in% c(7.0) ~ "retired",
      ecoactdtpa15 %in% c(8.0) ~ "sick_disabled",
      ecoactdtpa15 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactma16 = case_when(
      ecoactdtma16 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtma16 %in% c(3.0) ~ "unemployed",
      ecoactdtma16 %in% c(4.0) ~ "training",
      ecoactdtma16 %in% c(5.0) ~ "education",
      ecoactdtma16 %in% c(6.0) ~ "home",
      ecoactdtma16 %in% c(7.0) ~ "retired",
      ecoactdtma16 %in% c(8.0) ~ "sick_disabled",
      ecoactdtma16 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactpa16 = case_when(
      ecoactdtpa16 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtpa16 %in% c(3.0) ~ "unemployed",
      ecoactdtpa16 %in% c(4.0) ~ "training",
      ecoactdtpa16 %in% c(5.0) ~ "education",
      ecoactdtpa16 %in% c(6.0) ~ "home",
      ecoactdtpa16 %in% c(7.0) ~ "retired",
      ecoactdtpa16 %in% c(8.0) ~ "sick_disabled",
      ecoactdtpa16 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactma17 = case_when(
      ecoactdtma17 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtma17 %in% c(3.0) ~ "unemployed",
      ecoactdtma17 %in% c(4.0) ~ "training",
      ecoactdtma17 %in% c(5.0) ~ "education",
      ecoactdtma17 %in% c(6.0) ~ "home",
      ecoactdtma17 %in% c(7.0) ~ "retired",
      ecoactdtma17 %in% c(8.0) ~ "sick_disabled",
      ecoactdtma17 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    ),
    ecoactpa17 = case_when(
      ecoactdtpa17 %in% c(1.0, 2.0) ~ "paid_work",
      ecoactdtpa17 %in% c(3.0) ~ "unemployed",
      ecoactdtpa17 %in% c(4.0) ~ "training",
      ecoactdtpa17 %in% c(5.0) ~ "education",
      ecoactdtpa17 %in% c(6.0) ~ "home",
      ecoactdtpa17 %in% c(7.0) ~ "retired",
      ecoactdtpa17 %in% c(8.0) ~ "sick_disabled",
      ecoactdtpa17 %in% c(9.0) ~ "other",
      TRUE ~ NA_character_
    )
  )

# Convert to factors with labels
merged_data <- merged_data %>%
  mutate(
    ecoactdtma14 = factor(ecoactdtma14, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtpa14 = factor(ecoactdtpa14, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtma15 = factor(ecoactdtma15, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtpa15 = factor(ecoactdtpa15, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtma16 = factor(ecoactdtma16, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtpa16 = factor(ecoactdtpa16, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtma17 = factor(ecoactdtma17, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactdtpa17 = factor(ecoactdtpa17, levels = c(-9, -8, -1, -3, -2, -7, 1:9),
                           labels = c("Refusal", "Don't know/insufficient information", "Item not applicable", 
                                     "Not asked at the fieldwork stage/participated/interviewed", 
                                     "Schedule not applicable/Script error/information lost", 
                                     "Prefer not to say", "Doing paid work for 30 or more hours a week", 
                                     "Doing paid work for fewer than 30 hours a week", "Unemployed/ Looking for a job", 
                                     "On a training course or scheme", "In full-time education/ at school", 
                                     "Looking after the family/ household", "Retired from work altogether", 
                                     "Sick/ disabled", "Other")),
    ecoactma14 = factor(ecoactma14, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactpa14 = factor(ecoactpa14, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactma15 = factor(ecoactma15, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactpa15 = factor(ecoactpa15, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactma16 = factor(ecoactma16, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactpa16 = factor(ecoactpa16, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactma17 = factor(ecoactma17, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other")),
    ecoactpa17 = factor(ecoactpa17, levels = c("paid_work", "unemployed", "training", "education", "home", "retired", "sick_disabled", "other"))
  )

# Select final variables for output
final_data <- merged_data %>%
  select(NSID, ecoactdtma14, ecoactdtpa14, ecoactdtma15, ecoactdtpa15, ecoactdtma16, ecoactdtpa16, ecoactdtma17, ecoactdtpa17,
         ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)