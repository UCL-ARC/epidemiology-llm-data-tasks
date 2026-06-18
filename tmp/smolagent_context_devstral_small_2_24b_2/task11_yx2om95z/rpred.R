library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Define a function to recode missing values
recode_missing <- function(var) {
  case_when(
    var %in% c(-99, -98, -996) ~ -3,
    var %in% c(-94, -92) ~ -8,
    var %in% c(-999, -97, -100) ~ -2,
    TRUE ~ var
  )
}

# Recode missing values for each variable
merged_data <- merged_data %>%
  mutate(
    W1empsmum = recode_missing(W1empsmum),
    W1empsdad = recode_missing(W1empsdad),
    W2empsmum = recode_missing(W2empsmum),
    W2empsdad = recode_missing(W2empsdad),
    W3empsmum = recode_missing(W3empsmum),
    W3empsdad = recode_missing(W3empsdad),
    w4empsmum = recode_missing(w4empsmum),
    w4empsdad = recode_missing(w4empsdad)
  )

# Create labelled factors for each variable
merged_data <- merged_data %>%
  mutate(
    ecoactma14 = factor(W1empsmum, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactpa14 = factor(W1empsdad, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactma15 = factor(W2empsmum, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactpa15 = factor(W2empsdad, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactma16 = factor(W3empsmum, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactpa16 = factor(W3empsdad, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactma17 = factor(w4empsmum, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    )),
    ecoactpa17 = factor(w4empsdad, levels = c(1:9, -9, -8, -7, -3, -2, -1), labels = c(
      "Doing paid work for 30 or more hours a week",
      "Doing paid work for fewer than 30 hours a week",
      "Unemployed/ Looking for a job",
      "On a training course or scheme",
      "In full-time education/ at school",
      "Looking after the family/ household",
      "Retired from work altogether",
      "Sick/ disabled",
      "Other",
      "Refusal",
      "Don't know / insufficient information",
      "Prefer not to say",
      "Not asked at the fieldwork stage / not interviewed",
      "Schedule not applicable / script error / information lost",
      "Item not applicable"
    ))
  )

# Select only the required variables
final_data <- merged_data %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write the output file
write_csv(final_data, "data/output/cleaned_data.csv")