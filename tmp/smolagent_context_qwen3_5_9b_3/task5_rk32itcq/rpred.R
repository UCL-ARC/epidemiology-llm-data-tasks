library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

options(warn = -1)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Merge all datasets into one full data frame
full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

cat("Total rows after merge:", nrow(full_data), "\n")

# Create partnr19 from W6MarStatYP (age 19)
# Categories: 1=Single, 2=Married, 3=Separated, 4=Divorced, 5=Widowed
# Missing values: -997 to -1 become -3
full_data <- full_data %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP %in% c(1:5) ~ W6MarStatYP,
      TRUE ~ -3
    )
  )

# Create partnradu25 from W8DMARSTAT (age 25, detailed)
# Categories: 1=Single/CP, 2=Married, 3=Separated legally married, 4=Divorced, 5=Widowed, 6=Civil Partner, 7=Separated legally CP, 8=Former CP, 9=Surviving CP
# Missing: -9, -8, -1 become -3
full_data <- full_data %>%
  mutate(
    partnradu25 = case_when(
      W8DMARSTAT %in% c(1:9) ~ W8DMARSTAT,
      TRUE ~ -3
    )
  )

# Create partnr25 by collapsing partnradu25
full_data <- full_data %>%
  mutate(
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 4,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 1,
      W8DMARSTAT == 7 ~ 3,
      W8DMARSTAT == 8 ~ -8,
      W8DMARSTAT == 9 ~ -8,
      TRUE ~ -3
    )
  )

# Create partnradu32 from W9DMARSTAT (age 32, detailed)
# Categories: 1=Single/res never in CP, 2=Married, 3=Divorced, 4=Legally separated, 5=Widowed, 6=CP, 7=Former CP (dissolved), 8=Surviving CP
# Missing: -8, -9 become -3
full_data <- full_data %>%
  mutate(
    partnradu32 = case_when(
      W9DMARSTAT %in% c(1:8) ~ W9DMARSTAT,
      TRUE ~ -3
    )
  )

# Create partnr32 by collapsing partnradu32
full_data <- full_data %>%
  mutate(
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 4,
      W9DMARSTAT == 4 ~ 3,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 1,
      W9DMARSTAT == 7 ~ -8,
      W9DMARSTAT == 8 ~ -8,
      TRUE ~ -3
    )
  )

# Convert any remaining NA to -3
full_data$partnr19[is.na(full_data$partnr19)] <- -3
full_data$partnr25[is.na(full_data$partnr25)] <- -3
full_data$partnr32[is.na(full_data$partnr32)] <- -3
full_data$partnradu25[is.na(full_data$partnradu25)] <- -3
full_data$partnradu32[is.na(full_data$partnradu32)] <- -3

# Create labelled factors
full_data <- full_data %>%
  mutate(
    partnr19 = factor(partnr19, levels = c(1, 2, 3, 4, 5, -3),
                      labels = c("Single", "Married", "Separated", "Divorced", "Widowed", "Missing")),
    partnr25 = factor(partnr25, levels = c(1, 2, 3, 4, 5, -3),
                      labels = c("Single", "Married", "Separated", "Divorced", "Widowed", "Missing")),
    partnr32 = factor(partnr32, levels = c(1, 2, 3, 4, 5, -3),
                      labels = c("Single", "Married", "Separated", "Divorced", "Widowed", "Missing")),
    partnradu25 = factor(partnradu25, levels = c(1:9, -3),
                         labels = c("Single/CP", "Married", "Separated legally married", "Divorced", "Widowed", "Civil Partner", "Separated legally CP", "Former CP", "Surviving CP", "Missing")),
    partnradu32 = factor(partnradu32, levels = c(1:8, -3),
                         labels = c("Single", "Married", "Divorced", "Separated", "Widowed", "Civil Partner", "Former CP", "Surviving CP", "Missing"))
  )

# Select only the final derived variables
final_data <- full_data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat("File written successfully\n")
cat("Rows:", nrow(final_data), "\n")
cat("Columns:", names(final_data), "\n")
