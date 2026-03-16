library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

setwd("data/input")

# Load all files
wave1 <- read_delim("wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("ns9_2022_derived_variables.tab", delim = "\t")

# Merge all files by NSID
combined <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Recode W8DBMI (bmi25)
bmi25_raw <- combined$W8DBMI
bmi25 <- ifelse(is.na(bmi25_raw), -3,
  ifelse(bmi25_raw == -9, -9,
    ifelse(bmi25_raw == -8, -8,
      ifelse(bmi25_raw == -1, -1, -3))))
bmi25 <- as.numeric(bmi25)

# Recode W9DBMI (bmi32)
bmi32_raw <- combined$W9DBMI
bmi32 <- ifelse(is.na(bmi32_raw), -3,
  ifelse(bmi32_raw == -9, -9,
    ifelse(bmi32_raw == -8, -8,
      ifelse(bmi32_raw == -1, -1, -3))))
bmi32 <- as.numeric(bmi32)

# Create final dataset
final_data <- combined %>%
  select(NSID, W8DBMI, W9DBMI) %>%
  rename(bmi25 = W8DBMI, bmi32 = W9DBMI)

final_data$bmi25 <- as.numeric(final_data$bmi25)
final_data$bmi32 <- as.numeric(final_data$bmi32)

# Create output directory
dir.create("../output", showWarnings = FALSE, recursive = TRUE)

# Write output CSV
write_csv(final_data, "../output/cleaned_data.csv")

print("Done")
print(paste("Rows:", nrow(final_data)))
print(paste("Cols:", ncol(final_data)))