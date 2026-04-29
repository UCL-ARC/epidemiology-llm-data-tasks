library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab",
  "ns9_2022_main_interview.tab"
)

# Load each file
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Check if W8AUDIT1 exists in wave8
if("W8AUDIT1" %in% names(wave8)) {
  cat("W8AUDIT1 found in wave8\n")
} else {
  cat("W8AUDIT1 NOT found in wave8\n")
}

# Select variables from each wave
wave1_sel <- wave1 %>% select(NSID, W1alceverYP, W1alcmonYP)
wave2_sel <- wave2 %>% select(NSID, W2alceverYP, W2alcfreqYP)
wave3_sel <- wave3 %>% select(NSID, W3alceverYP, W3alcfreqYP)
wave4_sel <- wave4 %>% select(NSID, W4AlcEverYP, W4AlcFreqYP)
wave6_sel <- wave6 %>% select(NSID, W6AlcEverYP, W6AlcFreqYP)
wave7_sel <- wave7 %>% select(NSID, W7AlcEverYP, W7AlcFreqYP)
wave8_sel <- wave8 %>% select(NSID, W8AUDIT1, W8AUDIT2, W8AUDIT6)
wave9_sel <- wave9 %>% select(NSID, W9AUDIT1, W9AUDIT2, W9AUDIT3)

# Merge all waves
merged <- full_join(wave1_sel, wave2_sel, by = "NSID")
merged <- full_join(merged, wave3_sel, by = "NSID")
merged <- full_join(merged, wave4_sel, by = "NSID")
merged <- full_join(merged, wave6_sel, by = "NSID")
merged <- full_join(merged, wave7_sel, by = "NSID")
merged <- full_join(merged, wave8_sel, by = "NSID")
merged <- full_join(merged, wave9_sel, by = "NSID")

cat("Merged dataset dimensions:", nrow(merged), "rows,", ncol(merged), "columns\n")
cat("Number of NSIDs:", length(unique(merged$NSID)), "\n")

# Create indicator variables for each age
# Age 14: need BOTH W1alceverYP=1 AND W1alcmonYP=1
merged$drinker_14 <- ifelse(merged$W1alceverYP == 1 & merged$W1alcmonYP == 1, 1, 0)

# Age 15: W2alceverYP=1
merged$drinker_15 <- ifelse(merged$W2alceverYP == 1, 1, 0)

# Age 16: W3alceverYP=1
merged$drinker_16 <- ifelse(merged$W3alceverYP == 1, 1, 0)

# Age 17: W4AlcEverYP=1
merged$drinker_17 <- ifelse(merged$W4AlcEverYP == 1, 1, 0)

# Age 19: W6AlcEverYP=1
merged$drinker_19 <- ifelse(merged$W6AlcEverYP == 1, 1, 0)

# Age 20: W7AlcEverYP=1
merged$drinker_20 <- ifelse(merged$W7AlcEverYP == 1, 1, 0)

# Age 25: W8AUDIT1 (frequency) > 1 (not "Never")
# W8AUDIT1 values: 1=Never, 2=Less than monthly, 3=Monthly, 4=Weekly, 5=Daily
merged$drinker_25 <- ifelse(merged$W8AUDIT1 > 1, 1, 0)

# Age 32: W9AUDIT1 (frequency) > 1 (not "Never")
# W9AUDIT1 values: 1=Never, 2=Monthly or less, 3=2-4 times a month, 4=2-3 times a week, 5=4 or more times a week
merged$drinker_32 <- ifelse(merged$W9AUDIT1 > 1, 1, 0)

# Check for missing values in each indicator
cat("\nMissing values in drinker indicators:\n")
cat("Age 14:", sum(is.na(merged$drinker_14)), "\n")
cat("Age 15:", sum(is.na(merged$drinker_15)), "\n")
cat("Age 16:", sum(is.na(merged$drinker_16)), "\n")
cat("Age 17:", sum(is.na(merged$drinker_17)), "\n")
cat("Age 19:", sum(is.na(merged$drinker_19)), "\n")
cat("Age 20:", sum(is.na(merged$drinker_20)), "\n")
cat("Age 25:", sum(is.na(merged$drinker_25)), "\n")
cat("Age 32:", sum(is.na(merged$drinker_32)), "\n")

# Derive alcfst
# For each individual, find the minimum age at which they were identified as a drinker
# If confirmed never drinker across all waves, assign code 99
# Otherwise assign -8

# Check if any sweep shows drinking
merged$any_drinker <- ifelse(
  merged$drinker_14 == 1 | merged$drinker_15 == 1 | merged$drinker_16 == 1 | 
  merged$drinker_17 == 1 | merged$drinker_19 == 1 | merged$drinker_20 == 1 | 
  merged$drinker_25 == 1 | merged$drinker_32 == 1, 1, 0)

# Check if all observed sweep indicators show never drinking and NONE are missing
merged$all_never_no_missing <- ifelse(
  (merged$drinker_14 == 0 | is.na(merged$drinker_14)) &
  (merged$drinker_15 == 0 | is.na(merged$drinker_15)) &
  (merged$drinker_16 == 0 | is.na(merged$drinker_16)) &
  (merged$drinker_17 == 0 | is.na(merged$drinker_17)) &
  (merged$drinker_19 == 0 | is.na(merged$drinker_19)) &
  (merged$drinker_20 == 0 | is.na(merged$drinker_20)) &
  (merged$drinker_25 == 0 | is.na(merged$drinker_25)) &
  (merged$drinker_32 == 0 | is.na(merged$drinker_32)) &
  !is.na(merged$drinker_14) & !is.na(merged$drinker_15) & !is.na(merged$drinker_16) &
  !is.na(merged$drinker_17) & !is.na(merged$drinker_19) & !is.na(merged$drinker_20) &
  !is.na(merged$drinker_25) & !is.na(merged$drinker_32), 1, 0)

# Derive alcfst
merged$alcfst <- case_when(
  merged$any_drinker == 1 ~ pmin(
    ifelse(merged$drinker_14 == 1, 14, NA),
    ifelse(merged$drinker_15 == 1, 15, NA),
    ifelse(merged$drinker_16 == 1, 16, NA),
    ifelse(merged$drinker_17 == 1, 17, NA),
    ifelse(merged$drinker_19 == 1, 19, NA),
    ifelse(merged$drinker_20 == 1, 20, NA),
    ifelse(merged$drinker_25 == 1, 25, NA),
    ifelse(merged$drinker_32 == 1, 32, NA),
    na.rm = TRUE
  ),
  merged$all_never_no_missing == 1 ~ 99,
  TRUE ~ -8
)

# Convert to factor with specified levels
merged$alcfst <- factor(merged$alcfst, 
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information"))

# Output only NSID and alcfst
output <- merged %>% select(NSID, alcfst)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")

# Summary of alcfst distribution
cat("\nDistribution of alcfst:\n")
print(table(output$alcfst, useNA = "ifany"))
