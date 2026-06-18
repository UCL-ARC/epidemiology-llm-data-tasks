library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var, wave) {
  case_when(
    var %in% c(-92, -9) ~ -9,  # Refusal
    var %in% c(-1, -8) ~ -8,   # Don't know / insufficient information
    var %in% c(-97, -7) ~ -7,  # Prefer not to say
    var %in% c(-997, -996, -995, -99, -3) ~ -3,  # Not asked / not interviewed / script error
    var %in% c(-91, -2) ~ -1,  # Not applicable
    TRUE ~ var
  )
}

# Process each wave's alcohol consumption variables
wave1$W1alceverYP <- map_missing(wave1$W1alceverYP, 1)
wave2$W2alceverYP <- map_missing(wave2$W2alceverYP, 2)
wave3$W3alceverYP <- map_missing(wave3$W3alceverYP, 3)
wave4$W4AlcEverYP <- map_missing(wave4$W4AlcEverYP, 4)
wave6$W6AlcEverYP <- map_missing(wave6$W6AlcEverYP, 6)
wave7$W7AlcEverYP <- map_missing(wave7$W7AlcEverYP, 7)

# Determine the earliest age at which alcohol was consumed
# Initialize alcfst with 99 (never drunk alcohol)
merged_data$alcfst <- 99

# Check each wave in order (14, 15, 16, 17, 19, 20)
for (i in 1:nrow(merged_data)) {
  nsid <- merged_data$NSID[i]
  
  # Find the row in each wave
  row_wave1 <- which(wave1$NSID == nsid)
  row_wave2 <- which(wave2$NSID == nsid)
  row_wave3 <- which(wave3$NSID == nsid)
  row_wave4 <- which(wave4$NSID == nsid)
  row_wave6 <- which(wave6$NSID == nsid)
  row_wave7 <- which(wave7$NSID == nsid)
  
  # Check each wave in order
  if (length(row_wave1) > 0 && wave1$W1alceverYP[row_wave1] == 1) {
    merged_data$alcfst[i] <- 14
  } else if (length(row_wave2) > 0 && wave2$W2alceverYP[row_wave2] == 1) {
    merged_data$alcfst[i] <- 15
  } else if (length(row_wave3) > 0 && wave3$W3alceverYP[row_wave3] == 1) {
    merged_data$alcfst[i] <- 16
  } else if (length(row_wave4) > 0 && wave4$W4AlcEverYP[row_wave4] == 1) {
    merged_data$alcfst[i] <- 17
  } else if (length(row_wave6) > 0 && wave6$W6AlcEverYP[row_wave6] == 1) {
    merged_data$alcfst[i] <- 19
  } else if (length(row_wave7) > 0 && wave7$W7AlcEverYP[row_wave7] == 1) {
    merged_data$alcfst[i] <- 20
  }
}

# Select only NSID and alcfst for output
output_data <- merged_data %>% select(NSID, alcfst)

# Write the output CSV
write_csv(output_data, "data/output/cleaned_data.csv")

# Return the path to the output file
"data/output/cleaned_data.csv"