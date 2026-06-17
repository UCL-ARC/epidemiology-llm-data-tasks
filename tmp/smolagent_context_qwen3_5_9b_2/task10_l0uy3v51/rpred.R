library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Create a master dataset
cohort <- full_join(wave1, wave4, by = "NSID")
cohort <- full_join(cohort, wave5, by = "NSID")
cohort <- full_join(cohort, wave6, by = "NSID")
cohort <- full_join(cohort, wave7, by = "NSID")
cohort <- full_join(cohort, wave8, by = "NSID")
cohort <- full_join(cohort, wave9, by = "NSID")

# Define value label mappings for collapsed 6-category scheme
# Wave 4 (Age 17)
map_w4_labels <- c(
  "-999" = -3,
  "-94" = -3,
  "-92" = -3,
  "-91" = -3,
  "1" = 1,
  "2" = 1,
  "3" = 4,
  "4" = 2,
  "5" = 3,
  "6" = 5,
  "7" = 6,
  "8" = 6,
  "9" = 6
)

map_w5_labels <- c(
  "-94" = -3,
  "1" = 2,
  "2" = 1,
  "3" = 1,
  "4" = 3,
  "5" = 2,
  "6" = 2,
  "7" = 4,
  "8" = 5,
  "9" = 6,
  "10" = 6,
  "11" = 6
)

map_w6_labels <- c(
  "-91" = -3,
  "1" = 3,
  "2" = 3,
  "3" = 1,
  "4" = 2,
  "5" = 2,
  "6" = 6,
  "7" = 5,
  "8" = 4,
  "9" = 6,
  "10" = 1,
  "11" = 6
)

map_w7_labels <- c(
  "-91" = -3,
  "1" = 3,
  "2" = 3,
  "3" = 1,
  "4" = 2,
  "5" = 2,
  "6" = 6,
  "7" = 5,
  "8" = 4,
  "9" = 1,
  "10" = 6,
  "11" = 2,
  "12" = 6,
  "13" = 6,
  "14" = 6,
  "15" = 6
)

map_w8_labels <- c(
  "-9" = -3,
  "-8" = -3,
  "-1" = -3,
  "1" = 1,
  "2" = 1,
  "3" = 6,
  "4" = 4,
  "5" = 3,
  "6" = 2,
  "7" = 2,
  "8" = 6,
  "9" = 5,
  "10" = 6
)

map_w9_labels <- c(
  "-9" = -3,
  "-8" = -3,
  "-1" = -3,
  "1" = 1,
  "2" = 1,
  "3" = 6,
  "4" = 4,
  "5" = 3,
  "6" = 2,
  "7" = 2,
  "8" = 6,
  "9" = 5,
  "10" = 6
)

# Function to apply label mapping
apply_labels <- function(val, labels) {
  result <- as.numeric(as.character(val))
  result <- result[!is.na(result)]
  result <- labels[as.character(result[!is.na(result)])]
  result[!is.na(result)]
  result
}

# Apply mappings
col17_raw <- cohort$W4empsYP
ecoact17 <- as.character(col17_raw)
ecoact17 <- map_w4_labels[ecoact17]
ecoact17 <- as.numeric(ecoact17)
ecoact17[is.na(ecoact17) | ecoact17 %in% c(-3, -4, -5, -6)] <- -3
ecoact17[ecoact17 > 6] <- -3  # Catch any unmapped values
ecoact17[is.na(ecoact17)] <- -3

ecoact18_raw <- cohort$W5mainactYP
ecoact18 <- as.character(ecoact18_raw)
ecoact18 <- map_w5_labels[ecoact18]
ecoact18 <- as.numeric(ecoact18)
ecoact18[is.na(ecoact18) | ecoact18 %in% c(-3, -4, -5, -6)] <- -3
ecoact18[ecoact18 > 6] <- -3
ecoact18[is.na(ecoact18)] <- -3

ecoact19_raw <- cohort$W6TCurrentAct
ecoact19 <- as.character(ecoact19_raw)
ecoact19 <- map_w6_labels[ecoact19]
ecoact19 <- as.numeric(ecoact19)
ecoact19[is.na(ecoact19) | ecoact19 %in% c(-3, -4, -5, -6)] <- -3
ecoact19[ecoact19 > 6] <- -3
ecoact19[is.na(ecoact19)] <- -3

ecoact20_raw <- cohort$W7TCurrentAct
ecoact20 <- as.character(ecoact20_raw)
ecoact20 <- map_w7_labels[ecoact20]
ecoact20 <- as.numeric(ecoact20)
ecoact20[is.na(ecoact20) | ecoact20 %in% c(-3, -4, -5, -6)] <- -3
ecoact20[ecoact20 > 6] <- -3
ecoact20[is.na(ecoact20)] <- -3

ecoact25_raw <- cohort$W8DACTIVITYC
ecoact25 <- as.character(ecoact25_raw)
ecoact25 <- map_w8_labels[ecoact25]
ecoact25 <- as.numeric(ecoact25)
ecoact25[is.na(ecoact25) | ecoact25 %in% c(-3, -4, -5, -6)] <- -3
ecoact25[ecoact25 > 6] <- -3
ecoact25[is.na(ecoact25)] <- -3

ecoact32_raw <- cohort$W9DACTIVITYC
ecoact32 <- as.character(ecoact32_raw)
ecoact32 <- map_w9_labels[ecoact32]
ecoact32 <- as.numeric(ecoact32)
ecoact32[is.na(ecoact32) | ecoact32 %in% c(-3, -4, -5, -6)] <- -3
ecoact32[ecoact32 > 6] <- -3
ecoact32[is.na(ecoact32)] <- -3

# Create detailed variables
ecoactadu25 <- cohort$W8DACTIVITYC
ecoactadu32 <- cohort$W9DACTIVITYC

cohort <- cohort %>%
  mutate(
    ecoact17 = ecoact17,
    ecoact18 = ecoact18,
    ecoact19 = ecoact19,
    ecoact20 = ecoact20,
    ecoact25 = ecoact25,
    ecoact32 = ecoact32,
    ecoactadu25 = ecoactadu25,
    ecoactadu32 = ecoactadu32
  ) %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(cohort, "data/output/cleaned_data.csv")

print("Script completed successfully")