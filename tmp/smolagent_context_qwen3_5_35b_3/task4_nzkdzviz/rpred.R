library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets using full_join by NSID
cleaned <- full_join(wave1, wave4, by = "NSID")
cleaned <- full_join(cleaned, wave6, by = "NSID")
cleaned <- full_join(cleaned, wave7, by = "NSID")
cleaned <- full_join(cleaned, ns8, by = "NSID")
cleaned <- full_join(cleaned, ns9, by = "NSID")

# Derive sori19 from W6SexualityYP (Age 19, Wave 6)
# Map: -97 to -9 (Refusal), -92 to -9 (Refusal), -91 to -1 (Not applicable), -1 to -8 (Don't know)
# Also convert NA to -3 (Not asked)
sori19 <- cleaned$W6SexualityYP
sori19[sori19 == -97 | sori19 == -92] <- -9
sori19[sori19 == -91] <- -1
sori19[sori19 == -1] <- -8
sori19[is.na(sori19)] <- -3

# Derive sori20 from W7SexualityYP (Age 20, Wave 7)
# Map: -100 to -9 (Refusal), -97 to -9 (Refusal), -92 to -9 (Refusal), -91 to -1 (Not applicable), -1 to -8 (Don't know)
sori20 <- cleaned$W7SexualityYP
sori20[sori20 == -100 | sori20 == -97 | sori20 == -92] <- -9
sori20[sori20 == -91] <- -1
sori20[sori20 == -1] <- -8
sori20[is.na(sori20)] <- -3

# Derive sori25 from W8SEXUALITY (Age 25, Wave 8)
# -9 to -9 (Refusal), -8 to -8 (Don't know), -1 to -1 (Not applicable)
sori25 <- cleaned$W8SEXUALITY
sori25[is.na(sori25)] <- -3

# Derive sori32 from W9SORI (Age 32, Wave 9)
# -9 to -9 (Refusal), -8 to -8 (Don't know), -3 to -3 (Not asked), -1 to -1 (Not applicable), 5 to -7 (Prefer not to say)
sori32 <- cleaned$W9SORI
sori32[sori32 == 5] <- -7
sori32[is.na(sori32)] <- -3

# Create output dataframe
output <- data.frame(
  NSID = cleaned$NSID,
  sori19 = sori19,
  sori20 = sori20,
  sori25 = sori25,
  sori32 = sori32
)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

print("Cleaned data saved successfully!")
print(paste("Number of rows:", nrow(output)))
print("First 10 rows:")
head(output, 10)
print("Summary statistics:")
colSums(is.na(output[, -1]))
