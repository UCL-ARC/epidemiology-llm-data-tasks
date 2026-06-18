
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Load and merge data files
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets
merged_data <- wave6 %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Create output directory
if(!dir.exists("data/output")) {
  dir.create("data/output")
}

# Process sexual orientation variables
# Create sori19
merged_data$sori19 <- as.character(merged_data$W6SexualityYP)
merged_data$sori19[is.na(merged_data$sori19)] <- "-3"
merged_data$sori19 <- factor(merged_data$sori19,
                           levels = c("-3", "-9", "-8", "-2", "-1", "1", "2", "3", "4", "5"),
                           labels = c("Not asked at fieldwork stage", "Refused", "Don't know", "Schedule not applicable", "Not applicable", "Heterosexual", "Gay/Lesbian", "Bisexual", "Other", "Prefer not to say"))

# Create sori20
merged_data$sori20 <- as.character(merged_data$W7SexualityYP)
merged_data$sori20[is.na(merged_data$sori20)] <- "-3"
merged_data$sori20 <- factor(merged_data$sori20,
                           levels = c("-3", "-9", "-8", "-2", "-1", "1", "2", "3", "4", "5"),
                           labels = c("Not asked at fieldwork stage", "Refused", "Don't know", "Schedule not applicable", "Not applicable", "Heterosexual", "Gay/Lesbian", "Bisexual", "Other", "Prefer not to say"))

# Create sori25
merged_data$sori25 <- as.character(merged_data$W8SEXUALITY)
merged_data$sori25[is.na(merged_data$sori25)] <- "-3"
merged_data$sori25 <- factor(merged_data$sori25,
                           levels = c("-3", "-9", "-8", "-2", "-1", "1", "2", "3", "4", "5"),
                           labels = c("Not asked at fieldwork stage", "Refused", "Don't know", "Schedule not applicable", "Not applicable", "Heterosexual", "Gay/Lesbian", "Bisexual", "Other", "Prefer not to say"))

# Create sori32
merged_data$sori32 <- as.character(merged_data$W9SORI)
merged_data$sori32[is.na(merged_data$sori32)] <- "-3"
merged_data$sori32 <- factor(merged_data$sori32,
                           levels = c("-3", "-9", "-8", "-2", "-1", "1", "2", "3", "4", "5"),
                           labels = c("Not asked at fieldwork stage", "Refused", "Don't know", "Schedule not applicable", "Not applicable", "Heterosexual", "Gay/Lesbian", "Bisexual", "Other", "Prefer not to say"))

# Select only required variables
final_data <- merged_data[, c("NSID", "sori19", "sori20", "sori25", "sori32")]

# Write output file
write_csv(final_data, "data/output/cleaned_data.csv")
