# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(wave1, wave4, by = "NSID")
merged <- full_join(merged, wave6, by = "NSID")
merged <- full_join(merged, wave7, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Create sori19 from W6SexualityYP (wave 6, age 19)
# Map missing values: -97 -> -9, -92 -> -9, -91 -> -1, -1 -> -8, -999 -> -2
sori19 <- merged$W6SexualityYP
sori19[sori19 == -97] <- -9
sori19[sori19 == -92] <- -9
sori19[sori19 == -91] <- -1
sori19[sori19 == -1] <- -8
sori19[sori19 == -999] <- -2

# Create sori20 from W7SexualityYP (wave 7, age 20)
# Map missing values: -100 -> -9, -97 -> -9, -92 -> -9, -91 -> -1, -1 -> -8
sori20 <- merged$W7SexualityYP
sori20[sori20 == -100] <- -9
sori20[sori20 == -97] <- -9
sori20[sori20 == -92] <- -9
sori20[sori20 == -91] <- -1
sori20[sori20 == -1] <- -8

# Create sori25 from W8SEXUALITY (wave 8, age 25)
# Map missing values: -9 -> -9, -8 -> -8, -1 -> -1 (already match standard)
sori25 <- merged$W8SEXUALITY

# Create sori32 from W9SORI (wave 9, age 32)
# Map missing values: -9 -> -9, -8 -> -8, -3 -> -3, -1 -> -1
# Map 5 (Prefer not to say) to -7
sori32 <- merged$W9SORI
sori32[sori32 == 5] <- -7

# Convert to numeric (handling any factors)
sori19 <- as.numeric(as.character(sori19))
sori20 <- as.numeric(as.character(sori20))
sori25 <- as.numeric(as.character(sori25))
sori32 <- as.numeric(as.character(sori32))

# Create final output dataframe with NSID and the four sori variables
output <- tibble(
  NSID = merged$NSID,
  sori19 = sori19,
  sori20 = sori20,
  sori25 = sori25,
  sori32 = sori32
)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
cat("Output created successfully. Dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("NSID count:", length(unique(output$NSID)), "\n")
cat("Non-NA counts - sori19:", sum(!is.na(sori19)), "sori20:", sum(!is.na(sori20)), "sori25:", sum(!is.na(sori25)), "sori32:", sum(!is.na(sori32)), "\n")
