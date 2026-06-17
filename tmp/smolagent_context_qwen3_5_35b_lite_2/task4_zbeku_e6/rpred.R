library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load all files from metadata
# Wave 1 (Age 14) - Young Person Data File
df14 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 4 (Age 17) - Young Person Data File
df17 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 6 (Age 19) - Young Person Data File
df19 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 7 (Age 20) - Young Person Data File
df20 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 8 (Age 25) - Self-Completion Data File
df25 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)

# Wave 9 (Age 32) - Main Interview Data File
df32 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
merged <- full_join(df14, df17, by = "NSID")
merged <- full_join(merged, df19, by = "NSID")
merged <- full_join(merged, df20, by = "NSID")
merged <- full_join(merged, df25, by = "NSID")
merged <- full_join(merged, df32, by = "NSID")

cat("Merged data dimensions:", nrow(merged), "rows,", ncol(merged), "columns\n")

# Create function to convert NA to -3 (Not asked at fieldwork stage / not interviewed)
convert_na_to_missing <- function(x) {
  x[is.na(x)] <- -3
  x
}

# Standardize W6SexualityYP (Age 19)
# -97: Respondent declined self completion -> -2 (schedule not applicable)
# -92: Refused -> -9 (Refusal)
# -91: Not applicable -> -1 (Item not applicable)
# -1: Don't know -> -8 (Don't know)

merged$sori19 <- merged$W6SexualityYP

# Apply standardization
merged$sori19[merged$sori19 == -97] <- -2
merged$sori19[merged$sori19 == -92] <- -9
merged$sori19[merged$sori19 == -91] <- -1
merged$sori19[merged$sori19 == -1] <- -8

# Convert remaining NA to -3
merged$sori19[is.na(merged$sori19)] <- -3

# Standardize W7SexualityYP (Age 20)
# -100: Respondent declined sexual experience questions -> -2 (schedule not applicable)
# -97: Refused self completion -> -9 (Refusal)
# -92: Refused -> -9 (Refusal)
# -91: Not applicable -> -1 (Item not applicable)
# -1: Don't know -> -8 (Don't know)

merged$sori20 <- merged$W7SexualityYP

merged$sori20[merged$sori20 == -100] <- -2
merged$sori20[merged$sori20 == -97] <- -9
merged$sori20[merged$sori20 == -92] <- -9
merged$sori20[merged$sori20 == -91] <- -1
merged$sori20[merged$sori20 == -1] <- -8

# Convert remaining NA to -3
merged$sori20[is.na(merged$sori20)] <- -3

# Standardize W8SEXUALITY (Age 25)
# -9: Refused -> -9 (Refusal)
# -8: Don't know -> -8 (Don't know)
# -1: Not applicable -> -1 (Item not applicable)

merged$sori25 <- merged$W8SEXUALITY

merged$sori25[merged$sori25 == -9] <- -9
merged$sori25[merged$sori25 == -8] <- -8
merged$sori25[merged$sori25 == -1] <- -1

# Convert remaining NA to -3
merged$sori25[is.na(merged$sori25)] <- -3

# Standardize W9SORI (Age 32)
# -9: Refused -> -9 (Refusal)
# -8: Don't know -> -8 (Don't know)
# -3: Not asked at fieldwork stage -> -3 (Not asked at fieldwork stage)
# -1: Not applicable -> -1 (Item not applicable)
# 5: Prefer not to say -> -7 (Prefer not to say)

merged$sori32 <- merged$W9SORI

merged$sori32[merged$sori32 == -9] <- -9
merged$sori32[merged$sori32 == -8] <- -8
merged$sori32[merged$sori32 == -3] <- -3
merged$sori32[merged$sori32 == -1] <- -1
merged$sori32[merged$sori32 == 5] <- -7

# Convert remaining NA to -3
merged$sori32[is.na(merged$sori32)] <- -3

# Create output dataframe with only ID and final derived variables
output <- merged %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Sample of output:\n")
print(head(output, 10))

cat("\nMissing value distribution for each variable:\n")
cat("sori19:", table(output$sori19, useNA = "ifany"), "\n")
cat("sori20:", table(output$sori20, useNA = "ifany"), "\n")
cat("sori25:", table(output$sori25, useNA = "ifany"), "\n")
cat("sori32:", table(output$sori32, useNA = "ifany"), "\n")
