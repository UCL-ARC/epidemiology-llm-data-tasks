library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, ns8, by = "NSID")
df <- full_join(df, ns9, by = "NSID")

# Convert missing values for W8DBMI (Wave 8, Age 25)
# -9.0 = Refused, -8.0 = Insufficient information, -1.0 = Not applicable
df$W8DBMI <- as.numeric(df$W8DBMI)
df$W8DBMI[df$W8DBMI == -9.0] <- -9
df$W8DBMI[df$W8DBMI == -8.0] <- -8
df$W8DBMI[df$W8DBMI == -1.0] <- -1

# Convert missing values for W9DBMI (Wave 9, Age 32)
# -9.0 = Refused, -8.0 = Insufficient information, -1.0 = Not applicable
df$W9DBMI <- as.numeric(df$W9DBMI)
df$W9DBMI[df$W9DBMI == -9.0] <- -9
df$W9DBMI[df$W9DBMI == -8.0] <- -8
df$W9DBMI[df$W9DBMI == -1.0] <- -1

# Create final BMI variables at ages 25 and 32
# These are continuous variables, so keep as numeric
df$bmi25 <- df$W8DBMI
df$bmi32 <- df$W9DBMI

# Clean up intermediate variables
df <- df %>% select(NSID, bmi25, bmi32)

# Write output
cat("Data dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Summary of bmi25:", summary(df$bmi25), "\n")
cat("Summary of bmi32:", summary(df$bmi32), "\n")

write_csv(df, "data/output/cleaned_data.csv")
cat("\nOutput written to data/output/cleaned_data.csv\n")
