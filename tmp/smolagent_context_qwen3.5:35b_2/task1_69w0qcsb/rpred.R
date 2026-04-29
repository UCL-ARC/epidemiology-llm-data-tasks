library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Load all files
load_file <- function(filename) {
  read_delim(paste0("data/input/", filename), delim = "\t", show_col_types = FALSE)
}

# Load datasets
df1 <- load_file(files[1])  # Age 14
df2 <- load_file(files[2])  # Age 15
df3 <- load_file(files[3])  # Age 16
df4 <- load_file(files[4])  # Age 17
df5 <- load_file(files[5])  # Age 18
df6 <- load_file(files[6])  # Age 19
df7 <- load_file(files[7])  # Age 20
df8 <- load_file(files[8])  # Wave 8
df9 <- load_file(files[9])  # Age 32

# Merge all datasets by NSID using full_join
df_merged <- full_join(df1, df2, by = "NSID")
df_merged <- full_join(df_merged, df3, by = "NSID")
df_merged <- full_join(df_merged, df4, by = "NSID")
df_merged <- full_join(df_merged, df5, by = "NSID")
df_merged <- full_join(df_merged, df6, by = "NSID")
df_merged <- full_join(df_merged, df7, by = "NSID")
df_merged <- full_join(df_merged, df8, by = "NSID")
df_merged <- full_join(df_merged, df9, by = "NSID")

# Function to standardize missing value codes
standardize_missing <- function(x) {
  x[x == -999] <- -3  # Not asked
  x[x == -998] <- -3  # Not asked
  x[x == -997] <- -3  # Not asked
  x[x == -995] <- -3  # Not asked
  x[x == -99] <- -3   # Not asked/YP not interviewed
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -9] <- -9    # Refused
  x[x == -8] <- -8    # Don't know
  x[x == -1] <- -1    # Not applicable/Don't know
  x
}

# Standardize sex variables from merged dataframe columns
# Age 14
sex_14 <- standardize_missing(df_merged$W1sexYP)
# Age 15
sex_15 <- standardize_missing(df_merged$W2SexYP)
# Age 16
sex_16 <- standardize_missing(df_merged$W3sexYP)
# Age 17
sex_17 <- standardize_missing(df_merged$W4SexYP)
# Age 18
sex_18 <- standardize_missing(df_merged$W5SexYP)
# Age 19
sex_19 <- standardize_missing(df_merged$W6Sex)
# Age 20
sex_20 <- standardize_missing(df_merged$W7Sex)
# Wave 8
sex_8 <- standardize_missing(df_merged$W8CMSEX)
# Age 32
sex_32 <- standardize_missing(df_merged$W9DSEX)

# Create time-invariant sex variable: prioritize most recent valid response
# Valid values are 1 (Male) and 2 (Female)
# Use if_else with proper NA handling
sex <- rep(NA_real_, length(sex_32))

for (i in seq_along(sex)) {
  # Check from most recent to earliest
  if (!is.na(sex_32[i]) && sex_32[i] %in% c(1, 2)) {
    sex[i] <- sex_32[i]
  } else if (!is.na(sex_8[i]) && sex_8[i] %in% c(1, 2)) {
    sex[i] <- sex_8[i]
  } else if (!is.na(sex_20[i]) && sex_20[i] %in% c(1, 2)) {
    sex[i] <- sex_20[i]
  } else if (!is.na(sex_19[i]) && sex_19[i] %in% c(1, 2)) {
    sex[i] <- sex_19[i]
  } else if (!is.na(sex_18[i]) && sex_18[i] %in% c(1, 2)) {
    sex[i] <- sex_18[i]
  } else if (!is.na(sex_17[i]) && sex_17[i] %in% c(1, 2)) {
    sex[i] <- sex_17[i]
  } else if (!is.na(sex_16[i]) && sex_16[i] %in% c(1, 2)) {
    sex[i] <- sex_16[i]
  } else if (!is.na(sex_15[i]) && sex_15[i] %in% c(1, 2)) {
    sex[i] <- sex_15[i]
  } else if (!is.na(sex_14[i]) && sex_14[i] %in% c(1, 2)) {
    sex[i] <- sex_14[i]
  }
}

# Create factor with labels
sex <- factor(sex, levels = c(1, 2, -9, -8, -1, -3),
              labels = c("Male", "Female", "Refusal", "Don't know", "Not applicable", "Not asked"))

# Create output dataframe with ID and derived variables
output_df <- df_merged %>%
  select(NSID) %>%
  mutate(sex = sex)

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("Cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(output_df), "\n")
cat("Number of variables:", ncol(output_df), "\n")