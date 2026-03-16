library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Select and rename IMDRSCORE from wave_two (age 15) and wave_three (age 16)
wave_two_imd <- wave_two %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15 = IMDRSCORE)

wave_three_imd <- wave_three %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16 = IMDRSCORE)

# Select only W9DIMDD from ns9_derived (age 32)
ns9_imd <- ns9_derived %>%
  select(NSID, W9DIMDD) %>%
  rename(imd32 = W9DIMDD)

# Merge all files by NSID using full_join
df_merged <- full_join(wave_two_imd, wave_three_imd, by = "NSID")
df_merged <- full_join(df_merged, ns9_imd, by = "NSID")

# Check for missing value codes
cat("Missing value codes in imd15 (wave_two):\n")
print(table(wave_two_imd$imd15, useNA = "ifany"))
cat("\nMissing value codes in imd16 (wave_three):\n")
print(table(wave_three_imd$imd16, useNA = "ifany"))
cat("\nMissing value codes in imd32 (ns9_derived):\n")
print(table(ns9_imd$imd32, useNA = "ifany"))

# Recode missing values to standard scheme
# -94 to -8 (Don't know / insufficient information)
# NA to -3 (Not asked / not interviewed)

df_cleaned <- df_merged %>%
  mutate(
    imd15 = case_when(
      imd15 == -94 ~ -8,
      is.na(imd15) ~ -3,
      TRUE ~ imd15
    ),
    imd16 = case_when(
      imd16 == -94 ~ -8,
      is.na(imd16) ~ -3,
      TRUE ~ imd16
    ),
    imd32 = case_when(
      imd32 == -8 ~ -8,
      is.na(imd32) ~ -3,
      TRUE ~ imd32
    )
  )

# Apply labels using labelled::set_value_labels
df_final <- df_cleaned %>%
  mutate(
    imd15 = labelled::set_value_labels(imd15, c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1)),
    imd16 = labelled::set_value_labels(imd16, c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1)),
    imd32 = labelled::set_value_labels(imd32, c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked at the fieldwork stage / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1))
  )

# Select only required variables
df_output <- df_final %>%
  select(NSID, imd15, imd16, imd32)

# Write output using write.csv with row.names = FALSE
write.csv(df_output, "data/output/cleaned_data.csv", row.names = FALSE)

cat("\nOutput file created successfully!\n")
cat("\nOutput structure:\n")
print(str(df_output))
cat("\nFirst few rows:\n")
print(head(df_output))
cat("\nLast few rows:\n")
print(tail(df_output))