library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Extract relevant variables from each dataset before merging
# Wave 2 (Age 15): IMDRSCORE -> imd15
df_w2 <- wave_two %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd15 = IMDRSCORE)

# Wave 3 (Age 16): IMDRSCORE -> imd16
df_w3 <- wave_three %>%
  select(NSID, IMDRSCORE) %>%
  rename(imd16 = IMDRSCORE)

# Wave 9 (Age 32): W9DIMDD -> imd32
df_w9 <- ns9_derived %>%
  select(NSID, W9DIMDD) %>%
  rename(imd32 = W9DIMDD)

# Merge all datasets by NSID using full_join
df_clean <- full_join(df_w2, df_w3, by = "NSID")
df_clean <- full_join(df_clean, df_w9, by = "NSID")

# Recode missing value codes
# -94 to -8 (Don't know / insufficient information)
df_clean <- df_clean %>%
  mutate(
    imd15 = ifelse(imd15 == -94, -8, imd15),
    imd16 = ifelse(imd16 == -94, -8, imd16),
    imd32 = ifelse(imd32 == -8, -8, imd32)
  )

# Recode NA to -3 (Not asked at the fieldwork stage / not interviewed)
df_clean <- df_clean %>%
  mutate(
    imd15 = ifelse(is.na(imd15), -3, imd15),
    imd16 = ifelse(is.na(imd16), -3, imd16),
    imd32 = ifelse(is.na(imd32), -3, imd32)
  )

# Convert to labelled numeric variables with labels for missing codes
# Create labels as a named numeric vector where names are labels and values are codes
missing_labels <- c(`Refusal` = -9, `Don't know / insufficient information` = -8, `Not asked at the fieldwork stage / not interviewed` = -3, `Script error / information lost` = -2, `Not applicable` = -1)

# Apply labels using labelled() - labels should be a named numeric vector
df_clean$imd15 <- labelled(df_clean$imd15, labels = missing_labels)
df_clean$imd16 <- labelled(df_clean$imd16, labels = missing_labels)
df_clean$imd32 <- labelled(df_clean$imd32, labels = missing_labels)

# Select only required variables
df_final <- df_clean %>%
  select(NSID, imd15, imd16, imd32)

# Write output to CSV - write_csv doesn't have row_names argument
colnames(df_final) <- c("NSID", "imd15", "imd16", "imd32")
write_csv(df_final, "data/output/cleaned_data.csv")

cat("Cleaned data saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(df_final), "\n")
cat("Variables:", names(df_final), "\n")