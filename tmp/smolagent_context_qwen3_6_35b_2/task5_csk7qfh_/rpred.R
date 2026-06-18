library(dplyr)
library(readr)
library(labelled)
library(tidyr)
library(purrr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all files
merged <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define labels for collapsed variables (harmonised across all waves)
# Format: c("label" = code, "label" = code, ...)
collapsed_labels <- c(
  "Single" = 1,
  "Married" = 2,
  "Separated" = 3,
  "Divorced" = 4,
  "Widowed" = 5,
  "Civil Partner" = 6,
  "Former Civil Partner" = 7,
  "Surviving Civil Partner" = 8,
  "Item not applicable" = -1,
  "Schedule not applicable / script error" = -2,
  "Not asked at fieldwork" = -3,
  "Prefer not to say" = -7,
  "Don't know / insufficient information" = -8,
  "Refusal" = -9
)

# Create partnr19 from W6MarStatYP (Age 19)
merged <- merged %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -997 ~ -2,
      W6MarStatYP == -97 ~ -7,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      TRUE ~ -3
    )
  )

# Set labels using haven::labelled
merged$partnr19 <- haven::labelled(merged$partnr19, labels = collapsed_labels)

# Create detailed adult variables
merged <- merged %>%
  mutate(
    partnradu25 = W8DMARSTAT,
    partnradu32 = W9DMARSTAT
  )

# Define labels for detailed variables
w8_labels <- c(
  "Single and never married or in a CP" = 1,
  "Married" = 2,
  "Separated but still legally married" = 3,
  "Divorced" = 4,
  "Widowed" = 5,
  "A Civil Partner" = 6,
  "Separated but still legally in a CP" = 7,
  "A former Civil Partner" = 8,
  "A surviving Civil Partner" = 9,
  "Not applicable" = -1,
  "Insufficient information" = -8,
  "Refused" = -9
)

w9_labels <- c(
  "Single that is never married or never in a Civil Partnership" = 1,
  "Married" = 2,
  "Divorced" = 3,
  "Legally separated" = 4,
  "Widowed" = 5,
  "A Civil Partner in a legally recognised Civil Partnership" = 6,
  "A former Civil Partner (where Civil Partnership legally dissolved)" = 7,
  "A surviving Civil Partner (where Civil Partner has died)" = 8,
  "Insufficient information" = -8,
  "Refused" = -9
)

# Convert NAs to -3 for detailed variables
merged <- merged %>%
  mutate(
    partnradu25 = ifelse(is.na(partnradu25), -3, as.numeric(partnradu25)),
    partnradu32 = ifelse(is.na(partnradu32), -3, as.numeric(partnradu32))
  )

# Set labels for detailed variables
merged$partnradu25 <- haven::labelled(merged$partnradu25, labels = w8_labels)
merged$partnradu32 <- haven::labelled(merged$partnradu32, labels = w9_labels)

# Create collapsed variables for ages 25 and 32
merged <- merged %>%
  mutate(
    partnr25 = case_when(
      partnradu25 == 1 ~ 1,
      partnradu25 == 2 ~ 2,
      partnradu25 == 3 ~ 3,
      partnradu25 == 4 ~ 4,
      partnradu25 == 5 ~ 5,
      partnradu25 == 6 ~ 6,
      partnradu25 == 7 ~ 3,
      partnradu25 == 8 ~ 7,
      partnradu25 == 9 ~ 8,
      partnradu25 == -9 ~ -9,
      partnradu25 == -8 ~ -8,
      partnradu25 == -1 ~ -1,
      TRUE ~ -3
    ),
    partnr32 = case_when(
      partnradu32 == 1 ~ 1,
      partnradu32 == 2 ~ 2,
      partnradu32 == 3 ~ 4,
      partnradu32 == 4 ~ 3,
      partnradu32 == 5 ~ 5,
      partnradu32 == 6 ~ 6,
      partnradu32 == 7 ~ 7,
      partnradu32 == 8 ~ 8,
      partnradu32 == -9 ~ -9,
      partnradu32 == -8 ~ -8,
      TRUE ~ -3
    )
  )

# Set labels for collapsed variables
merged$partnr25 <- haven::labelled(merged$partnr25, labels = collapsed_labels)
merged$partnr32 <- haven::labelled(merged$partnr32, labels = collapsed_labels)

# Select only required columns
output <- merged %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")

cat("\nSummary of partnr19:\n")
print(table(output$partnr19, useNA = "ifany"))
cat("\nSummary of partnr25:\n")
print(table(output$partnr25, useNA = "ifany"))
cat("\nSummary of partnr32:\n")
print(table(output$partnr32, useNA = "ifany"))
