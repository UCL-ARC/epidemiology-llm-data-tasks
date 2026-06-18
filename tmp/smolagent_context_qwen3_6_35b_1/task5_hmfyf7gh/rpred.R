library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files
df1 <- read_delim(file.path("data/input", files[[1]]), delim = "\t", show_col_types = FALSE)
df4 <- read_delim(file.path("data/input", files[[2]]), delim = "\t", show_col_types = FALSE)
df6 <- read_delim(file.path("data/input", files[[3]]), delim = "\t", show_col_types = FALSE)
df8 <- read_delim(file.path("data/input", files[[4]]), delim = "\t", show_col_types = FALSE)
df9 <- read_delim(file.path("data/input", files[[5]]), delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
df_merged <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

cat("Merged dataset dimensions:", nrow(df_merged), "rows,", ncol(df_merged), "cols\n")

# ============================================================
# STEP 1: Create partnr19 from W6MarStatYP (Wave 6, Age 19)
# Collapsed harmonised categories
# ============================================================

df_merged <- df_merged %>%
  mutate(
    partnr19 = W6MarStatYP,
    # Map missing values for W6MarStatYP
    partnr19 = case_when(
      partnr19 == -997 ~ -2,    # Script error
      partnr19 == -97 ~ -7,     # Respondent declined self completion -> Prefer not to say
      partnr19 == -92 ~ -9,     # Refused
      partnr19 == -91 ~ -1,     # Not applicable
      partnr19 == -1 ~ -8,      # Don't know
      is.na(partnr19) ~ -3,     # Not asked / not interviewed
      TRUE ~ partnr19
    )
  )

cat("partnr19 created\n")

# ============================================================
# STEP 2: Create detailed adult variables from Wave 8 (Age 25)
# partnradu25 from W8DMARSTAT
# ============================================================

df_merged <- df_merged %>%
  mutate(
    partnradu25 = W8DMARSTAT,
    # Map missing values for W8DMARSTAT
    partnradu25 = case_when(
      partnradu25 == -9 ~ -9,   # Refused
      partnradu25 == -8 ~ -8,   # Insufficient information
      partnradu25 == -1 ~ -1,   # Not applicable
      is.na(partnradu25) ~ -3,  # Not asked / not interviewed
      TRUE ~ partnradu25
    )
  )

cat("partnradu25 created (detailed)\n")

# ============================================================
# STEP 3: Create collapsed partnr25 from partnradu25
# Collapsed harmonised categories for Wave 8 (Age 25)
# ============================================================

df_merged <- df_merged %>%
  mutate(
    partnr25 = case_when(
      partnradu25 == 1 ~ 1,     # Single and never married or in a CP
      partnradu25 == 2 ~ 2,     # Married
      partnradu25 == 3 ~ 3,     # Separated but still legally married
      partnradu25 == 4 ~ 4,     # Divorced
      partnradu25 == 5 ~ 5,     # Widowed
      partnradu25 == 6 ~ 2,     # A Civil Partner -> Married equivalent
      partnradu25 == 7 ~ 3,     # Separated but still legally in a CP -> Separated
      partnradu25 == 8 ~ 4,     # A former Civil Partner -> Divorced
      partnradu25 == 9 ~ 5,     # A surviving Civil Partner -> Widowed
      partnradu25 == -1 ~ -1,   # Not applicable
      partnradu25 == -8 ~ -8,   # Insufficient information
      partnradu25 == -9 ~ -9,   # Refused
      partnradu25 == -3 ~ -3,   # Not asked
      TRUE ~ partnradu25        # Keep other values as-is
    )
  )

cat("partnr25 created (collapsed)\n")

# ============================================================
# STEP 4: Create detailed adult variables from Wave 9 (Age 32)
# partnradu32 from W9DMARSTAT
# ============================================================

df_merged <- df_merged %>%
  mutate(
    partnradu32 = W9DMARSTAT,
    # Map missing values for W9DMARSTAT
    partnradu32 = case_when(
      partnradu32 == -9 ~ -9,   # Refused
      partnradu32 == -8 ~ -8,   # Insufficient information
      is.na(partnradu32) ~ -3,  # Not asked / not interviewed
      TRUE ~ partnradu32
    )
  )

cat("partnradu32 created (detailed)\n")

# ============================================================
# STEP 5: Create collapsed partnr32 from partnradu32
# Collapsed harmonised categories for Wave 9 (Age 32)
# ============================================================

df_merged <- df_merged %>%
  mutate(
    partnr32 = case_when(
      partnradu32 == 1 ~ 1,     # Single that is never married or never in a Civil Partnership
      partnradu32 == 2 ~ 2,     # Married
      partnradu32 == 3 ~ 4,     # Divorced
      partnradu32 == 4 ~ 3,     # Legally separated -> Separated
      partnradu32 == 5 ~ 5,     # Widowed
      partnradu32 == 6 ~ 2,     # A Civil Partner -> Married
      partnradu32 == 7 ~ 4,     # A former Civil Partner -> Divorced
      partnradu32 == 8 ~ 5,     # A surviving Civil Partner -> Widowed
      partnradu32 == -8 ~ -8,   # Insufficient information
      partnradu32 == -9 ~ -9,   # Refused
      partnradu32 == -3 ~ -3,   # Not asked
      TRUE ~ partnradu32        # Keep other values as-is
    )
  )

cat("partnr32 created (collapsed)\n")

# ============================================================
# STEP 6: Add value labels to all derived variables
# ============================================================

# Define common collapsed category labels
collapsed_labels <- setNames(c("Single", "Married", "Separated", "Divorced", "Widowed",
                               "Not applicable", "Schedule not applicable / script error",
                               "Not asked", "Prefer not to say",
                               "Insufficient information", "Refused"),
                             c(1, 2, 3, 4, 5, -1, -2, -3, -7, -8, -9))

# Detailed adult variable labels for Wave 8 (Age 25)
detailed_labels_25 <- setNames(c("Single and never married or in a CP",
                                 "Married", "Separated but still legally married",
                                 "Divorced", "Widowed",
                                 "A Civil Partner", "Separated but still legally in a CP",
                                 "A former Civil Partner", "A surviving Civil Partner",
                                 "Not applicable", "Insufficient information", "Refused"),
                               c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -8, -9))

# Detailed adult variable labels for Wave 9 (Age 32)
detailed_labels_32 <- setNames(c("Single that is never married or never in a Civil Partnership",
                                 "Married", "Divorced", "Legally separated", "Widowed",
                                 "A Civil Partner in a legally recognised Civil Partnership",
                                 "A former Civil Partner (where Civil Partnership legally dissolved)",
                                 "A surviving Civil Partner (where Civil Partner has died)",
                                 "Insufficient information", "Refused"),
                               c(1, 2, 3, 4, 5, 6, 7, 8, -8, -9))

# Apply labels directly using attr()
attr(df_merged$partnr19, "labels") <- collapsed_labels
class(df_merged$partnr19) <- c("labelled", class(df_merged$partnr19))

attr(df_merged$partnr25, "labels") <- collapsed_labels
class(df_merged$partnr25) <- c("labelled", class(df_merged$partnr25))

attr(df_merged$partnr32, "labels") <- collapsed_labels
class(df_merged$partnr32) <- c("labelled", class(df_merged$partnr32))

attr(df_merged$partnradu25, "labels") <- detailed_labels_25
class(df_merged$partnradu25) <- c("labelled", class(df_merged$partnradu25))

attr(df_merged$partnradu32, "labels") <- detailed_labels_32
class(df_merged$partnradu32) <- c("labelled", class(df_merged$partnradu32))

cat("Labels applied\n")

# ============================================================
# STEP 7: Keep only NSID and derived variables
# ============================================================

df_output <- df_merged %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

cat("Output dataframe dimensions:", nrow(df_output), "rows,", ncol(df_output), "cols\n")

# ============================================================
# STEP 8: Write to CSV
# ============================================================

write_csv(df_output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")

# Print summary statistics
cat("\n=== Summary Statistics ===\n")
cat("\npartnr19 (Wave 6, Age 19):\n")
print(table(df_output$partnr19, useNA = "always"))

cat("\npartnradu25 (Wave 8, Age 25, detailed):\n")
print(table(df_output$partnradu25, useNA = "always"))

cat("\npartnr25 (Wave 8, Age 25, collapsed):\n")
print(table(df_output$partnr25, useNA = "always"))

cat("\npartnradu32 (Wave 9, Age 32, detailed):\n")
print(table(df_output$partnradu32, useNA = "always"))

cat("\npartnr32 (Wave 9, Age 32, collapsed):\n")
print(table(df_output$partnr32, useNA = "always"))
