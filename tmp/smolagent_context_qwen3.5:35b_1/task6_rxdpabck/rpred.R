library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing value codes
standard_missing <- c(-9, -8, -1, -3, -2, -7)

# Function to standardize missing value codes
standardize_missing <- function(x) {
  # Map wave-specific codes to standard codes
  x <- case_when(
    x %in% c(-999, -998, -997, -995, -92, -91, -99, -100) ~ -3,
    x %in% c(-94) ~ -8,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    x == -3 ~ -3,
    x == -2 ~ -2,
    x == -7 ~ -7,
    is.na(x) ~ -3,
    TRUE ~ x
  )
  return(x)
}

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

cat("Files loaded successfully\n")
cat("Wave 1:", nrow(wave1), "rows,", ncol(wave1), "cols\n")
cat("Wave 2:", nrow(wave2), "rows,", ncol(wave2), "cols\n")
cat("Wave 3:", nrow(wave3), "rows,", ncol(wave3), "cols\n")
cat("Wave 4:", nrow(wave4), "rows,", ncol(wave4), "cols\n")
cat("NS8:", nrow(ns8), "rows,", ncol(ns8), "cols\n")
cat("NS9 Derived:", nrow(ns9_derived), "rows,", ncol(ns9_derived), "cols\n")
cat("NS9 Main:", nrow(ns9_main), "rows,", ncol(ns9_main), "cols\n")

# Standardize missing values in wave 2 and 3 urbind and gor
wave2$urbind <- standardize_missing(wave2$urbind)
wave2$gor <- standardize_missing(wave2$gor)
wave3$urbind <- standardize_missing(wave3$urbind)
wave3$gor <- standardize_missing(wave3$gor)

# Standardize missing values in ns8 W8DGOR
ns8$W8DGOR <- standardize_missing(ns8$W8DGOR)

# Standardize missing values in ns9_derived W9DRGN
ns9_derived$W9DRGN <- standardize_missing(ns9_derived$W9DRGN)

# Standardize missing values in ns9_main W9NATIONRES
ns9_main$W9NATIONRES <- standardize_missing(ns9_main$W9NATIONRES)

# Merge all datasets using full_join by NSID
cleaned_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

cat("\nMerged dataset:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "cols\n")

# Create derived variables from merged data

# 1. Age-specific urban/rural classification (urbind)
# Wave 2 (age 15) and Wave 3 (age 16) have urbind
# Create collapsed harmonized variable (Urban vs Rural)
create_urb_rural <- function(x) {
  case_when(
    x %in% c(-9, -8, -1, -3, -2, -7) ~ x,
    x %in% c(1, 5) ~ 1,  # Urban >= 10k
    x %in% c(2, 6) ~ 2,  # Town & Fringe
    x %in% c(3, 7) ~ 3,  # Village
    x %in% c(4, 8) ~ 4,  # Hamlet and Isolated Dwelling
    TRUE ~ x
  )
}

# Create age-specific urban/rural variables from merged data
if("urbind" %in% names(cleaned_data)) {
  # Wave 2 urbind is at age 15
  cleaned_data$urb15 <- create_urb_rural(cleaned_data$urbind)
}

# Wave 3 urbind is at age 16
if("urbind" %in% names(cleaned_data)) {
  # We need to identify which urbind is from wave 3
  # Since wave 3 was joined after wave 2, we need to check
  # For now, let's assume the second urbind column or use a different approach
  # Actually, since we're doing full_join, both wave2$urbind and wave3$urbind will be in cleaned_data
  # But they will have the same name "urbind" - the second one will overwrite
  # Let's rename them before merging
}

# Let's redo the merge with renamed columns
wave2_renamed <- wave2 %>% rename(urbind15 = urbind, gor15 = gor)
wave3_renamed <- wave3 %>% rename(urbind16 = urbind, gor16 = gor)

# Standardize missing values in renamed columns
wave2_renamed$urbind15 <- standardize_missing(wave2_renamed$urbind15)
wave2_renamed$gor15 <- standardize_missing(wave2_renamed$gor15)
wave3_renamed$urbind16 <- standardize_missing(wave3_renamed$urbind16)
wave3_renamed$gor16 <- standardize_missing(wave3_renamed$gor16)

# Re-merge with renamed columns
cleaned_data <- wave1 %>%
  full_join(wave2_renamed, by = "NSID") %>%
  full_join(wave3_renamed, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

cat("\nRe-merged dataset:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "cols\n")

# Now create derived variables from renamed columns
if("urbind15" %in% names(cleaned_data)) {
  cleaned_data$urb15 <- create_urb_rural(cleaned_data$urbind15)
}
if("urbind16" %in% names(cleaned_data)) {
  cleaned_data$urb16 <- create_urb_rural(cleaned_data$urbind16)
}

# 2. Age-specific Government Office Region (gor)
# Wave 2 (age 15) and Wave 3 (age 16) have gor (now gor15 and gor16)
# Wave 8 has W8DGOR
# Wave 9 has W9DRGN

clean_gor <- function(x) {
  case_when(
    x %in% c(-9, -8, -1, -3, -2, -7) ~ x,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x == 7 ~ 7,
    x == 8 ~ 8,
    x == 9 ~ 9,
    x == 10 ~ 10,
    x == 11 ~ 11,
    x == 12 ~ 12,
    x == 13 ~ 13,
    TRUE ~ x
  )
}

if("gor15" %in% names(cleaned_data)) {
  cleaned_data$gor15 <- clean_gor(cleaned_data$gor15)
}
if("gor16" %in% names(cleaned_data)) {
  cleaned_data$gor16 <- clean_gor(cleaned_data$gor16)
}
if("W8DGOR" %in% names(cleaned_data)) {
  cleaned_data$gor18 <- clean_gor(cleaned_data$W8DGOR)
}
if("W9DRGN" %in% names(cleaned_data)) {
  cleaned_data$gor32 <- clean_gor(cleaned_data$W9DRGN)
}

# 3. Nation of UK (W9NATIONRES) - age 32
if("W9NATIONRES" %in% names(cleaned_data)) {
  cleaned_data$nat32 <- clean_gor(cleaned_data$W9NATIONRES)
}

# 4. Create UK/abroad indicator from nation of UK
create_uk_abroad <- function(x) {
  case_when(
    x %in% c(-9, -8, -1, -3, -2, -7) ~ x,
    x %in% c(1, 2, 3, 4) ~ 1,  # UK countries
    x == 5 ~ 2,  # Outside UK
    TRUE ~ x
  )
}

if("nat32" %in% names(cleaned_data)) {
  cleaned_data$uk_abroad32 <- create_uk_abroad(cleaned_data$nat32)
}

# Convert key variables to factors with labels
if("urb15" %in% names(cleaned_data)) {
  cleaned_data$urb15 <- factor(cleaned_data$urb15, 
    levels = c(-9, -8, -1, -3, -2, -7, 1, 2, 3, 4),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say", 
               "Urban >= 10k", "Town & Fringe", "Village", "Hamlet & Isolated Dwelling"))
}

if("urb16" %in% names(cleaned_data)) {
  cleaned_data$urb16 <- factor(cleaned_data$urb16,
    levels = c(-9, -8, -1, -3, -2, -7, 1, 2, 3, 4),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "Urban >= 10k", "Town & Fringe", "Village", "Hamlet & Isolated Dwelling"))
}

if("gor15" %in% names(cleaned_data)) {
  cleaned_data$gor15 <- factor(cleaned_data$gor15,
    levels = c(-9, -8, -1, -3, -2, -7, 1:13),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
               "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown"))
}

if("gor16" %in% names(cleaned_data)) {
  cleaned_data$gor16 <- factor(cleaned_data$gor16,
    levels = c(-9, -8, -1, -3, -2, -7, 1:13),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
               "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown"))
}

if("gor18" %in% names(cleaned_data)) {
  cleaned_data$gor18 <- factor(cleaned_data$gor18,
    levels = c(-9, -8, -1, -3, -2, -7, 1:13),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
               "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown"))
}

if("gor32" %in% names(cleaned_data)) {
  cleaned_data$gor32 <- factor(cleaned_data$gor32,
    levels = c(-9, -8, -1, -3, -2, -7, 1:13),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
               "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown"))
}

if("nat32" %in% names(cleaned_data)) {
  cleaned_data$nat32 <- factor(cleaned_data$nat32,
    levels = c(-9, -8, -1, -3, -2, -7, 1:5),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "England", "Scotland", "Wales", "Northern Ireland", "Outside UK or unknown"))
}

if("uk_abroad32" %in% names(cleaned_data)) {
  cleaned_data$uk_abroad32 <- factor(cleaned_data$uk_abroad32,
    levels = c(-9, -8, -1, -3, -2, -7, 1, 2),
    labels = c("Refused", "Don't know", "Not applicable", "Not asked", "Schedule not applicable", "Prefer not to say",
               "UK", "Abroad"))
}

# Select only the final output variables (ID + derived variables)
output_vars <- c("NSID")
if("urb15" %in% names(cleaned_data)) output_vars <- c(output_vars, "urb15")
if("urb16" %in% names(cleaned_data)) output_vars <- c(output_vars, "urb16")
if("gor15" %in% names(cleaned_data)) output_vars <- c(output_vars, "gor15")
if("gor16" %in% names(cleaned_data)) output_vars <- c(output_vars, "gor16")
if("gor18" %in% names(cleaned_data)) output_vars <- c(output_vars, "gor18")
if("gor32" %in% names(cleaned_data)) output_vars <- c(output_vars, "gor32")
if("nat32" %in% names(cleaned_data)) output_vars <- c(output_vars, "nat32")
if("uk_abroad32" %in% names(cleaned_data)) output_vars <- c(output_vars, "uk_abroad32")

final_data <- cleaned_data %>% select(all_of(output_vars))

# Write output
cat("\nWriting output to data/output/cleaned_data.csv\n")
write_csv(final_data, "data/output/cleaned_data.csv")

cat("\nFinal dataset:", nrow(final_data), "rows,", ncol(final_data), "cols\n")
cat("Variables:", paste(output_vars, collapse = ", "), "\n")
cat("\nDone!\n")
