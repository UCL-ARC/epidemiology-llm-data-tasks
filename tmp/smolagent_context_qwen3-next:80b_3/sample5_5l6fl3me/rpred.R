library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Full join all waves by NSID
data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Harmonize W6MarStatYP (Age 19) missing codes to standard
# -997 -> -2 (Script error), -97 -> -3 (Declined), -92 -> -9 (Refused), -91 -> -1 (Not applicable), -1 -> -8 (Don't know)
data <- data %>%
  mutate(
    W6MarStatYP_clean = case_when(
      W6MarStatYP == -997 ~ -2,
      W6MarStatYP == -97 ~ -3,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      W6MarStatYP %in% c(1, 2, 3, 4, 5) ~ W6MarStatYP,
      is.na(W6MarStatYP) ~ -3,
      TRUE ~ W6MarStatYP
    )
  )

# Harmonize W8DMARSTAT missing codes (already uses standard -9, -8, -1)
data <- data %>%
  mutate(
    W8DMARSTAT_clean = case_when(
      W8DMARSTAT %in% c(-9, -8, -1) ~ W8DMARSTAT,
      W8DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ W8DMARSTAT,
      is.na(W8DMARSTAT) ~ -3,
      TRUE ~ W8DMARSTAT
    )
  )

# Harmonize W9DMARSTAT missing codes (already uses standard -9, -8)
data <- data %>%
  mutate(
    W9DMARSTAT_clean = case_when(
      W9DMARSTAT %in% c(-9, -8) ~ W9DMARSTAT,
      W9DMARSTAT %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ W9DMARSTAT,
      is.na(W9DMARSTAT) ~ -3,
      TRUE ~ W9DMARSTAT
    )
  )

# Create collapsed harmonized marital status variable (partnr)
# Map all waves to common categories: 1=Single, 2=Married, 3=Separated, 4=Divorced, 5=Widowed, 6=Civil Partner
data <- data %>%
  mutate(
    # Collapsed marital status at age 19
    partnr19 = case_when(
      W6MarStatYP_clean %in% c(-9, -8, -3, -2, -1, -7) ~ W6MarStatYP_clean,
      W6MarStatYP_clean == 1 ~ 1,  # Single
      W6MarStatYP_clean == 2 ~ 2,  # Married
      W6MarStatYP_clean == 3 ~ 3,  # Separated
      W6MarStatYP_clean == 4 ~ 4,  # Divorced
      W6MarStatYP_clean == 5 ~ 5,  # Widowed
      TRUE ~ -3
    ),
    
    # Collapsed marital status at age ~25 (Wave 8)
    partnr25 = case_when(
      W8DMARSTAT_clean %in% c(-9, -8, -3, -2, -1, -7) ~ W8DMARSTAT_clean,
      W8DMARSTAT_clean == 1 ~ 1,  # Single
      W8DMARSTAT_clean == 2 ~ 2,  # Married
      W8DMARSTAT_clean %in% c(3, 7) ~ 3,  # Separated (including CP)
      W8DMARSTAT_clean %in% c(4, 8) ~ 4,  # Divorced/Former CP
      W8DMARSTAT_clean %in% c(5, 9) ~ 5,  # Widowed/Surviving CP
      W8DMARSTAT_clean == 6 ~ 6,  # Civil Partner
      TRUE ~ -3
    ),
    
    # Collapsed marital status at age 32 (Wave 9)
    partnr32 = case_when(
      W9DMARSTAT_clean %in% c(-9, -8, -3, -2, -1, -7) ~ W9DMARSTAT_clean,
      W9DMARSTAT_clean == 1 ~ 1,  # Single
      W9DMARSTAT_clean == 2 ~ 2,  # Married
      W9DMARSTAT_clean == 4 ~ 3,  # Separated (note: code 4 in W9)
      W9DMARSTAT_clean == 3 ~ 4,  # Divorced (note: code 3 in W9)
      W9DMARSTAT_clean == 5 ~ 5,  # Widowed
      W9DMARSTAT_clean == 6 ~ 6,  # Civil Partner
      W9DMARSTAT_clean %in% c(7, 8) ~ 4,  # Former/Surviving CP -> map to divorced category for collapsed
      TRUE ~ -3
    )
  )

# Create detailed adult marital status variables (partnradu)
data <- data %>%
  mutate(
    # Detailed marital status at age 19 (same as collapsed since no CP categories)
    partnradu19 = W6MarStatYP_clean,
    
    # Detailed marital status at age ~25 (Wave 8) - preserve all categories
    partnradu25 = W8DMARSTAT_clean,
    
    # Detailed marital status at age 32 (Wave 9) - preserve all categories
    partnradu32 = W9DMARSTAT_clean
  )

# Add value labels to factors - ensuring unique labels
data <- data %>%
  mutate(
    partnr19 = labelled(partnr19, labels = c("Single" = 1, "Married" = 2, "Separated" = 3, "Divorced" = 4, "Widowed" = 5, 
                                              "Refused" = -9, "Insufficient information" = -8, "Not applicable" = -1, 
                                              "Not asked" = -3, "Script error" = -2, "Prefer not to say" = -7)),
    partnr25 = labelled(partnr25, labels = c("Single" = 1, "Married" = 2, "Separated" = 3, "Divorced" = 4, "Widowed" = 5, "Civil Partner" = 6,
                                              "Refused" = -9, "Insufficient information" = -8, "Not applicable" = -1,
                                              "Not asked" = -3, "Script error" = -2, "Prefer not to say" = -7)),
    partnr32 = labelled(partnr32, labels = c("Single" = 1, "Married" = 2, "Separated" = 3, "Divorced" = 4, "Widowed" = 5, "Civil Partner" = 6,
                                              "Refused" = -9, "Insufficient information" = -8, "Not applicable" = -1,
                                              "Not asked" = -3, "Script error" = -2, "Prefer not to say" = -7)),
    partnradu19 = labelled(partnradu19, labels = c("Single" = 1, "Married" = 2, "Separated" = 3, "Divorced" = 4, "Widowed" = 5,
                                                    "Refused" = -9, "Insufficient information" = -8, "Not applicable" = -1,
                                                    "Not asked" = -3, "Script error" = -2)),
    partnradu25 = labelled(partnradu25, labels = c("Single" = 1, "Married" = 2, "Separated" = 3, "Divorced" = 4, "Widowed" = 5,
                                                    "Civil Partner" = 6, "Separated CP" = 7, "Former CP" = 8, "Surviving CP" = 9,
                                                    "Refused" = -9, "Insufficient information" = -8, "Not applicable" = -1,
                                                    "Not asked" = -3, "Script error" = -2)),
    partnradu32 = labelled(partnradu32, labels = c("Single" = 1, "Married" = 2, "Divorced" = 3, "Separated" = 4, "Widowed" = 5,
                                                    "Civil Partner" = 6, "Former CP" = 7, "Surviving CP" = 8,
                                                    "Refused" = -9, "Insufficient information" = -8,
                                                    "Not asked" = -3, "Script error" = -2))
  )

# Select final output variables
output_data <- data %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu19, partnradu25, partnradu32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output_data), "\n")
cat("Number of columns:", ncol(output_data), "\n")
