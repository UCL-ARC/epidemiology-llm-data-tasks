library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Check if output directory exists, create if not
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Check if input directory exists
if (!dir.exists("data/input")) {
  stop("Input directory does not exist")
}

# Load files from data/input/
message("Loading Wave 1 (Age 14)...")
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
message("Loaded Wave 1 with", nrow(wave1), "rows")
message("Wave 1 columns:", names(wave1))

message("Loading Wave 2 (Age 15)...")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
message("Loaded Wave 2 with", nrow(wave2), "rows")
message("Wave 2 columns:", names(wave2))

message("Loading Wave 3 (Age 16)...")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
message("Loaded Wave 3 with", nrow(wave3), "rows")
message("Wave 3 columns:", names(wave3))

message("Loading Wave 4 (Age 17)...")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
message("Loaded Wave 4 with", nrow(wave4), "rows")
message("Wave 4 columns:", names(wave4))

# Check for income variables
message("\nChecking income variables:")
message("Wave 1 - W1GrsswkHH exists:", "W1GrsswkHH" %in% names(wave1))
message("Wave 2 - W2GrsswkHH exists:", "W2GrsswkHH" %in% names(wave2))
message("Wave 3 - W3incestw exists:", "W3incestw" %in% names(wave3))
message("Wave 4 - w4IncEstW exists:", "w4IncEstW" %in% names(wave4))

# Process Wave 1 (Age 14) - continuous and banded
message("\nProcessing Wave 1 (Age 14)...")
wave1 <- wave1 %>%
  mutate(
    incwhhcnt14 = case_when(
      W1GrsswkHH %in% c(-999, -992, -996) ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -3 ~ -3,
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ W1GrsswkHH
    ),
    incwhh14 = case_when(
      W1GrsswkHH >= 1 & W1GrsswkHH <= 49 ~ 1,
      W1GrsswkHH >= 50 & W1GrsswkHH <= 99 ~ 2,
      W1GrsswkHH >= 100 & W1GrsswkHH <= 199 ~ 3,
      W1GrsswkHH >= 200 & W1GrsswkHH <= 299 ~ 4,
      W1GrsswkHH >= 300 & W1GrsswkHH <= 399 ~ 5,
      W1GrsswkHH >= 400 & W1GrsswkHH <= 499 ~ 6,
      W1GrsswkHH >= 500 & W1GrsswkHH <= 599 ~ 7,
      W1GrsswkHH >= 600 & W1GrsswkHH <= 699 ~ 8,
      W1GrsswkHH >= 700 & W1GrsswkHH <= 799 ~ 9,
      W1GrsswkHH >= 800 & W1GrsswkHH <= 899 ~ 10,
      W1GrsswkHH >= 900 & W1GrsswkHH <= 999 ~ 11,
      W1GrsswkHH >= 1000 ~ 12,
      W1GrsswkHH %in% c(-999, -992, -996) ~ -3,
      W1GrsswkHH == -94 ~ -8,
      W1GrsswkHH == -92 ~ -9,
      W1GrsswkHH == -91 ~ -1,
      W1GrsswkHH == -3 ~ -3,
      W1GrsswkHH == -1 ~ -8,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    incwhh14 = factor(incwhh14,
      levels = c(-3, -9, -8, -1, 1:12),
      labels = c("Not asked/interviewed", "Refused", "Don't know/insufficient information", 
                 "Item not applicable",
                 "Up to £49", "£50 up to £99", "£100 up to £199",
                 "£200 up to £299", "£300 up to £399",
                 "£400 up to £499", "£500 up to £599",
                 "£600 up to £699", "£700 up to £799",
                 "£800 up to £899", "£900 up to £999",
                 "£1,000 or more")
    )
  )
message("Wave 1 processing complete")

# Process Wave 2 (Age 15) - continuous and banded
message("Processing Wave 2 (Age 15)...")
wave2 <- wave2 %>%
  mutate(
    incwhhcnt15 = case_when(
      W2GrsswkHH %in% c(-999, -992, -996) ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -3 ~ -3,
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ W2GrsswkHH
    ),
    incwhh15 = case_when(
      W2GrsswkHH >= 1 & W2GrsswkHH <= 49 ~ 1,
      W2GrsswkHH >= 50 & W2GrsswkHH <= 99 ~ 2,
      W2GrsswkHH >= 100 & W2GrsswkHH <= 199 ~ 3,
      W2GrsswkHH >= 200 & W2GrsswkHH <= 299 ~ 4,
      W2GrsswkHH >= 300 & W2GrsswkHH <= 399 ~ 5,
      W2GrsswkHH >= 400 & W2GrsswkHH <= 499 ~ 6,
      W2GrsswkHH >= 500 & W2GrsswkHH <= 599 ~ 7,
      W2GrsswkHH >= 600 & W2GrsswkHH <= 699 ~ 8,
      W2GrsswkHH >= 700 & W2GrsswkHH <= 799 ~ 9,
      W2GrsswkHH >= 800 & W2GrsswkHH <= 899 ~ 10,
      W2GrsswkHH >= 900 & W2GrsswkHH <= 999 ~ 11,
      W2GrsswkHH >= 1000 ~ 12,
      W2GrsswkHH %in% c(-999, -992, -996) ~ -3,
      W2GrsswkHH == -94 ~ -8,
      W2GrsswkHH == -92 ~ -9,
      W2GrsswkHH == -91 ~ -1,
      W2GrsswkHH == -3 ~ -3,
      W2GrsswkHH == -1 ~ -8,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    incwhh15 = factor(incwhh15,
      levels = c(-3, -9, -8, -1, 1:12),
      labels = c("Not asked/interviewed", "Refused", "Don't know/insufficient information", 
                 "Item not applicable",
                 "Up to £49", "£50 up to £99", "£100 up to £199",
                 "£200 up to £299", "£300 up to £399",
                 "£400 up to £499", "£500 up to £599",
                 "£600 up to £699", "£700 up to £799",
                 "£800 up to £899", "£900 up to £999",
                 "£1,000 or more")
    )
  )
message("Wave 2 processing complete")

# Process Wave 3 (Age 16) - factor (categorical bands)
message("Processing Wave 3 (Age 16)...")
wave3 <- wave3 %>%
  mutate(
    incwhh16 = case_when(
      W3incestw %in% c(-999, -92) ~ -3,
      W3incestw == -1 ~ -8,
      W3incestw == -99 ~ -3,
      TRUE ~ W3incestw
    )
  ) %>%
  mutate(
    incwhh16 = factor(incwhh16,
      levels = c(-3, -9, -8, 1:12),
      labels = c("Not asked/interviewed", "Refused", "Don't know/insufficient information",
                 "Up to £49", "£50 up to £99", "£100 up to £199",
                 "£200 up to £299", "£300 up to £399",
                 "£400 up to £499", "£500 up to £599",
                 "£600 up to £699", "£700 up to £799",
                 "£800 up to £899", "£900 up to £990",
                 "£1,000 or more")
    )
  )
message("Wave 3 processing complete")

# Process Wave 4 (Age 17) - factor (categorical bands)
message("Processing Wave 4 (Age 17)...")
wave4 <- wave4 %>%
  mutate(
    incwhh17 = case_when(
      w4IncEstW %in% c(-999, -996) ~ -3,
      w4IncEstW == -99 ~ -3,
      w4IncEstW == -92 ~ -9,
      w4IncEstW == -1 ~ -8,
      TRUE ~ w4IncEstW
    )
  ) %>%
  mutate(
    incwhh17 = factor(incwhh17,
      levels = c(-1, -3, -9, -8, 1:12),
      labels = c("Item not applicable", "Not asked/interviewed", "Refused", "Don't know/insufficient information",
                 "Up to £49", "£50 up to £99", "£100 up to £199",
                 "£200 up to £299", "£300 up to £399",
                 "£400 up to £499", "£500 up to £599",
                 "£600 up to £699", "£700 up to £799",
                 "£800 up to £899", "£900 up to £999",
                 "£1,000 or more")
    )
  )
message("Wave 4 processing complete")

# Merge all waves
cleaned_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Select only required variables
cleaned_data <- cleaned_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

message("\nFinal dataset:")
message("Rows:", nrow(cleaned_data))
message("Columns:", names(cleaned_data))
message("Column types:")
for (col in names(cleaned_data)) {
  message(col, ":", class(cleaned_data[[col]]))
}

# Write output
message("\nWriting output to data/output/cleaned_data.csv")
write_csv(cleaned_data, "data/output/cleaned_data.csv")
message("Output written successfully")
