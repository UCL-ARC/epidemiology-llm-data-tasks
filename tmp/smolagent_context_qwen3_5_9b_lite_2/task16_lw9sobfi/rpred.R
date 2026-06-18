library(haven)
library(dplyr)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

cat("Data loaded successfully.\n")
cat("Wave 1 (age 14):", nrow(wave1), "cases\n")
cat("Wave 2 (age 15):", nrow(wave2), "cases\n")
cat("Wave 3 (age 16):", nrow(wave3), "cases\n")
cat("Wave 4 (age 17):", nrow(wave4), "cases\n")

# Merge all datasets by NSID
final_data <- full_join(wave1, wave2, by = "NSID")
final_data <- full_join(final_data, wave3, by = "NSID")
final_data <- full_join(final_data, wave4, by = "NSID")

cat("Merged dataset has:", nrow(final_data), "cases\n")

# Create banded income variables with standard missing codes
# Wave 1 (age 14) - W1GrsswkHH
final_data <- final_data %>%
  mutate(inc14 = case_when(
    W1GrsswkHH == -999 | W1GrsswkHH == -992 ~ -2,
    W1GrsswkHH == -99 ~ -3,
    W1GrsswkHH == -94 ~ -8,
    W1GrsswkHH == -92 ~ -9,
    W1GrsswkHH == -91 ~ -1,
    W1GrsswkHH == -3 ~ -3,
    W1GrsswkHH == -1 ~ -8,
    TRUE ~ W1GrsswkHH
  ))

# Wave 2 (age 15) - W2GrsswkHH
final_data <- final_data %>%
  mutate(inc15 = case_when(
    W2GrsswkHH == -999 | W2GrsswkHH == -992 ~ -2,
    W2GrsswkHH == -99 ~ -3,
    W2GrsswkHH == -94 ~ -8,
    W2GrsswkHH == -92 ~ -9,
    W2GrsswkHH == -91 ~ -1,
    W2GrsswkHH == -3 ~ -3,
    W2GrsswkHH == -1 ~ -8,
    TRUE ~ W2GrsswkHH
  ))

# Wave 3 (age 16) - W3incestw
final_data <- final_data %>%
  mutate(inc16 = case_when(
    W3incestw == -99 ~ -3,
    W3incestw == -92 ~ -9,
    W3incestw == -1 ~ -8,
    TRUE ~ W3incestw
  ))

# Wave 4 (age 17) - w4IncEstW
final_data <- final_data %>%
  mutate(inc17 = case_when(
    w4IncEstW == -996 ~ -1,
    w4IncEstW == -99 ~ -3,
    w4IncEstW == -92 ~ -9,
    w4IncEstW == -1 ~ -8,
    TRUE ~ w4IncEstW
  ))

# Create continuous variables for ages 14 and 15
final_data <- final_data %>%
  mutate(
    inc_cont14 = W1GrsswkHH,
    inc_cont15 = W2GrsswkHH
  )

# Remove source variables, keep only NSID and derived variables
final_data <- final_data %>%
  select(
    NSID,
    inc14,
    inc15,
    inc16,
    inc17,
    inc_cont14,
    inc_cont15
  )

cat("Writing cleaned data...\n")
write_csv(final_data, "data/output/cleaned_data.csv")
cat("Done. Output written to data/output/cleaned_data.csv\n")