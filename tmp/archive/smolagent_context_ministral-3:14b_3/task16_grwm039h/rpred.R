
# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load datasets
wave_one <- readr::read_delim(file = "data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim(file = "data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim(file = "data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim(file = "data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Convert NSID to character to ensure consistency
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_three$NSID <- as.character(wave_three$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Step 3: Recode missing values for continuous variables
# Age 14
merged_data <- merged_data %>%
  mutate(W1GrsswkHH = case_when(
    W1GrsswkHH == -999 | W1GrsswkHH == -992 | W1GrsswkHH == -99 | is.na(W1GrsswkHH) ~ NA_integer_,
    W1GrsswkHH == -94 ~ -8,
    W1GrsswkHH == -92 ~ -9,
    W1GrsswkHH == -91 | W1GrsswkHH == -3 ~ -1,
    W1GrsswkHH == -1 ~ -8,
    TRUE ~ as.integer(W1GrsswkHH)
  ))

# Age 15
merged_data <- merged_data %>%
  mutate(W2GrsswkHH = case_when(
    W2GrsswkHH == -999 | W2GrsswkHH == -992 | W2GrsswkHH == -99 | is.na(W2GrsswkHH) ~ NA_integer_,
    W2GrsswkHH == -94 ~ -8,
    W2GrsswkHH == -92 ~ -9,
    W2GrsswkHH == -91 | W2GrsswkHH == -3 ~ -1,
    W2GrsswkHH == -1 ~ -8,
    TRUE ~ as.integer(W2GrsswkHH)
  ))

# Step 4: Derive banded variables for ages 14 and 15
age_14_bands <- c(0, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf)
age_14_labels <- c(
  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299",
  "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699",
  "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more"
)

# Create continuous variables
merged_data <- merged_data %>%
  mutate(incwhhcnt14 = ifelse(W1GrsswkHH > 0, as.numeric(W1GrsswkHH), NA_real_)) %>%
  mutate(incwhhcnt15 = ifelse(W2GrsswkHH > 0, as.numeric(W2GrsswkHH), NA_real_))

# Create banded variables
merged_data <- merged_data %>%
  mutate(incwhh14 = cut(ifelse(W1GrsswkHH > 0, W1GrsswkHH, NA_real_), breaks = age_14_bands,
                        labels = age_14_labels, include.lowest = TRUE, right = TRUE)) %>%
  mutate(incwhh14 = ifelse(is.na(incwhh14), NA_character_, incwhh14)) %>%
  mutate(incwhh14 = factor(incwhh14, levels = c(age_14_labels, "-9", "-8", "-3", "-2", "-1")))

merged_data <- merged_data %>%
  mutate(incwhh15 = cut(ifelse(W2GrsswkHH > 0, W2GrsswkHH, NA_real_), breaks = age_14_bands,
                        labels = age_14_labels, include.lowest = TRUE, right = TRUE)) %>%
  mutate(incwhh15 = ifelse(is.na(incwhh15), NA_character_, incwhh15)) %>%
  mutate(incwhh15 = factor(incwhh15, levels = c(age_14_labels, "-9", "-8", "-3", "-2", "-1")))

# Step 5: Handle age 16 and 17 (already banded)
# Recode missing values for age 16
merged_data <- merged_data %>%
  mutate(W3incestw = case_when(
    W3incestw == -99 ~ -9,
    W3incestw == -92 ~ -9,
    W3incestw == -1 ~ -8,
    W3incestw == -999 | is.na(W3incestw) ~ -3,
    TRUE ~ W3incestw
  )) %>%
  mutate(incwhh16 = factor(W3incestw,
                           levels = c(-9, -8, -3, -2, -1, 1:12),
                           labels = c("Refusal", "Don't know/insufficient info", "Not asked/interviewed", "Script error/lost", "Item not applicable", age_14_labels)))

# Recode missing values for age 17
merged_data <- merged_data %>%
  mutate(w4IncEstW = case_when(
    w4IncEstW == -996 ~ -3,
    w4IncEstW == -99 ~ -3,
    w4IncEstW == -92 ~ -9,
    w4IncEstW == -1 ~ -8,
    w4IncEstW == -999 | is.na(w4IncEstW) ~ -3,
    TRUE ~ w4IncEstW
  )) %>%
  mutate(incwhh17 = factor(w4IncEstW,
                           levels = c(-996, -9, -8, -3, -2, -1, 1:12),
                           labels = c("No parent in household", "Refusal", "Don't know/insufficient info", "Not asked/interviewed", "Script error/lost", "Item not applicable", age_14_labels)))

# Step 6: Adjust factor levels to include missing values properly
merged_data$incwhh14 <- factor(merged_data$incwhh14,
                              levels = c(age_14_labels, "-9", "-8", "-3", "-2", "-1"),
                              ordered = TRUE)

merged_data$incwhh15 <- factor(merged_data$incwhh15,
                              levels = c(age_14_labels, "-9", "-8", "-3", "-2", "-1"),
                              ordered = TRUE)

# Step 7: Select required variables
final_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Step 8: Write output
write.csv(final_data, file = "data/output/cleaned_data.csv", row.names = FALSE)
    