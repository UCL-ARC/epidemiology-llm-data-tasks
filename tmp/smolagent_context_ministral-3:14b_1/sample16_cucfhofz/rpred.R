library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")

# Step 3: Standardize missing value codes for income variables only
standardize_missing <- function(df) {
  df %>%
    mutate(
      W1GrsswkHH = case_when(
        W1GrsswkHH %in% c(-999, -992, -99, -94, -91) | is.na(W1GrsswkHH) ~ -3,
        W1GrsswkHH == -92 ~ -9,
        W1GrsswkHH %in% c(-90, -1) ~ -8,
        W1GrsswkHH == -3 ~ -3,
        W1GrsswkHH == -2 ~ -2,
        TRUE ~ W1GrsswkHH
      ),
      W2GrsswkHH = case_when(
        W2GrsswkHH %in% c(-999, -992, -99, -94, -91) | is.na(W2GrsswkHH) ~ -3,
        W2GrsswkHH == -92 ~ -9,
        W2GrsswkHH %in% c(-90, -1) ~ -8,
        W2GrsswkHH == -3 ~ -3,
        W2GrsswkHH == -2 ~ -2,
        TRUE ~ W2GrsswkHH
      ),
      W3incestw = case_when(
        W3incestw %in% c(-99, -92, -1) | is.na(W3incestw) ~ -3,
        W3incestw == -92 ~ -9,
        W3incestw == -1 ~ -8,
        TRUE ~ W3incestw
      ),
      w4IncEstW = case_when(
        w4IncEstW %in% c(-999, -99, -92, -1) | is.na(w4IncEstW) ~ -3,
        w4IncEstW == -92 ~ -9,
        w4IncEstW == -1 ~ -8,
        TRUE ~ w4IncEstW
      )
    )
}

# Step 4: Rename variables
clean_data <- merged_data %>%
  standardize_missing() %>%
  rename(
    incwhhcnt14 = W1GrsswkHH,
    incwhhcnt15 = W2GrsswkHH,
    incwhh16 = W3incestw,
    incwhh17 = w4IncEstW
  )

# Step 5: Vectorized banding function for ages 14 and 15
band_14_15 <- function(x) {
  ifelse(x <= 0, x, 
         cut(x, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf),
              labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
              right = TRUE, include.lowest = TRUE))
}

# Apply banding for ages 14 and 15
clean_data <- clean_data %>%
  mutate(
    incwhh14 = band_14_15(incwhhcnt14),
    incwhh15 = band_14_15(incwhhcnt15)
  )

# Step 6: Define factor labels
band_labels_14_15 <- c("-9" = "Refusal", "-8" = "Don't know/insufficient information", "-3" = "Not asked/interviewed", "-2" = "Script error/lost", "-1" = "Item not applicable", "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199", "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499", "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799", "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more")

band_labels_16_17 <- c("-9" = "Refusal", "-8" = "Don't know/insufficient information", "-3" = "Not asked/interviewed", "-2" = "Script error/lost", "-1" = "Item not applicable", "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199", "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499", "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799", "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more")

# Step 7: Convert banded variables to factors
clean_data <- clean_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = names(band_labels_14_15), labels = band_labels_14_15),
    incwhh15 = factor(incwhh15, levels = names(band_labels_14_15), labels = band_labels_14_15),
    incwhh16 = factor(incwhh16, levels = names(band_labels_16_17), labels = band_labels_16_17),
    incwhh17 = factor(incwhh17, levels = names(band_labels_16_17), labels = band_labels_16_17)
  )

# Step 8: Select required variables
final_data <- clean_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Step 9: Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Cleaned dataset has been written to data/output/cleaned_data.csv\n")