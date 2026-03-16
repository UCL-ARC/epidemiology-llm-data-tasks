
# Load required packages
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(purrr)
  library(labelled)
  library(readr)
})

# Load datasets from input directory
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Ensure NSID is treated as character
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_three$NSID <- as.character(wave_three$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Merge datasets by NSID
merged_data <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Function to recode missing values
recode_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3  # Replace NA with -3

  # Map wave-specific missing codes to standard codes
  x[x == -999] <- -2  # Missing in error -> Script error/lost
  x[x == -992] <- -9  # No information -> Refusal
  x[x == -99] <- -3   # HH not interviewed -> Not asked/interviewed
  x[x == -94] <- -8   # Insufficient information -> Don't know/insufficient information
  x[x == -92] <- -9   # Refused -> Refusal
  x[x == -91] <- -1   # Not applicable -> Item not applicable
  x[x == -3] <- -3    # Not yet paid -> Not asked/interviewed
  x[x == -1] <- -8    # Don't know -> Don't know/insufficient information

  return(x)
}

# Recode missing values for continuous income variables
merged_data <- merged_data %>%
  mutate(
    W1GrsswkHH = recode_missing(W1GrsswkHH),
    W2GrsswkHH = recode_missing(W2GrsswkHH),
    W3incestw = recode_missing(W3incestw),
    w4IncEstW = recode_missing(w4IncEstW)
  )

# Define banding schemes for ages 14-15 (continuous income)
banding_14_15 <- function(x) {
  x <- as.numeric(x)
  result <- x

  # Preserve missing values and non-positive values
  missing_codes <- c(-9, -8, -3, -2, -1)
  result[is.na(x) | x %in% missing_codes | x <= 0] <- x[is.na(x) | x %in% missing_codes | x <= 0]

  # Banding scheme for ages 14-15 (continuous income)
  result[x > 0 & x <= 50] <- 1
  result[x > 50 & x <= 100] <- 2
  result[x > 100 & x <= 200] <- 3
  result[x > 200 & x <= 300] <- 4
  result[x > 300 & x <= 400] <- 5
  result[x > 400 & x <= 500] <- 6
  result[x > 500 & x <= 600] <- 7
  result[x > 600 & x <= 700] <- 8
  result[x > 700 & x <= 800] <- 9
  result[x > 800 & x <= 900] <- 10
  result[x > 900] <- 11

  return(result)
}

# Create banded variables for ages 14-15
merged_data <- merged_data %>%
  mutate(
    incwhh14 = banding_14_15(W1GrsswkHH),
    incwhh15 = banding_14_15(W2GrsswkHH),
    incwhhcnt14 = W1GrsswkHH,
    incwhhcnt15 = W2GrsswkHH
  )

# Define factor levels for banded variables (ages 14-15)
factor_levels_14_15 <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
factor_labels_14_15 <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £50", "£50 up to £100", "£100 up to £200", "£200 up to £300", "£300 up to £400", "£400 up to £500", "£500 up to £600", "£600 up to £700", "£700 up to £800", "£800 up to £900", "£900 or more"
)

# Convert banded variables to factors with labels
merged_data <- merged_data %>%
  mutate(
    incwhh14 = factor(incwhh14, levels = factor_levels_14_15, labels = factor_labels_14_15),
    incwhh15 = factor(incwhh15, levels = factor_levels_14_15, labels = factor_labels_14_15)
  )

# Define factor levels and labels for ages 16-17 (already banded)
factor_levels_16_17 <- c(-9, -8, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
factor_labels_16_17 <- c(
  "Refusal", "Don't know/insufficient information", "Not asked/interviewed", "Script error/lost", "Item not applicable",
  "Up to £49", "£50 up to £99", "£100 up to £199", "£200 up to £299", "£300 up to £399", "£400 up to £499", "£500 up to £599", "£600 up to £699", "£700 up to £799", "£800 up to £899", "£900 up to £999", "£1,000 or more"
)

# Convert banded variables for ages 16-17 to factors with labels
merged_data <- merged_data %>%
  mutate(
    incwhh16 = factor(W3incestw, levels = factor_levels_16_17, labels = factor_labels_16_17),
    incwhh17 = factor(w4IncEstW, levels = factor_levels_16_17, labels = factor_labels_16_17)
  )

# Select only required variables
cleaned_data <- merged_data %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output to CSV with explicit directory creation
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Confirm output
cat("Data cleaning and harmonization completed successfully!")
cat("Output file saved to: data/output/cleaned_data.csv")
cat("Number of rows in output:", nrow(cleaned_data), "\n")
