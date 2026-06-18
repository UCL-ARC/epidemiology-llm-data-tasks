library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Function to harmonize missing values to standard codes
harmonize_missing <- function(x) {
  x[is.na(x)] <- -3  # Default: not asked / not interviewed
  x
}

# Function to map wave-specific missing codes to standard codes
map_missing_codes <- function(x, wave) {
  if (wave == 1) {
    # W1ethnic2YP: -999=household data lost(-2), -94=insufficient(-8), -92=refused(-9), -91=not applicable(-1), -1=don't know(-8)
    x[x == -999] <- -2
    x[x == -94] <- -8
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -1] <- -8
  } else if (wave == 2) {
    # W2ethnicYP: -998=interviewer missed(-2), -997=script error(-2), -995=missing history(-2), -99=not interviewed(-3), -92=refused(-9), -91=not applicable(-1), -1=don't know(-8)
    x[x == -998] <- -2
    x[x == -997] <- -2
    x[x == -995] <- -2
    x[x == -99] <- -3
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -1] <- -8
  } else if (wave == 4) {
    # w4ethnic2YP: -94=insufficient(-8), -1=don't know(-8)
    x[x == -94] <- -8
    x[x == -1] <- -8
  } else if (wave == 8) {
    # W8DETHN15: -9=refused(-9), -8=insufficient(-8), -1=not applicable(-1)
    x[x == -9] <- -9
    x[x == -8] <- -8
    x[x == -1] <- -1
  } else if (wave == 9) {
    # W9DETHN15: -8=insufficient(-8)
    x[x == -8] <- -8
  }
  x
}

# Map source variables to standard codes
w1$W1ethnic2YP <- map_missing_codes(w1$W1ethnic2YP, 1)
w2$W2ethnicYP <- map_missing_codes(w2$W2ethnicYP, 2)
w4$w4ethnic2YP <- map_missing_codes(w4$w4ethnic2YP, 4)
w8$W8DETHN15 <- map_missing_codes(w8$W8DETHN15, 8)
w9$W9DETHN15 <- map_missing_codes(w9$W9DETHN15, 9)

# Merge all files by NSID
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w4, by = "NSID")
df <- full_join(df, w8, by = "NSID")
df <- full_join(df, w9, by = "NSID")

# Select only NSID and ethnicity variables
eth_vars <- df %>% select(NSID, W1ethnic2YP, W2ethnicYP, w4ethnic2YP, W8DETHN15, W9DETHN15)

# Create consolidated eth variable using earliest-valid-first
# Valid substantive codes are 1-16
eth <- rep(NA_real_, nrow(eth_vars))

for (i in 1:nrow(eth_vars)) {
  vals <- c(eth_vars$W1ethnic2YP[i], eth_vars$W2ethnicYP[i], eth_vars$w4ethnic2YP[i], 
            eth_vars$W8DETHN15[i], eth_vars$W9DETHN15[i])
  # Find first valid substantive response (1-16)
  for (v in vals) {
    if (!is.na(v) && v >= 1 && v <= 16) {
      eth[i] <- v
      break
    }
  }
  # If no valid substantive response, use the first non-missing value
  if (is.na(eth[i])) {
    for (v in vals) {
      if (!is.na(v)) {
        eth[i] <- v
        break
      }
    }
  }
}

eth_vars$eth <- eth

# Define the ethnic category labels for the consolidated variable
# Use Wave 1 labels as the reference (most detailed)
ethnic_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background"
)

# Create labelled factor for eth
eth_vars$eth <- factor(eth_vars$eth, levels = c(1:16, -1, -2, -3, -8, -9),
                       labels = c(ethnic_labels, 
                                  "Not applicable", "Schedule not applicable / script error",
                                  "Not asked / not interviewed", "Don't know / insufficient information",
                                  "Refusal"))

# Keep only NSID and eth
output <- eth_vars %>% select(NSID, eth)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("Eth distribution:\n")
print(table(output$eth, useNA = "ifany"))
