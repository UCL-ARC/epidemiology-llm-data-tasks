library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

# Load each file
data_w1 <- read_tsv(paste0("data/input/", files[1]), show_col_types = FALSE)
data_w2 <- read_tsv(paste0("data/input/", files[2]), show_col_types = FALSE)
data_w3 <- read_tsv(paste0("data/input/", files[3]), show_col_types = FALSE)
data_w4 <- read_tsv(paste0("data/input/", files[4]), show_col_types = FALSE)

# Merge all files by NSID
clean_data <- full_join(data_w1, data_w2, by = "NSID")
clean_data <- full_join(clean_data, data_w3, by = "NSID")
clean_data <- full_join(clean_data, data_w4, by = "NSID")

cat("Original data dimensions:", dim(clean_data), "\n")

# Define banded income function (1-12 categories)
band_income <- function(x) {
  case_when(
    x >= 1000 ~ 12,
    x >= 900 ~ 11,
    x >= 800 ~ 10,
    x >= 700 ~ 9,
    x >= 600 ~ 8,
    x >= 500 ~ 7,
    x >= 400 ~ 6,
    x >= 300 ~ 5,
    x >= 200 ~ 4,
    x >= 100 ~ 3,
    x >= 50 ~ 2,
    x > 0 ~ 1,
    TRUE ~ x
  )
}

# Process Age 14 (Wave 1) - W1GrsswkHH
w1_income_raw <- clean_data$W1GrsswkHH

# Step 1: Apply sweep-specific mappings
w1_income_clean <- w1_income_raw
w1_income_clean[w1_income_clean == -3] <- -1
w1_income_clean[w1_income_clean == -1] <- -8
w1_income_clean[w1_income_clean == -992] <- -9
other_neg <- c(-99, -94, -91, -999)
w1_income_clean[w1_income_clean %in% other_neg] <- -2

# Step 2: Remap remaining NA to -3
w1_income_clean[is.na(w1_income_clean)] <- -3

# Step 3: Create banded variable
w1_income_banded <- w1_income_clean
w1_income_banded[w1_income_banded > 0] <- band_income(w1_income_banded[w1_income_banded > 0])

# Process Age 15 (Wave 2) - W2GrsswkHH
w2_income_raw <- clean_data$W2GrsswkHH

# Step 1: Apply sweep-specific mappings
w2_income_clean <- w2_income_raw
w2_income_clean[w2_income_clean == -3] <- -1
w2_income_clean[w2_income_clean == -1] <- -8
w2_income_clean[w2_income_clean == -992] <- -9
w2_income_clean[w2_income_clean == -996] <- -3
other_neg <- c(-99, -94, -92, -91, -999)
w2_income_clean[w2_income_clean %in% other_neg] <- -2

# Step 2: Remap remaining NA to -3
w2_income_clean[is.na(w2_income_clean)] <- -3

# Step 3: Create banded variable
w2_income_banded <- w2_income_clean
w2_income_banded[w2_income_banded > 0] <- band_income(w2_income_banded[w2_income_banded > 0])

# Process Age 16 (Wave 3) - W3incestw (already banded)
w3_income_raw <- clean_data$W3incestw

# Step 1: Apply sweep-specific mappings
w3_income_clean <- w3_income_raw
w3_income_clean[w3_income_clean == -996] <- -3
w3_income_clean[w3_income_clean == -992] <- -9
w3_income_clean[w3_income_clean == -1] <- -8
w3_income_clean[w3_income_clean == -92] <- -9
w3_income_clean[w3_income_clean == -99] <- -2

# Step 2: Remap remaining NA to -3
w3_income_clean[is.na(w3_income_clean)] <- -3

# Process Age 17 (Wave 4) - w4IncEstW (already banded)
w4_income_raw <- clean_data$w4IncEstW

# Step 1: Apply sweep-specific mappings
w4_income_clean <- w4_income_raw
w4_income_clean[w4_income_clean == -996] <- -3
w4_income_clean[w4_income_clean == -992] <- -9
w4_income_clean[w4_income_clean == -1] <- -8
w4_income_clean[w4_income_clean == -92] <- -9
w4_income_clean[w4_income_clean == -99] <- -2

# Step 2: Remap remaining NA to -3
w4_income_clean[is.na(w4_income_clean)] <- -3

# Create output dataframe with only required columns
output_data <- clean_data %>%
  mutate(
    incwhh14 = w1_income_banded,
    incwhh15 = w2_income_banded,
    incwhh16 = w3_income_clean,
    incwhh17 = w4_income_clean,
    incwhhcnt14 = w1_income_clean,
    incwhhcnt15 = w2_income_clean
  ) %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

cat("Output data dimensions:", dim(output_data), "\n")
cat("Variables:", names(output_data), "\n")

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")

cat("Successfully wrote data/output/cleaned_data.csv\n")