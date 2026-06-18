library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file_w1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file_w2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file_w3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file_w4 <- "data/input/wave_four_lsype_family_background_2020.tab"

data_w1 <- read_delim(file_w1, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data_w2 <- read_delim(file_w2, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data_w3 <- read_delim(file_w3, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))
data_w4 <- read_delim(file_w4, delim = "\t", col_types = cols(.default = "numeric", NSID = "character"))

# Merge datasets
full_df <- data_w1 %>%
  full_join(data_w2, by = "NSID") %>%
  full_join(data_w3, by = "NSID") %>%
  full_join(data_w4, by = "NSID")

# Helper for missing value mapping
map_missing <- function(x, wave_age) {
  # Special mappings from requirements
  # -3 ("Not yet paid", ages 14-15) -> -1
  # -1 ("Don't know", ages 14-15) -> -8
  # -992 ("No information - refused") -> -9
  # -996 ("No parent in household", age 17) -> -3
  
  res <- x
  
  if (wave_age %in% c(14, 15)) {
    res[x == -3] <- -1
    res[x == -1] <- -8
  }
  
  if (wave_age == 17) {
    res[x == -996] <- -3
  }
  
  # Common mappings
  res[x == -992] <- -9
  
  # General Guidance mappings
  # -92 -> -9 (Refused)
  # -91 -> -1 (Not applicable)
  # -99 -> -3 (Not interviewed)
  # -999, -998, -997, -995 -> -2 (Schedule not applicable/lost)
  # -94 -> -8 (Insufficient info)
  
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -99] <- -3
  res[x %in% c(-999, -998, -997, -995)] <- -2
  res[x == -94] <- -8
  
  # Convert NAs to -3
  res[is.na(res)] <- -3
  
  return(res)
}

# Banding function based on the logic for ages 16/17
# 1: Up to 49, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+
band_income <- function(x) {
  res <- x
  # Only band positive values
  pos <- x > 0
  val <- x[pos]
  
  bands <- rep(NA, length(val))
  bands[val < 50] <- 1
  bands[val >= 50 & val < 100] <- 2
  bands[val >= 100 & val < 200] <- 3
  bands[val >= 200 & val < 300] <- 4
  bands[val >= 300 & val < 400] <- 5
  bands[val >= 400 & val < 500] <- 6
  bands[val >= 500 & val < 600] <- 7
  bands[val >= 600 & val < 700] <- 8
  bands[val >= 700 & val < 800] <- 9
  bands[val >= 800 & val < 900] <- 10
  bands[val >= 900 & val < 1000] <- 11
  bands[val >= 1000] <- 12
  
  res[pos] <- bands
  return(res)
}

# Process Wave 1 (14)
incwhhcnt14 <- map_missing(full_df$W1GrsswkHH, 14)
incwhh14 <- band_income(incwhhcnt14)
# Ensure missing codes in incwhh14 are preserved from incwhhcnt14
incwhh14[incwhhcnt14 <= 0] <- incwhhcnt14[incwhhcnt14 <= 0]

# Process Wave 2 (15)
incwhhcnt15 <- map_missing(full_df$W2GrsswkHH, 15)
incwhh15 <- band_income(incwhhcnt15)
incwhh15[incwhhcnt15 <= 0] <- incwhhcnt15[incwhhcnt15 <= 0]

# Process Wave 3 (16)
incwhh16 <- map_missing(full_df$W3incestw, 16)

# Process Wave 4 (17)
incwhh17 <- map_missing(full_df$w4IncEstW, 17)

# Create final dataframe
final_df <- data.frame(
  NSID = full_df$NSID,
  incwhh14 = incwhh14,
  incwhh15 = incwhh15,
  incwhh16 = incwhh16,
  incwhh17 = incwhh17,
  incwhhcnt14 = incwhhcnt14,
  incwhhcnt15 = incwhhcnt15
)

# Factor labels for banded variables
band_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know / insufficient information", 
  "-7" = "Prefer not to say", "-3" = "Not asked / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable"
)

# Apply labels to banded variables
apply_band_labels <- function(x) {
  factor(x, levels = as.numeric(names(band_labels)), labels = band_labels)
}

# Since we used numeric for calculations, we convert to factor at the end
final_df$incwhh14 <- apply_band_labels(final_df$incwhh14)
final_df$incwhh15 <- apply_band_labels(final_df$incwhh15)
final_df$incwhh16 <- apply_band_labels(final_df$incwhh16)
final_df$incwhh17 <- apply_band_labels(final_df$incwhh17)

# Write CSV
write_csv(final_df, "data/output/cleaned_data.csv")
