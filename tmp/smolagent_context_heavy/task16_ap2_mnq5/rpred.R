library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"
file1 <- readr::read_delim(paste0(path, "wave_one_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file2 <- readr::read_delim(paste0(path, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file3 <- readr::read_delim(paste0(path, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- readr::read_delim(paste0(path, "wave_four_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))

# Convert necessary columns to numeric for processing
file1$W1GrsswkHH <- as.numeric(file1$W1GrsswkHH)
file2$W2GrsswkHH <- as.numeric(file2$W2GrsswkHH)
file3$W3incestw <- as.numeric(file3$W3incestw)
file4$w4IncEstW <- as.numeric(file4$w4IncEstW)

# Merge datasets
df <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID")

# Helper function for missing value mapping based on prompt rules
map_missing <- function(x, age) {
  # 1. Specific mappings from requirements
  # "Not yet paid" (ages 14-15) -> -1
  if (age %in% c(14, 15)) {
    x[x == -3] <- -1
  }
  # "Don't know" (ages 14-15) -> -8
  if (age %in% c(14, 15)) {
    x[x == -1] <- -8
  }
  # "No information - refused" -> -9
  x[x == -992] <- -9
  # "No parent in household" (age 17) -> -3
  if (age == 17) {
    x[x == -996] <- -3
  }
  
  # 2. Master Prompt Defaults (Label meaning priority)
  # -92 usually maps to -9 (Refused)
  x[x == -92] <- -9
  # -91 usually maps to -1 (Not applicable)
  x[x == -91] <- -1
  # -99 usually maps to -3 (Not interviewed/Not asked)
  x[x == -99] <- -3
  # -999, -998, -997, -995 -> -2 (Schedule not applicable/error)
  x[x %in% c(-999, -998, -997, -995)] <- -2
  # -94 (insufficient information) -> -8
  x[x == -94] <- -8
  
  # 3. Remaining NAs to -3
  x[is.na(x)] <- -3
  
  return(x)
}

# Process Wave 1 (Age 14)
incwhhcnt14 <- map_missing(df$W1GrsswkHH, 14)
incwhh14_raw <- incwhhcnt14
# Banding logic for 14 & 15 based on later waves:
# 1: Up to 49, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+
band_income <- function(x) {
  res <- rep(NA, length(x))
  valid <- x >= 0
  res[valid & x < 50] <- 1
  res[valid & x >= 50 & x < 100] <- 2
  res[valid & x >= 100 & x < 200] <- 3
  res[valid & x >= 200 & x < 300] <- 4
  res[valid & x >= 300 & x < 400] <- 5
  res[valid & x >= 400 & x < 500] <- 6
  res[valid & x >= 500 & x < 600] <- 7
  res[valid & x >= 600 & x < 700] <- 8
  res[valid & x >= 700 & x < 800] <- 9
  res[valid & x >= 800 & x < 900] <- 10
  res[valid & x >= 900 & x < 1000] <- 11
  res[valid & x >= 1000] <- 12
  
  # Preserve missing codes
  missing_codes <- c(-9, -8, -7, -3, -2, -1)
  for(code in missing_codes) {
    res[x == code] <- code
  }
  return(res)
}
incwhh14 <- band_income(incwhhcnt14)

# Process Wave 2 (Age 15)
incwhhcnt15 <- map_missing(df$W2GrsswkHH, 15)
incwhh15 <- band_income(incwhhcnt15)

# Process Wave 3 (Age 16)
incwhh16 <- map_missing(df$W3incestw, 16)

# Process Wave 4 (Age 17)
incwhh17 <- map_missing(df$w4IncEstW, 17)

# Define labels for banded income
income_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say",
  "-3" = "Not asked", "-2" = "Not applicable/Error", "-1" = "Not applicable"
)

# Create final dataframe
final_df <- data.frame(NSID = df$NSID)
final_df$incwhh14 <- factor(incwhh14, levels = names(income_labels), labels = income_labels)
final_df$incwhh15 <- factor(incwhh15, levels = names(income_labels), labels = income_labels)
final_df$incwhh16 <- factor(incwhh16, levels = names(income_labels), labels = income_labels)
final_df$incwhh17 <- factor(incwhh17, levels = names(income_labels), labels = income_labels)
final_df$incwhhcnt14 <- incwhhcnt14
final_df$incwhhcnt15 <- incwhhcnt15

# Write to CSV
readr::write_csv(final_df, "data/output/cleaned_data.csv")