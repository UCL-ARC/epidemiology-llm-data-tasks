
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Define missing value mapping function
map_missing_values <- function(x) {
  if (!is.numeric(x)) return(x)
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -7] <- -7
  x[x == -1] <- -1
  return(x)
}

# Load wave 1 (age 14)
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
if ("W1hous12HH" %in% names(wave1)) {
  wave1$W1hous12HH <- map_missing_values(wave1$W1hous12HH)
  wave1$hownteen14 <- ifelse(wave1$W1hous12HH == 1, 1,
                           ifelse(wave1$W1hous12HH == 2, 2,
                           ifelse(wave1$W1hous12HH == 3, 3,
                           ifelse(wave1$W1hous12HH == 4, 4,
                           ifelse(wave1$W1hous12HH == 5, 5,
                           ifelse(wave1$W1hous12HH == 6, 6,
                           ifelse(wave1$W1hous12HH == 7, 7,
                           ifelse(wave1$W1hous12HH == 8, 8, NA_real_))))))))
  wave1$hownteen14 <- map_missing_values(wave1$hownteen14)
  wave1$hown14 <- ifelse(wave1$W1hous12HH %in% c(1, 2, 3), 1,
                      ifelse(wave1$W1hous12HH %in% c(4, 5, 6, 7, 8), 2, NA_real_))
  wave1$hown14 <- map_missing_values(wave1$hown14)
}

# Load wave 2 (age 15)
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
if ("W2Hous12HH" %in% names(wave2)) {
  wave2$W2Hous12HH <- map_missing_values(wave2$W2Hous12HH)
  wave2$hownteen15 <- ifelse(wave2$W2Hous12HH == 1, 1,
                           ifelse(wave2$W2Hous12HH == 2, 2,
                           ifelse(wave2$W2Hous12HH == 3, 3,
                           ifelse(wave2$W2Hous12HH == 4, 4,
                           ifelse(wave2$W2Hous12HH == 5, 5,
                           ifelse(wave2$W2Hous12HH == 6, 6,
                           ifelse(wave2$W2Hous12HH == 7, 7,
                           ifelse(wave2$W2Hous12HH == 8, 8, NA_real_))))))))
  wave2$hownteen15 <- map_missing_values(wave2$hownteen15)
  wave2$hown15 <- ifelse(wave2$W2Hous12HH %in% c(1, 2, 3), 1,
                      ifelse(wave2$W2Hous12HH %in% c(4, 5, 6, 7, 8), 2, NA_real_))
  wave2$hown15 <- map_missing_values(wave2$hown15)
}

# Merge waves 1 and 2
merged_data <- full_join(wave1, wave2, by = "NSID")

# Load wave 3 (age 16)
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
if ("W3hous12HH" %in% names(wave3)) {
  wave3$W3hous12HH <- map_missing_values(wave3$W3hous12HH)
  wave3$hownteen16 <- ifelse(wave3$W3hous12HH == 1, 1,
                           ifelse(wave3$W3hous12HH == 2, 2,
                           ifelse(wave3$W3hous12HH == 3, 3,
                           ifelse(wave3$W3hous12HH == 4, 4,
                           ifelse(wave3$W3hous12HH == 5, 5,
                           ifelse(wave3$W3hous12HH == 6, 6,
                           ifelse(wave3$W3hous12HH == 7, 7,
                           ifelse(wave3$W3hous12HH == 8, 8, NA_real_))))))))
  wave3$hownteen16 <- map_missing_values(wave3$hownteen16)
  wave3$hown16 <- ifelse(wave3$W3hous12HH %in% c(1, 2, 3), 1,
                      ifelse(wave3$W3hous12HH %in% c(4, 5, 6, 7, 8), 2, NA_real_))
  wave3$hown16 <- map_missing_values(wave3$hown16)
}

# Merge with wave 3
merged_data <- full_join(merged_data, wave3, by = "NSID")

# Select only the required variables
final_vars <- c("NSID", "hownteen14", "hown14", "hownteen15", "hown15", "hownteen16", "hown16")
final_data <- merged_data[, final_vars, drop = FALSE]

# Write the final dataset to CSV
readr::write_csv(final_data, "data/output/cleaned_data.csv")

# Print confirmation
cat("Cleaned dataset has been written to data/output/cleaned_data.csv\n")
