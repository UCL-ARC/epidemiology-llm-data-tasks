# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab",
  "data/input/wave_five_lsype_family_background_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_main_interview.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load files
w1 <- read_delim(files[1], delim = "\t")
w2 <- read_delim(files[2], delim = "\t")
w3 <- read_delim(files[3], delim = "\t")
w4 <- read_delim(files[4], delim = "\t")
w5 <- read_delim(files[5], delim = "\t")
w6 <- read_delim(files[6], delim = "\t")
w7 <- read_delim(files[7], delim = "\t")
w8 <- read_delim(files[8], delim = "\t")
w9 <- read_delim(files[9], delim = "\t")

# Merge using full_join by NSID
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w3, by = "NSID")
df <- full_join(df, w4, by = "NSID")
df <- full_join(df, w5, by = "NSID")
df <- full_join(df, w6, by = "NSID")
df <- full_join(df, w7, by = "NSID")
df <- full_join(df, w8, by = "NSID")
df <- full_join(df, w9, by = "NSID")

# Function to handle missing codes properly
handle_missing <- function(val) {
  if (is.na(val)) return(-3)
  if (val == -92) return(-9)  # Refusal
  if (val == -91) return(-1)  # Not applicable
  if (val == -1) return(-8)  # Don't know
  if (val == -999) return(-3)  # Not asked
  if (val == -99) return(-3)  # Not asked
  if (val == -997) return(-2)  # Script error
  if (val == -998) return(-2)  # Script error
  if (val == -995) return(-2)  # Script error
  if (val == -9) return(-9)  # Refusal
  if (val == -8) return(-8)  # Don't know
  return(NA)  # Not a missing code
}

# Map adolescent wave codes to collapsed scheme
map_adolescent_tenure <- function(val, wave) {
  miss <- handle_missing(val)
  if (!is.na(miss)) return(miss)
  
  # Wave 1 (Age 14) - W1hous12HH codes
  if (wave == "w1") {
    if (val == 1) return(1)
    if (val == 2) return(2)
    if (val == 3) return(3)
    if (val %in% c(4, 5, 6)) return(4)
    if (val == 7) return(5)
    if (val == 8) return(7)
  }
  
  # Wave 2 (Age 15) - W2Hous12HH codes
  if (wave == "w2") {
    if (val == 1) return(1)
    if (val == 2) return(2)
    if (val == 3) return(3)
    if (val %in% c(4, 5, 6)) return(4)
    if (val == 7) return(5)
    if (val == 8) return(7)
  }
  
  # Wave 3 (Age 16) - W3hous12HH codes
  if (wave == "w3") {
    if (val == 1) return(1)
    if (val == 2) return(2)
    if (val == 3) return(3)
    if (val %in% c(4, 5, 6)) return(4)
    if (val == 7) return(5)
    if (val == 8) return(7)
  }
  
  # Wave 4 (Age 17) - W4Hous12HH codes
  if (wave == "w4") {
    if (val == 1) return(1)
    if (val == 2) return(2)
    if (val == 3) return(3)
    if (val %in% c(4, 5, 6)) return(4)
    if (val == 7) return(5)
    if (val == 8) return(7)
  }
  
  return(7)
}

# Create detailed adolescent variables (hownteen[age])
# Age 14 - W1hous12HH
df$hownteen14 <- sapply(df$W1hous12HH, map_adolescent_tenure, wave = "w1")

# Age 15 - W2Hous12HH
df$hownteen15 <- sapply(df$W2Hous12HH, map_adolescent_tenure, wave = "w2")

# Age 16 - W3hous12HH
df$hownteen16 <- sapply(df$W3hous12HH, map_adolescent_tenure, wave = "w3")

# Age 17 - W4Hous12HH
df$hownteen17 <- sapply(df$W4Hous12HH, map_adolescent_tenure, wave = "w4")

# Age 18 - W5 (complex: W5Hous12HH + W5Hous12BHH + W5Hous12CHH)
map_w5_tenure <- function(type, owned, rented) {
  result <- numeric(length(type))
  for (i in seq_along(type)) {
    t <- type[i]
    o <- owned[i]
    r <- rented[i]
    
    # Check if type is missing
    miss_t <- handle_missing(t)
    if (!is.na(miss_t)) {
      result[i] <- miss_t
      next
    }
    
    if (t == 1) {  # Owned
      miss_o <- handle_missing(o)
      if (!is.na(miss_o)) {
        result[i] <- miss_o
      } else if (o == 1) result[i] <- 1
      else if (o == 2) result[i] <- 2
      else if (o == 3) result[i] <- 3
      else if (o == 4) result[i] <- 7
      else result[i] <- 7
    } else if (t == 2) {  # Rented
      miss_r <- handle_missing(r)
      if (!is.na(miss_r)) {
        result[i] <- miss_r
      } else if (r == 1) result[i] <- 4
      else if (r == 2) result[i] <- 4
      else if (r == 3) result[i] <- 4
      else if (r == 4) result[i] <- 5
      else if (r == 5) result[i] <- 7
      else result[i] <- 7
    } else if (t == 3) {  # Something else
      result[i] <- 7
    } else {
      result[i] <- -3
    }
  }
  result
}

df$hownteen18 <- map_w5_tenure(df$W5Hous12HH, df$W5Hous12BHH, df$W5Hous12CHH)

# Age 19 - W6 (complex: W6Hous12YP + W6Hous12bYP + W6Hous12cYP)
map_w6_tenure <- function(type, owned, rented) {
  result <- numeric(length(type))
  for (i in seq_along(type)) {
    t <- type[i]
    o <- owned[i]
    r <- rented[i]
    
    # Check if type is missing
    miss_t <- handle_missing(t)
    if (!is.na(miss_t)) {
      result[i] <- miss_t
      next
    }
    
    if (t == 1) {  # Owned
      miss_o <- handle_missing(o)
      if (!is.na(miss_o)) {
        result[i] <- miss_o
      } else if (o == 1) result[i] <- 1
      else if (o == 2) result[i] <- 2
      else if (o == 3) result[i] <- 3
      else if (o == 4) result[i] <- 7
      else result[i] <- 7
    } else if (t == 2) {  # Rented
      miss_r <- handle_missing(r)
      if (!is.na(miss_r)) {
        result[i] <- miss_r
      } else if (r == 1) result[i] <- 4
      else if (r == 2) result[i] <- 4
      else if (r == 3) result[i] <- 4
      else if (r == 4) result[i] <- 5
      else if (r == 5) result[i] <- 7
      else result[i] <- 7
    } else if (t == 3) {  # Something else
      result[i] <- 7
    } else {
      result[i] <- -3
    }
  }
  result
}

df$hownteen19 <- map_w6_tenure(df$W6Hous12YP, df$W6Hous12bYP, df$W6Hous12cYP)

# Age 20 - W7 (complex: W7Hous12YP + W7Hous12bYP + W7Hous12cYP)
map_w7_tenure <- function(type, owned, rented) {
  result <- numeric(length(type))
  for (i in seq_along(type)) {
    t <- type[i]
    o <- owned[i]
    r <- rented[i]
    
    # Check if type is missing
    miss_t <- handle_missing(t)
    if (!is.na(miss_t)) {
      result[i] <- miss_t
      next
    }
    
    if (t == 1) {  # Owned
      miss_o <- handle_missing(o)
      if (!is.na(miss_o)) {
        result[i] <- miss_o
      } else if (o == 1) result[i] <- 1
      else if (o == 2) result[i] <- 2
      else if (o == 3) result[i] <- 3
      else if (o == 4) result[i] <- 7
      else result[i] <- 7
    } else if (t == 2) {  # Rented
      miss_r <- handle_missing(r)
      if (!is.na(miss_r)) {
        result[i] <- miss_r
      } else if (r == 1) result[i] <- 4
      else if (r == 2) result[i] <- 4
      else if (r == 3) result[i] <- 4
      else if (r == 4) result[i] <- 5
      else if (r == 5) result[i] <- 7
      else result[i] <- 7
    } else if (t == 3) {  # Something else
      result[i] <- 7
    } else {
      result[i] <- -3
    }
  }
  result
}

df$hownteen20 <- map_w7_tenure(df$W7Hous12YP, df$W7Hous12bYP, df$W7Hous12cYP)

# Create collapsed variables (hown[age]) - same as detailed for adolescents
df$hown14 <- df$hownteen14
df$hown15 <- df$hownteen15
df$hown16 <- df$hownteen16
df$hown17 <- df$hownteen17
df$hown18 <- df$hownteen18
df$hown19 <- df$hownteen19
df$hown20 <- df$hownteen20

# Age 25 (W8TENURE) - recode to collapsed scheme
map_w8_tenure <- function(val) {
  miss <- handle_missing(val)
  if (!is.na(miss)) return(miss)
  
  if (val == 1) return(1)
  if (val == 2) return(2)
  if (val == 3) return(3)
  if (val == 4) return(4)
  if (val == 5) return(5)
  if (val == 6) return(6)
  if (val == 7) return(7)
  return(-3)
}

df$hown25 <- sapply(df$W8TENURE, map_w8_tenure)

# Age 32 (W9DTENURE) - recode to collapsed scheme
map_w9_tenure <- function(val) {
  miss <- handle_missing(val)
  if (!is.na(miss)) return(miss)
  
  if (val == 1) return(1)
  if (val == 2) return(2)
  if (val == 3) return(3)
  if (val == 4) return(4)
  if (val == 5) return(5)
  if (val == 6) return(6)
  if (val == 7) return(7)
  return(-3)
}

df$hown32 <- sapply(df$W9DTENURE, map_w9_tenure)

# Keep only required variables
output_vars <- c("NSID", 
                 "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20", "hown25", "hown32",
                 "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18", "hownteen19", "hownteen20")

cleaned_data <- df[, output_vars]

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")
