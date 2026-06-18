library(readr)
library(dplyr)
library(labelled)
library(haven)

# Create output directory if it doesn\'t exist
if(!dir.exists("data/output")) dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Function to map missing codes for W6MarStatYP
map_missing_w6 <- function(x) {
  y <- x
  y[is.na(y)] <- -3
  y[y == -997] <- -2
  y[y == -999] <- -2
  y[y == -97]  <- -9
  y[y == -92]  <- -9
  y[y == -91]  <- -1
  y[y == -1]   <- -8
  return(y)
}

# Function to map missing codes for adult variables
map_missing_w8 <- function(x) {
  y <- x
  y[is.na(y)] <- -3
  y[y == -9] <- -9
  y[y == -8] <- -8
  y[y == -1] <- -1
  return(y)
}

map_missing_w9 <- function(x) {
  y <- x
  y[is.na(y)] <- -3
  y[y == -9] <- -9
  y[y == -8] <- -8
  return(y)
}

# Collapse adult marital status into harmonised categories 1-5
collapse_w8_to_5 <- function(x) {
  y <- x
  y[y == 1] <- 1
  y[y == 2] <- 2
  y[y == 3] <- 3
  y[y == 4] <- 4
  y[y == 5] <- 5
  y[y == 6] <- 2
  y[y == 7] <- 3
  y[y == 8] <- 1
  y[y == 9] <- 5
  return(y)
}

collapse_w9_to_5 <- function(x) {
  y <- x
  y[y == 1] <- 1
  y[y == 2] <- 2
  y[y == 3] <- 3
  y[y == 4] <- 4
  y[y == 5] <- 5
  y[y == 6] <- 2
  y[y == 7] <- 3
  y[y == 8] <- 5
  return(y)
}

# Read all input files
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets preserving all IDs
df <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Derive partnership variables
df <- df %>%
  mutate(
    partnr19_raw = map_missing_w6(`W6MarStatYP`),
    partnr19 = partnr19_raw,
    partnradu25_raw = map_missing_w8(`W8DMARSTAT`),
    partnradu25 = partnradu25_raw,
    partnradu32_raw = map_missing_w9(`W9DMARSTAT`),
    partnradu32 = partnradu32_raw,
    partnr25_raw = collapse_w8_to_5(partnradu25),
    partnr25 = partnr25_raw,
    partnr32_raw = collapse_w9_to_5(partnradu32),
    partnr32 = partnr32_raw
  )

# Define labels with numeric values
partnr_labels <- c(
  "-9" = -9,
  "-8" = -8,
  "-7" = -7,
  "-3" = -3,
  "-2" = -2,
  "-1" = -1,
  "1"  = 1,
  "2"  = 2,
  "3"  = 3,
  "4"  = 4,
  "5"  = 5
)

partnradu25_labels <- c(
  "-9" = -9,
  "-8" = -8,
  "-1" = -1,
  "1"  = 1,
  "2"  = 2,
  "3"  = 3,
  "4"  = 4,
  "5"  = 5,
  "6"  = 6,
  "7"  = 7,
  "8"  = 8,
  "9"  = 9
)

partnradu32_labels <- c(
  "-8" = -8,
  "1"  = 1,
  "2"  = 2,
  "3"  = 3,
  "4"  = 4,
  "5"  = 5,
  "6"  = 6,
  "7"  = 7,
  "8"  = 8
)

# Apply labels
df <- df %>%
  mutate(
    partnr19   = labelled(partnr19,   partnr_labels),
    partnr25   = labelled(partnr25,   partnr_labels),
    partnr32   = labelled(partnr32,   partnr_labels),
    partnradu25 = labelled(partnradu25, partnradu25_labels),
    partnradu32 = labelled(partnradu32, partnradu32_labels)
  )

# Select final variables
final_df <- df %>% select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
