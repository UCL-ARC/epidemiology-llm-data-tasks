library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Ensure output directory exists
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

# File paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_self_completion.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

# Helper to read a .tab file and standardise column names
read_tab <- function(path) {
  df <- read_delim(path, delim = "\t", col_types = cols(.default = "c"))
  # Trim whitespace from column names
  names(df) <- trimws(names(df))
  # Ensure 'NSID' column exists; if not, try case-insensitive match
  if (!"NSID" %in% names(df)) {
    idx <- which(tolower(names(df)) == "nsid")
    if (length(idx) == 1) names(df)[idx] <- "NSID"
  }
  return(df)
}

# Read all files
wave_one <- read_tab(file_paths$wave_one)
wave_four <- read_tab(file_paths$wave_four)
wave_six <- read_tab(file_paths$wave_six)
wave_seven <- read_tab(file_paths$wave_seven)
wave_eight <- read_tab(file_paths$wave_eight)
wave_nine <- read_tab(file_paths$wave_nine)

# Verify NSID column present in each
stopifnot(all(c("NSID") %in% names(wave_one)))
stopifnot(all(c("NSID") %in% names(wave_four)))
stopifnot(all(c("NSID") %in% names(wave_six)))
stopifnot(all(c("NSID") %in% names(wave_seven)))
stopifnot(all(c("NSID") %in% names(wave_eight)))
stopifnot(all(c("NSID") %in% names(wave_nine)))

# Merge datasets by NSID
merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Convert relevant variables to numeric (if they exist)
merged <- merged %>%
  mutate(
    W6SexualityYP = as.numeric(W6SexualityYP),
    W7SexualityYP = as.numeric(W7SexualityYP),
    W8SEXUALITY = as.numeric(W8SEXUALITY),
    W9SORI = as.numeric(W9SORI)
  )

# Function to recode missing values according to mapping
recode_missing <- function(x, mapping) {
  out <- x
  for (k in names(mapping)) {
    out[!is.na(out) & out == as.numeric(k)] <- mapping[[k]]
  }
  out[is.na(out)] <- -3L  # standard missing for not asked at fieldwork stage
  return(out)
}

# Define missing-value mappings for each wave
w6_map <- c("-97" = "-2", "-92" = "-9", "-91" = "-1", "-1" = "-8")
w7_map <- c("-100" = "-2", "-97" = "-9", "-92" = "-9", "-91" = "-1", "-1" = "-8")
w8_map <- c("-9" = "-9", "-8" = "-8", "-1" = "-1")
w9_map <- c("-9" = "-9", "-8" = "-8", "-3" = "-3", "-1" = "-1", "5" = "-7")

# Create final sexual orientation variables
merged <- merged %>%
  mutate(
    sori19 = recode_missing(W6SexualityYP, w6_map),
    sori20 = recode_missing(W7SexualityYP, w7_map),
    sori25 = recode_missing(W8SEXUALITY, w8_map),
    sori32 = recode_missing(W9SORI, w9_map)
  )

# Keep only ID and final derived variables
final_data <- merged %>% select(NSID, sori19, sori20, sori25, sori32)

# Write cleaned data to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
