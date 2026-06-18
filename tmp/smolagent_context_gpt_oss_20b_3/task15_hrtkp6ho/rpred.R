library(readr)
library(dplyr)

# File paths
files <- list(
  wave_one_lsype_young_person_2020 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_four_lsype_young_person_2020 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_2015_derived = "data/input/ns8_2015_derived.tab",
  ns9_2022_derived_variables = "data/input/ns9_2022_derived_variables.tab"
)

# Load datasets
wave_one <- read_delim(files$wave_one_lsype_young_person_2020, delim = "\t", col_types = cols())
wave_four <- read_delim(files$wave_four_lsype_young_person_2020, delim = "\t", col_types = cols())
ns8 <- read_delim(files$ns8_2015_derived, delim = "\t", col_types = cols())
ns9 <- read_delim(files$ns9_2022_derived_variables, delim = "\t", col_types = cols())

# Merge all waves by NSID
full <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Helper function to map income bands to labelled factor
make_inc_factor <- function(raw, label_vec) {
  # Convert to numeric
  raw <- as.numeric(raw)
  # Replace NA with standard missing code -3
  raw[is.na(raw)] <- -3
  # Define factor levels: -1 (not applicable), -3 (missing), 1:16
  levels_vec <- c(-1, -3, 1:16)
  # Build labels for levels; use label_vec for 1:16, add custom labels for -1 and -3
  labels_vec <- c("Not applicable", "Not asked at fieldwork", label_vec)
  factor(raw, levels = levels_vec, labels = labels_vec, ordered = TRUE)
}

# Value labels for income bands (1:16) – copied from metadata
inc_labels <- c(
  "less than 25",
  "25 to 50",
  "50 to 90",
  "90 to 140",
  "140 to 240",
  "240 to 300",
  "300 to 350",
  "350 to 400",
  "400 to 500",
  "500 to 600",
  "600 to 700",
  "700 to 800",
  "800 to 900",
  "900 to 1200",
  "1200 to 1400",
  "more than 1400"
)

# Create income variables
inc25 <- make_inc_factor(full$W8DINCB, inc_labels)
inc32 <- make_inc_factor(full$W9DINCB, inc_labels)

# Construct final dataset
final_df <- data.frame(
  NSID = full$NSID,
  inc25 = inc25,
  inc32 = inc32,
  stringsAsFactors = FALSE
)

# Write CSV output
write_csv(final_df, "data/output/cleaned_data.csv")
