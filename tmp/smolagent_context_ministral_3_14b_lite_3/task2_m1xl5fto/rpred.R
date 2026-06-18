
# Load required libraries
library(dplyr)
library(readr)

# Load and merge data
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

merged <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

# Create a simple function to map missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x[x == -998 | x == -997 | x == -995] <- -2
  x[x == -99] <- -3
  x[x == -8] <- -8
  x[x == -9] <- -9
  return(x)
}

# Extract ethnicity variables with explicit checks and handling
eth_values <- list()

# Wave 1 - Using W1ethnicYP instead of W1ethnic2YP
if("W1ethnicYP" %in% names(merged)) {
  eth_values$wave1 <- map_missing(merged$W1ethnicYP)
} else {
  eth_values$wave1 <- rep(-3, nrow(merged))
}

# Wave 2
if("W2ethnicYP" %in% names(merged)) {
  eth_values$wave2 <- map_missing(merged$W2ethnicYP)
} else {
  eth_values$wave2 <- rep(-3, nrow(merged))
}

# Wave 4
if("w4ethnic2YP" %in% names(merged)) {
  eth_values$wave4 <- map_missing(merged$w4ethnic2YP)
} else {
  eth_values$wave4 <- rep(-3, nrow(merged))
}

# Wave 8
if("W8DETHN15" %in% names(merged)) {
  eth_values$wave8 <- map_missing(merged$W8DETHN15)
} else {
  eth_values$wave8 <- rep(-3, nrow(merged))
}

# Wave 9
if("W9DETHN15" %in% names(merged)) {
  eth_values$wave9 <- map_missing(merged$W9DETHN15)
} else {
  eth_values$wave9 <- rep(-3, nrow(merged))
}

# Create final dataset with consolidated ethnicity
final_data <- data.frame(
  NSID = merged$NSID,
  eth = pmax(
    ifelse(eth_values$wave1 %in% 1:16, eth_values$wave1, -3),
    ifelse(eth_values$wave2 %in% 1:16, eth_values$wave2, -3),
    ifelse(eth_values$wave4 %in% 1:16, eth_values$wave4, -3),
    ifelse(eth_values$wave8 %in% 1:16, eth_values$wave8, -3),
    ifelse(eth_values$wave9 %in% 1:16, eth_values$wave9, -3),
    -3
  )
)

# Define factor levels and labels
levels <- c(-9, -8, -3, -2, -1, 1:16)
labels <- c(
  "Refused", "Insufficient information", "Not interviewed",
  "Schedule not applicable", "Item not applicable",
  "White - British", "White - Irish", "Any other White background",
  "Mixed - White and Black Caribbean", "Mixed - White and Black African",
  "Mixed - White and Asian", "Any other mixed background",
  "Indian", "Pakistani", "Bangladeshi", "Any other Asian background",
  "Black Caribbean", "Black African", "Any other Black background",
  "Chinese", "Any other ethnic background"
)

# Create labeled factor
final_data$eth <- factor(
  final_data$eth,
  levels = levels,
  labels = labels
)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
