library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
data_w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
data_w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
data_w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
data_w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
data_w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# W1ethnic2YP (age 14): -999=-2, -94=-8, -92=-9, -91=-1, -1=-8, NA=-3
map_w1 <- function(x) {
  result <- rep(x, length.out = length(x))
  result[x == -999] <- -2
  result[x == -94] <- -8
  result[x == -92] <- -9
  result[x == -91] <- -1
  result[x == -1] <- -8
  result[is.na(result)] <- -3
  result
}

# W2ethnicYP (age 15): -998=-3, -997=-2, -995=-2, -99=-3, -92=-9, -91=-1, -1=-8, NA=-3
map_w2 <- function(x) {
  result <- rep(x, length.out = length(x))
  result[x == -998] <- -3
  result[x == -997] <- -2
  result[x == -995] <- -2
  result[x == -99] <- -3
  result[x == -92] <- -9
  result[x == -91] <- -1
  result[x == -1] <- -8
  result[is.na(result)] <- -3
  result
}

# w4ethnic2YP (age 17): -94=-8, -1=-8, NA=-3
map_w4 <- function(x) {
  result <- rep(x, length.out = length(x))
  result[x == -94] <- -8
  result[x == -1] <- -8
  result[is.na(result)] <- -3
  result
}

# W8DETHN15 (age 25): -9=-9, -8=-8, -1=-1, NA=-3
map_w8 <- function(x) {
  result <- rep(x, length.out = length(x))
  result[x == -9] <- -9
  result[x == -8] <- -8
  result[x == -1] <- -1
  result[is.na(result)] <- -3
  result
}

# W9DETHN15 (age 32): -8=-8, NA=-3
map_w9 <- function(x) {
  result <- rep(x, length.out = length(x))
  result[x == -8] <- -8
  result[is.na(result)] <- -3
  result
}

# Apply mappings
data_w1$W1ethnic2YP_mapped <- map_w1(data_w1$W1ethnic2YP)
data_w2$W2ethnicYP_mapped <- map_w2(data_w2$W2ethnicYP)
data_w4$w4ethnic2YP_mapped <- map_w4(data_w4$w4ethnic2YP)
data_w8$W8DETHN15_mapped <- map_w8(data_w8$W8DETHN15)
data_w9$W9DETHN15_mapped <- map_w9(data_w9$W9DETHN15)

# Merge all datasets
data_merged <- full_join(data_w1, data_w2, by = "NSID")
data_merged <- full_join(data_merged, data_w4, by = "NSID")
data_merged <- full_join(data_merged, data_w8, by = "NSID")
data_merged <- full_join(data_merged, data_w9, by = "NSID")

# Define category labels (common scheme)
labels <- c(
  "White - British" = 1,
  "White - Irish" = 2,
  "Any other White background" = 3,
  "Mixed - White and Black Caribbean" = 4,
  "Mixed - White and Black African" = 5,
  "Mixed - White and Asian" = 6,
  "Any other mixed background" = 7,
  "Indian" = 8,
  "Pakistani" = 9,
  "Bangladeshi" = 10,
  "Any other Asian background" = 11,
  "Black Caribbean" = 12,
  "Black African" = 13,
  "Any other Black background" = 14,
  "Chinese" = 15,
  "Any other ethnic background" = 16
)

# Create eth variable using earliest-valid-first
eth <- data_merged %>%
  mutate(
    eth = case_when(
      !is.na(W1ethnic2YP_mapped) ~ W1ethnic2YP_mapped,
      !is.na(W2ethnicYP_mapped) ~ W2ethnicYP_mapped,
      !is.na(w4ethnic2YP_mapped) ~ w4ethnic2YP_mapped,
      !is.na(W8DETHN15_mapped) ~ W8DETHN15_mapped,
      !is.na(W9DETHN15_mapped) ~ W9DETHN15_mapped,
      TRUE ~ NA_integer_
    ),
    .after = "NSID"
  ) %>%
  select(NSID, eth)

# Create final output with properly labelled factor - exclude NA from levels
final_data <- data.frame(
  NSID = eth$NSID,
  eth = factor(eth$eth, levels = 1:16, labels = labels, exclude = NA)
)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

cat("First 10 rows:\n")
print(head(final_data, 10))
cat("\nEth variable summary:\n")
print(summary(final_data$eth))
}]