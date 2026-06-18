library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Set up file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_self_completion.tab",
  "ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(paste0("data/input/", files[1]), delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(paste0("data/input/", files[2]), delim = "\t", show_col_types = FALSE)
wave3 <- read_delim(paste0("data/input/", files[3]), delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(paste0("data/input/", files[4]), delim = "\t", show_col_types = FALSE)
wave6 <- read_delim(paste0("data/input/", files[5]), delim = "\t", show_col_types = FALSE)
wave7 <- read_delim(paste0("data/input/", files[6]), delim = "\t", show_col_types = FALSE)
wave8 <- read_delim(paste0("data/input/", files[7]), delim = "\t", show_col_types = FALSE)
wave9 <- read_delim(paste0("data/input/", files[8]), delim = "\t", show_col_types = FALSE)

# Define wave ages
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)

# Define alcohol variable names and recoding
# W1-W7: 1=Yes (has drunk), 2=No (never), NA=missing
# W8-W9 (AUDIT1): 1=Never, 2-5=has drunk, NA=missing

# Extract and recode alcohol ever variables
# I'll create a function to recode all the ever variables

code_alc_ever <- function(x) {
  # NA and missing codes (negative values)
  x[is.na(x) | x >= -999 & x <= -91] <- NA_integer_
  # 1 = has drunk
  # 2 = never drank
  return(as.integer(x))
}

code_audit_ever <- function(x) {
  # AUDIT1: 1=Never, 2-5=has drunk
  x[is.na(x) | x >= -9 & x <= -1] <- NA_integer_
  # Convert: 1->0 (never), 2-5->1 (has drunk)
  x[x == 1] <- 0L
  x[x >= 2 & x <= 5] <- 1L
  return(as.integer(x))
}

# Apply recoding
wave1$W1alceverYP <- code_alc_ever(wave1$W1alceverYP)
wave2$W2alceverYP <- code_alc_ever(wave2$W2alceverYP)
wave3$W3alceverYP <- code_alc_ever(wave3$W3alceverYP)
wave4$W4AlcEverYP <- code_alc_ever(wave4$W4AlcEverYP)
wave6$W6AlcEverYP <- code_alc_ever(wave6$W6AlcEverYP)
wave7$W7AlcEverYP <- code_alc_ever(wave7$W7AlcEverYP)
wave8$W8AUDIT1 <- code_audit_ever(wave8$W8AUDIT1)
wave9$W9AUDIT1 <- code_audit_ever(wave9$W9AUDIT1)

# Merge all datasets by NSID
result <- wave1
result <- full_join(result, wave2, by = "NSID")
result <- full_join(result, wave3, by = "NSID")
result <- full_join(result, wave4, by = "NSID")
result <- full_join(result, wave6, by = "NSID")
result <- full_join(result, wave7, by = "NSID")
result <- full_join(result, wave8, by = "NSID")
result <- full_join(result, wave9, by = "NSID")

# Create has_drank indicators (1=has drunk at this wave, NA=missing)
result <- result %>%
  mutate(
    drank_w1 = as.integer(!is.na(W1alceverYP) & W1alceverYP == 1),
    drank_w2 = as.integer(!is.na(W2alceverYP) & W2alceverYP == 1),
    drank_w3 = as.integer(!is.na(W3alceverYP) & W3alceverYP == 1),
    drank_w4 = as.integer(!is.na(W4AlcEverYP) & W4AlcEverYP == 1),
    drank_w6 = as.integer(!is.na(W6AlcEverYP) & W6AlcEverYP == 1),
    drank_w7 = as.integer(!is.na(W7AlcEverYP) & W7AlcEverYP == 1),
    drank_w8 = as.integer(!is.na(W8AUDIT1) & W8AUDIT1 == 1),
    drank_w9 = as.integer(!is.na(W9AUDIT1) & W9AUDIT1 == 1)
  )

# Create alcfst: earliest age of alcohol consumption
# If never found valid "Yes" (drank=1), alcfst = 99
result <- result %>%
  group_by(NSID) %>%
  summarise(
    alcfst = case_when(
      any(drank_w1 == 1, na.rm = TRUE) ~ 14L,
      any(drank_w2 == 1, na.rm = TRUE) ~ 15L,
      any(drank_w3 == 1, na.rm = TRUE) ~ 16L,
      any(drank_w4 == 1, na.rm = TRUE) ~ 17L,
      any(drank_w6 == 1, na.rm = TRUE) ~ 19L,
      any(drank_w7 == 1, na.rm = TRUE) ~ 20L,
      any(drank_w8 == 1, na.rm = TRUE) ~ 25L,
      any(drank_w9 == 1, na.rm = TRUE) ~ 32L,
      TRUE ~ 99L
    ),
    .groups = "drop"
  )

# Select only NSID and alcfst for output
result <- result %>%
  select(NSID, alcfst)

# Write output
write_csv(result, "data/output/cleaned_data.csv")

cat("Preview of first 10 rows:\n")
print(head(result, 10))
cat("\nTotal rows:", nrow(result), "\n")
cat("Unique alcfst values:", sort(unique(result$alcfst)), "\n")
cat("Count of alcfst=99 (never drank):", sum(result$alcfst == 99), "\n")