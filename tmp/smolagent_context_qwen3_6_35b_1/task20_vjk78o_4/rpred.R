library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(labelled)

# Load all sweep files
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
all_data <- s1 %>%
  full_join(s2, by = "NSID") %>%
  full_join(s3, by = "NSID") %>%
  full_join(s4, by = "NSID") %>%
  full_join(s6, by = "NSID") %>%
  full_join(s7, by = "NSID") %>%
  full_join(s8, by = "NSID") %>%
  full_join(s9, by = "NSID")

# --- Create drinking indicators for each sweep ---

# Helper: convert NAs to -3
na_to_missing <- function(x) {
  ifelse(is.na(x), -3, as.numeric(x))
}

# Sweep 1 (Age 14): BOTH W1alceverYP = 1 AND W1alcmonYP = 1
s1 <- s1 %>%
  mutate(
    W1alceverYP = na_to_missing(W1alceverYP),
    W1alcmonYP = na_to_missing(W1alcmonYP),
    S1_drink = case_when(
      W1alceverYP == 1 & W1alcmonYP == 1 ~ 1L,
      W1alceverYP == 2 & W1alcmonYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 2 (Age 15)
s2 <- s2 %>%
  mutate(
    W2alceverYP = na_to_missing(W2alceverYP),
    S2_drink = case_when(
      W2alceverYP == 1 ~ 1L,
      W2alceverYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 3 (Age 16)
s3 <- s3 %>%
  mutate(
    W3alceverYP = na_to_missing(W3alceverYP),
    S3_drink = case_when(
      W3alceverYP == 1 ~ 1L,
      W3alceverYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 4 (Age 17)
s4 <- s4 %>%
  mutate(
    W4AlcEverYP = na_to_missing(W4AlcEverYP),
    S4_drink = case_when(
      W4AlcEverYP == 1 ~ 1L,
      W4AlcEverYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 6 (Age 19)
s6 <- s6 %>%
  mutate(
    W6AlcEverYP = na_to_missing(W6AlcEverYP),
    S6_drink = case_when(
      W6AlcEverYP == 1 ~ 1L,
      W6AlcEverYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 7 (Age 20)
s7 <- s7 %>%
  mutate(
    W7AlcEverYP = na_to_missing(W7AlcEverYP),
    S7_drink = case_when(
      W7AlcEverYP == 1 ~ 1L,
      W7AlcEverYP == 2 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 8 (Age 25): AUDIT > 1 -> drinking
s8 <- s8 %>%
  mutate(
    W8AUDIT1 = na_to_missing(W8AUDIT1),
    S8_drink = case_when(
      W8AUDIT1 > 1 ~ 1L,
      W8AUDIT1 == 1 ~ -2L,
      TRUE ~ -3L
    )
  )

# Sweep 9 (Age 32): AUDIT > 1 -> drinking
s9 <- s9 %>%
  mutate(
    W9AUDIT1 = na_to_missing(W9AUDIT1),
    S9_drink = case_when(
      W9AUDIT1 > 1 ~ 1L,
      W9AUDIT1 == 1 ~ -2L,
      TRUE ~ -3L
    )
  )

# Merge all into final data frame
final_data <- s1 %>%
  select(NSID, S1_drink) %>%
  full_join(s2 %>% select(NSID, S2_drink), by = "NSID") %>%
  full_join(s3 %>% select(NSID, S3_drink), by = "NSID") %>%
  full_join(s4 %>% select(NSID, S4_drink), by = "NSID") %>%
  full_join(s6 %>% select(NSID, S6_drink), by = "NSID") %>%
  full_join(s7 %>% select(NSID, S7_drink), by = "NSID") %>%
  full_join(s8 %>% select(NSID, S8_drink), by = "NSID") %>%
  full_join(s9 %>% select(NSID, S9_drink), by = "NSID")

# --- Derive alcfst: earliest age of first alcohol consumption ---
derive_alcfst <- function(S1, S2, S3, S4, S6, S7, S8, S9) {
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  vals <- c(S1, S2, S3, S4, S6, S7, S8, S9)
  
  # If all values are NA (person not in any sweep), return -8
  if (all(is.na(vals))) {
    return(-8L)
  }
  
  # If any drinking observed (1), return minimum age where drinking occurred
  if (any(vals == 1, na.rm = TRUE)) {
    earliest_idx <- which(vals == 1)[1]
    return(as.integer(ages[earliest_idx]))
  }
  
  # If all observed show not-drinking (-2) and no missing values
  if (all(vals == -2, na.rm = FALSE) && !any(is.na(vals))) {
    return(99L)
  }
  
  # Otherwise (no drinking observed but at least one missing), assign -8
  return(-8L)
}

# Apply to each row
final_data <- final_data %>%
  rowwise() %>%
  mutate(alcfst = derive_alcfst(S1_drink, S2_drink, S3_drink, S4_drink,
                                 S6_drink, S7_drink, S8_drink, S9_drink)) %>%
  ungroup()

# Convert to factor with specified levels and labels
final_data <- final_data %>%
  mutate(
    alcfst = factor(
      alcfst,
      levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
      labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20",
                 "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")
    )
  )

# Select only NSID and final derived variable
output <- final_data %>%
  select(NSID, alcfst)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Done. Output has", nrow(output), "rows and", ncol(output), "columns.\n")
cat("alcfst distribution:\n")
print(table(output$alcfst, useNA = "ifany"))
