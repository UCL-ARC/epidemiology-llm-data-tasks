
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets
merged_data <- full_join(wave_one, wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Create educaim17 (Age 17)
if (!"w4saim" %in% names(merged_data)) {
  stop("Required variable w4saim not found in wave 4 data")
}
merged_data$educaim17 <- case_when(
  merged_data$w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,
  merged_data$w4saim %in% c(12) ~ 3,
  merged_data$w4saim %in% c(13) ~ 3,
  merged_data$w4saim %in% c(14) ~ 5,
  merged_data$w4saim == -94 ~ -8,
  merged_data$w4saim == -91 ~ -1,
  TRUE ~ -3
)

# Create educaim19 (Age 19)
if (!"W6Saim" %in% names(merged_data)) {
  stop("Required variable W6Saim not found in wave 6 data")
}
merged_data$educaim19 <- case_when(
  merged_data$W6Saim %in% c(1, 2, 3, 4) ~ 0,
  merged_data$W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 1,
  merged_data$W6Saim %in% c(13) ~ 3,
  merged_data$W6Saim %in% c(14, 15) ~ 3,
  merged_data$W6Saim == 16 ~ 5,
  merged_data$W6Saim == -999 ~ -3,
  TRUE ~ -3
)

# Create educaim20 (Age 20)
if (!"W7SAim" %in% names(merged_data)) {
  stop("Required variable W7SAim not found in wave 7 data")
}
merged_data$educaim20 <- case_when(
  merged_data$W7SAim %in% c(10, 11, 12, 13) ~ 0,
  merged_data$W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
  merged_data$W7SAim %in% c(14) ~ 3,
  merged_data$W7SAim == -91 ~ -1,
  merged_data$W7SAim == -94 ~ -8,
  TRUE ~ -3
)

# Create educaim25 (Age 25)
if (!"W8ACTIVITY05" %in% names(merged_data)) {
  stop("Required variable W8ACTIVITY05 not found in wave 8 data")
}

currently_studying_25 <- merged_data$W8ACTIVITY05 %in% c(0, -1, -2, -3, -8, -9)

# NVQ 4-5 qualifications (higher education)
NVQ_4_5_cols_25 <- c("W8ACQUC0A", "W8ACQUC0B", "W8ACQUC0C", "W8ACQUC0D", "W8ACQUC0E", "W8VCQUC0A", "W8VCQUC0J", "W8VCQUC0K")
NVQ_4_5_cols_25 <- NVQ_4_5_cols_25[NVQ_4_5_cols_25 %in% names(merged_data)]
NVQ_4_5_ticked_25 <- if (length(NVQ_4_5_cols_25) > 0) {
  rowSums(merged_data %>% select(all_of(NVQ_4_5_cols_25)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# NVQ 1-3 qualifications (lower/mid-level)
NVQ_1_3_cols_25 <- c("W8ACQUC0F", "W8ACQUC0G", "W8ACQUC0H", "W8ACQUC0I", "W8VCQUC0B", "W8VCQUC0C", "W8VCQUC0D", "W8VCQUC0E")
NVQ_1_3_cols_25 <- NVQ_1_3_cols_25[NVQ_1_3_cols_25 %in% names(merged_data)]
NVQ_1_3_ticked_25 <- if (length(NVQ_1_3_cols_25) > 0) {
  rowSums(merged_data %>% select(all_of(NVQ_1_3_cols_25)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# Entry level qualifications
entry_level_cols_25 <- c("W8VCQUC0D", "W8VCQUC0F")
entry_level_cols_25 <- entry_level_cols_25[entry_level_cols_25 %in% names(merged_data)]
entry_level_ticked_25 <- if (length(entry_level_cols_25) > 0) {
  rowSums(merged_data %>% select(all_of(entry_level_cols_25)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# Other qualifications
other_cols_25 <- grep("^W8VCQUC0|^W8ACQUC0", names(merged_data), value = TRUE)
other_ticked_25 <- if (length(other_cols_25) > 0) {
  rowSums(merged_data %>% select(all_of(other_cols_25)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# None of these qualifications
none_of_these_ticked_25 <- if ("W8ACQUC0O" %in% names(merged_data)) {
  merged_data$W8ACQUC0O == 1
} else {
  FALSE
}

merged_data$educaim25 <- case_when(
  currently_studying_25 ~ 5,
  NVQ_4_5_ticked_25 ~ 0,
  NVQ_1_3_ticked_25 ~ 1,
  entry_level_ticked_25 ~ 2,
  other_ticked_25 ~ 3,
  none_of_these_ticked_25 ~ 4,
  TRUE ~ -3
)

# Create educaim32 (Age 32)
if (!"W9ECONACT2" %in% names(merged_data)) {
  stop("Required variable W9ECONACT2 not found in wave 9 data")
}

currently_studying_32 <- merged_data$W9ECONACT2 %in% c(0, -1, -2, -3, -8, -9)

# NVQ 4-5 qualifications (higher education)
NVQ_4_5_cols_32 <- grep("^W9ACQUC0[A-E]|^W9VCQUC0[A,C]", names(merged_data), value = TRUE)
NVQ_4_5_ticked_32 <- if (length(NVQ_4_5_cols_32) > 0) {
  rowSums(merged_data %>% select(all_of(NVQ_4_5_cols_32)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# NVQ 1-3 qualifications (lower/mid-level)
NVQ_1_3_cols_32 <- grep("^W9ACQUC0[F-I]|^W9VCQUC0[B-E]", names(merged_data), value = TRUE)
NVQ_1_3_ticked_32 <- if (length(NVQ_1_3_cols_32) > 0) {
  rowSums(merged_data %>% select(all_of(NVQ_1_3_cols_32)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# Entry level qualifications
entry_level_cols_32 <- grep("^W9VCQUC0[FKQ]", names(merged_data), value = TRUE)
entry_level_ticked_32 <- if (length(entry_level_cols_32) > 0) {
  rowSums(merged_data %>% select(all_of(entry_level_cols_32)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# Other qualifications
other_cols_32 <- grep("^W9VCQUC0|^W9ACQUC0", names(merged_data), value = TRUE)
other_ticked_32 <- if (length(other_cols_32) > 0) {
  rowSums(merged_data %>% select(all_of(other_cols_32)) == 1, na.rm = TRUE) > 0
} else {
  FALSE
}

# None of these qualifications
none_of_these_ticked_32 <- if ("W9ACQUC0S" %in% names(merged_data)) {
  merged_data$W9ACQUC0S == 1
} else {
  FALSE
}

merged_data$educaim32 <- case_when(
  currently_studying_32 ~ 5,
  NVQ_4_5_ticked_32 ~ 0,
  NVQ_1_3_ticked_32 ~ 1,
  entry_level_ticked_32 ~ 2,
  other_ticked_32 ~ 3,
  none_of_these_ticked_32 ~ 4,
  TRUE ~ -3
)

# Create labeled factors for all educaim variables
for (age in c(17, 19, 20, 25, 32)) {
  var_name <- paste0("educaim", age)
  if (var_name %in% names(merged_data)) {
    merged_data[[var_name]] <- factor(
      merged_data[[var_name]],
      levels = c(-9, -8, -7, -3, -2, -1, 0, 1, 2, 3, 4, 5),
      labels = c(
        "Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable",
        "Item not applicable", "NVQ 4-5 equivalent", "NVQ 1-3 equivalent", "None/entry level",
        "Other (level unknown)", "None of these", "Not currently studying"
      )
    )
  }
}

# Verify all educaim variables were created
educaim_vars <- c("educaim17", "educaim19", "educaim20", "educaim25", "educaim32")
missing_vars <- educaim_vars[!educaim_vars %in% names(merged_data)]
if (length(missing_vars) > 0) {
  stop(paste("Missing educaim variables:", paste(missing_vars, collapse = ", ")))
}

# Select only NSID and educaim variables
final_data <- merged_data %>% select(NSID, starts_with("educaim"))

# Write output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

# Verify output file was created
if (!file.exists("data/output/cleaned_data.csv")) {
  stop("Output file was not created successfully")
}

# Print summary of the output
cat("Data cleaning completed successfully!")
cat("Output file:", "data/output/cleaned_data.csv")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")
cat("Columns:", paste(names(final_data), collapse = ", "), "\n")
