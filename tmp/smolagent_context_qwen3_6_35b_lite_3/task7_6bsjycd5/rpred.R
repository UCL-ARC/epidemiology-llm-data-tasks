# Load required libraries
library(dplyr)
library(readr)

# --- 1. Load all input files ---
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

cat("Files loaded.\n")

# --- 2. Recode Wave 4 (Age 17): w4saim ---
wave4 <- wave4 %>% mutate(educaim17 = case_when(
  w4saim == 14 ~ 6,
  w4saim == 13 ~ 6,
  w4saim == 12 ~ 6,
  w4saim == 11 ~ 5,
  w4saim == 10 ~ 5,
  w4saim == 9 ~ 5,
  w4saim == 8 ~ 4,
  w4saim == 7 ~ 4,
  w4saim == 6 ~ 4,
  w4saim == 5 ~ 4,
  w4saim == 4 ~ 3,
  w4saim == 3 ~ 3,
  w4saim == 2 ~ 3,
  w4saim == 1 ~ 3,
  is.na(w4saim) ~ -3,
  w4saim == -9 ~ -9,
  w4saim == -8 ~ -8,
  w4saim == -1 ~ -1,
  TRUE ~ -3
))

cat("Wave 4 recoded.\n")

# --- 3. Recode Wave 6 (Age 19): W6Saim ---
wave6 <- wave6 %>% mutate(educaim19 = case_when(
  W6Saim == 16 ~ 6,
  W6Saim == 15 ~ 6,
  W6Saim == 14 ~ 6,
  W6Saim == 13 ~ 5,
  W6Saim == 12 ~ 5,
  W6Saim == 11 ~ 4,
  W6Saim == 10 ~ 4,
  W6Saim == 9 ~ 4,
  W6Saim == 8 ~ 3,
  W6Saim == 7 ~ 3,
  W6Saim == 6 ~ 3,
  W6Saim == 5 ~ 3,
  W6Saim == 4 ~ 2,
  W6Saim == 3 ~ 2,
  W6Saim == 2 ~ 1,
  W6Saim == 1 ~ 1,
  is.na(W6Saim) ~ -3,
  W6Saim == -9 ~ -9,
  W6Saim == -8 ~ -8,
  W6Saim == -1 ~ -1,
  TRUE ~ -3
))

cat("Wave 6 recoded.\n")

# --- 4. Recode Wave 7 (Age 20): W7SAim ---
wave7 <- wave7 %>% mutate(educaim20 = case_when(
  W7SAim == 13 ~ 1,
  W7SAim == 11 ~ 1,
  W7SAim == 10 ~ 2,
  W7SAim == 12 ~ 2,
  W7SAim == 6 ~ 3,
  W7SAim == 9 ~ 3,
  W7SAim == 8 ~ 3,
  W7SAim == 7 ~ 3,
  W7SAim == 5 ~ 4,
  W7SAim == 4 ~ 4,
  W7SAim == 3 ~ 4,
  W7SAim == 2 ~ 5,
  W7SAim == 1 ~ 5,
  W7SAim == 14 ~ 6,
  W7SAim == -91 ~ -1,
  W7SAim == -94 ~ -3,
  is.na(W7SAim) ~ -3,
  W7SAim == -9 ~ -9,
  W7SAim == -8 ~ -8,
  W7SAim == -1 ~ -1,
  TRUE ~ -3
))

cat("Wave 7 recoded.\n")

# --- 5. Recode Wave 8 (Age 25) ---
acq_cols_w8 <- grep("^W8ACQUC0", names(wave8), value = TRUE)
vcq_cols_w8 <- grep("^W8VCQUC0", names(wave8), value = TRUE)

acq_mat <- as.matrix(wave8[, acq_cols_w8])
vcq_mat <- as.matrix(wave8[, vcq_cols_w8])

acq_highest <- apply(acq_mat, 1, function(row) {
  idx <- which(row == 1)
  if (length(idx) > 0) max(idx) else 0
})

vcq_highest <- apply(vcq_mat, 1, function(row) {
  idx <- which(row == 1)
  if (length(idx) > 0) max(idx) else 0
})

# Academic mapping
acq_level <- rep(6, length(acq_highest))
acq_level[acq_highest == 1] <- 1
acq_level[acq_highest %in% c(2, 3)] <- 2
acq_level[acq_highest %in% c(4, 5, 6, 7, 8)] <- 3
acq_level[acq_highest %in% c(9, 10, 11, 12)] <- 4
acq_level[acq_highest == 13] <- 5
acq_level[acq_highest == 0] <- -1  # Will handle separately

# Vocational mapping
vcq_level <- rep(6, length(vcq_highest))
vcq_level[vcq_highest == 1] <- 5
vcq_level[vcq_highest == 2] <- 6
vcq_level[vcq_highest == 3] <- 5
vcq_level[vcq_highest == 4] <- 5
vcq_level[vcq_highest == 5] <- 4
vcq_level[vcq_highest == 6] <- 1
vcq_level[vcq_highest == 7] <- 2
vcq_level[vcq_highest == 0] <- -1  # Will handle separately

# Combine: take min (highest qualification)
combined <- ifelse(acq_level == -1 & vcq_level == -1, 0,
                   ifelse(acq_level == -1, vcq_level,
                   ifelse(vcq_level == -1, acq_level,
                   pmin(acq_level, vcq_level))))

# Handle cases where both are -1 (no qualifications)
wave8$educaim25 <- ifelse(combined == 0,
  ifelse(wave8$W8ACTIVITY05 == 0, 6, -3),
  combined)

cat("Wave 8 recoded.\n")

# --- 6. Recode Wave 9 (Age 32) ---
acq_cols_w9 <- grep("^W9ACQUC0[A-V]", names(wave9), value = TRUE)
vcq_cols_w9 <- grep("^W9VCQUC0[A-D]", names(wave9), value = TRUE)

acq_mat_w9 <- as.matrix(wave9[, acq_cols_w9])
vcq_mat_w9 <- as.matrix(wave9[, vcq_cols_w9])

acq_highest_w9 <- apply(acq_mat_w9, 1, function(row) {
  idx <- which(row == 1)
  if (length(idx) > 0) max(idx) else 0
})

vcq_highest_w9 <- apply(vcq_mat_w9, 1, function(row) {
  idx <- which(row == 1)
  if (length(idx) > 0) max(idx) else 0
})

# Academic mapping
acq_level_w9 <- rep(6, length(acq_highest_w9))
acq_level_w9[acq_highest_w9 == 1] <- 1
acq_level_w9[acq_highest_w9 == 2] <- 2
acq_level_w9[acq_highest_w9 %in% c(3, 4, 5, 6, 7, 8)] <- 3
acq_level_w9[acq_highest_w9 %in% c(9, 10, 11)] <- 4
acq_level_w9[acq_highest_w9 %in% c(12, 13, 14, 15, 16, 17)] <- 5
acq_level_w9[acq_highest_w9 == 0] <- -1

# Vocational mapping
vcq_level_w9 <- rep(6, length(vcq_highest_w9))
vcq_level_w9[vcq_highest_w9 == 1] <- 1
vcq_level_w9[vcq_highest_w9 %in% c(3, 29, 30)] <- 2
vcq_level_w9[vcq_highest_w9 %in% c(2, 4, 7, 9, 12, 15, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)] <- 3
vcq_level_w9[vcq_highest_w9 %in% c(5, 8, 10, 13, 16, 24)] <- 4
vcq_level_w9[vcq_highest_w9 %in% c(6, 11, 14, 17, 31)] <- 5
vcq_level_w9[vcq_highest_w9 == 0] <- -1

# Combine
combined_w9 <- ifelse(acq_level_w9 == -1 & vcq_level_w9 == -1, 0,
                      ifelse(acq_level_w9 == -1, vcq_level_w9,
                      ifelse(vcq_level_w9 == -1, acq_level_w9,
                      pmin(acq_level_w9, vcq_level_w9))))

wave9$educaim32 <- ifelse(combined_w9 == 0, -3, combined_w9)

cat("Wave 9 recoded.\n")

# --- 7. Merge all files ---
full_data <- wave1 %>%
  select(NSID) %>%
  full_join(wave4 %>% select(NSID, educaim17), by = "NSID") %>%
  full_join(wave6 %>% select(NSID, educaim19), by = "NSID") %>%
  full_join(wave7 %>% select(NSID, educaim20), by = "NSID") %>%
  full_join(wave8 %>% select(NSID, educaim25), by = "NSID") %>%
  full_join(wave9 %>% select(NSID, educaim32), by = "NSID")

cat("Merged data:", nrow(full_data), "rows,", ncol(full_data), "cols\n")

# --- 8. Write output ---
write_csv(full_data, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(full_data), "\n")
cat("Variables:", names(full_data), "\n")

# Summary of educaim variables
for (var in c("educaim17", "educaim19", "educaim20", "educaim25", "educaim32")) {
  cat(sprintf("\n%s:\n", var))
  cat(table(full_data[[var]], useNA = "ifany"), "\n")
}
