library(dplyr)
library(readr)
library(labelled)
library(tidyr)
library(purrr)
library(haven)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets
merged <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w5, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

cat("Merged data dimensions:", dim(merged), "\n")

# Wave 4 (Age 17): W4empsYP recode
recode_w4 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] %in% c(1, 2)) result[i] <- 1.0
      else if (x[i] == 3) result[i] <- 2.0
      else if (x[i] == 4) result[i] <- 4.0
      else if (x[i] == 5) result[i] <- 3.0
      else if (x[i] == 6) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      if (x[i] == -92) result[i] <- -9.0
      else if (x[i] == -94) result[i] <- -8.0
      else if (x[i] == -91) result[i] <- -3.0
      else if (x[i] <= -999) result[i] <- -2.0
      else result[i] <- -3.0
    }
  }
  result
}

# Wave 5 (Age 18): W5mainactYP recode
recode_w5 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] == 3) result[i] <- 1.0
      else if (x[i] == 7) result[i] <- 2.0
      else if (x[i] == 4) result[i] <- 3.0
      else if (x[i] %in% c(1, 2, 5, 6)) result[i] <- 4.0
      else if (x[i] == 8) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      if (x[i] == -94) result[i] <- -8.0
      else result[i] <- -3.0
    }
  }
  result
}

# Wave 6 (Age 19): W6TCurrentAct recode
recode_w6 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] == 3) result[i] <- 1.0
      else if (x[i] == 8) result[i] <- 2.0
      else if (x[i] %in% c(1, 2)) result[i] <- 3.0
      else if (x[i] %in% c(4, 5, 10)) result[i] <- 4.0
      else if (x[i] == 7) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      if (x[i] == -91) result[i] <- -8.0
      else result[i] <- -3.0
    }
  }
  result
}

# Wave 7 (Age 20): W7TCurrentAct recode
recode_w7 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] %in% c(3, 9)) result[i] <- 1.0
      else if (x[i] == 8) result[i] <- 2.0
      else if (x[i] %in% c(1, 2)) result[i] <- 3.0
      else if (x[i] %in% c(4, 5, 11)) result[i] <- 4.0
      else if (x[i] == 7) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      result[i] <- -3.0
    }
  }
  result
}

# Wave 8 (Age 25): W8DACTIVITYC recode
recode_w8 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] %in% c(1, 2)) result[i] <- 1.0
      else if (x[i] == 4) result[i] <- 2.0
      else if (x[i] == 5) result[i] <- 3.0
      else if (x[i] %in% c(6, 7)) result[i] <- 4.0
      else if (x[i] == 9) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      if (x[i] == -9) result[i] <- -9.0
      else if (x[i] == -8) result[i] <- -8.0
      else if (x[i] == -1) result[i] <- -3.0
      else result[i] <- -3.0
    }
  }
  result
}

# Wave 9 (Age 32): W9DACTIVITYC recode
recode_w9 <- function(x) {
  result <- rep(-3.0, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- -3.0
    } else if (x[i] > 0) {
      if (x[i] %in% c(1, 2)) result[i] <- 1.0
      else if (x[i] == 4) result[i] <- 2.0
      else if (x[i] == 5) result[i] <- 3.0
      else if (x[i] %in% c(6, 7)) result[i] <- 4.0
      else if (x[i] == 9) result[i] <- 5.0
      else result[i] <- 6.0
    } else {
      if (x[i] == -9) result[i] <- -9.0
      else if (x[i] == -8) result[i] <- -8.0
      else if (x[i] == -1) result[i] <- -3.0
      else result[i] <- -3.0
    }
  }
  result
}

# Apply recoding
merged$ecoact17 <- recode_w4(merged$W4empsYP)
merged$ecoact18 <- recode_w5(merged$W5mainactYP)
merged$ecoact19 <- recode_w6(merged$W6TCurrentAct)
merged$ecoact20 <- recode_w7(merged$W7TCurrentAct)
merged$ecoact25 <- recode_w8(merged$W8DACTIVITYC)
merged$ecoact32 <- recode_w9(merged$W9DACTIVITYC)

# Create detailed adult variables for ages 25 and 32
merged$ecoactadu25 <- merged$W8DACTIVITYC
merged$ecoactadu32 <- merged$W9DACTIVITYC

# Convert raw missing codes to standard codes for detailed variables
for (col in c("ecoactadu25", "ecoactadu32")) {
  for (i in seq_along(merged[[col]])) {
    if (!is.na(merged[[col]][i]) && merged[[col]][i] < 0) {
      val <- merged[[col]][i]
      if (val == -9) merged[[col]][i] <- -9.0
      else if (val == -8) merged[[col]][i] <- -8.0
      else if (val == -1) merged[[col]][i] <- -3.0
      else merged[[col]][i] <- -3.0
    }
  }
}

# Select only ID and derived variables
output <- merged %>% select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written successfully\n")
cat("Output dimensions:", dim(output), "\n")
cat("Variables:", names(output), "\n")

# Summary of values
for (var in names(output)) {
  cat("\n", var, ":\n")
  print(summary(output[[var]]))
}
