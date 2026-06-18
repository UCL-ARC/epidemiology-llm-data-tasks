# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- list.files("data/input", pattern = "\\.tab$", full.names = TRUE)

# Load each file explicitly
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
df <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cat("Merged dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Wave 4 (Age 17) - w4saim
df$educaim17 <- df$w4saim

map_w4_educaim <- function(x) {
  result <- rep(NA_real_, length(x))
  
  result[x == 14] <- 6  # Not studying
  result[x == 8] <- 1   # GCSE
  result[x %in% c(5, 7)] <- 2  # NVQ 2, Other level 2
  result[x %in% c(1, 2, 3, 4)] <- 3  # NVQ 3, AVCE, A/AS, Other level 3
  result[x %in% c(9, 11)] <- 2  # NVQ 1, Other level 1
  result[x == 10] <- 2  # Foundation
  result[x == 6] <- 2   # Intermediate GNVQ
  result[x == 12] <- 2  # Other
  result[x == 13] <- 1  # No detail
  
  result[x < -1] <- -2
  
  return(result)
}

df$educaim17 <- map_w4_educaim(df$w4saim)
df$educaim17[is.na(df$educaim17)] <- -3

# Wave 6 (Age 19) - W6Saim
df$educaim19 <- df$W6Saim

map_w6_educaim <- function(x) {
  result <- rep(NA_real_, length(x))
  
  result[x == 16] <- 6  # Not studying
  result[x == 11] <- 1  # GCSE
  result[x %in% c(9, 10)] <- 2  # NVQ 2, Other level 2
  result[x %in% c(1, 3, 5, 6, 7, 8)] <- 3  # NVQ 5, NVQ 4, NVQ 3, AVCE, A/AS, Other level 3
  result[x == 2] <- 5  # First/Other Degree
  result[x == 4] <- 4  # Other HE
  result[x %in% c(12, 13)] <- 2  # NVQ 1, Other level 1
  result[x == 14] <- 2  # Other (level unknown)
  result[x == 15] <- 1  # No detail
  
  result[x < -1] <- -2
  
  return(result)
}

df$educaim19 <- map_w6_educaim(df$W6Saim)
df$educaim19[is.na(df$educaim19)] <- -3

# Wave 7 (Age 20) - W7SAim
df$educaim20 <- df$W7SAim

map_w7_educaim <- function(x) {
  result <- rep(NA_real_, length(x))
  
  result[x == -91] <- 6  # Not applicable (not studying)
  result[x == -94] <- -8  # Insufficient information
  
  result[x == 4] <- 1   # GCSE
  result[x %in% c(3, 5)] <- 2  # NVQ 2, Other level 2
  result[x %in% c(6, 7, 8, 9)] <- 3  # NVQ 3, A/AS, AVCE, Other level 3
  result[x %in% c(10, 13)] <- 4  # NVQ 4, NVQ 5
  result[x == 11] <- 5  # First/Other Degree
  result[x == 12] <- 4  # Other HE
  result[x == 14] <- 2  # Other (level unknown)
  result[x %in% c(1, 2)] <- 2  # NVQ 1, Other level 1
  
  return(result)
}

df$educaim20 <- map_w7_educaim(df$W7SAim)
df$educaim20[is.na(df$educaim20)] <- -3

cat("Wave 4 (age 17) educaim17 summary:\n")
print(table(df$educaim17, useNA = "ifany"))

cat("\nWave 6 (age 19) educaim19 summary:\n")
print(table(df$educaim19, useNA = "ifany"))

cat("\nWave 7 (age 20) educaim20 summary:\n")
print(table(df$educaim20, useNA = "ifany"))

# Wave 8 (Age 25)
in_education_25 <- df$W8ACTIVITY05 == 1

has_academic_deg <- df$W8ACQUC0A == 1
has_academic_first_deg <- df$W8ACQUC0B == 1
has_academic_he <- df$W8ACQUC0C == 1
has_academic_aslevel <- df$W8ACQUC0F == 1
has_academic_gcse <- df$W8ACQUC0L == 1

has_voc_nvq3_5 <- df$W8VCQUC0J == 1
has_voc_hnc_hnd <- df$W8VCQUC0K == 1

derive_educaim25 <- function(in_edu, acad_deg, acad_first_deg, acad_he, acad_as, acad_gcse, voc_nvq35, voc_hnc_hnd) {
  result <- rep(NA_real_, length(in_edu))
  
  in_edu_idx <- which(in_edu == 1)
  if (length(in_edu_idx) > 0) {
    result[in_edu_idx[acad_deg[in_edu_idx] == 1]] <- 5
    result[in_edu_idx[acad_he[in_edu_idx] == 1 | voc_hnc_hnd[in_edu_idx] == 1]] <- 4
    result[in_edu_idx[acad_as[in_edu_idx] == 1 | voc_nvq35[in_edu_idx] == 1]] <- 3
    result[in_edu_idx[acad_gcse[in_edu_idx] == 1]] <- 2
    result[in_edu_idx[is.na(result[in_edu_idx])]] <- 2
  }
  
  not_in_edu_idx <- which(in_edu != 1)
  if (length(not_in_edu_idx) > 0) {
    result[not_in_edu_idx[acad_deg[not_in_edu_idx] == 1]] <- 5
    result[not_in_edu_idx[acad_first_deg[not_in_edu_idx] == 1 | acad_he[not_in_edu_idx] == 1 | voc_hnc_hnd[not_in_edu_idx] == 1]] <- 4
    result[not_in_edu_idx[acad_as[not_in_edu_idx] == 1 | voc_nvq35[not_in_edu_idx] == 1]] <- 3
    result[not_in_edu_idx[acad_gcse[not_in_edu_idx] == 1]] <- 2
    result[not_in_edu_idx[df$W8ACTIVITY05[not_in_edu_idx] == -1]] <- -1
    result[not_in_edu_idx[is.na(result[not_in_edu_idx])]] <- -3
  }
  
  return(result)
}

df$educaim25 <- derive_educaim25(
  in_education_25,
  has_academic_deg,
  has_academic_first_deg,
  has_academic_he,
  has_academic_aslevel,
  has_academic_gcse,
  has_voc_nvq3_5,
  has_voc_hnc_hnd
)

cat("\nWave 8 (age 25) educaim25 summary:\n")
print(table(df$educaim25, useNA = "ifany"))

# Wave 9 (Age 32)
in_education_32 <- df$W9ECONACT2 %in% c(6, 7)

has_acad_doctorate <- df$W9ACQUC0A == 1
has_acad_masters <- df$W9ACQUC0B == 1
has_acad_undergrad <- df$W9ACQUC0C == 1
has_acad_diploma <- df$W9ACQUC0D == 1
has_acad_he_dip <- df$W9ACQUC0E == 1
has_acad_a_level <- df$W9ACQUC0G == 1
has_acad_gcse_high <- df$W9ACQUC0H == 1
has_acad_gcse_low <- df$W9ACQUC0I == 1

has_voc_prof_deg <- df$W9VCQUC0A == 1
has_voc_level45 <- df$W9VCQUC0C == 1
has_voc_level3 <- df$W9VCQUC0D == 1
has_voc_level2 <- df$W9VCQUC0E == 1
has_voc_level1 <- df$W9VCQUC0F == 1
has_voc_advanced_dip <- df$W9VCQUC0R == 1
has_voc_higher_dip <- df$W9VCQUC0S == 1
has_voc_hnd_hnc <- df$W9VCQUCAC == 1

derive_educaim32 <- function(in_edu, acad_doct, acad_mast, acad_under, acad_diploma, acad_he_dip, acad_a, acad_gcse_h, acad_gcse_l, voc_prof, voc_45, voc_3, voc_2, voc_1, voc_adv_dip, voc_high_dip, voc_hnd) {
  result <- rep(NA_real_, length(in_edu))
  
  in_edu_idx <- which(in_edu == 1)
  if (length(in_edu_idx) > 0) {
    result[in_edu_idx[acad_doct[in_edu_idx] == 1]] <- 5
    result[in_edu_idx[acad_mast[in_edu_idx] == 1 | acad_diploma[in_edu_idx] == 1]] <- 4
    result[in_edu_idx[acad_under[in_edu_idx] == 1 | acad_he_dip[in_edu_idx] == 1 | voc_prof[in_edu_idx] == 1 | voc_45[in_edu_idx] == 1 | voc_adv_dip[in_edu_idx] == 1 | voc_high_dip[in_edu_idx] == 1 | voc_hnd[in_edu_idx] == 1]] <- 4
    result[in_edu_idx[acad_a[in_edu_idx] == 1 | voc_3[in_edu_idx] == 1]] <- 3
    result[in_edu_idx[acad_gcse_h[in_edu_idx] == 1 | acad_gcse_l[in_edu_idx] == 1 | voc_2[in_edu_idx] == 1 | voc_1[in_edu_idx] == 1]] <- 2
    result[in_edu_idx[is.na(result[in_edu_idx])]] <- 2
  }
  
  not_in_edu_idx <- which(in_edu != 1)
  if (length(not_in_edu_idx) > 0) {
    result[not_in_edu_idx[acad_doct[not_in_edu_idx] == 1]] <- 5
    result[not_in_edu_idx[acad_mast[not_in_edu_idx] == 1 | acad_diploma[not_in_edu_idx] == 1 | acad_under[not_in_edu_idx] == 1 | acad_he_dip[not_in_edu_idx] == 1 | voc_prof[not_in_edu_idx] == 1 | voc_45[not_in_edu_idx] == 1 | voc_adv_dip[not_in_edu_idx] == 1 | voc_high_dip[not_in_edu_idx] == 1 | voc_hnd[not_in_edu_idx] == 1]] <- 4
    result[not_in_edu_idx[acad_a[not_in_edu_idx] == 1 | voc_3[not_in_edu_idx] == 1]] <- 3
    result[not_in_edu_idx[acad_gcse_h[not_in_edu_idx] == 1 | acad_gcse_l[not_in_edu_idx] == 1 | voc_2[not_in_edu_idx] == 1 | voc_1[not_in_edu_idx] == 1]] <- 2
    result[not_in_edu_idx[is.na(result[not_in_edu_idx])]] <- -3
  }
  
  return(result)
}

df$educaim32 <- derive_educaim32(
  in_education_32,
  has_acad_doctorate,
  has_acad_masters,
  has_acad_undergrad,
  has_acad_diploma,
  has_acad_he_dip,
  has_acad_a_level,
  has_acad_gcse_high,
  has_acad_gcse_low,
  has_voc_prof_deg,
  has_voc_level45,
  has_voc_level3,
  has_voc_level2,
  has_voc_level1,
  has_voc_advanced_dip,
  has_voc_higher_dip,
  has_voc_hnd_hnc
)

cat("\nWave 9 (age 32) educaim32 summary:\n")
print(table(df$educaim32, useNA = "ifany"))

# Create value labels as a named character vector
labels <- c(
  "1" = "No qualification",
  "2" = "Level 1-2",
  "3" = "Level 3",
  "4" = "Level 4-5",
  "5" = "Degree or higher",
  "6" = "Not studying",
  "-9" = "Refused",
  "-8" = "Don\'t know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Not applicable",
  "-1" = "Item not applicable"
)

# Create labelled vectors by converting numeric to factor with labels
create_labeled <- function(x, labels) {
  # Convert to factor with custom labels for valid values
  f <- factor(x, levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6),
              labels = c("Refused", "Don't know", "Prefer not to say", 
                        "Not asked", "Not applicable", "Item not applicable",
                        "No qualification", "Level 1-2", "Level 3", "Level 4-5", 
                        "Degree or higher", "Not studying"))
  return(f)
}

df$educaim17 <- create_labeled(df$educaim17, labels)
df$educaim19 <- create_labeled(df$educaim19, labels)
df$educaim20 <- create_labeled(df$educaim20, labels)
df$educaim25 <- create_labeled(df$educaim25, labels)
df$educaim32 <- create_labeled(df$educaim32, labels)

# Select only required variables for output
output_vars <- c("NSID", "educaim17", "educaim19", "educaim20", "educaim25", "educaim32")
output_df <- df %>% select(all_of(output_vars))

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(output_df), "rows,", ncol(output_df), "columns\n")
cat("Output columns:", paste(output_vars, collapse = ", "), "\n")

# Verify the output
head(output_df)
