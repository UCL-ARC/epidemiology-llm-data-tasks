library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

input_dir <- "data/input/"
output_dir <- "data/output/"

wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t")
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t")
ns8_main <- read_delim(paste0(input_dir, "ns8_2015_main_interview.tab"), delim = "\t")
ns8_derived <- read_delim(paste0(input_dir, "ns8_2015_derived.tab"), delim = "\t")
ns9_main <- read_delim(paste0(input_dir, "ns9_2022_main_interview.tab"), delim = "\t")
ns9_derived <- read_delim(paste0(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t")

df <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

cat("Loaded and merged data. Dimensions:", nrow(df), "x", ncol(df), "\n")

# EDUC25: Collapsed 5-level NVQ scheme at age 25 (Wave 8)
df$W8VCQU0A_lvl <- ifelse(df$W8VCQU0A == 1, 3, ifelse(df$W8VCQU0A == -8, -8, ifelse(df$W8VCQU0A == -9, -9, ifelse(df$W8VCQU0A == -1 | is.na(df$W8VCQU0A), NA, 0))))
df$W8VCQU0B_lvl <- ifelse(df$W8VCQU0B == 1, 3, ifelse(df$W8VCQU0B == -8, -8, ifelse(df$W8VCQU0B == -9, -9, ifelse(df$W8VCQU0B == -1 | is.na(df$W8VCQU0B), NA, 0))))
df$W8VCQU0C_lvl <- ifelse(df$W8VCQU0C == 1, 3, ifelse(df$W8VCQU0C == -8, -8, ifelse(df$W8VCQU0C == -9, -9, ifelse(df$W8VCQU0C == -1 | is.na(df$W8VCQU0C), NA, 0))))
df$W8VCQU0D_lvl <- ifelse(df$W8VCQU0D == 1, 2, ifelse(df$W8VCQU0D == -8, -8, ifelse(df$W8VCQU0D == -9, -9, ifelse(df$W8VCQU0D == -1 | is.na(df$W8VCQU0D), NA, 0))))
df$W8VCQU0E_lvl <- ifelse(df$W8VCQU0E == 1, 3, ifelse(df$W8VCQU0E == -8, -8, ifelse(df$W8VCQU0E == -9, -9, ifelse(df$W8VCQU0E == -1 | is.na(df$W8VCQU0E), NA, 0))))
df$W8VCQU0F_lvl <- ifelse(df$W8VCQU0F == 1, 3, ifelse(df$W8VCQU0F == -8, -8, ifelse(df$W8VCQU0F == -9, -9, ifelse(df$W8VCQU0F == -1 | is.na(df$W8VCQU0F), NA, 0))))
df$W8VCQU0G_lvl <- ifelse(df$W8VCQU0G == 1, 3, ifelse(df$W8VCQU0G == -8, -8, ifelse(df$W8VCQU0G == -9, -9, ifelse(df$W8VCQU0G == -1 | is.na(df$W8VCQU0G), NA, 0))))
df$W8VCQU0H_lvl <- ifelse(df$W8VCQU0H == 1, 3, ifelse(df$W8VCQU0H == -8, -8, ifelse(df$W8VCQU0H == -9, -9, ifelse(df$W8VCQU0H == -1 | is.na(df$W8VCQU0H), NA, 0))))
df$W8VCQU0I_lvl <- ifelse(df$W8VCQU0I == 1, 3, ifelse(df$W8VCQU0I == -8, -8, ifelse(df$W8VCQU0I == -9, -9, ifelse(df$W8VCQU0I == -1 | is.na(df$W8VCQU0I), NA, 0))))
df$W8VCQU0J_lvl <- ifelse(df$W8VCQU0J == 1, 5, ifelse(df$W8VCQU0J == -8, -8, ifelse(df$W8VCQU0J == -9, -9, ifelse(df$W8VCQU0J == -1 | is.na(df$W8VCQU0J), NA, 0))))
df$W8VCQU0K_lvl <- ifelse(df$W8VCQU0K == 1, 5, ifelse(df$W8VCQU0K == -8, -8, ifelse(df$W8VCQU0K == -9, -9, ifelse(df$W8VCQU0K == -1 | is.na(df$W8VCQU0K), NA, 0))))
df$W8VCQU0L_lvl <- ifelse(df$W8VCQU0L == 1, 5, ifelse(df$W8VCQU0L == -8, -8, ifelse(df$W8VCQU0L == -9, -9, ifelse(df$W8VCQU0L == -1 | is.na(df$W8VCQU0L), NA, 0))))
df$W8VCQU0M_lvl <- ifelse(df$W8VCQU0M == 1, 3, ifelse(df$W8VCQU0M == -8, -8, ifelse(df$W8VCQU0M == -9, -9, ifelse(df$W8VCQU0M == -1 | is.na(df$W8VCQU0M), NA, 0))))
df$W8VCQU0N_lvl <- ifelse(df$W8VCQU0N == 1, 3, ifelse(df$W8VCQU0N == -8, -8, ifelse(df$W8VCQU0N == -9, -9, ifelse(df$W8VCQU0N == -1 | is.na(df$W8VCQU0N), NA, 0))))
df$W8VCQU0O_lvl <- ifelse(df$W8VCQU0O == 1, 1, ifelse(df$W8VCQU0O == -8, -8, ifelse(df$W8VCQU0O == -9, -9, ifelse(df$W8VCQU0O == -1 | is.na(df$W8VCQU0O), NA, 0))))
df$W8VCQU0P_lvl <- ifelse(df$W8VCQU0P == 1, 0, ifelse(df$W8VCQU0P == -8, -8, ifelse(df$W8VCQU0P == -9, -9, ifelse(df$W8VCQU0P == -1 | is.na(df$W8VCQU0P), NA, 0))))
df$W8VCQU0Q_lvl <- ifelse(df$W8VCQU0Q == -8, -8, ifelse(df$W8VCQU0Q == -9, -9, ifelse(df$W8VCQU0Q == -1 | is.na(df$W8VCQU0Q), NA, 0)))
df$W8VCQU0R_lvl <- ifelse(df$W8VCQU0R == -9, -9, ifelse(df$W8VCQU0R == -8, -8, ifelse(df$W8VCQU0R == -1 | is.na(df$W8VCQU0R), NA, 0)))

df$W8DHANVQH_lvl <- ifelse(df$W8DHANVQH >= 1 & df$W8DHANVQH <= 5, df$W8DHANVQH,
                     ifelse(df$W8DHANVQH == 95, 3,
                     ifelse(df$W8DHANVQH == 96, 2,
                     ifelse(df$W8DHANVQH == -8, -8,
                     ifelse(df$W8DHANVQH == -9, -9,
                     ifelse(df$W8DHANVQH == -1 | is.na(df$W8DHANVQH), NA, 2))))))

level_vars <- list(df$W8DHANVQH_lvl, 
                 df$W8VCQU0A_lvl, df$W8VCQU0B_lvl, df$W8VCQU0C_lvl, df$W8VCQU0D_lvl,
                 df$W8VCQU0E_lvl, df$W8VCQU0F_lvl, df$W8VCQU0G_lvl, df$W8VCQU0H_lvl,
                 df$W8VCQU0I_lvl, df$W8VCQU0J_lvl, df$W8VCQU0K_lvl, df$W8VCQU0L_lvl,
                 df$W8VCQU0M_lvl, df$W8VCQU0N_lvl, df$W8VCQU0O_lvl, df$W8VCQU0P_lvl,
                 df$W8VCQU0Q_lvl, df$W8VCQU0R_lvl)

max_valid_level_row <- function(x) {
  substantive <- x[x >= 0 & !is.na(x)]
  if (length(substantive) > 0) {
    return(max(substantive, na.rm = TRUE))
  }
  negatives <- x[x %in% c(-1, -8, -9) & !is.na(x)]
  if (length(negatives) > 0) {
    return(min(negatives))
  }
  return(NA)
}

df$educ25_raw <- sapply(1:nrow(df), function(i) {
  row_vals <- sapply(level_vars, function(v) v[i])
  max_valid_level_row(row_vals)
})

df$educ25 <- factor(df$educ25_raw, levels = c(-9, -8, -1, 0, 1, 2, 3, 4),
                    labels = c("Refused", "Don't know", "Not applicable", 
                               "NVQ 4-5 equivalent", "NVQ 1-3 equivalent",
                               "Entry level or no qualifications",
                               "Other qualifications not mappable",
                               "None of these qualifications"))

# EDUC32: Collapsed 5-level NVQ scheme at age 32 (Wave 9)
df$educ32_raw <- NA_real_

for (i in 1:nrow(df)) {
  danvqh <- df$W9DANVQH[i]
  dvnvqh <- df$W9DVNVQH[i]
  
  if (!is.na(danvqh) && danvqh >= 0) {
    if (!is.na(dvnvqh) && dvnvqh >= 0 && dvnvqh > danvqh) {
      df$educ32_raw[i] <- dvnvqh
    } else {
      df$educ32_raw[i] <- danvqh
    }
  } else if (is.na(danvqh) || danvqh < 0) {
    if (!is.na(dvnvqh) && dvnvqh >= 0) {
      df$educ32_raw[i] <- dvnvqh
    } else if (!is.na(dvnvqh) && dvnvqh < 0) {
      if (danvqh < dvnvqh) {
        df$educ32_raw[i] <- danvqh
      } else {
        df$educ32_raw[i] <- dvnvqh
      }
    } else {
      df$educ32_raw[i] <- danvqh
    }
  }
}

map_to_5level <- function(x) {
  result <- rep(NA_integer_, length(x))
  result[x == 0] <- 2
  result[x == 1] <- 1
  result[x == 2] <- 1
  result[x == 3] <- 1
  result[x == 4] <- 0
  result[x == 5] <- 0
  result[x == 95] <- 3
  result[x == 96] <- 4
  result[x == -1] <- -1
  result[x == -8] <- -8
  result[x == -9] <- -9
  return(result)
}

df$educ32_mapped <- map_to_5level(df$educ32_raw)

df$educ32 <- factor(df$educ32_mapped, levels = c(-9, -8, -1, 0, 1, 2, 3, 4),
                    labels = c("Refused", "Don't know", "Not applicable", 
                               "NVQ 4-5 equivalent", "NVQ 1-3 equivalent",
                               "Entry level or no qualifications",
                               "Other qualifications not mappable",
                               "None of these qualifications"))

# EDUCADTL32: Detailed academic qualifications at age 32
acq_vars <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", "W9ACQU0E", 
              "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", "W9ACQU0I", "W9ACQU0J",
              "W9ACQU0K", "W9ACQU0L", "W9ACQU0M", "W9ACQU0N", "W9ACQU0O",
              "W9ACQU0P", "W9ACQU0Q", "W9ACQU0R", "W9ACQU0S", "W9ACQU0T",
              "W9ACQU0U", "W9ACQU0V")

acq_vars <- acq_vars[acq_vars %in% names(df)]
cat("Academic qualification variables found:", length(acq_vars), "\n")

acq_label_map <- list(
  "W9ACQU0A" = "Doctorate or equivalent",
  "W9ACQU0B" = "Masters or equivalent",
  "W9ACQU0C" = "Undergraduate or equivalent",
  "W9ACQU0D" = "Post-graduate Diplomas and Certificates",
  "W9ACQU0E" = "Diplomas in higher education and other higher education qualifications",
  "W9ACQU0F" = "Teaching qualifications for schools or further education (below degree level)",
  "W9ACQU0G" = "A/AS Levels or equivalent",
  "W9ACQU0H" = "Grade A-C, Level 4-9",
  "W9ACQU0I" = "Grade D-G, Level 1-3",
  "W9ACQU0J" = "SCE Higher",
  "W9ACQU0K" = "Scottish Certificate Sixth Year Studies",
  "W9ACQU0L" = "SCE Standard",
  "W9ACQU0M" = "National 4 and 5",
  "W9ACQU0N" = "National 2 and 3",
  "W9ACQU0O" = "Leaving Certificate",
  "W9ACQU0P" = "Junior Certificate grade A-C",
  "W9ACQU0Q" = "Junior Certificate grade D and below",
  "W9ACQU0R" = "Other academic qualifications (including overseas)",
  "W9ACQU0S" = "None of these qualifications",
  "W9ACQU0T" = "Don't know",
  "W9ACQU0U" = "Refused",
  "W9ACQU0V" = "No answer"
)

# Position indices (1-based)
dont_know_pos <- 20
refused_pos <- 21
no_answer_pos <- 22

educadtl32_vals <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  row_vals <- sapply(acq_vars, function(vn) df[[vn]][i])
  n <- length(acq_vars)
  
  # Check non-substantive first (handle NA properly)
  if (!is.na(row_vals[dont_know_pos]) && row_vals[dont_know_pos] == 1) {
    educadtl32_vals[i] <- -8
    next
  }
  if (!is.na(row_vals[refused_pos]) && row_vals[refused_pos] == 1) {
    educadtl32_vals[i] <- -9
    next
  }
  if (!is.na(row_vals[no_answer_pos]) && row_vals[no_answer_pos] == 1) {
    educadtl32_vals[i] <- -2
    next
  }
  
  # Check all -1
  if (all(row_vals == -1 | is.na(row_vals))) {
    educadtl32_vals[i] <- -1
    next
  }
  
  # Check all -3 or NA
  if (all(is.na(row_vals) | row_vals == -3)) {
    educadtl32_vals[i] <- -3
    next
  }
  
  # Count Yes responses
  yes_count <- sum(row_vals == 1, na.rm = TRUE)
  if (yes_count > 0) {
    educadtl32_vals[i] <- yes_count
  } else {
    educadtl32_vals[i] <- n + 1
  }
}

substantive_max <- max(educadtl32_vals[educadtl32_vals > 0], na.rm = TRUE)
substantive_vars <- acq_vars[1:substantive_max]
level_labels <- sapply(substantive_vars, function(vn) {
  if (vn %in% names(acq_label_map)) return(acq_label_map[[vn]])
  return("Unknown")
})

df$educadtl32 <- factor(educadtl32_vals, 
                        levels = c(-9, -8, -3, -2, -1, 1:(substantive_max+1)),
                        labels = c("Refused", "Don't know", "Not asked at the fieldwork stage",
                                   "No answer", "Not applicable", level_labels, "None of these qualifications"))

cat("educadtl32 levels:", length(levels(df$educadtl32)), "\n")

# EDUCVDTL32: Detailed vocational qualifications at age 32
vcq_vars <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", "W9VCQU0E",
              "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", "W9VCQU0I", "W9VCQU0J",
              "W9VCQU0K", "W9VCQU0L", "W9VCQU0M", "W9VCQU0N", "W9VCQU0O",
              "W9VCQU0P", "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T",
              "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", "W9VCQU0Y",
              "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", "W9VCQUAC", "W9VCQUAD",
              "W9VCQUAE", "W9VCQUAF", "W9VCQUAG", "W9VCQUAH", "W9VCQUAI")

vcq_vars <- vcq_vars[vcq_vars %in% names(df)]
cat("Vocational qualification variables found:", length(vcq_vars), "\n")

vcq_label_map <- list(
  "W9VCQU0A" = "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  "W9VCQU0B" = "Nursing or other medical qualifications (below degree level)",
  "W9VCQU0C" = "Level 4 or 5",
  "W9VCQU0D" = "Level 3",
  "W9VCQU0E" = "Level 2",
  "W9VCQU0F" = "Level 1",
  "W9VCQU0G" = "GNVQ Advanced",
  "W9VCQU0H" = "GNVQ Intermediate",
  "W9VCQU0I" = "Level 3",
  "W9VCQU0J" = "Level 2",
  "W9VCQU0K" = "Level Foundation",
  "W9VCQU0L" = "Advanced Craft, Part III",
  "W9VCQU0M" = "Craft, Part II",
  "W9VCQU0N" = "Craft, Part I",
  "W9VCQU0O" = "Level 3",
  "W9VCQU0P" = "Level 2",
  "W9VCQU0Q" = "Level 1",
  "W9VCQU0R" = "Advanced Diploma",
  "W9VCQU0S" = "Higher Diploma",
  "W9VCQU0T" = "RSA Diploma",
  "W9VCQU0U" = "RSA Stage I, II,III",
  "W9VCQU0V" = "Higher Level BTEC",
  "W9VCQU0W" = "BTEC National",
  "W9VCQU0X" = "BTEC First",
  "W9VCQU0Y" = "SCOTVEC National Certificate",
  "W9VCQU0Z" = "SCOTVEC first or general diploma",
  "W9VCQUAA" = "SCOTVEC general diploma",
  "W9VCQUAB" = "SCOTVEC modules",
  "W9VCQUAC" = "HND or HNC",
  "W9VCQUAD" = "OND or ONCM",
  "W9VCQUAE" = "Junior certificate",
  "W9VCQUAF" = "Other vocational qualifications (including some overseas)",
  "W9VCQUAG" = "None of these qualifications",
  "W9VCQUAH" = "Don't know",
  "W9VCQUAI" = "Refused"
)

# For vocational: only Don't know (AH = index 35) and Refused (AI = index 36)
dont_know_pos_voc <- 35
refused_pos_voc <- 36

educvdtl32_vals <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  row_vals <- sapply(vcq_vars, function(vn) df[[vn]][i])
  n <- length(vcq_vars)
  
  # Check non-substantive (only Don't know and Refused for vocational)
  if (!is.na(row_vals[dont_know_pos_voc]) && row_vals[dont_know_pos_voc] == 1) {
    educvdtl32_vals[i] <- -8
    next
  }
  if (!is.na(row_vals[refused_pos_voc]) && row_vals[refused_pos_voc] == 1) {
    educvdtl32_vals[i] <- -9
    next
  }
  
  # Check all -1
  if (all(row_vals == -1 | is.na(row_vals))) {
    educvdtl32_vals[i] <- -1
    next
  }
  
  # Check all -3 or NA
  if (all(is.na(row_vals) | row_vals == -3)) {
    educvdtl32_vals[i] <- -3
    next
  }
  
  # Count Yes responses
  yes_count <- sum(row_vals == 1, na.rm = TRUE)
  if (yes_count > 0) {
    educvdtl32_vals[i] <- yes_count
  } else {
    educvdtl32_vals[i] <- n + 1
  }
}

substantive_max_voc <- max(educvdtl32_vals[educvdtl32_vals > 0], na.rm = TRUE)
substantive_vars_voc <- vcq_vars[1:substantive_max_voc]
level_labels_voc <- sapply(substantive_vars_voc, function(vn) {
  if (vn %in% names(vcq_label_map)) return(vcq_label_map[[vn]])
  return("Unknown")
})

df$educvdtl32 <- factor(educvdtl32_vals, 
                        levels = c(-9, -8, -3, -1, 1:(substantive_max_voc+1)),
                        labels = c("Refused", "Don't know", "Not asked at the fieldwork stage",
                                   "Not applicable", level_labels_voc, "None of these qualifications"))

cat("educvdtl32 levels:", length(levels(df$educvdtl32)), "\n")

# OUTPUT
output_vars <- c("NSID", "educ25", "educ32", "educadtl32", "educvdtl32")
output_df <- df[, output_vars]

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(output_df, paste0(output_dir, "cleaned_data.csv"))

cat("\nOutput written to:", paste0(output_dir, "cleaned_data.csv"), "\n")
cat("Dimensions:", nrow(output_df), "x", ncol(output_df), "\n")
cat("Variables:", paste(names(output_df), collapse = ", "), "\n")

cat("\nSummary of educ25:\n")
print(table(df$educ25, useNA = "ifany"))

cat("\nSummary of educ32:\n")
print(table(df$educ32, useNA = "ifany"))

cat("\nSummary of educadtl32:\n")
print(table(df$educadtl32, useNA = "ifany"))

cat("\nSummary of educvdtl32:\n")
print(table(df$educvdtl32, useNA = "ifany"))