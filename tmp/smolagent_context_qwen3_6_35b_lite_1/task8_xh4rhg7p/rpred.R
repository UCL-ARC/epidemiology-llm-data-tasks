library(dplyr)
library(readr)
library(haven)
library(labelled)

# 1. Load all files from the metadata
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# 2. Merge all datasets using full_join by NSID
df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, ns8_main, by = "NSID")
df <- full_join(df, ns8_derived, by = "NSID")
df <- full_join(df, ns9_main, by = "NSID")
df <- full_join(df, ns9_derived, by = "NSID")

# 3. Create output directory if needed
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# 4. Define the 5-level NVQ scheme mapping function
map_nvq_5level <- function(x) {
  case_when(
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -3 ~ -3,
    x == -2 ~ -2,
    x == -1 ~ -1,
    x == 0 ~ 0,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x >= 3 & x <= 5 ~ 3,
    x == 95 ~ 95,
    x == 96 ~ 96,
    TRUE ~ -2
  )
}

# Create value labels as named numeric vectors
educ25_labels <- setNames(c(0, 1, 2, 3, 95, 96, -9, -8, -7, -3, -2, -1),
                          c("NVQ Entry Level", "NVQ Level 1", "NVQ Level 2", "NVQ Level 3-5",
                            "Other qualification", "None of these qualifications",
                            "Refusal", "Don\'t know", "Prefer not to say",
                            "Not asked at fieldwork stage", "Schedule not applicable",
                            "Item not applicable"))

acad_labels <- setNames(c(0, 1, 2, 3, 95, 96, -9, -8, -7, -3, -2, -1),
                        c("NVQ Entry Level", "NVQ Level 1", "NVQ Level 2", "NVQ Level 3-5",
                          "Other academic qualification", "None of these qualifications",
                          "Refusal", "Don\'t know", "Prefer not to say",
                          "Not asked at fieldwork stage", "Schedule not applicable",
                          "Item not applicable"))

voc_labels <- setNames(c(0, 1, 2, 3, 95, 96, -9, -8, -7, -3, -2, -1),
                       c("NVQ Entry Level", "NVQ Level 1", "NVQ Level 2", "NVQ Level 3-5",
                         "Other vocational qualification", "None of these qualifications",
                         "Refusal", "Don\'t know", "Prefer not to say",
                         "Not asked at fieldwork stage", "Schedule not applicable",
                         "Item not applicable"))

# 5. Function to get highest NVQ level from a set of qualification variables
get_highest_nvq_cols <- function(df, vvars, level_map) {
  vals <- df[, vvars]
  vals[] <- lapply(vals, as.numeric)
  
  result <- numeric(nrow(df))
  result[] <- NA_real_
  
  for (i in seq_len(nrow(df))) {
    highest <- NA_real_
    for (j in seq_along(vvars)) {
      v <- vals[i, j]
      var_name <- vvars[j]
      if (is.na(v) || v == 2) next
      if (v == 1) {
        lv <- level_map[[var_name]]
        if (!is.na(lv)) {
          if (is.na(highest) || lv > highest) {
            highest <- lv
          }
        }
      }
    }
    result[i] <- highest
  }
  return(result)
}

# 6. Create educ25 (age 25, Wave 8, vocational qualifications)
ns8_voc_vars <- paste0("W8VCQU0", c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O"))

level_map_voc <- c(
  "W8VCQU0A" = 0, "W8VCQU0B" = 0, "W8VCQU0C" = 0, "W8VCQU0D" = 0,
  "W8VCQU0E" = 0, "W8VCQU0F" = 0, "W8VCQU0G" = 0, "W8VCQU0H" = 0,
  "W8VCQU0I" = 1, "W8VCQU0J" = 3, "W8VCQU0K" = 3, "W8VCQU0L" = 3,
  "W8VCQU0M" = 0, "W8VCQU0N" = 0, "W8VCQU0O" = 0
)

df$highest_nvq_25 <- get_highest_nvq_cols(df, ns8_voc_vars, level_map_voc)

# Check for None of the above
none_mask <- is.na(df$highest_nvq_25) & !is.na(df$W8VCQU0P) & df$W8VCQU0P == 1
df$highest_nvq_25[none_mask] <- 96

# Set missing codes for refusal/don\'t know
refused_mask <- is.na(df$highest_nvq_25) & !is.na(df$W8VCQU0R) & df$W8VCQU0R == 1
df$highest_nvq_25[refused_mask] <- -9

dk_mask <- is.na(df$highest_nvq_25) & !is.na(df$W8VCQU0Q) & df$W8VCQU0Q == 1
df$highest_nvq_25[dk_mask] <- -8

df$educ25 <- map_nvq_5level(df$highest_nvq_25)

df$educ25 <- haven::labelled(df$educ25, labels = educ25_labels)

# 7. Create educ32 (age 32, Wave 9, highest of academic or vocational)
# Use vectorized operations instead of apply
highest_acad_32 <- map_nvq_5level(df$W9DANVQH)
highest_voc_32 <- map_nvq_5level(df$W9DVNVQH)

# Vectorized logic for choosing highest
df$educ32 <- if_else(
  is.na(highest_acad_32) & is.na(highest_voc_32),
  # Both missing - use the non-missing missing code or -3
  case_when(
    !is.na(df$W9DANVQH) & df$W9DANVQH < 0 ~ df$W9DANVQH,
    !is.na(df$W9DVNVQH) & df$W9DVNVQH < 0 ~ df$W9DVNVQH,
    TRUE ~ -3
  ),
  # At least one is valid
  case_when(
    is.na(highest_acad_32) ~ highest_voc_32,
    is.na(highest_voc_32) ~ highest_acad_32,
    # Both valid
    TRUE ~ {
      # Compare only substantive values (not 95/96)
      acad_sub <- if_else(highest_acad_32 %in% c(95, 96), -2, highest_acad_32)
      voc_sub <- if_else(highest_voc_32 %in% c(95, 96), -2, highest_voc_32)
      
      case_when(
        acad_sub >= 0 & voc_sub >= 0 ~ if_else(acad_sub >= voc_sub, highest_acad_32, highest_voc_32),
        acad_sub >= 0 ~ highest_acad_32,
        TRUE ~ highest_voc_32
      )
    }
  )
)

df$educ32 <- haven::labelled(df$educ32, labels = educ25_labels)

# 8. Create educadtl32 (detailed academic qualifications at age 32)
acad_to_nvq <- c(
  "W9ACQU0A" = 4, "W9ACQU0B" = 4, "W9ACQU0C" = 4,
  "W9ACQU0D" = 3, "W9ACQU0E" = 3, "W9ACQU0F" = 3,
  "W9ACQU0G" = 2, "W9ACQU0H" = 2,
  "W9ACQU0I" = 1, "W9ACQU0J" = 1, "W9ACQU0K" = 1,
  "W9ACQU0L" = 1, "W9ACQU0M" = 1, "W9ACQU0N" = 1,
  "W9ACQU0O" = 1, "W9ACQU0P" = 1, "W9ACQU0Q" = 1,
  "W9ACQU0R" = 0
)

df$highest_acad_detail <- get_highest_nvq_cols(df, names(acad_to_nvq), acad_to_nvq)

# Check for None/Don\'t know/Refused
none_acad_mask <- is.na(df$highest_acad_detail) & !is.na(df$W9ACQU0S) & df$W9ACQU0S == 1
df$highest_acad_detail[none_acad_mask] <- 96

refused_acad_mask <- is.na(df$highest_acad_detail) & !is.na(df$W9ACQU0U) & df$W9ACQU0U == 1
df$highest_acad_detail[refused_acad_mask] <- -9

dk_acad_mask <- is.na(df$highest_acad_detail) & !is.na(df$W9ACQU0T) & df$W9ACQU0T == 1
df$highest_acad_detail[dk_acad_mask] <- -8

df$educadtl32 <- map_nvq_5level(df$highest_acad_detail)

df$educadtl32 <- haven::labelled(df$educadtl32, labels = acad_labels)

# 9. Create educvdtl32 (detailed vocational qualifications at age 32)
voc_to_nvq <- c(
  "W9VCQU0A" = 4, "W9VCQU0B" = 4,
  "W9VCQU0C" = 3, "W9VCQU0D" = 3, "W9VCQU0E" = 3, "W9VCQU0F" = 3,
  "W9VCQU0G" = 3, "W9VCQU0H" = 2,
  "W9VCQU0I" = 3, "W9VCQU0J" = 2, "W9VCQU0K" = 1,
  "W9VCQU0L" = 3, "W9VCQU0M" = 2, "W9VCQU0N" = 1,
  "W9VCQU0O" = 3, "W9VCQU0P" = 2, "W9VCQU0Q" = 1,
  "W9VCQU0R" = 3, "W9VCQU0S" = 3, "W9VCQU0T" = 3,
  "W9VCQU0U" = 3, "W9VCQU0V" = 3, "W9VCQU0W" = 3, "W9VCQU0X" = 1,
  "W9VCQU0Y" = 3, "W9VCQU0Z" = 2, "W9VCQUAA" = 2, "W9VCQUAB" = 1,
  "W9VCQUAC" = 3, "W9VCQUAD" = 3,
  "W9VCQUAE" = 1, "W9VCQUAF" = 0
)

df$highest_voc_detail <- get_highest_nvq_cols(df, names(voc_to_nvq), voc_to_nvq)

# Check for None/Don\'t know/Refused
none_voc_mask <- is.na(df$highest_voc_detail) & !is.na(df$W9VCQUAG) & df$W9VCQUAG == 1
df$highest_voc_detail[none_voc_mask] <- 96

refused_voc_mask <- is.na(df$highest_voc_detail) & !is.na(df$W9VCQUAI) & df$W9VCQUAI == 1
df$highest_voc_detail[refused_voc_mask] <- -9

dk_voc_mask <- is.na(df$highest_voc_detail) & !is.na(df$W9VCQUAH) & df$W9VCQUAH == 1
df$highest_voc_detail[dk_voc_mask] <- -8

df$educvdtl32 <- map_nvq_5level(df$highest_voc_detail)

df$educvdtl32 <- haven::labelled(df$educvdtl32, labels = voc_labels)

# 10. Select only final derived variables and write output
output_vars <- c("NSID", "educ25", "educ32", "educadtl32", "educvdtl32")
output_df <- df %>% select(all_of(output_vars))

write_csv(output_df, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Variables:", paste(names(output_df), collapse = ", "), "\n")
cat("Rows:", nrow(output_df), "\n")

# Summary
cat("\neduc25 distribution:\n")
print(table(output_df$educ25, useNA = "ifany"))
cat("\neduc32 distribution:\n")
print(table(output_df$educ32, useNA = "ifany"))
cat("\neducadtl32 distribution:\n")
print(table(output_df$educadtl32, useNA = "ifany"))
cat("\neducvdtl32 distribution:\n")
print(table(output_df$educvdtl32, useNA = "ifany"))
