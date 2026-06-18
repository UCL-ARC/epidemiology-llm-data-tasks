# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(haven)
library(labelled)

# Load all files from data/input/
df1 <- read_tsv("data/input/wave_one_lsype_young_person_2020.tab")
df4 <- read_tsv("data/input/wave_four_lsype_young_person_2020.tab")
df8_main <- read_tsv("data/input/ns8_2015_main_interview.tab")
df8_derived <- read_tsv("data/input/ns8_2015_derived.tab")
df9_main <- read_tsv("data/input/ns9_2022_main_interview.tab")
df9_derived <- read_tsv("data/input/ns9_2022_derived_variables.tab")

# Merge all files by NSID
merged <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df8_main, by = "NSID") %>%
  full_join(df8_derived, by = "NSID") %>%
  full_join(df9_main, by = "NSID") %>%
  full_join(df9_derived, by = "NSID")

cat("Merged dataset dimensions:", dim(merged), "\n")

# Define NVQ tier mapping for W8VCQU* vocational variables
w8vcqu_tier_map <- c(
  "W8VCQU0A" = 2,
  "W8VCQU0B" = 2,
  "W8VCQU0C" = 2,
  "W8VCQU0D" = 2,
  "W8VCQU0E" = 1,
  "W8VCQU0F" = 1,
  "W8VCQU0G" = 1,
  "W8VCQU0H" = 1,
  "W8VCQU0I" = 1,
  "W8VCQU0J" = 0,
  "W8VCQU0K" = 0,
  "W8VCQU0L" = 0,
  "W8VCQU0M" = 1,
  "W8VCQU0N" = 1,
  "W8VCQU0O" = 3,
  "W8VCQU0P" = 4,
  "W8VCQU0Q" = -8,
  "W8VCQU0R" = -9
)

w8vcqu_vars <- names(w8vcqu_tier_map)

# Map W8DHANVQH to NVQ tier codes
map_w8dhanvqh <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2, 3) ~ 1,
    x %in% c(4, 5) ~ 0,
    x == 95 ~ 3,
    x == 96 ~ 4,
    x == -8 ~ -8,
    x == -9 ~ -9,
    x == -1 ~ -1,
    TRUE ~ NA_real_
  )
}

# Derive educ25
merged <- merged %>%
  mutate(
    educ25_academic = map_w8dhanvqh(W8DHANVQH)
  )

# Map vocational tick-boxes: if Yes (1), get tier; else NA
for (var in w8vcqu_vars) {
  merged[[paste0(var, "_tier")]] <- ifelse(!is.na(merged[[var]]) & merged[[var]] == 1, w8vcqu_tier_map[[var]], NA_real_)
}

# Consolidate educ25 for each row
consolidate_educ25 <- function(academic, vocational_tiers) {
  all_vals <- c(academic, as.vector(vocational_tiers))
  
  # Check for Refused
  if (any(all_vals == -9, na.rm = TRUE)) return(-9)
  # Check for Don't know
  if (any(all_vals == -8, na.rm = TRUE)) return(-8)
  # Check for Not applicable (all sources are -1)
  if (all(all_vals %in% c(-1, NA, -8, -9), na.rm = TRUE)) return(-1)
  # Find minimum valid code (0-4)
  valid_codes <- all_vals[all_vals %in% 0:4]
  if (length(valid_codes) > 0) return(min(valid_codes))
  # All missing
  return(-3)
}

# Apply consolidating row-wise
voc_mat <- as.matrix(merged[, paste0(w8vcqu_vars, "_tier")])

merged$educ25 <- vapply(seq_len(nrow(merged)), function(i) {
  consolidate_educ25(merged$educ25_academic[i], voc_mat[i, ])
}, numeric(1))

# Convert educ25 to factor
educ25_labels <- c(
  `0` = "NVQ 4-5 equivalent",
  `1` = "NVQ 1-3 equivalent",
  `2` = "Entry level or no qualifications",
  `3` = "Other qualifications not mappable to the NVQ framework",
  `4` = "None of these qualifications",
  `-1` = "Not applicable",
  `-3` = "Not asked at fieldwork stage / not interviewed",
  `-8` = "Don't know",
  `-9` = "Refused"
)

merged$educ25 <- factor(merged$educ25, 
                        levels = c(0, 1, 2, 3, 4, -1, -3, -8, -9),
                        labels = educ25_labels,
                        ordered = FALSE)

# Clean up temporary columns
merged <- merged %>%
  select(-educ25_academic, -any_of(paste0(w8vcqu_vars, "_tier")))

cat("educ25 distribution:\n")
print(table(merged$educ25, useNA = "ifany"))

# Derive educ32
map_nvqh <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2, 3) ~ 1,
    x %in% c(4, 5) ~ 0,
    x == 95 ~ 3,
    x == 96 ~ 4,
    x == 0 ~ 2,
    x == -8 ~ -8,
    x == -9 ~ -9,
    x == -1 ~ -1,
    TRUE ~ NA_real_
  )
}

merged <- merged %>%
  mutate(
    educ32_academic = map_nvqh(W9DANVQH),
    educ32_vocational = map_nvqh(W9DVNVQH)
  )

# Consolidate educ32
consolidate_educ32 <- function(academic, vocational) {
  if (!is.na(academic) && !is.na(vocational) && academic %in% 0:4 && vocational %in% 0:4) {
    return(min(academic, vocational))
  }
  if (!is.na(academic) && academic %in% 0:4) return(academic)
  if (!is.na(vocational) && vocational %in% 0:4) return(vocational)
  if (!is.na(academic) && academic %in% c(-1, -8, -9)) return(academic)
  if (!is.na(vocational) && vocational %in% c(-1, -8, -9)) return(vocational)
  return(-3)
}

merged$educ32 <- mapply(consolidate_educ32, merged$educ32_academic, merged$educ32_vocational)

# Convert educ32 to factor
merged$educ32 <- factor(merged$educ32, 
                        levels = c(0, 1, 2, 3, 4, -1, -3, -8, -9),
                        labels = educ25_labels,
                        ordered = FALSE)

# Clean up temporary columns
merged <- merged %>%
  select(-educ32_academic, -educ32_vocational)

cat("\neduc32 distribution:\n")
print(table(merged$educ32, useNA = "ifany"))

# Derive educadtl32
w9acqu_substantive <- c("W9ACQU0A", "W9ACQU0B", "W9ACQU0C", "W9ACQU0D", 
                        "W9ACQU0E", "W9ACQU0F", "W9ACQU0G", "W9ACQU0H", 
                        "W9ACQU0I", "W9ACQU0J", "W9ACQU0K", "W9ACQU0L", 
                        "W9ACQU0M", "W9ACQU0N", "W9ACQU0O", "W9ACQU0P", 
                        "W9ACQU0Q", "W9ACQU0R")
w9acqu_none <- "W9ACQU0S"
w9acqu_nonsubstantive <- c("W9ACQU0T", "W9ACQU0U", "W9ACQU0V")

# Get variable labels for academic qualifications from metadata
acq_labels_raw <- c(
  "Doctorate or equivalent",
  "Masters or equivalent",
  "Undergraduate or equivalent",
  "Post-graduate Diplomas and Certificates",
  "Diplomas in higher education and other higher education qualifications",
  "Teaching qualifications for schools or further education (below degree level)",
  "A/AS Levels or equivalent",
  "Grade A-C, Level 4-9",
  "Grade D-G, Level 1-3",
  "SCE Higher",
  "Scottish Certificate Sixth Year Studies",
  "SCE Standard",
  "National 4 and 5",
  "National 2 and 3",
  "Leaving Certificate",
  "Junior Certificate grade A-C",
  "Junior Certificate grade D and below",
  "Other academic qualifications (including overseas)"
)

# Derive educadtl32
consolidate_educadtl32 <- function(vars_substantive, var_none, vars_nonsubstantive) {
  n_sub <- length(vars_substantive)
  vars_substantive <- as.numeric(vars_substantive)
  var_none <- as.numeric(var_none)
  vars_nonsubstantive <- as.numeric(vars_nonsubstantive)
  
  # Check non-substantive indicators first
  for (i in seq_along(vars_nonsubstantive)) {
    if (!is.na(vars_nonsubstantive[i]) && vars_nonsubstantive[i] == 1) {
      if (i == 1) return(-8)
      if (i == 2) return(-9)
      if (i == 3) return(-2)
    }
  }
  
  # Check all variables for -1
  all_vars <- c(vars_substantive, var_none, vars_nonsubstantive)
  if (any(all_vars == -1, na.rm = TRUE)) return(-1)
  
  # Check all variables for -3 or NA
  if (any(is.na(all_vars) | all_vars == -3, na.rm = TRUE)) return(-3)
  
  # Scan substantive indicators in order
  for (i in seq_along(vars_substantive)) {
    if (!is.na(vars_substantive[i]) && vars_substantive[i] == 1) {
      return(i)
    }
  }
  
  # All substantive indicators are No or not asked -> "None of these"
  return(n_sub + 1)
}

# Apply row-wise
merged$educadtl32 <- vapply(seq_len(nrow(merged)), function(i) {
  consolidate_educadtl32(
    as.numeric(merged[i, w9acqu_substantive]),
    merged[i, w9acqu_none],
    as.numeric(merged[i, w9acqu_nonsubstantive])
  )
}, numeric(1))

# Create factor labels for educadtl32
n_ac_sub <- length(w9acqu_substantive)
acdtl_levels <- c(seq_len(n_ac_sub), 
                  n_ac_sub + 1,
                  -1, -2, -3, -8, -9)
acdtl_labels <- c(acq_labels_raw, 
                  "None of these qualifications",
                  "Not applicable",
                  "No answer",
                  "Not asked at fieldwork stage / not interviewed",
                  "Don't know",
                  "Refused")

merged$educadtl32 <- factor(merged$educadtl32,
                            levels = acdtl_levels,
                            labels = acdtl_labels,
                            ordered = FALSE)

cat("\neducadtl32 distribution:\n")
print(table(merged$educadtl32, useNA = "ifany"))

# Derive educvdtl32
w9vcqu_substantive <- c("W9VCQU0A", "W9VCQU0B", "W9VCQU0C", "W9VCQU0D", 
                        "W9VCQU0E", "W9VCQU0F", "W9VCQU0G", "W9VCQU0H", 
                        "W9VCQU0I", "W9VCQU0J", "W9VCQU0K", "W9VCQU0L", 
                        "W9VCQU0M", "W9VCQU0N", "W9VCQU0O", "W9VCQU0P", 
                        "W9VCQU0Q", "W9VCQU0R", "W9VCQU0S", "W9VCQU0T", 
                        "W9VCQU0U", "W9VCQU0V", "W9VCQU0W", "W9VCQU0X", 
                        "W9VCQU0Y", "W9VCQU0Z", "W9VCQUAA", "W9VCQUAB", 
                        "W9VCQUAC", "W9VCQUAD", "W9VCQUAE", "W9VCQUAF")
w9vcqu_none <- "W9VCQUAG"
w9vcqu_nonsubstantive <- c("W9VCQUAH", "W9VCQUAI")

# Get variable labels for vocational qualifications from metadata
vcq_labels_raw <- c(
  "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  "Nursing or other medical qualifications (below degree level)",
  "Level 4 or 5",
  "Level 3",
  "Level 2",
  "Level 1",
  "GNVQ Advanced",
  "GNVQ Intermediate",
  "Level 3",
  "Level 2",
  "Level Foundation",
  "Advanced Craft, Part III",
  "Craft, Part II",
  "Craft, Part I",
  "Level 3",
  "Level 2",
  "Level 1",
  "Advanced Diploma",
  "Higher Diploma",
  "RSA Diploma",
  "RSA Stage I, II,III",
  "Higher Level BTEC",
  "BTEC National",
  "BTEC First",
  "SCOTVEC National Certificate",
  "SCOTVEC first or general diploma",
  "SCOTVEC general diploma",
  "SCOTVEC modules",
  "HND or  HNC",
  "OND or ONCM",
  "Junior certificate",
  "Other vocational qualifications (including some overseas)"
)

# Derive educvdtl32
consolidate_educvdtl32 <- function(vars_substantive, var_none, vars_nonsubstantive) {
  n_sub <- length(vars_substantive)
  vars_substantive <- as.numeric(vars_substantive)
  var_none <- as.numeric(var_none)
  vars_nonsubstantive <- as.numeric(vars_nonsubstantive)
  
  # Check non-substantive indicators first (only Don't know and Refused)
  for (i in seq_along(vars_nonsubstantive)) {
    if (!is.na(vars_nonsubstantive[i]) && vars_nonsubstantive[i] == 1) {
      if (i == 1) return(-8)
      if (i == 2) return(-9)
    }
  }
  
  # Check all variables for -1
  all_vars <- c(vars_substantive, var_none, vars_nonsubstantive)
  if (any(all_vars == -1, na.rm = TRUE)) return(-1)
  
  # Check all variables for -3 or NA
  if (any(is.na(all_vars) | all_vars == -3, na.rm = TRUE)) return(-3)
  
  # Scan substantive indicators in order
  for (i in seq_along(vars_substantive)) {
    if (!is.na(vars_substantive[i]) && vars_substantive[i] == 1) {
      return(i)
    }
  }
  
  # All substantive indicators are No or not asked -> "None of these"
  return(n_sub + 1)
}

# Apply row-wise
merged$educvdtl32 <- vapply(seq_len(nrow(merged)), function(i) {
  consolidate_educvdtl32(
    as.numeric(merged[i, w9vcqu_substantive]),
    merged[i, w9vcqu_none],
    as.numeric(merged[i, w9vcqu_nonsubstantive])
  )
}, numeric(1))

# Create factor labels for educvdtl32
n_vc_sub <- length(w9vcqu_substantive)
vdtl_levels <- c(seq_len(n_vc_sub), 
                 n_vc_sub + 1,
                 -1, -3, -8, -9)
vdtl_labels <- c(vcq_labels_raw, 
                 "None of these qualifications",
                 "Not applicable",
                 "Not asked at fieldwork stage / not interviewed",
                 "Don't know",
                 "Refused")

merged$educvdtl32 <- factor(merged$educvdtl32,
                            levels = vdtl_levels,
                            labels = vdtl_labels,
                            ordered = FALSE)

cat("\neducvdtl32 distribution:\n")
print(table(merged$educvdtl32, useNA = "ifany"))

# Select only NSID and final derived variables
output <- merged %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(output, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", dim(output), "\n")
cat("Variables in output:", names(output), "\n")
