# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load data files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged <- full_join(wave1, wave4, by = "NSID")
merged <- full_join(merged, ns8_main, by = "NSID")
merged <- full_join(merged, ns8_derived, by = "NSID")
merged <- full_join(merged, ns9_main, by = "NSID")
merged <- full_join(merged, ns9_derived, by = "NSID")

# ============ Create educ25 (age 25, harmonized 5-level NVQ) ============
# From ns8_derived: W8DHANVQH contains NVQ levels from academic qualifications
# Need to derive vocational NVQ levels from ns8_main

# Map vocational qualifications to NVQ levels for age 25
merged$w8_voc_nvq <- ifelse(merged$W8VCQU0J == 1 | merged$W8VCQU0K == 1 | 
                             merged$W8VCQU0L == 1 | merged$W8VCQU0M == 1 | 
                             merged$W8VCQU0N == 1 | merged$W8VCQU0O == 1, 2,
                         ifelse(merged$W8VCQU0I == 1, 1,
                         ifelse(merged$W8VCQU0P == 1 | merged$W8VCQU0Q == 1 | 
                                merged$W8VCQU0R == 1, 6, -3)))

# Get highest NVQ level from academic qualifications
# W8DHANVQH: 1=Level 1, 2=Level 2, 3=Level 3, 4=Level 4, 5=Level 5, 
#            95=Other academic, 96=None

# Combine academic and vocational to get highest overall NVQ level
merged$w8_highest_nvq <- ifelse(merged$W8DHANVQH != -1 & merged$W8DHANVQH >= merged$w8_voc_nvq, 
                                 merged$W8DHANVQH, merged$w8_voc_nvq)

# Map to harmonized 5-level scheme for educ25
# Categories: 1=NVQ 1, 2=NVQ 2, 3=NVQ 3, 4=NVQ 4-5, 5=None, 6=Other
merged$educ25 <- ifelse(merged$w8_highest_nvq == -1, -1,
  ifelse(merged$w8_highest_nvq == 1, 1,
  ifelse(merged$w8_highest_nvq == 2, 2,
  ifelse(merged$w8_highest_nvq == 3, 3,
  ifelse(merged$w8_highest_nvq == 4 | merged$w8_highest_nvq == 5, 4,
  ifelse(merged$w8_highest_nvq == 95, 5,
  ifelse(merged$w8_highest_nvq == 96, 5,
  ifelse(merged$w8_highest_nvq == -9, -9,
  ifelse(merged$w8_highest_nvq == -8, -8,
  ifelse(merged$w8_highest_nvq == -3, -3,
  -3))))))))))

# ============ Create educ32 (age 32, harmonized 5-level NVQ) ============
# From ns9_derived: W9DANVQH (academic) and W9DVNVQH (vocational)
# W9DANVQH: 0=Entry, 1=Level 1, 2=Level 2, 3=Level 3, 4=Level 4, 5=Level 5,
#           95=Other academic, 96=None
# W9DVNVQH: 0=Entry, 1=Level 1, 2=Level 2, 3=Level 3, 4=Level 4, 5=Level 5,
#           95=Other vocational, 96=None

# Convert W9DANVQH (academic NVQ levels) to harmonized 5-level
merged$educ32_academic <- ifelse(merged$W9DANVQH == -1, -1,
  ifelse(merged$W9DANVQH == 0 | merged$W9DANVQH == 1, 1,
  ifelse(merged$W9DANVQH == 2, 2,
  ifelse(merged$W9DANVQH == 3, 3,
  ifelse(merged$W9DANVQH == 4 | merged$W9DANVQH == 5, 4,
  ifelse(merged$W9DANVQH == 95, 5,
  ifelse(merged$W9DANVQH == 96, 5,
  ifelse(merged$W9DANVQH == -9, -9,
  ifelse(merged$W9DANVQH == -8, -8,
  ifelse(merged$W9DANVQH == -3, -3,
  -3))))))))))

# Convert W9DVNVQH (vocational NVQ levels) to harmonized 5-level
merged$educ32_vocational <- ifelse(merged$W9DVNVQH == -1, -1,
  ifelse(merged$W9DVNVQH == 0 | merged$W9DVNVQH == 1, 1,
  ifelse(merged$W9DVNVQH == 2, 2,
  ifelse(merged$W9DVNVQH == 3, 3,
  ifelse(merged$W9DVNVQH == 4 | merged$W9DVNVQH == 5, 4,
  ifelse(merged$W9DVNVQH == 95, 5,
  ifelse(merged$W9DVNVQH == 96, 5,
  ifelse(merged$W9DVNVQH == -9, -9,
  ifelse(merged$W9DVNVQH == -8, -8,
  ifelse(merged$W9DVNVQH == -3, -3,
  -3))))))))))

# Combine to get highest overall (prioritize academic if both present)
merged$educ32 <- ifelse(!is.na(merged$educ32_academic), merged$educ32_academic,
  ifelse(!is.na(merged$educ32_vocational), merged$educ32_vocational, -3))

# ============ Create educadtl32 (detailed academic qualifications at age 32) ============
merged$educadtl32 <- ifelse(merged$W9DANVQH == -1, -1,
  ifelse(merged$W9DANVQH == 0 | merged$W9DANVQH == 1, 1,
  ifelse(merged$W9DANVQH == 2, 2,
  ifelse(merged$W9DANVQH == 3, 3,
  ifelse(merged$W9DANVQH == 4 | merged$W9DANVQH == 5, 4,
  ifelse(merged$W9DANVQH == 95, 5,
  ifelse(merged$W9DANVQH == 96, 5,
  ifelse(merged$W9DANVQH == -9, -9,
  ifelse(merged$W9DANVQH == -8, -8,
  ifelse(merged$W9DANVQH == -3, -3,
  -3))))))))))

# ============ Create educvdtl32 (detailed vocational qualifications at age 32) ============
merged$educvdtl32 <- ifelse(merged$W9DVNVQH == -1, -1,
  ifelse(merged$W9DVNVQH == 0 | merged$W9DVNVQH == 1, 1,
  ifelse(merged$W9DVNVQH == 2, 2,
  ifelse(merged$W9DVNVQH == 3, 3,
  ifelse(merged$W9DVNVQH == 4 | merged$W9DVNVQH == 5, 4,
  ifelse(merged$W9DVNVQH == 95, 5,
  ifelse(merged$W9DVNVQH == 96, 5,
  ifelse(merged$W9DVNVQH == -9, -9,
  ifelse(merged$W9DVNVQH == -8, -8,
  ifelse(merged$W9DVNVQH == -3, -3,
  -3))))))))))

# Keep only final variables
final_data <- merged %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

# Check output
print(head(final_data))
print(summary(final_data))
