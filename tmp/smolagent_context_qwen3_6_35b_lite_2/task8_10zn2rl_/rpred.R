library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Define file paths
files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  ns8_main = "data/input/ns8_2015_main_interview.tab",
  ns8_derived = "data/input/ns8_2015_derived.tab",
  ns9_main = "data/input/ns9_2022_main_interview.tab",
  ns9_derived = "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
print("Loading files...")
wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
ns8_main <- read_delim(files$ns8_main, delim = "\t", show_col_types = FALSE)
ns8_derived <- read_delim(files$ns8_derived, delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim(files$ns9_main, delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim(files$ns9_derived, delim = "\t", show_col_types = FALSE)

print(paste("Wave 1:", nrow(wave1), "cases"))
print(paste("Wave 4:", nrow(wave4), "cases"))
print(paste("NS8 Main:", nrow(ns8_main), "cases"))
print(paste("NS8 Derived:", nrow(ns8_derived), "cases"))
print(paste("NS9 Main:", nrow(ns9_main), "cases"))
print(paste("NS9 Derived:", nrow(ns9_derived), "cases"))

# Merge all datasets
print("Merging datasets...")
df <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

print(paste("Merged dataset:", nrow(df), "cases"))

# ============================================================
# Step 1: Create educ25 from W8DHANVQH (Wave 8 = Age 25)
# ============================================================

# W8DHANVQH has:
# -9: Refused, -8: Insufficient information, -1: Not applicable
# 1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3
# 4: NVQ Level 4, 5: NVQ Level 5
# 95: Other academic qualification, 96: None of these qualifications

# Create educ25 with 5-level NVQ scheme
# Harmonised categories: Entry Level, Level 1, Level 2, Level 3, Level 4-5
# Plus: Other qualification, None

df <- df %>%
  mutate(educ25 = case_when(
    W8DHANVQH == 1 ~ 1,
    W8DHANVQH == 2 ~ 2,
    W8DHANVQH == 3 ~ 3,
    W8DHANVQH %in% c(4, 5) ~ 4,
    W8DHANVQH == 95 ~ 5,
    W8DHANVQH == 96 ~ 6,
    W8DHANVQH == -9 ~ -9,
    W8DHANVQH == -8 ~ -8,
    W8DHANVQH == -1 ~ -1,
    is.na(W8DHANVQH) ~ -3,
    TRUE ~ -3
  ))

# Set labels for educ25
labels_educ25 <- c(
  "NVQ Level 1" = 1,
  "NVQ Level 2" = 2,
  "NVQ Level 3" = 3,
  "NVQ Level 4-5" = 4,
  "Other qualification" = 5,
  "None of these qualifications" = 6
)

df$educ25 <- haven::labelled_spss(
  df$educ25,
  labels = labels_educ25,
  na_values = c(-9, -8, -1, -3)
)

print("educ25 created")
print(table(df$educ25, useNA = "ifany"))

# ============================================================
# Step 2: Create educ32 from W9DANVQH and W9DVNVQH (Wave 9 = Age 32)
# ============================================================

# W9DANVQH (academic) has:
# 0: NVQ Entry Level, 1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3
# 4: NVQ Level 4, 5: NVQ Level 5
# 95: Other academic qualification, 96: None of these qualifications
# Missing: -9, -8, -1

# W9DVNVQH (vocational) has:
# 0: NVQ Entry Level, 1: NVQ Level 1, 2: NVQ Level 2, 3: NVQ Level 3
# 4: NVQ Level 4, 5: NVQ Level 5
# 95: Other vocational qualification, 96: None of these qualifications
# Missing: -9, -8, -1

# For educ32, take the highest NVQ level from either academic or vocational
# Prioritize substantive responses over missing codes
# Use most-recent-valid-first rule (both from same wave, so just take highest)

df <- df %>%
  mutate(
    # Determine which has valid data
    acad_valid = !is.na(W9DANVQH) & W9DANVQH >= 0,
    voc_valid = !is.na(W9DVNVQH) & W9DVNVQH >= 0,
    
    # Get the highest NVQ level (0-5 for Entry-5, or 95/96 for other/none)
    max_nvq = pmax(
      ifelse(acad_valid, W9DANVQH, -1),
      ifelse(voc_valid, W9DVNVQH, -1),
      na.rm = TRUE
    ),
    
    # Check if any valid response exists
    has_valid = acad_valid | voc_valid
  )

df <- df %>%
  mutate(educ32 = case_when(
    !has_valid ~ -3,
    max_nvq == 0 ~ 0,
    max_nvq == 1 ~ 1,
    max_nvq == 2 ~ 2,
    max_nvq == 3 ~ 3,
    max_nvq %in% c(4, 5) ~ 4,
    max_nvq == 95 ~ 5,
    max_nvq == 96 ~ 6,
    TRUE ~ -3
  ))

# Set labels for educ32
labels_educ32 <- c(
  "NVQ Entry Level" = 0,
  "NVQ Level 1" = 1,
  "NVQ Level 2" = 2,
  "NVQ Level 3" = 3,
  "NVQ Level 4-5" = 4,
  "Other qualification" = 5,
  "None of these qualifications" = 6
)

df$educ32 <- haven::labelled_spss(
  df$educ32,
  labels = labels_educ32,
  na_values = c(-9, -8, -1, -3)
)

print("educ32 created")
print(table(df$educ32, useNA = "ifany"))

# ============================================================
# Step 3: Create educadtl32 (detailed academic qualifications at age 32)
# ============================================================

# Use W9ACQU0A through W9ACQU0U to determine highest academic qualification
# Create a detailed classification based on qualification levels

# Academic qualification variables (Yes=1, No=2)
# We need to find the highest level achieved

# Define qualification hierarchy for detailed academic variables
# Higher education degrees: Doctorate, Masters, Undergraduate, Post-grad diplomas
# Higher education diplomas: Diplomas in HE
# A-Levels and equivalents
# GCSE/O-level equivalents
# Scottish qualifications
# International qualifications
# None

# First, create a flag for each academic qualification type
df <- df %>%
  mutate(
    # Higher education degrees (highest to lowest)
    has_doctorate = (W9ACQU0A == 1),
    has_masters = (W9ACQU0B == 1),
    has_undergrad = (W9ACQU0C == 1),
    has_postgrad_dip = (W9ACQU0D == 1),
    has_he_diploma = (W9ACQU0E == 1),
    has_teaching_qual = (W9ACQU0F == 1),
    
    # A/AS Levels and equivalents
    has_alevels = (W9ACQU0G == 1),
    has_gcse_ac = (W9ACQU0H == 1),
    
    # GCSE/O-level equivalents (D-G, Level 1-3)
    has_gcse_dg = (W9ACQU0I == 1),
    
    # Scottish qualifications
    has_sce_higher = (W9ACQU0J == 1),
    has_scottish_sixth = (W9ACQU0K == 1),
    has_sce_standard = (W9ACQU0L == 1),
    has_national_45 = (W9ACQU0M == 1),
    has_national_23 = (W9ACQU0N == 1),
    
    # International
    has_leaving_cert = (W9ACQU0O == 1),
    has_junior_ac = (W9ACQU0P == 1),
    has_junior_dg = (W9ACQU0Q == 1),
    
    # Other
    has_other_acad = (W9ACQU0R == 1),
    has_none = (W9ACQU0S == 1),
    
    # Missing
    acad_dk = (W9ACQU0T == 1),
    acad_refused = (W9ACQU0U == 1),
    acad_noanswer = (W9ACQU0V == 1)
  )

# Now derive the detailed academic qualification
df <- df %>%
  mutate(educadtl32 = case_when(
    # Missing codes
    acad_refused ~ -9,
    is.na(acad_refused) & (acad_dk == 1 | acad_noanswer == 1) ~ -8,
    is.na(acad_refused) & is.na(acad_dk) & is.na(acad_noanswer) ~ -3,
    
    # Highest academic qualifications (in priority order)
    has_doctorate ~ 1,
    has_masters ~ 2,
    has_undergrad ~ 3,
    has_postgrad_dip ~ 4,
    has_he_diploma ~ 5,
    has_teaching_qual ~ 6,
    has_alevels ~ 7,
    has_gcse_ac ~ 8,
    has_sce_higher ~ 9,
    has_scottish_sixth ~ 10,
    has_gcse_dg ~ 11,
    has_sce_standard ~ 12,
    has_national_45 ~ 13,
    has_national_23 ~ 14,
    has_leaving_cert ~ 15,
    has_junior_ac ~ 16,
    has_junior_dg ~ 17,
    has_other_acad ~ 18,
    has_none ~ 19,
    TRUE ~ -3
  ))

# Set labels for educadtl32
labels_educadtl32 <- c(
  "Doctorate or equivalent" = 1,
  "Masters or equivalent" = 2,
  "Undergraduate degree or equivalent" = 3,
  "Post-graduate Diploma/Certificate" = 4,
  "Diploma in higher education" = 5,
  "Teaching qualification (below degree)" = 6,
  "A/AS Levels or equivalent" = 7,
  "GCSE A-C / Grade 4-9" = 8,
  "SCE Higher / Scottish Higher" = 9,
  "Scottish Certificate Sixth Year" = 10,
  "GCSE D-G / Grade 1-3" = 11,
  "SCE Standard / Scottish Standard" = 12,
  "National 4 and 5" = 13,
  "National 2 and 3" = 14,
  "Leaving Certificate" = 15,
  "Junior Certificate A-C" = 16,
  "Junior Certificate D and below" = 17,
  "Other academic qualification" = 18,
  "None of these qualifications" = 19
)

df$educadtl32 <- haven::labelled_spss(
  df$educadtl32,
  labels = labels_educadtl32,
  na_values = c(-9, -8, -3)
)

print("educadtl32 created")
print(table(df$educadtl32, useNA = "ifany"))

# ============================================================
# Step 4: Create educvdtl32 (detailed vocational qualifications at age 32)
# ============================================================

# Use W9VCQU0A through W9VCQUAI to determine highest vocational qualification

# Define vocational qualification hierarchy
df <- df %>%
  mutate(
    # Professional/degree-level vocational
    has_prof_deg = (W9VCQU0A == 1),
    has_nursing_med = (W9VCQU0B == 1),
    has_hnd_hnc = (W9VCQUAC == 1),
    has_ond_ondcm = (W9VCQUAD == 1),
    
    # Level 4-5 vocational
    has_voc_level5 = (W9VCQU0C == 1),
    
    # Advanced diplomas and equivalents
    has_adv_diploma = (W9VCQU0R == 1),
    has_high_diploma = (W9VCQU0S == 1),
    has_high_btec = (W9VCQU0V == 1),
    
    # Level 3 vocational
    has_btec_national = (W9VCQU0W == 1),
    has_voc_level3 = (W9VCQU0D == 1) | (W9VCQU0I == 1) | (W9VCQU0O == 1),
    has_rsa_dip = (W9VCQU0T == 1),
    has_rsa_stages = (W9VCQU0U == 1),
    has_scotvec_nat = (W9VCQU0Y == 1),
    has_scotvec_dip = (W9VCQU0Z == 1) | (W9VCQUAA == 1),
    has_gnvq_adv = (W9VCQU0G == 1),
    has_adv_craft = (W9VCQU0L == 1),
    
    # Level 2 vocational
    has_btec_first = (W9VCQU0X == 1),
    has_scotvec_mod = (W9VCQUAB == 1),
    has_junior_cert = (W9VCQUAE == 1),
    has_voc_level2 = (W9VCQU0E == 1) | (W9VCQU0J == 1) | (W9VCQU0P == 1),
    has_gnvq_int = (W9VCQU0H == 1),
    has_craft_part2 = (W9VCQU0M == 1),
    
    # Level 1 vocational
    has_voc_level1 = (W9VCQU0F == 1) | (W9VCQU0Q == 1),
    has_craft_part1 = (W9VCQU0N == 1),
    has_foundation = (W9VCQU0K == 1),
    
    # Other/None/Missing
    has_other_voc = (W9VCQUAF == 1),
    has_none_voc = (W9VCQUAG == 1),
    voc_dk = (W9VCQUAH == 1),
    voc_refused = (W9VCQUAI == 1)
  )

# Derive detailed vocational qualification
df <- df %>%
  mutate(educvdtl32 = case_when(
    # Missing codes
    voc_refused ~ -9,
    is.na(voc_refused) & (voc_dk == 1) ~ -8,
    is.na(voc_refused) & is.na(voc_dk) ~ -3,
    
    # Professional/degree level
    has_prof_deg ~ 1,
    has_nursing_med ~ 2,
    has_hnd_hnc ~ 3,
    has_ond_ondcm ~ 4,
    has_voc_level5 ~ 5,
    has_high_diploma ~ 6,
    has_high_btec ~ 7,
    
    # Advanced diplomas
    has_adv_diploma ~ 8,
    has_rsa_dip ~ 9,
    has_gnvq_adv ~ 10,
    
    # Level 3
    has_btec_national ~ 11,
    has_scotvec_nat ~ 12,
    has_scotvec_dip ~ 13,
    has_voc_level3 ~ 14,
    has_adv_craft ~ 15,
    has_rsa_stages ~ 16,
    
    # Level 2
    has_btec_first ~ 17,
    has_scotvec_mod ~ 18,
    has_junior_cert ~ 19,
    has_gnvq_int ~ 20,
    has_voc_level2 ~ 21,
    has_craft_part2 ~ 22,
    
    # Level 1
    has_voc_level1 ~ 23,
    has_craft_part1 ~ 24,
    has_foundation ~ 25,
    
    # Other/None
    has_other_voc ~ 26,
    has_none_voc ~ 27,
    TRUE ~ -3
  ))

# Set labels for educvdtl32
labels_educvdtl32 <- c(
  "Professional qualification at degree level" = 1,
  "Nursing or medical qualifications" = 2,
  "HND or HNC" = 3,
  "OND or ONCM" = 4,
  "Level 5 vocational" = 5,
  "Higher Diploma" = 6,
  "Higher Level BTEC" = 7,
  "Advanced Diploma" = 8,
  "RSA Diploma" = 9,
  "GNVQ Advanced" = 10,
  "BTEC National / Level 3" = 11,
  "SCOTVEC National Certificate" = 12,
  "SCOTVEC Diploma" = 13,
  "Other Level 3 vocational" = 14,
  "Advanced Craft Part III" = 15,
  "RSA Stages I-III" = 16,
  "BTEC First" = 17,
  "SCOTVEC Modules" = 18,
  "Junior Certificate" = 19,
  "GNVQ Intermediate" = 20,
  "Other Level 2 vocational" = 21,
  "Craft Part II" = 22,
  "Level 1 vocational" = 23,
  "Craft Part I" = 24,
  "Foundation level" = 25,
  "Other vocational qualification" = 26,
  "None of these qualifications" = 27
)

df$educvdtl32 <- haven::labelled_spss(
  df$educvdtl32,
  labels = labels_educvdtl32,
  na_values = c(-9, -8, -3)
)

print("educvdtl32 created")
print(table(df$educvdtl32, useNA = "ifany"))

# ============================================================
# Step 5: Create final output with only required variables
# ============================================================

# Keep only NSID and the four derived variables
final_df <- df %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

print("Final dataset structure:")
print(str(final_df))
print(paste("Final dataset rows:", nrow(final_df)))

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

print("Output written to data/output/cleaned_data.csv")
print("Done!")
