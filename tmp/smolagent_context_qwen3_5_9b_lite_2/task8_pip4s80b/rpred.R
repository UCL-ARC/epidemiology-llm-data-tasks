library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

setwd("data")

# ============================================
# STEP 1: Load all datasets
# ============================================

wave1 <- read_delim("input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_main <- read_delim("input/ns8_2015_main_interview.tab", delim = "\t")
wave8_derived <- read_delim("input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("input/ns9_2022_derived_variables.tab", delim = "\t")

cat("All files loaded.\n")

# ============================================
# STEP 2: Merge all datasets by NSID
# ============================================

cohort <- full_join(wave1, wave4, by = "NSID")
cohort <- full_join(cohort, wave8_main, by = "NSID")
cohort <- full_join(cohort, wave8_derived, by = "NSID")
cohort <- full_join(cohort, wave9_main, by = "NSID")
cohort <- full_join(cohort, wave9_derived, by = "NSID")

cat("Merged cohort:", nrow(cohort), "rows\n")

# ============================================
# STEP 3: Create educ25 (Age 25 harmonised NVQ - 5 levels)
# ============================================

cohort <- cohort %>%
  mutate(
    educ25 = case_when(
      is.na(W8DHANVQH) ~ -3,
      W8DHANVQH == 95 ~ 5,
      W8DHANVQH == 96 ~ -1,
      TRUE ~ W8DHANVQH
    )
  )

cat("educ25 unique values:", sort(unique(cohort$educ25)), "\n\n")

# ============================================
# STEP 4: Create educ32 (Age 32 harmonised NVQ - 5 levels)
# ============================================

cohort <- cohort %>%
  mutate(
    educ32 = case_when(
      is.na(W9DANVQH) ~ -3,
      W9DANVQH == 0 ~ -1,
      W9DANVQH == 95 ~ 5,
      W9DANVQH == 96 ~ -1,
      TRUE ~ W9DANVQH
    )
  )

cat("educ32 unique values:", sort(unique(cohort$educ32)), "\n\n")

# ============================================
# STEP 5: Create educadtl32 (Detailed academic qualifications at age 32)
# ============================================

# Initialize educadtl32 to -3 (missing)
cohort <- cohort %>%
  mutate(educadtl32 = -3)

# Create priority columns for each academic variable (Yes=1)
# Doctorate (100) > Masters (90) > Undergraduate (80) > Post-grad diplomas (70) > 
# Diplomas HE (60) > Teaching (50) > A/AS (40) > A-C (30) > D-G (20) > SCE Higher (10) > 
# SCE Sixth (5) > SCE Standard (4) > National 4/5 (3) > National 2/3 (2) > 
# Leaving Cert (1) > Junior Cert A-C (0) > Junior Cert D- (-1) > Other (-2) > 
# None (-3) > Don't know (-4) > Refused (-5) > No answer (-6)

# Create all priority columns in one mutate
cohort <- cohort %>%
  mutate(
    p_A = ifelse(W9ACQU0A == 1, 100, 0),
    p_B = ifelse(W9ACQU0B == 1, 90, 0),
    p_C = ifelse(W9ACQU0C == 1, 80, 0),
    p_D = ifelse(W9ACQU0D == 1, 70, 0),
    p_E = ifelse(W9ACQU0E == 1, 60, 0),
    p_F = ifelse(W9ACQU0F == 1, 50, 0),
    p_G = ifelse(W9ACQU0G == 1, 40, 0),
    p_H = ifelse(W9ACQU0H == 1, 30, 0),
    p_I = ifelse(W9ACQU0I == 1, 20, 0),
    p_J = ifelse(W9ACQU0J == 1, 10, 0),
    p_K = ifelse(W9ACQU0K == 1, 5, 0),
    p_L = ifelse(W9ACQU0L == 1, 4, 0),
    p_M = ifelse(W9ACQU0M == 1, 3, 0),
    p_N = ifelse(W9ACQU0N == 1, 2, 0),
    p_O = ifelse(W9ACQU0O == 1, 1, 0),
    p_P = ifelse(W9ACQU0P == 1, 0, 0),
    p_Q = ifelse(W9ACQU0Q == 1, -1, 0),
    p_R = ifelse(W9ACQU0R == 1, -2, 0),
    p_S = ifelse(W9ACQU0S == 1, -3, 0),
    p_T = ifelse(W9ACQU0T == 1, -4, 0),
    p_U = ifelse(W9ACQU0U == 1, -5, 0),
    p_V = ifelse(W9ACQU0V == 1, -6, 0)
  )

# Get max priority
cohort <- cohort %>%
  mutate(best_priority = pmax(p_A, p_B, p_C, p_D, p_E, p_F, p_G, p_H, p_I, p_J,
                              p_K, p_L, p_M, p_N, p_O, p_P, p_Q, p_R, p_S, p_T, p_U, p_V))

# Map priority to educadtl32
cohort <- cohort %>%
  mutate(
    educadtl32 = case_when(
      best_priority >= 100 ~ 101,
      best_priority >= 90 ~ 102,
      best_priority >= 80 ~ 103,
      best_priority >= 70 ~ 201,
      best_priority >= 60 ~ 202,
      best_priority >= 50 ~ 203,
      best_priority >= 40 ~ 301,
      best_priority >= 30 ~ 302,
      best_priority >= 20 ~ 303,
      best_priority >= 10 ~ 401,
      best_priority >= 5 ~ 402,
      best_priority >= 4 ~ 403,
      best_priority >= 3 ~ 501,
      best_priority >= 2 ~ 502,
      best_priority >= 1 ~ 601,
      best_priority == 0 ~ 602,
      best_priority == -1 ~ 603,
      best_priority == -2 ~ 701,
      best_priority == -3 ~ 999,
      best_priority == -4 ~ -8,
      best_priority == -5 ~ -9,
      best_priority == -6 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  select(-p_A:p_V, -best_priority)

cat("educadtl32 unique values:", sort(unique(cohort$educadtl32)), "\n\n")

# ============================================
# STEP 6: Create educvdtl32 (Detailed vocational qualifications at age 32)
# ============================================

# Initialize to -3
cohort <- cohort %>%
  mutate(educvdtl32 = -3)

# Priority mapping for vocational qualifications
# Professional degree (500) > Nursing/Medical (400) > Level 4-5 (300) > Level 3 (200) > 
# Level 2 (150) > Level 1 (100) > GNVQ Advanced (90) > GNVQ Intermediate (80) > 
# Advanced Diploma (70) > Higher Diploma (60) > RSA Diploma (50) > RSA Stage (40) > 
# BTEC National (30) > SCOTVEC National (20) > HNC (10) > OND (5) > Junior Cert (0) > 
# Other (15) > None (-1) > Don't know (-2) > Refused (-3)

cohort <- cohort %>%
  mutate(
    v_A = ifelse(W9VCQU0A == 1, 500, 0),
    v_B = ifelse(W9VCQU0B == 1, 400, 0),
    v_C = ifelse(W9VCQU0C == 1, 300, 0),
    v_D = ifelse(W9VCQU0D == 1, 200, 0),
    v_E = ifelse(W9VCQU0E == 1, 150, 0),
    v_F = ifelse(W9VCQU0F == 1, 100, 0),
    v_G = ifelse(W9VCQU0G == 1, 90, 0),
    v_H = ifelse(W9VCQU0H == 1, 80, 0),
    v_I = ifelse(W9VCQU0I == 1, 80, 0),
    v_J = ifelse(W9VCQU0J == 1, 70, 0),
    v_K = ifelse(W9VCQU0K == 1, 60, 0),
    v_L = ifelse(W9VCQU0L == 1, 50, 0),
    v_M = ifelse(W9VCQU0M == 1, 40, 0),
    v_N = ifelse(W9VCQU0N == 1, 30, 0),
    v_O = ifelse(W9VCQU0O == 1, 30, 0),
    v_P = ifelse(W9VCQU0P == 1, 20, 0),
    v_Q = ifelse(W9VCQU0Q == 1, 10, 0),
    v_R = ifelse(W9VCQU0R == 1, 95, 0),
    v_S = ifelse(W9VCQU0S == 1, 90, 0),
    v_T = ifelse(W9VCQU0T == 1, 85, 0),
    v_U = ifelse(W9VCQU0U == 1, 80, 0),
    v_V = ifelse(W9VCQU0V == 1, 75, 0),
    v_W = ifelse(W9VCQU0W == 1, 70, 0),
    v_X = ifelse(W9VCQU0X == 1, 60, 0),
    v_Y = ifelse(W9VCQU0Y == 1, 50, 0),
    v_Z = ifelse(W9VCQU0Z == 1, 45, 0),
    v_AA = ifelse(W9VCQUAA == 1, 40, 0),
    v_AB = ifelse(W9VCQUAB == 1, 35, 0),
    v_AC = ifelse(W9VCQUAC == 1, 30, 0),
    v_AD = ifelse(W9VCQUAD == 1, 25, 0),
    v_AE = ifelse(W9VCQUAE == 1, 20, 0),
    v_AF = ifelse(W9VCQUAF == 1, 15, 0),
    v_AG = ifelse(W9VCQUAG == 1, -1, 0),
    v_AH = ifelse(W9VCQUAH == 1, -2, 0),
    v_AI = ifelse(W9VCQUAI == 1, -3, 0)
  )

cohort <- cohort %>%
  mutate(best_voc_priority = pmax(v_A, v_B, v_C, v_D, v_E, v_F, v_G, v_H, v_I, v_J,
                                  v_K, v_L, v_M, v_N, v_O, v_P, v_Q, v_R, v_S, v_T,
                                  v_U, v_V, v_W, v_X, v_Y, v_Z, v_AA, v_AB, v_AC,
                                  v_AD, v_AE, v_AF, v_AG, v_AH, v_AI))

cohort <- cohort %>%
  mutate(
    educvdtl32 = case_when(
      best_voc_priority >= 500 ~ 501,
      best_voc_priority >= 400 ~ 401,
      best_voc_priority >= 300 ~ 301,
      best_voc_priority >= 200 ~ 201,
      best_voc_priority >= 150 ~ 151,
      best_voc_priority >= 100 ~ 101,
      best_voc_priority >= 95 ~ 951,
      best_voc_priority >= 90 ~ 901,
      best_voc_priority >= 85 ~ 851,
      best_voc_priority >= 80 ~ 801,
      best_voc_priority >= 75 ~ 751,
      best_voc_priority >= 70 ~ 701,
      best_voc_priority >= 60 ~ 601,
      best_voc_priority >= 50 ~ 502,
      best_voc_priority >= 45 ~ 451,
      best_voc_priority >= 40 ~ 402,
      best_voc_priority >= 35 ~ 351,
      best_voc_priority >= 30 ~ 302,
      best_voc_priority >= 25 ~ 251,
      best_voc_priority >= 20 ~ 202,
      best_voc_priority >= 15 ~ 152,
      best_voc_priority == -1 ~ -1,
      best_voc_priority == -2 ~ -8,
      best_voc_priority == -3 ~ -9,
      TRUE ~ -3
    )
  ) %>%
  select(-v_A:v_AI, -best_voc_priority)

cat("educvdtl32 unique values:", sort(unique(cohort$educvdtl32)), "\n\n")

# ============================================
# STEP 7: Final output
# ============================================

final_output <- cohort %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

cat("Final output:", nrow(final_output), "rows", ncol(final_output), "columns\n")
cat("Variables:", paste(names(final_output), collapse=", "), "\n\n")

write_csv(final_output, "output/cleaned_data.csv")
cat("Script completed successfully.\n")