library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all input files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Process educ25 (wave 8)
educ25_data <- merged_data %>%
  mutate(
    # W8DHANVQH mapping
    w8dh_lev = case_when(
      W8DHANVQH %in% c(-9, -8, -3, -2, -1) ~ W8DHANVQH,
      W8DHANVQH == 4 | W8DHANVQH == 5 ~ 0,
      W8DHANVQH %in% c(1, 2, 3) ~ 1,
      W8DHANVQH == 95 ~ 3,
      W8DHANVQH == 96 ~ 4,
      TRUE ~ NA_real_
    ),
    # W8VCQU variables mapping
    w8vcqu0A_lev = ifelse(W8VCQU0A == 1.0, 2, NA_real_),
    w8vcqu0B_lev = ifelse(W8VCQU0B == 1.0, 2, NA_real_),
    w8vcqu0C_lev = ifelse(W8VCQU0C == 1.0, 2, NA_real_),
    w8vcqu0D_lev = ifelse(W8VCQU0D == 1.0, 2, NA_real_),
    w8vcqu0E_lev = ifelse(W8VCQU0E == 1.0, 0, NA_real_),
    w8vcqu0F_lev = ifelse(W8VCQU0F == 1.0, 1, NA_real_),
    w8vcqu0G_lev = ifelse(W8VCQU0G == 1.0, 1, NA_real_),
    w8vcqu0H_lev = ifelse(W8VCQU0H == 1.0, 1, NA_real_),
    w8vcqu0I_lev = ifelse(W8VCQU0I == 1.0, 1, NA_real_),
    w8vcqu0J_lev = ifelse(W8VCQU0J == 1.0, 0, NA_real_),
    w8vcqu0K_lev = ifelse(W8VCQU0K == 1.0, 0, NA_real_),
    w8vcqu0L_lev = ifelse(W8VCQU0L == 1.0, 0, NA_real_),
    w8vcqu0M_lev = ifelse(W8VCQU0M == 1.0, 1, NA_real_),
    w8vcqu0N_lev = ifelse(W8VCQU0N == 1.0, 1, NA_real_),
    w8vcqu0O_lev = ifelse(W8VCQU0O == 1.0, 3, NA_real_),
    w8vcqu0P_lev = ifelse(W8VCQU0P == 1.0, 4, NA_real_),
    w8vcqu0Q_lev = NA_real_,  # "Don't know" is missing code
    w8vcqu0R_lev = NA_real_   # "Refused" is missing code
  ) %>%
  rowwise() %>%
  mutate(
    educ25 = {
      vec <- c(w8dh_lev, w8vcqu0A_lev, w8vcqu0B_lev, w8vcqu0C_lev, w8vcqu0D_lev, w8vcqu0E_lev, w8vcqu0F_lev, w8vcqu0G_lev, w8vcqu0H_lev, w8vcqu0I_lev, w8vcqu0J_lev, w8vcqu0K_lev, w8vcqu0L_lev, w8vcqu0M_lev, w8vcqu0N_lev, w8vcqu0O_lev, w8vcqu0P_lev, w8vcqu0Q_lev, w8vcqu0R_lev)
      valid_vec <- vec[!is.na(vec)]
      if (length(valid_vec) > 0) min(valid_vec) else -3
    }
  ) %>%
  ungroup() %>%
  mutate(educ25 = factor(educ25, levels = c(0, 1, 2, 3, 4, -9, -8, -3, -2, -1)))

# Process educ32 (wave 9)
educ32_data <- merged_data %>%
  mutate(
    # W9DANVQH mapping
    w9da_lev = case_when(
      W9DANVQH %in% c(-9, -8, -3, -2, -1) ~ W9DANVQH,
      W9DANVQH == 0 ~ 2,
      W9DANVQH %in% c(1, 2, 3) ~ 1,
      W9DANVQH %in% c(4, 5) ~ 0,
      W9DANVQH == 95 ~ 3,
      W9DANVQH == 96 ~ 4,
      TRUE ~ NA_real_
    ),
    # W9DVNVQH mapping
    w9dv_lev = case_when(
      W9DVNVQH %in% c(-9, -8, -3, -2, -1) ~ W9DVNVQH,
      W9DVNVQH == 0 ~ 2,
      W9DVNVQH %in% c(1, 2, 3) ~ 1,
      W9DVNVQH %in% c(4, 5) ~ 0,
      W9DVNVQH == 95 ~ 3,
      W9DVNVQH == 96 ~ 4,
      TRUE ~ NA_real_
    ),
    # Handle missing codes properly
    w9da_lev_valid = ifelse(w9da_lev %in% c(-9, -8, -3, -2, -1), NA_real_, w9da_lev),
    w9dv_lev_valid = ifelse(w9dv_lev %in% c(-9, -8, -3, -2, -1), NA_real_, w9dv_lev),
    educ32 = ifelse(
      is.na(w9da_lev_valid) & is.na(w9dv_lev_valid),
      -3,
      min(c(w9da_lev_valid, w9dv_lev_valid), na.rm = TRUE)
    )
  ) %>%
  mutate(educ32 = factor(educ32, levels = c(0, 1, 2, 3, 4, -9, -8, -3, -2, -1)))

# Process educadtl32 (academic detail)
educadtl32_data <- merged_data %>%
  mutate(
    W9ACQU0A_label = ifelse(W9ACQU0A == 1.0, "Doctorate or equivalent", ifelse(W9ACQU0A %in% c(-3, -1), W9ACQU0A, NA_character_)),
    W9ACQU0B_label = ifelse(W9ACQU0B == 1.0, "Masters or equivalent", ifelse(W9ACQU0B %in% c(-3, -1), W9ACQU0B, NA_character_)),
    W9ACQU0C_label = ifelse(W9ACQU0C == 1.0, "Undergraduate or equivalent", ifelse(W9ACQU0C %in% c(-3, -1), W9ACQU0C, NA_character_)),
    W9ACQU0D_label = ifelse(W9ACQU0D == 1.0, "Post-graduate Diplomas and Certificates", ifelse(W9ACQU0D %in% c(-3, -1), W9ACQU0D, NA_character_)),
    W9ACQU0E_label = ifelse(W9ACQU0E == 1.0, "Diplomas in higher education and other higher education qualifications", ifelse(W9ACQU0E %in% c(-3, -1), W9ACQU0E, NA_character_)),
    W9ACQU0F_label = ifelse(W9ACQU0F == 1.0, "Teaching qualifications for schools or further education (below degree level)", ifelse(W9ACQU0F %in% c(-3, -1), W9ACQU0F, NA_character_)),
    W9ACQU0G_label = ifelse(W9ACQU0G == 1.0, "A/AS Levels or equivalent", ifelse(W9ACQU0G %in% c(-3, -1), W9ACQU0G, NA_character_)),
    W9ACQU0H_label = ifelse(W9ACQU0H == 1.0, "Grade A-C, Level 4-9", ifelse(W9ACQU0H %in% c(-3, -1), W9ACQU0H, NA_character_)),
    W9ACQU0I_label = ifelse(W9ACQU0I == 1.0, "Grade D-G, Level 1-3", ifelse(W9ACQU0I %in% c(-3, -1), W9ACQU0I, NA_character_)),
    W9ACQU0J_label = ifelse(W9ACQU0J == 1.0, "SCE Higher", ifelse(W9ACQU0J %in% c(-3, -1), W9ACQU0J, NA_character_)),
    W9ACQU0K_label = ifelse(W9ACQU0K == 1.0, "Scottish Certificate Sixth Year Studies", ifelse(W9ACQU0K %in% c(-3, -1), W9ACQU0K, NA_character_)),
    W9ACQU0L_label = ifelse(W9ACQU0L == 1.0, "SCE Standard", ifelse(W9ACQU0L %in% c(-3, -1), W9ACQU0L, NA_character_)),
    W9ACQU0M_label = ifelse(W9ACQU0M == 1.0, "National 4 and 5", ifelse(W9ACQU0M %in% c(-3, -1), W9ACQU0M, NA_character_)),
    W9ACQU0N_label = ifelse(W9ACQU0N == 1.0, "National 2 and 3", ifelse(W9ACQU0N %in% c(-3, -1), W9ACQU0N, NA_character_)),
    W9ACQU0O_label = ifelse(W9ACQU0O == 1.0, "Leaving Certificate", ifelse(W9ACQU0O %in% c(-3, -1), W9ACQU0O, NA_character_)),
    W9ACQU0P_label = ifelse(W9ACQU0P == 1.0, "Junior Certificate grade A-C", ifelse(W9ACQU0P %in% c(-3, -1), W9ACQU0P, NA_character_)),
    W9ACQU0Q_label = ifelse(W9ACQU0Q == 1.0, "Junior Certificate grade D and below", ifelse(W9ACQU0Q %in% c(-3, -1), W9ACQU0Q, NA_character_)),
    W9ACQU0R_label = ifelse(W9ACQU0R == 1.0, "Other academic qualifications (including overseas)", ifelse(W9ACQU0R %in% c(-3, -1), W9ACQU0R, NA_character_)),
    W9ACQU0S_label = ifelse(W9ACQU0S == 1.0, "None of these qualifications", ifelse(W9ACQU0S %in% c(-3, -1), W9ACQU0S, NA_character_)),
    W9ACQU0T_label = ifelse(W9ACQU0T == 1.0, "Don't know", ifelse(W9ACQU0T %in% c(-3, -1), W9ACQU0T, NA_character_)),
    W9ACQU0U_label = ifelse(W9ACQU0U == 1.0, "Refused", ifelse(W9ACQU0U %in% c(-3, -1), W9ACQU0U, NA_character_)),
    W9ACQU0V_label = ifelse(W9ACQU0V == 1.0, "No answer", ifelse(W9ACQU0V %in% c(-3, -1), W9ACQU0V, NA_character_)),
    educadtl32 = coalesce(
      W9ACQU0A_label,
      W9ACQU0B_label,
      W9ACQU0C_label,
      W9ACQU0D_label,
      W9ACQU0E_label,
      W9ACQU0F_label,
      W9ACQU0G_label,
      W9ACQU0H_label,
      W9ACQU0I_label,
      W9ACQU0J_label,
      W9ACQU0K_label,
      W9ACQU0L_label,
      W9ACQU0M_label,
      W9ACQU0N_label,
      W9ACQU0O_label,
      W9ACQU0P_label,
      W9ACQU0Q_label,
      W9ACQU0R_label,
      W9ACQU0S_label,
      W9ACQU0T_label,
      W9ACQU0U_label,
      W9ACQU0V_label,
      "None of these qualifications"
    )
  )

# Process educvdtl32 (vocational detail)
educvdtl32_data <- merged_data %>%
  mutate(
    W9VCQU0A_label = ifelse(W9VCQU0A == 1.0, "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor", ifelse(W9VCQU0A %in% c(-3, -1), W9VCQU0A, NA_character_)),
    W9VCQU0B_label = ifelse(W9VCQU0B == 1.0, "Nursing or other medical qualifications (below degree level)", ifelse(W9VCQU0B %in% c(-3, -1), W9VCQU0B, NA_character_)),
    W9VCQU0C_label = ifelse(W9VCQU0C == 1.0, "Level 4 or 5", ifelse(W9VCQU0C %in% c(-3, -1), W9VCQU0C, NA_character_)),
    W9VCQU0D_label = ifelse(W9VCQU0D == 1.0, "Level 3", ifelse(W9VCQU0D %in% c(-3, -1), W9VCQU0D, NA_character_)),
    W9VCQU0E_label = ifelse(W9VCQU0E == 1.0, "Level 2", ifelse(W9VCQU0E %in% c(-3, -1), W9VCQU0E, NA_character_)),
    W9VCQU0F_label = ifelse(W9VCQU0F == 1.0, "Level 1", ifelse(W9VCQU0F %in% c(-3, -1), W9VCQU0F, NA_character_)),
    W9VCQU0G_label = ifelse(W9VCQU0G == 1.0, "GNVQ Advanced", ifelse(W9VCQU0G %in% c(-3, -1), W9VCQU0G, NA_character_)),
    W9VCQU0H_label = ifelse(W9VCQU0H == 1.0, "GNVQ Intermediate", ifelse(W9VCQU0H %in% c(-3, -1), W9VCQU0H, NA_character_)),
    W9VCQU0I_label = ifelse(W9VCQU0I == 1.0, "Level 3", ifelse(W9VCQU0I %in% c(-3, -1), W9VCQU0I, NA_character_)),
    W9VCQU0J_label = ifelse(W9VCQU0J == 1.0, "Level 2", ifelse(W9VCQU0J %in% c(-3, -1), W9VCQU0J, NA_character_)),
    W9VCQU0K_label = ifelse(W9VCQU0K == 1.0, "Level Foundation", ifelse(W9VCQU0K %in% c(-3, -1), W9VCQU0K, NA_character_)),
    W9VCQU0L_label = ifelse(W9VCQU0L == 1.0, "Advanced Craft, Part III", ifelse(W9VCQU0L %in% c(-3, -1), W9VCQU0L, NA_character_)),
    W9VCQU0M_label = ifelse(W9VCQU0M == 1.0, "Craft, Part II", ifelse(W9VCQU0M %in% c(-3, -1), W9VCQU0M, NA_character_)),
    W9VCQU0N_label = ifelse(W9VCQU0N == 1.0, "Craft, Part I", ifelse(W9VCQU0N %in% c(-3, -1), W9VCQU0N, NA_character_)),
    W9VCQU0O_label = ifelse(W9VCQU0O == 1.0, "Level 3", ifelse(W9VCQU0O %in% c(-3, -1), W9VCQU0O, NA_character_)),
    W9VCQU0P_label = ifelse(W9VCQU0P == 1.0, "Level 2", ifelse(W9VCQU0P %in% c(-3, -1), W9VCQU0P, NA_character_)),
    W9VCQU0Q_label = ifelse(W9VCQU0Q == 1.0, "Level 1", ifelse(W9VCQU0Q %in% c(-3, -1), W9VCQU0Q, NA_character_)),
    W9VCQU0R_label = ifelse(W9VCQU0R == 1.0, "Advanced Diploma", ifelse(W9VCQU0R %in% c(-3, -1), W9VCQU0R, NA_character_)),
    W9VCQU0S_label = ifelse(W9VCQU0S == 1.0, "Higher Diploma", ifelse(W9VCQU0S %in% c(-3, -1), W9VCQU0S, NA_character_)),
    W9VCQU0T_label = ifelse(W9VCQU0T == 1.0, "RSA Diploma", ifelse(W9VCQU0T %in% c(-3, -1), W9VCQU0T, NA_character_)),
    W9VCQU0U_label = ifelse(W9VCQU0U == 1.0, "RSA Stage I, II,III", ifelse(W9VCQU0U %in% c(-3, -1), W9VCQU0U, NA_character_)),
    W9VCQU0V_label = ifelse(W9VCQU0V == 1.0, "Higher Level BTEC", ifelse(W9VCQU0V %in% c(-3, -1), W9VCQU0V, NA_character_)),
    W9VCQU0W_label = ifelse(W9VCQU0W == 1.0, "BTEC National", ifelse(W9VCQU0W %in% c(-3, -1), W9VCQU0W, NA_character_)),
    W9VCQU0X_label = ifelse(W9VCQU0X == 1.0, "BTEC First", ifelse(W9VCQU0X %in% c(-3, -1), W9VCQU0X, NA_character_)),
    W9VCQU0Y_label = ifelse(W9VCQU0Y == 1.0, "SCOTVEC National Certificate", ifelse(W9VCQU0Y %in% c(-3, -1), W9VCQU0Y, NA_character_)),
    W9VCQU0Z_label = ifelse(W9VCQU0Z == 1.0, "SCOTVEC first or general diploma", ifelse(W9VCQU0Z %in% c(-3, -1), W9VCQU0Z, NA_character_)),
    W9VCQUAA_label = ifelse(W9VCQUAA == 1.0, "SCOTVEC general diploma", ifelse(W9VCQUAA %in% c(-3, -1), W9VCQUAA, NA_character_)),
    W9VCQUAB_label = ifelse(W9VCQUAB == 1.0, "SCOTVEC modules", ifelse(W9VCQUAB %in% c(-3, -1), W9VCQUAB, NA_character_)),
    W9VCQUAC_label = ifelse(W9VCQUAC == 1.0, "HND or HNC", ifelse(W9VCQUAC %in% c(-3, -1), W9VCQUAC, NA_character_)),
    W9VCQUAD_label = ifelse(W9VCQUAD == 1.0, "OND or ONCM", ifelse(W9VCQUAD %in% c(-3, -1), W9VCQUAD, NA_character_)),
    W9VCQUAE_label = ifelse(W9VCQUAE == 1.0, "Junior certificate", ifelse(W9VCQUAE %in% c(-3, -1), W9VCQUAE, NA_character_)),
    W9VCQUAF_label = ifelse(W9VCQUAF == 1.0, "Other vocational qualifications (including some overseas)", ifelse(W9VCQUAF %in% c(-3, -1), W9VCQUAF, NA_character_)),
    W9VCQUAG_label = ifelse(W9VCQUAG == 1.0, "None of these qualifications", ifelse(W9VCQUAG %in% c(-3, -1), W9VCQUAG, NA_character_)),
    W9VCQUAH_label = ifelse(W9VCQUAH == 1.0, "Don't know", ifelse(W9VCQUAH %in% c(-3, -1), W9VCQUAH, NA_character_)),
    W9VCQUAI_label = ifelse(W9VCQUAI == 1.0, "Refused", ifelse(W9VCQUAI %in% c(-3, -1), W9VCQUAI, NA_character_)),
    educvdtl32 = coalesce(
      W9VCQU0A_label,
      W9VCQU0B_label,
      W9VCQU0C_label,
      W9VCQU0D_label,
      W9VCQU0E_label,
      W9VCQU0F_label,
      W9VCQU0G_label,
      W9VCQU0H_label,
      W9VCQU0I_label,
      W9VCQU0J_label,
      W9VCQU0K_label,
      W9VCQU0L_label,
      W9VCQU0M_label,
      W9VCQU0N_label,
      W9VCQU0O_label,
      W9VCQU0P_label,
      W9VCQU0Q_label,
      W9VCQU0R_label,
      W9VCQU0S_label,
      W9VCQU0T_label,
      W9VCQU0U_label,
      W9VCQU0V_label,
      W9VCQU0W_label,
      W9VCQU0X_label,
      W9VCQU0Y_label,
      W9VCQU0Z_label,
      W9VCQUAA_label,
      W9VCQUAB_label,
      W9VCQUAC_label,
      W9VCQUAD_label,
      W9VCQUAE_label,
      W9VCQUAF_label,
      W9VCQUAG_label,
      W9VCQUAH_label,
      W9VCQUAI_label,
      "None of these qualifications"
    )
  )

# Combine all processed variables
final_data <- bind_cols(
  merged_data %>% select(NSID),
  educ25_data %>% select(educ25),
  educ32_data %>% select(educ32),
  educadtl32_data %>% select(educadtl32),
  educvdtl32_data %>% select(educvdtl32)
)

# Write output to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)