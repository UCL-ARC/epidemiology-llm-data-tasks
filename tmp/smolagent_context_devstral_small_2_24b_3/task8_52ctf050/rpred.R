library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# Function to map W8VCQU* variables to NVQ tiers
map_nvq_tier <- function(var_label) {
  if (grepl("NVQ/SVQ - Level 1 - 2", var_label)) return(1)
  if (grepl("NVQ/SVQ - Level 3 - 5", var_label)) return(3)
  if (grepl("HNC/HND", var_label)) return(4)
  if (grepl("ONC/OND", var_label)) return(3)
  if (grepl("BTEC/BEC/TEC/EdExcel/LQL", var_label)) return(3)
  if (grepl("SCOTVEC", var_label)) return(3)
  if (grepl("Other vocational, technical or professional", var_label)) return(3)
  if (grepl("Youth training certificate|Key Skills|Basic skills|Entry level qualifications", var_label)) return(0)
  if (grepl("Modern apprenticeship/trade apprenticeship", var_label)) return(2)
  if (grepl("RSA/OCR/Clerical and commercial qualifications", var_label)) return(2)
  if (grepl("City and Guilds Certificate", var_label)) return(2)
  if (grepl("GNVQ/GSVQ", var_label)) return(2)
  return(NA)
}

# Derive educ25
educ25 <- merged_data %>%
  mutate(
    # Map vocational qualifications to NVQ tiers
    vcqu_tier_0A = ifelse(W8VCQU0A == 1, map_nvq_tier("Vocational qualifications gained: NVQ/SVQ - Level 1 - 2"), NA),
    vcqu_tier_0B = ifelse(W8VCQU0B == 1, map_nvq_tier("Vocational qualifications gained: Key Skills"), NA),
    vcqu_tier_0C = ifelse(W8VCQU0C == 1, map_nvq_tier("Vocational qualifications gained: Basic skills"), NA),
    vcqu_tier_0D = ifelse(W8VCQU0D == 1, map_nvq_tier("Vocational qualifications gained: Entry level qualifications (Wales)"), NA),
    vcqu_tier_0E = ifelse(W8VCQU0E == 1, map_nvq_tier("Vocational qualifications gained: Modern apprenticeship/trade apprenticeship"), NA),
    vcqu_tier_0F = ifelse(W8VCQU0F == 1, map_nvq_tier("Vocational qualifications gained: RSA/OCR/Clerical and commercial qualifications"), NA),
    vcqu_tier_0G = ifelse(W8VCQU0G == 1, map_nvq_tier("Vocational qualifications gained: City and Guilds Certificate"), NA),
    vcqu_tier_0H = ifelse(W8VCQU0H == 1, map_nvq_tier("Vocational qualifications gained: GNVQ/GSVQ"), NA),
    vcqu_tier_0I = ifelse(W8VCQU0I == 1, map_nvq_tier("Vocational qualifications gained: NVQ/SVQ - Level 1 - 2"), NA),
    vcqu_tier_0J = ifelse(W8VCQU0J == 1, map_nvq_tier("Vocational qualifications gained: NVQ/SVQ - Level 3 - 5"), NA),
    vcqu_tier_0K = ifelse(W8VCQU0K == 1, map_nvq_tier("Vocational qualifications gained: HNC/HND"), NA),
    vcqu_tier_0L = ifelse(W8VCQU0L == 1, map_nvq_tier("Vocational qualifications gained: ONC/OND"), NA),
    vcqu_tier_0M = ifelse(W8VCQU0M == 1, map_nvq_tier("Vocational qualifications gained: BTEC/BEC/TEC/EdExcel/LQL"), NA),
    vcqu_tier_0N = ifelse(W8VCQU0N == 1, map_nvq_tier("Vocational qualifications gained: SCOTVEC, SCOTEC or SCOTBEC"), NA),
    vcqu_tier_0O = ifelse(W8VCQU0O == 1, map_nvq_tier("Vocational qualifications gained: Other vocational, technical or professional"), NA),
    vcqu_tier_0P = ifelse(W8VCQU0P == 1, map_nvq_tier("Vocational qualifications gained: None of the above"), NA),
    vcqu_tier_0Q = ifelse(W8VCQU0Q == 1, map_nvq_tier("Vocational qualifications gained: Don't know"), NA),
    vcqu_tier_0R = ifelse(W8VCQU0R == 1, map_nvq_tier("Vocational qualifications gained: Refused"), NA),
    # Handle missing codes
    vcqu_missing = case_when(
      W8VCQU0Q == 1 ~ -8,  # Don't know
      W8VCQU0R == 1 ~ -9,  # Refused
      TRUE ~ NA_real_
    ),
    # Get highest vocational tier
    highest_vcqu_tier = pmax(
      vcqu_tier_0A, vcqu_tier_0B, vcqu_tier_0C, vcqu_tier_0D, vcqu_tier_0E,
      vcqu_tier_0F, vcqu_tier_0G, vcqu_tier_0H, vcqu_tier_0I, vcqu_tier_0J,
      vcqu_tier_0K, vcqu_tier_0L, vcqu_tier_0M, vcqu_tier_0N, vcqu_tier_0O,
      vcqu_tier_0P, vcqu_tier_0Q, vcqu_tier_0R,
      na.rm = TRUE
    ),
    # Map academic NVQ to collapsed scheme
    academic_nvq = case_when(
      W8DHANVQH %in% c(4, 5) ~ 0,
      W8DHANVQH %in% c(1, 2, 3) ~ 1,
      W8DHANVQH == 0 ~ 2,
      W8DHANVQH == 95 ~ 3,
      W8DHANVQH == 96 ~ 4,
      W8DHANVQH == -9 ~ -9,
      W8DHANVQH == -8 ~ -8,
      W8DHANVQH == -1 ~ -1,
      TRUE ~ NA_real_
    ),
    # Map vocational tier to collapsed scheme
    vocational_nvq = case_when(
      highest_vcqu_tier %in% c(4, 5) ~ 0,
      highest_vcqu_tier %in% c(1, 2, 3) ~ 1,
      highest_vcqu_tier == 0 ~ 2,
      is.na(highest_vcqu_tier) & !is.na(vcqu_missing) ~ vcqu_missing,
      is.na(highest_vcqu_tier) & is.na(vcqu_missing) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    # Combine academic and vocational
    educ25 = case_when(
      !is.na(academic_nvq) & !is.na(vocational_nvq) ~ pmin(academic_nvq, vocational_nvq),
      !is.na(academic_nvq) ~ academic_nvq,
      !is.na(vocational_nvq) ~ vocational_nvq,
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educ25)

# Derive educ32
educ32 <- merged_data %>%
  mutate(
    # Map academic NVQ to collapsed scheme
    academic_nvq = case_when(
      W9DANVQH %in% c(4, 5) ~ 0,
      W9DANVQH %in% c(1, 2, 3) ~ 1,
      W9DANVQH == 0 ~ 2,
      W9DANVQH == 95 ~ 3,
      W9DANVQH == 96 ~ 4,
      W9DANVQH == -9 ~ -9,
      W9DANVQH == -8 ~ -8,
      W9DANVQH == -1 ~ -1,
      TRUE ~ NA_real_
    ),
    # Map vocational NVQ to collapsed scheme
    vocational_nvq = case_when(
      W9DVNVQH %in% c(4, 5) ~ 0,
      W9DVNVQH %in% c(1, 2, 3) ~ 1,
      W9DVNVQH == 0 ~ 2,
      W9DVNVQH == 95 ~ 3,
      W9DVNVQH == 96 ~ 4,
      W9DVNVQH == -9 ~ -9,
      W9DVNVQH == -8 ~ -8,
      W9DVNVQH == -1 ~ -1,
      TRUE ~ NA_real_
    ),
    # Combine academic and vocational
    educ32 = case_when(
      !is.na(academic_nvq) & !is.na(vocational_nvq) ~ pmin(academic_nvq, vocational_nvq),
      !is.na(academic_nvq) ~ academic_nvq,
      !is.na(vocational_nvq) ~ vocational_nvq,
      TRUE ~ NA_real_
    )
  ) %>%
  select(NSID, educ32)

# Derive educadtl32
educadtl32 <- merged_data %>%
  mutate(
    # Scan W9ACQU* variables in order
    acqu_0A = ifelse(W9ACQU0A == 1, 1, ifelse(W9ACQU0A == 2, 0, NA_real_)),
    acqu_0B = ifelse(W9ACQU0B == 1, 2, ifelse(W9ACQU0B == 2, 0, NA_real_)),
    acqu_0C = ifelse(W9ACQU0C == 1, 3, ifelse(W9ACQU0C == 2, 0, NA_real_)),
    acqu_0D = ifelse(W9ACQU0D == 1, 4, ifelse(W9ACQU0D == 2, 0, NA_real_)),
    acqu_0E = ifelse(W9ACQU0E == 1, 5, ifelse(W9ACQU0E == 2, 0, NA_real_)),
    acqu_0F = ifelse(W9ACQU0F == 1, 6, ifelse(W9ACQU0F == 2, 0, NA_real_)),
    acqu_0G = ifelse(W9ACQU0G == 1, 7, ifelse(W9ACQU0G == 2, 0, NA_real_)),
    acqu_0H = ifelse(W9ACQU0H == 1, 8, ifelse(W9ACQU0H == 2, 0, NA_real_)),
    acqu_0I = ifelse(W9ACQU0I == 1, 9, ifelse(W9ACQU0I == 2, 0, NA_real_)),
    acqu_0J = ifelse(W9ACQU0J == 1, 10, ifelse(W9ACQU0J == 2, 0, NA_real_)),
    acqu_0K = ifelse(W9ACQU0K == 1, 11, ifelse(W9ACQU0K == 2, 0, NA_real_)),
    acqu_0L = ifelse(W9ACQU0L == 1, 12, ifelse(W9ACQU0L == 2, 0, NA_real_)),
    acqu_0M = ifelse(W9ACQU0M == 1, 13, ifelse(W9ACQU0M == 2, 0, NA_real_)),
    acqu_0N = ifelse(W9ACQU0N == 1, 14, ifelse(W9ACQU0N == 2, 0, NA_real_)),
    acqu_0O = ifelse(W9ACQU0O == 1, 15, ifelse(W9ACQU0O == 2, 0, NA_real_)),
    acqu_0P = ifelse(W9ACQU0P == 1, 16, ifelse(W9ACQU0P == 2, 0, NA_real_)),
    acqu_0Q = ifelse(W9ACQU0Q == 1, 17, ifelse(W9ACQU0Q == 2, 0, NA_real_)),
    acqu_0R = ifelse(W9ACQU0R == 1, 18, ifelse(W9ACQU0R == 2, 0, NA_real_)),
    acqu_0S = ifelse(W9ACQU0S == 1, 19, ifelse(W9ACQU0S == 2, 0, NA_real_)),
    # Handle missing codes
    acqu_missing = case_when(
      W9ACQU0T == 1 ~ -8,  # Don't know
      W9ACQU0U == 1 ~ -9,  # Refused
      W9ACQU0V == 1 ~ -2,  # No answer
      TRUE ~ NA_real_
    ),
    # Get first substantive indicator
    educadtl32 = case_when(
      !is.na(acqu_0A) & acqu_0A > 0 ~ acqu_0A,
      !is.na(acqu_0B) & acqu_0B > 0 ~ acqu_0B,
      !is.na(acqu_0C) & acqu_0C > 0 ~ acqu_0C,
      !is.na(acqu_0D) & acqu_0D > 0 ~ acqu_0D,
      !is.na(acqu_0E) & acqu_0E > 0 ~ acqu_0E,
      !is.na(acqu_0F) & acqu_0F > 0 ~ acqu_0F,
      !is.na(acqu_0G) & acqu_0G > 0 ~ acqu_0G,
      !is.na(acqu_0H) & acqu_0H > 0 ~ acqu_0H,
      !is.na(acqu_0I) & acqu_0I > 0 ~ acqu_0I,
      !is.na(acqu_0J) & acqu_0J > 0 ~ acqu_0J,
      !is.na(acqu_0K) & acqu_0K > 0 ~ acqu_0K,
      !is.na(acqu_0L) & acqu_0L > 0 ~ acqu_0L,
      !is.na(acqu_0M) & acqu_0M > 0 ~ acqu_0M,
      !is.na(acqu_0N) & acqu_0N > 0 ~ acqu_0N,
      !is.na(acqu_0O) & acqu_0O > 0 ~ acqu_0O,
      !is.na(acqu_0P) & acqu_0P > 0 ~ acqu_0P,
      !is.na(acqu_0Q) & acqu_0Q > 0 ~ acqu_0Q,
      !is.na(acqu_0R) & acqu_0R > 0 ~ acqu_0R,
      !is.na(acqu_0S) & acqu_0S > 0 ~ acqu_0S,
      !is.na(acqu_missing) ~ acqu_missing,
      TRUE ~ 20  # None of these qualifications
    )
  ) %>%
  select(NSID, educadtl32)

# Derive educvdtl32
educvdtl32 <- merged_data %>%
  mutate(
    # Scan W9VCQU* variables in order
    vcqu_0A = ifelse(W9VCQU0A == 1, 1, ifelse(W9VCQU0A == 2, 0, NA_real_)),
    vcqu_0B = ifelse(W9VCQU0B == 1, 2, ifelse(W9VCQU0B == 2, 0, NA_real_)),
    vcqu_0C = ifelse(W9VCQU0C == 1, 3, ifelse(W9VCQU0C == 2, 0, NA_real_)),
    vcqu_0D = ifelse(W9VCQU0D == 1, 4, ifelse(W9VCQU0D == 2, 0, NA_real_)),
    vcqu_0E = ifelse(W9VCQU0E == 1, 5, ifelse(W9VCQU0E == 2, 0, NA_real_)),
    vcqu_0F = ifelse(W9VCQU0F == 1, 6, ifelse(W9VCQU0F == 2, 0, NA_real_)),
    vcqu_0G = ifelse(W9VCQU0G == 1, 7, ifelse(W9VCQU0G == 2, 0, NA_real_)),
    vcqu_0H = ifelse(W9VCQU0H == 1, 8, ifelse(W9VCQU0H == 2, 0, NA_real_)),
    vcqu_0I = ifelse(W9VCQU0I == 1, 9, ifelse(W9VCQU0I == 2, 0, NA_real_)),
    vcqu_0J = ifelse(W9VCQU0J == 1, 10, ifelse(W9VCQU0J == 2, 0, NA_real_)),
    vcqu_0K = ifelse(W9VCQU0K == 1, 11, ifelse(W9VCQU0K == 2, 0, NA_real_)),
    vcqu_0L = ifelse(W9VCQU0L == 1, 12, ifelse(W9VCQU0L == 2, 0, NA_real_)),
    vcqu_0M = ifelse(W9VCQU0M == 1, 13, ifelse(W9VCQU0M == 2, 0, NA_real_)),
    vcqu_0N = ifelse(W9VCQU0N == 1, 14, ifelse(W9VCQU0N == 2, 0, NA_real_)),
    vcqu_0O = ifelse(W9VCQU0O == 1, 15, ifelse(W9VCQU0O == 2, 0, NA_real_)),
    vcqu_0P = ifelse(W9VCQU0P == 1, 16, ifelse(W9VCQU0P == 2, 0, NA_real_)),
    vcqu_0Q = ifelse(W9VCQU0Q == 1, 17, ifelse(W9VCQU0Q == 2, 0, NA_real_)),
    vcqu_0R = ifelse(W9VCQU0R == 1, 18, ifelse(W9VCQU0R == 2, 0, NA_real_)),
    vcqu_0S = ifelse(W9VCQU0S == 1, 19, ifelse(W9VCQU0S == 2, 0, NA_real_)),
    vcqu_0T = ifelse(W9VCQU0T == 1, 20, ifelse(W9VCQU0T == 2, 0, NA_real_)),
    vcqu_0U = ifelse(W9VCQU0U == 1, 21, ifelse(W9VCQU0U == 2, 0, NA_real_)),
    vcqu_0V = ifelse(W9VCQU0V == 1, 22, ifelse(W9VCQU0V == 2, 0, NA_real_)),
    vcqu_0W = ifelse(W9VCQU0W == 1, 23, ifelse(W9VCQU0W == 2, 0, NA_real_)),
    vcqu_0X = ifelse(W9VCQU0X == 1, 24, ifelse(W9VCQU0X == 2, 0, NA_real_)),
    vcqu_0Y = ifelse(W9VCQU0Y == 1, 25, ifelse(W9VCQU0Y == 2, 0, NA_real_)),
    vcqu_0Z = ifelse(W9VCQU0Z == 1, 26, ifelse(W9VCQU0Z == 2, 0, NA_real_)),
    vcqu_AA = ifelse(W9VCQUAA == 1, 27, ifelse(W9VCQUAA == 2, 0, NA_real_)),
    vcqu_AB = ifelse(W9VCQUAB == 1, 28, ifelse(W9VCQUAB == 2, 0, NA_real_)),
    vcqu_AC = ifelse(W9VCQUAC == 1, 29, ifelse(W9VCQUAC == 2, 0, NA_real_)),
    vcqu_AD = ifelse(W9VCQUAD == 1, 30, ifelse(W9VCQUAD == 2, 0, NA_real_)),
    vcqu_AE = ifelse(W9VCQUAE == 1, 31, ifelse(W9VCQUAE == 2, 0, NA_real_)),
    vcqu_AF = ifelse(W9VCQUAF == 1, 32, ifelse(W9VCQUAF == 2, 0, NA_real_)),
    vcqu_AG = ifelse(W9VCQUAG == 1, 33, ifelse(W9VCQUAG == 2, 0, NA_real_)),
    # Handle missing codes
    vcqu_missing = case_when(
      W9VCQUAH == 1 ~ -8,  # Don't know
      W9VCQUAI == 1 ~ -9,  # Refused
      TRUE ~ NA_real_
    ),
    # Get first substantive indicator
    educvdtl32 = case_when(
      !is.na(vcqu_0A) & vcqu_0A > 0 ~ vcqu_0A,
      !is.na(vcqu_0B) & vcqu_0B > 0 ~ vcqu_0B,
      !is.na(vcqu_0C) & vcqu_0C > 0 ~ vcqu_0C,
      !is.na(vcqu_0D) & vcqu_0D > 0 ~ vcqu_0D,
      !is.na(vcqu_0E) & vcqu_0E > 0 ~ vcqu_0E,
      !is.na(vcqu_0F) & vcqu_0F > 0 ~ vcqu_0F,
      !is.na(vcqu_0G) & vcqu_0G > 0 ~ vcqu_0G,
      !is.na(vcqu_0H) & vcqu_0H > 0 ~ vcqu_0H,
      !is.na(vcqu_0I) & vcqu_0I > 0 ~ vcqu_0I,
      !is.na(vcqu_0J) & vcqu_0J > 0 ~ vcqu_0J,
      !is.na(vcqu_0K) & vcqu_0K > 0 ~ vcqu_0K,
      !is.na(vcqu_0L) & vcqu_0L > 0 ~ vcqu_0L,
      !is.na(vcqu_0M) & vcqu_0M > 0 ~ vcqu_0M,
      !is.na(vcqu_0N) & vcqu_0N > 0 ~ vcqu_0N,
      !is.na(vcqu_0O) & vcqu_0O > 0 ~ vcqu_0O,
      !is.na(vcqu_0P) & vcqu_0P > 0 ~ vcqu_0P,
      !is.na(vcqu_0Q) & vcqu_0Q > 0 ~ vcqu_0Q,
      !is.na(vcqu_0R) & vcqu_0R > 0 ~ vcqu_0R,
      !is.na(vcqu_0S) & vcqu_0S > 0 ~ vcqu_0S,
      !is.na(vcqu_0T) & vcqu_0T > 0 ~ vcqu_0T,
      !is.na(vcqu_0U) & vcqu_0U > 0 ~ vcqu_0U,
      !is.na(vcqu_0V) & vcqu_0V > 0 ~ vcqu_0V,
      !is.na(vcqu_0W) & vcqu_0W > 0 ~ vcqu_0W,
      !is.na(vcqu_0X) & vcqu_0X > 0 ~ vcqu_0X,
      !is.na(vcqu_0Y) & vcqu_0Y > 0 ~ vcqu_0Y,
      !is.na(vcqu_0Z) & vcqu_0Z > 0 ~ vcqu_0Z,
      !is.na(vcqu_AA) & vcqu_AA > 0 ~ vcqu_AA,
      !is.na(vcqu_AB) & vcqu_AB > 0 ~ vcqu_AB,
      !is.na(vcqu_AC) & vcqu_AC > 0 ~ vcqu_AC,
      !is.na(vcqu_AD) & vcqu_AD > 0 ~ vcqu_AD,
      !is.na(vcqu_AE) & vcqu_AE > 0 ~ vcqu_AE,
      !is.na(vcqu_AF) & vcqu_AF > 0 ~ vcqu_AF,
      !is.na(vcqu_AG) & vcqu_AG > 0 ~ vcqu_AG,
      !is.na(vcqu_missing) ~ vcqu_missing,
      TRUE ~ 34  # None of these qualifications
    )
  ) %>%
  select(NSID, educvdtl32)

# Combine all derived variables
final_data <- educ25 %>%
  full_join(educ32, by = "NSID") %>%
  full_join(educadtl32, by = "NSID") %>%
  full_join(educvdtl32, by = "NSID")

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
