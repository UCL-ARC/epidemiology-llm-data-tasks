library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
s8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select needed variables from each wave
s4_sel <- s4 %>% select(NSID, w4saim)
s6_sel <- s6 %>% select(NSID, W6Saim)
s7_sel <- s7 %>% select(NSID, W7SAim)
s8_sel <- s8 %>% select(NSID, W8ACTIVITY05, 
                         W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E,
                         W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J,
                         W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O,
                         W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E,
                         W8VCQUC0J, W8VCQUC0K)
s9_sel <- s9 %>% select(NSID, W9ECONACT2,
                         W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E,
                         W9ACQUC0F, W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J,
                         W9ACQUC0K, W9ACQUC0L, W9ACQUC0M, W9ACQUC0N, W9ACQUC0O,
                         W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S,
                         W9VCQUC0A, W9VCQUC0B, W9VCQUC0C, W9VCQUC0D, W9VCQUC0E,
                         W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I, W9VCQUC0J,
                         W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O,
                         W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T,
                         W9VCQUC0U, W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y,
                         W9VCQUC0Z, W9VCQUCAA, W9VCQUCAB, W9VCQUCAC, W9VCQUCAD,
                         W9VCQUCAE, W9VCQUCAF, W9VCQUCAG)

# Merge all datasets using full_join by NSID
merged <- s1 %>%
  full_join(s4_sel, by = "NSID") %>%
  full_join(s6_sel, by = "NSID") %>%
  full_join(s7_sel, by = "NSID") %>%
  full_join(s8_sel, by = "NSID") %>%
  full_join(s9_sel, by = "NSID")

# Function to recode Wave 4 (Age 17) - w4saim
recode_age17 <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,  # NVQ 1-3
    x %in% c(10, 11) ~ 2,  # None/entry level
    x %in% c(12, 13) ~ 3,  # Other
    x == 14 ~ 5,  # Not studying
    x == -9 ~ -9,  # Refusal
    x == -8 ~ -8,  # Don't know
    x == -3 ~ -3,  # Not asked
    x == -2 ~ -2,  # Schedule not applicable
    x == -1 ~ -1,  # Item not applicable
    is.na(x) ~ -3,  # Null values
    TRUE ~ as.numeric(x)
  )
}

# Function to recode Wave 6 (Age 19) - W6Saim
recode_age19 <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5
    x %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 1,  # NVQ 1-3
    x == 13 ~ 2,  # None/entry level
    x %in% c(14, 15) ~ 3,  # Other
    x == 16 ~ 5,  # Not studying
    x == -9 ~ -9,  # Refusal
    x == -8 ~ -8,  # Don't know
    x == -3 ~ -3,  # Not asked
    x == -2 ~ -2,  # Schedule not applicable
    x == -1 ~ -1,  # Item not applicable
    is.na(x) ~ -3,  # Null values
    TRUE ~ as.numeric(x)
  )
}

# Function to recode Wave 7 (Age 20) - W7SAim
recode_age20 <- function(x) {
  case_when(
    x %in% c(10, 11, 12, 13) ~ 0,  # NVQ 4-5
    x %in% c(1, 3, 4, 5, 6, 7, 8, 9) ~ 1,  # NVQ 1-3
    x == 2 ~ 2,  # None/entry level
    x == 14 ~ 3,  # Other
    x == -94 ~ -8,  # Insufficient information -> Don't know
    x == -91 ~ -1,  # Not applicable -> Item not applicable
    x == -9 ~ -9,  # Refusal
    x == -8 ~ -8,  # Don't know
    x == -3 ~ -3,  # Not asked
    x == -2 ~ -2,  # Schedule not applicable
    x == -1 ~ -1,  # Item not applicable
    is.na(x) ~ -3,  # Null values
    TRUE ~ as.numeric(x)
  )
}

# Function to recode Wave 8 (Age 25) - complex two-stage logic using case_when
recode_age25_vec <- function(activity, 
                             acq_a, acq_b, acq_c, acq_d, acq_e,
                             acq_f, acq_g, acq_h, acq_i, acq_j, acq_k,
                             acq_l, acq_m, acq_n, acq_o,
                             vcq_a, vcq_b, vcq_c, vcq_d, vcq_e,
                             vcq_j, vcq_k) {
  # Start with activity-based classification
  result <- case_when(
    # Handle activity missing codes first
    activity == -9 ~ -9,  # Refused
    activity == -8 ~ -8,  # Don't know
    activity == -1 ~ -1,  # Not applicable
    activity == 0 ~ 5,    # Not studying
    is.na(activity) ~ -3, # Null
    TRUE ~ NA_real_
  )
  
  # For those who are studying (activity == 1), classify by qualification
  studying_idx <- which(activity == 1)
  
  for (i in studying_idx) {
    # Check for NVQ 4-5 (category 0) - HE level
    if (!is.na(acq_a[i]) && acq_a[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_b[i]) && acq_b[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_c[i]) && acq_c[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_d[i]) && acq_d[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_e[i]) && acq_e[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_j[i]) && vcq_j[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_k[i]) && vcq_k[i] == 1) { result[i] <- 0; next }
    
    # Check for NVQ 1-3 (category 1)
    if (!is.na(acq_f[i]) && acq_f[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_g[i]) && acq_g[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_h[i]) && acq_h[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_i[i]) && acq_i[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_j[i]) && acq_j[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_k[i]) && acq_k[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_l[i]) && acq_l[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_m[i]) && acq_m[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_n[i]) && acq_n[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_a[i]) && vcq_a[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_b[i]) && vcq_b[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_e[i]) && vcq_e[i] == 1) { result[i] <- 1; next }
    
    # Check for None/entry level (category 2)
    if (!is.na(vcq_c[i]) && vcq_c[i] == 1) { result[i] <- 2; next }
    if (!is.na(vcq_d[i]) && vcq_d[i] == 1) { result[i] <- 2; next }
    
    # Check for None of the above (category 4)
    if (!is.na(acq_o[i]) && acq_o[i] == 1) { result[i] <- 4; next }
    
    # Default - if studying but no qualification indicated
    result[i] <- 5
  }
  
  return(result)
}

# Function to recode Wave 9 (Age 32) - complex two-stage logic
recode_age32_vec <- function(activity,
                             acq_a, acq_b, acq_c, acq_d, acq_e, acq_f,
                             acq_g, acq_h, acq_i, acq_j, acq_k, acq_l, acq_m, acq_n,
                             acq_o, acq_p, acq_q, acq_r, acq_s,
                             vcq_a, vcq_b, vcq_c,
                             vcq_d, vcq_e, vcq_f, vcq_g, vcq_h, vcq_i, vcq_j,
                             vcq_k, vcq_l, vcq_m, vcq_n, vcq_o, vcq_p, vcq_q,
                             vcq_r, vcq_s, vcq_t, vcq_u, vcq_v, vcq_w, vcq_x,
                             vcq_y, vcq_z, vcq_aa, vcq_ab, vcq_ac, vcq_ad,
                             vcq_ae, vcq_af, vcq_ag) {
  # Start with activity-based classification
  result <- case_when(
    activity == -9 ~ -9,  # Refused
    activity == -8 ~ -8,  # Don't know
    activity == -3 ~ -3,  # Not asked
    activity == -1 ~ -1,  # Not applicable
    !activity %in% c(6, 7, 8, 12) ~ 5,  # Not studying
    is.na(activity) ~ -3,
    TRUE ~ NA_real_
  )
  
  # For those who are studying (activity in 6, 7, 8, 12)
  studying_idx <- which(activity %in% c(6, 7, 8, 12))
  
  for (i in studying_idx) {
    # Check for NVQ 4-5 (category 0) - HE level academic
    if (!is.na(acq_a[i]) && acq_a[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_b[i]) && acq_b[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_c[i]) && acq_c[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_d[i]) && acq_d[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_e[i]) && acq_e[i] == 1) { result[i] <- 0; next }
    if (!is.na(acq_f[i]) && acq_f[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_a[i]) && vcq_a[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_b[i]) && vcq_b[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_c[i]) && vcq_c[i] == 1) { result[i] <- 0; next }
    if (!is.na(vcq_ac[i]) && vcq_ac[i] == 1) { result[i] <- 0; next }
    
    # Check for NVQ 1-3 (category 1)
    if (!is.na(acq_g[i]) && acq_g[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_h[i]) && acq_h[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_i[i]) && acq_i[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_j[i]) && acq_j[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_k[i]) && acq_k[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_l[i]) && acq_l[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_m[i]) && acq_m[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_n[i]) && acq_n[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_o[i]) && acq_o[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_p[i]) && acq_p[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_q[i]) && acq_q[i] == 1) { result[i] <- 1; next }
    if (!is.na(acq_r[i]) && acq_r[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_d[i]) && vcq_d[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_e[i]) && vcq_e[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_f[i]) && vcq_f[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_g[i]) && vcq_g[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_h[i]) && vcq_h[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_i[i]) && vcq_i[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_j[i]) && vcq_j[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_k[i]) && vcq_k[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_l[i]) && vcq_l[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_m[i]) && vcq_m[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_n[i]) && vcq_n[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_o[i]) && vcq_o[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_p[i]) && vcq_p[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_q[i]) && vcq_q[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_r[i]) && vcq_r[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_s[i]) && vcq_s[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_t[i]) && vcq_t[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_u[i]) && vcq_u[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_v[i]) && vcq_v[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_w[i]) && vcq_w[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_x[i]) && vcq_x[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_y[i]) && vcq_y[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_z[i]) && vcq_z[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_aa[i]) && vcq_aa[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_ab[i]) && vcq_ab[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_ad[i]) && vcq_ad[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_ae[i]) && vcq_ae[i] == 1) { result[i] <- 1; next }
    if (!is.na(vcq_af[i]) && vcq_af[i] == 1) { result[i] <- 1; next }
    
    # Check for None of these (category 4)
    if (!is.na(acq_s[i]) && acq_s[i] == 1) { result[i] <- 4; next }
    if (!is.na(vcq_ag[i]) && vcq_ag[i] == 1) { result[i] <- 4; next }
    
    # Default
    result[i] <- 5
  }
  
  return(result)
}

# Apply recoding functions
merged <- merged %>%
  mutate(
    educaim17 = recode_age17(w4saim),
    educaim19 = recode_age19(W6Saim),
    educaim20 = recode_age20(W7SAim),
    educaim25 = recode_age25_vec(
      W8ACTIVITY05,
      W8ACQUC0A, W8ACQUC0B, W8ACQUC0C, W8ACQUC0D, W8ACQUC0E,
      W8ACQUC0F, W8ACQUC0G, W8ACQUC0H, W8ACQUC0I, W8ACQUC0J,
      W8ACQUC0K, W8ACQUC0L, W8ACQUC0M, W8ACQUC0N, W8ACQUC0O,
      W8VCQUC0A, W8VCQUC0B, W8VCQUC0C, W8VCQUC0D, W8VCQUC0E,
      W8VCQUC0J, W8VCQUC0K
    ),
    educaim32 = recode_age32_vec(
      W9ECONACT2,
      W9ACQUC0A, W9ACQUC0B, W9ACQUC0C, W9ACQUC0D, W9ACQUC0E, W9ACQUC0F,
      W9ACQUC0G, W9ACQUC0H, W9ACQUC0I, W9ACQUC0J, W9ACQUC0K, W9ACQUC0L,
      W9ACQUC0M, W9ACQUC0N, W9ACQUC0O, W9ACQUC0P, W9ACQUC0Q, W9ACQUC0R, W9ACQUC0S,
      W9VCQUC0A, W9VCQUC0B, W9VCQUC0C,
      W9VCQUC0D, W9VCQUC0E, W9VCQUC0F, W9VCQUC0G, W9VCQUC0H, W9VCQUC0I,
      W9VCQUC0J, W9VCQUC0K, W9VCQUC0L, W9VCQUC0M, W9VCQUC0N, W9VCQUC0O,
      W9VCQUC0P, W9VCQUC0Q, W9VCQUC0R, W9VCQUC0S, W9VCQUC0T, W9VCQUC0U,
      W9VCQUC0V, W9VCQUC0W, W9VCQUC0X, W9VCQUC0Y, W9VCQUC0Z, W9VCQUCAA,
      W9VCQUCAB, W9VCQUCAC, W9VCQUCAD, W9VCQUCAE, W9VCQUCAF, W9VCQUCAG
    )
  )

# Select final variables
final_data <- merged %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")