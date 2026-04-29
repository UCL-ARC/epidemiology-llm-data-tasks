library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"

files_to_load <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

data_list <- map(files_to_load, ~read_delim(paste0(path, .x), delim = "\t", col_types = cols(.default = "numeric")))

# Need to ensure NSID is string as per metadata
# Let's reload them specifically to handle NSID as string
data_list <- map(files_to_load, function(f) {
  read_delim(paste0(path, f), delim = "\t", col_types = cols(NSID = col_character(), .default = "numeric"))
})

names(data_list) <- files_to_load

# Merge datasets
full_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_data <- full_join(full_data, data_list[[i]], by = "NSID")
}

# 2. Target Variable Derivation

# --- Age 17 (w4saim) ---
# Scheme: 0=NVQ4-5, 1=NVQ1-3, 2=None/Entry, 3=Other, 4=None of these, 5=Not studying
# Metadata: 1:NVQ3, 2:AVCE, 3:A/AS, 4:OtherL3, 5:NVQ2, 6:IntGNVQ, 7:OtherL2, 8:GCSE, 9:NVQ1, 10:Found, 11:OtherL1, 12:Other, 13:No detail, 14:Not studying
full_data <- full_data %>%
  mutate(educaim17 = case_when(
    w4saim %in% c(1, 2, 3, 4) ~ 1, # NVQ 1-3 eq (L3)
    w4saim %in% c(5, 6, 7, 8, 9, 10, 11) ~ 1, # Actually NVQ 1-3 range usually covers L1-L3
    # Let's re-evaluate based on the provided scheme
    # NVQ 1-3 equivalent (lower/mid): 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
    # Wait, usually NVQ 1-3 is L1-L3. AVCE/AS are L3.
    w4saim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,
    w4saim == 12 ~ 3, # Other
    w4saim == 13 ~ 3, # No detail -> other/unknown
    w4saim == 14 ~ 5, # Not studying
    w4saim >= -1 & w4saim < 0 ~ -3, # Standard missing
    TRUE ~ -3
  ))
# Correcting Age 17 mapping: 1-11 are all below NVQ 4-5. 
# Let's be more precise: NVQ 1-3 equivalent includes L1, L2, L3.
# 1-11 are L1-L3. 12-13 are other/unknown. 14 is not studying.

# --- Age 19 (W6Saim) ---
# 1:NVQ5, 2:FirstDeg, 3:NVQ4, 4:OtherHE -> 0
# 5:NVQ3, 6:AVCE, 7:A/AS, 8:OtherL3, 9:NVQ2, 10:OtherL2, 11:GCSE, 12:NVQ1, 13:OtherL1 -> 1
# 14:Other(unk), 15:No detail -> 3
# 16:Not studying -> 5
full_data <- full_data %>%
  mutate(educaim19 = case_when(
    W6Saim %in% c(1, 2, 3, 4) ~ 0,
    W6Saim %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13) ~ 1,
    W6Saim %in% c(14, 15) ~ 3,
    W6Saim == 16 ~ 5,
    W6Saim >= -1 & W6Saim < 0 ~ -3,
    TRUE ~ -3
  ))

# --- Age 20 (W7SAim) ---
# 10:NVQ4, 11:FirstDeg, 12:OtherHE, 13:NVQ5 -> 0
# 1:NVQ1, 2:OtherL1, 3:NVQ2, 4:GCSE, 5:OtherL2, 6:NVQ3, 7:A/AS, 8:AVCE, 9:OtherL3 -> 1
# 14:Other(unk) -> 3
# -91: Not applicable (not studying) -> 5
# -94: Insufficient info -> -8
full_data <- full_data %>%
  mutate(educaim20 = case_when(
    W7SAim %in% c(10, 11, 12, 13) ~ 0,
    W7SAim %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ 1,
    W7SAim == 14 ~ 3,
    W7SAim == -91 ~ 5,
    W7SAim == -94 ~ -8,
    W7SAim >= -1 & W7SAim < 0 ~ -3,
    TRUE ~ -3
  ))

# --- Age 25 (ns8) ---
# Priority: Econ Activity (W8ACTIVITY05 == 1 -> studying)
# If not studying (W8ACTIVITY05 == 0), then category 5.
# If studying: 
# NVQ 4-5: W8ACQUC0A, B, C, D, E, W8VCQUC0J, K
# NVQ 1-3: W8ACQUC0F, G, H, I, J, K, L, M, N, W8VCQUC0A, B, C, D, E
# Entry/None: W8ACQUC0O
# Other: W8ACQUC0P, Q

full_data <- full_data %>%
  mutate(educaim25 = case_when(
    # Not studying
    W8ACTIVITY05 == 0 ~ 5,
    # Studying - Check levels
    (W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 | W8ACQUC0D == 1 | W8ACQUC0E == 1 | W8VCQUC0J == 1 | W8VCQUC0K == 1) ~ 0,
    (W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 | W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 | W8ACQUC0N == 1 | W8VCQUC0A == 1 | W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1) ~ 1,
    W8ACQUC0O == 1 ~ 2,
    (W8ACQUC0P == 1 | W8ACQUC0Q == 1) ~ 3,
    # Missing handling
    # Refused/DK from activity or the tick boxes
    W8ACTIVITY05 == -9 ~ -9, W8ACTIVITY05 == -8 ~ -8,
    # If activity is Yes (1) but all tick boxes are NA or missing
    TRUE ~ -3
  ))

# --- Age 32 (ns9) ---
# Econ Activity: W9ECONACT2
# Not studying: Any value except 6, 7, 12
# Studying: 6(FT), 7(PT), 12(Apprentice)
# NVQ 4-5: W9ACQUC0A, B, C, D, E, W9VCQUC0A, C, S, V
# NVQ 1-3: W9ACQUC0F, G, H, I, J, K, L, M, N, O, P, Q, W9VCQUC0B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q
# None: W9ACQUC0S, W9VCQUCAG
# Other: W9ACQUC0R

full_data <- full_data %>%
  mutate(educaim32 = case_when(
    # Not studying: W9ECONACT2 is not 6, 7, 12
    !(W9ECONACT2 %in% c(6, 7, 12)) & !is.na(W9ECONACT2) & W9ECONACT2 >= 0 ~ 5,
    # Studying
    (W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 | W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9VCQUC0A == 1 | W9VCQUC0C == 1 | W9VCQUC0S == 1 | W9VCQUC0V == 1) ~ 0,
    (W9ACQUC0F == 1 | W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 | W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 | W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9VCQUC0B == 1 | W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 | W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 | W9VCQUC0J == 1 | W9VCQUC0K == 1 | W9VCQUC0L == 1 | W9VCQUC0M == 1 | W9VCQUC0N == 1 | W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1) ~ 1,
    (W9ACQUC0S == 1 | W9VCQUCAG == 1) ~ 2,
    W9ACQUC0R == 1 ~ 3,
    # Missing
    W9ECONACT2 == -9 ~ -9, W9ECONACT2 == -8 ~ -8, W9ECONACT2 == -3 ~ -3, W9ECONACT2 == -1 ~ -1,
    TRUE ~ -3
  ))

# Final factor labels
fact_labels <- c("0" = "NVQ 4–5 equivalent", "1" = "NVQ 1–3 equivalent", "2" = "None / entry level", "3" = "Other", "4" = "None of these qualifications", "5" = "Not currently studying", "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Not applicable/lost", "-1" = "Item not applicable")

# Apply factors and keep only required variables
final_vars <- c("NSID", "educaim17", "educaim19", "educaim20", "educaim25", "educaim32")
full_data_final <- full_data %>%
  select(all_of(final_vars)) %>%
  mutate(across(starts_with("educaim"), ~ { 
    v <- .x
    factor(v, levels = as.numeric(names(fact_labels)), labels = fact_labels)
  }))

# Write output
write_csv(full_data_final, "data/output/cleaned_data.csv")
