library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Load all files
prefix <- "data/input/"
load_tab <- function(fname){
  read_delim(paste0(prefix,fname), delim="\t", col_types = cols(), na=c("", "NA"))
}

wave_one <- load_tab("wave_one_lsype_young_person_2020.tab")
wave_four <- load_tab("wave_four_lsype_young_person_2020.tab")
wave_six <- load_tab("wave_six_lsype_young_person_2020.tab")
wave_seven <- load_tab("wave_seven_lsype_young_person_2020.tab")
wave_eight <- load_tab("ns8_2015_main_interview.tab")
wave_nine <- load_tab("ns9_2022_main_interview.tab")

# Merge by NSID
merged <- wave_one %>%
  full_join(wave_four, by="NSID") %>%
  full_join(wave_six, by="NSID") %>%
  full_join(wave_seven, by="NSID") %>%
  full_join(wave_eight, by="NSID") %>%
  full_join(wave_nine, by="NSID")

# Helper to map missing codes
map_missing <- function(vec){
  code_map <- c(
    "-9" = -9,
    "-8" = -8,
    "-7" = -7,
    "-3" = -3,
    "-2" = -2,
    "-1" = -1,
    "-94" = -8,
    "-92" = -9,
    "-91" = -1,
    "-99" = -3,
    "-100" = -2,
    "-97" = -2,
    "-999" = -2,
    "-998" = -2,
    "-997" = -2,
    "-995" = -2
  )
  vec_num <- as.numeric(vec)
  out <- vec_num
  for (code in names(code_map)){
    out[vec_num == as.numeric(code)] <- code_map[code]
  }
  out[is.na(out)] <- -3
  return(out)
}

# -------------- educaim17 from w4saim ----------------
merged <- merged %>%
  mutate(
    w4saim_clean = map_missing(as.numeric(w4saim)),
    educaim17 = case_when(
      w4saim_clean %in% c(1,2,3,4,5,6,7,9) ~ 1,
      w4saim_clean %in% c(8,10) ~ 2,
      w4saim_clean %in% c(12,13) ~ 3,
      w4saim_clean == 14 ~ 5,
      TRUE ~ -3
    ),
    educaim17 = factor(educaim17, levels = 0:5,
                      labels = c("NVQ 4–5 equivalent","NVQ 1–3 equivalent","None / entry level","Other","None of these qualifications","Not currently studying"))
  )

# -------------- educaim19 from W6Saim ----------------
merged <- merged %>%
  mutate(
    w6saim_clean = map_missing(as.numeric(W6Saim)),
    educaim19 = case_when(
      w6saim_clean %in% c(1,2,3,4) ~ 0,
      w6saim_clean %in% c(5,6,7,8,9,10,12,13) ~ 1,
      w6saim_clean == 11 ~ 2,
      w6saim_clean %in% c(14,15) ~ 3,
      w6saim_clean == 16 ~ 5,
      TRUE ~ -3
    ),
    educaim19 = factor(educaim19, levels = 0:5,
                      labels = c("NVQ 4–5 equivalent","NVQ 1–3 equivalent","None / entry level","Other","None of these qualifications","Not currently studying"))
  )

# -------------- educaim20 from W7SAim ----------------
merged <- merged %>%
  mutate(
    w7saim_clean = map_missing(as.numeric(W7SAim)),
    educaim20 = case_when(
      w7saim_clean %in% c(10,11,12,13) ~ 0,
      w7saim_clean %in% c(1,2,3,5,6,7,9) ~ 1,
      w7saim_clean == 4 ~ 2,
      w7saim_clean == 14 ~ 3,
      w7saim_clean == 5 ~ 5,
      TRUE ~ -3
    ),
    educaim20 = factor(educaim20, levels = 0:5,
                      labels = c("NVQ 4–5 equivalent","NVQ 1–3 equivalent","None / entry level","Other","None of these qualifications","Not currently studying"))
  )

# -------------- educaim25 from wave 8 ----------------
merged <- merged %>%
  mutate(act8_clean = map_missing(as.numeric(W8ACTIVITY05)))

# Determine existing variables for tick boxes
nvq4_5_vars_8 <- intersect(c("W8VCQUC0J"), names(merged))
nvq1_3_vars_8 <- intersect(c("W8ACQUC0A","W8ACQUC0B","W8ACQUC0C","W8ACQUC0D","W8ACQUC0E","W8ACQUC0F","W8ACQUC0G","W8ACQUC0H","W8ACQUC0I","W8ACQUC0K","W8ACQUC0L"), names(merged))
entry_level_vars_8 <- intersect(c("W8ACQUC0L"), names(merged))
qual_vars_8 <- intersect(c(paste0("W8ACQUC0", LETTERS[1:26]), paste0("W8VCQUC0", LETTERS[1:26])), names(merged))

merged <- merged %>%
  mutate(
    nvq4_5_8 = ifelse(rowSums(across(all_of(nvq4_5_vars_8)) == 1, na.rm = TRUE) > 0, 1, 0),
    nvq1_3_8 = ifelse(rowSums(across(all_of(nvq1_3_vars_8)) == 1, na.rm = TRUE) > 0, 1, 0),
    entry_level_8 = ifelse(rowSums(across(all_of(entry_level_vars_8)) == 1, na.rm = TRUE) > 0, 1, 0),
    other_8 = ifelse(rowSums(across(all_of(qual_vars_8)) == 1, na.rm = TRUE) > 0, 1, 0) -
      nvq4_5_8 - nvq1_3_8 - entry_level_8
  )

merged <- merged %>%
  mutate(
    educaim25 = case_when(
      act8_clean %in% c(0, -1, -8, -9) ~ 5,
      act8_clean == 1 & nvq4_5_8 == 1 ~ 0,
      act8_clean == 1 & nvq1_3_8 == 1 ~ 1,
      act8_clean == 1 & entry_level_8 == 1 ~ 2,
      act8_clean == 1 & other_8 == 1 ~ 3,
      act8_clean == 1 & nvq4_5_8 == 0 & nvq1_3_8 == 0 & entry_level_8 == 0 & other_8 == 0 ~ 4,
      TRUE ~ -3
    ),
    educaim25 = factor(educaim25, levels = 0:5,
                      labels = c("NVQ 4–5 equivalent","NVQ 1–3 equivalent","None / entry level","Other","None of these qualifications","Not currently studying"))
  )

# -------------- educaim32 from wave 9 ----------------
merged <- merged %>%
  mutate(act9_clean = map_missing(as.numeric(W9ECONACT2)))

# Determine existing variables for tick boxes
nvq4_5_vars_9 <- intersect(c("W9VCQUC0J"), names(merged))
nvq1_3_vars_9 <- intersect(c("W9ACQUC0A","W9ACQUC0B","W9ACQUC0C","W9ACQUC0D","W9ACQUC0E","W9ACQUC0F","W9ACQUC0G","W9ACQUC0H","W9ACQUC0I","W9ACQUC0J","W9ACQUC0K","W9ACQUC0L","W9ACQUC0M","W9ACQUC0N","W9ACQUC0O","W9ACQUC0P","W9ACQUC0Q","W9ACQUC0R","W9ACQUC0S","W9ACQUC0T","W9ACQUC0U","W9ACQUC0V","W9ACQUC0W","W9ACQUC0X","W9ACQUC0Y","W9ACQUC0Z"), names(merged))
entry_level_vars_9 <- intersect(c("W9ACQUC0L"), names(merged))
qual_vars_9 <- intersect(c(paste0("W9ACQUC0", LETTERS[1:26]), paste0("W9VCQUC0", LETTERS[1:26])), names(merged))

merged <- merged %>%
  mutate(
    nvq4_5_9 = ifelse(rowSums(across(all_of(nvq4_5_vars_9)) == 1, na.rm = TRUE) > 0, 1, 0),
    nvq1_3_9 = ifelse(rowSums(across(all_of(nvq1_3_vars_9)) == 1, na.rm = TRUE) > 0, 1, 0),
    entry_level_9 = ifelse(rowSums(across(all_of(entry_level_vars_9)) == 1, na.rm = TRUE) > 0, 1, 0),
    other_9 = ifelse(rowSums(across(all_of(qual_vars_9)) == 1, na.rm = TRUE) > 0, 1, 0) -
      nvq4_5_9 - nvq1_3_9 - entry_level_9
  )

merged <- merged %>%
  mutate(
    educaim32 = case_when(
      act9_clean %in% c(-3) ~ -3,
      act9_clean %in% c(0, -1, -8, -9) ~ 5,
      act9_clean %in% c(6,7) ~ case_when(
        nvq4_5_9 == 1 ~ 0,
        nvq1_3_9 == 1 ~ 1,
        entry_level_9 == 1 ~ 2,
        other_9 == 1 ~ 3,
        TRUE ~ 4
      ),
      TRUE ~ -3
    ),
    educaim32 = factor(educaim32, levels = 0:5,
                      labels = c("NVQ 4–5 equivalent","NVQ 1–3 equivalent","None / entry level","Other","None of these qualifications","Not currently studying"))
  )

# Final dataset
final_df <- merged %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write
write_csv(final_df, "data/output/cleaned_data.csv")
cat("Script executed successfully.")
