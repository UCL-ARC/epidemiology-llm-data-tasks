library(readr)
library(dplyr)

# Helper to recode 'ever drank' variables from waves 1-7
recode_alcever <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}
# Helper to recode AUDIT1 (w8,w9) variables
recode_audit1 <- function(x) {
  case_when(
    x == 1 ~ 0,          # Never
    x >= 2 & x <= 5 ~ 1, # Any drinking
    TRUE ~ NA_real_
  )
}

# Load files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", col_types = cols(), na = c(""))
ns8   <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_types = cols(), na = c(""))
ns9   <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = cols(), na = c(""))

# Create binary indicators
wave1 <- wave1 %>% mutate(drank14 = recode_alcever(W1alceverYP))
wave2 <- wave2 %>% mutate(drank15 = recode_alcever(W2alceverYP))
wave3 <- wave3 %>% mutate(drank16 = recode_alcever(W3alceverYP))
wave4 <- wave4 %>% mutate(drank17 = recode_alcever(W4AlcEverYP))
wave6 <- wave6 %>% mutate(drank19 = recode_alcever(W6AlcEverYP))
wave7 <- wave7 %>% mutate(drank20 = recode_alcever(W7AlcEverYP))
ns8   <- ns8   %>% mutate(drank25 = recode_audit1(W8AUDIT1))
ns9   <- ns9   %>% mutate(drank32 = recode_audit1(W9AUDIT1))

# Merge on NSID
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(ns8,   by = "NSID") %>%
  full_join(ns9,   by = "NSID")

# Ages vector corresponding to the drink columns
ages_vec <- c(14,15,16,17,19,20,25,32)

# Compute alcfst
alcfst_vec <- apply(merged %>% select(drank14:drank32), 1, function(row) {
  # Find first occurrence of 1
  idx <- which(row == 1)
  if(length(idx) > 0) {
    return(ages_vec[idx[1]])
  }
  # If all valid and no 1, set to 99
  if(all(row == 0, na.rm = TRUE) && all(!is.na(row))) {
    return(99)
  }
  return(NA_real_)
})

merged <- merged %>% mutate(alcfst = alcfst_vec)
# Replace NA with -3 standard missing
merged <- merged %>% mutate(alcfst = ifelse(is.na(alcfst), -3, alcfst))

# Output only NSID and alcfst
output_df <- merged %>% select(NSID, alcfst)

write_csv(output_df, "data/output/cleaned_data.csv")
