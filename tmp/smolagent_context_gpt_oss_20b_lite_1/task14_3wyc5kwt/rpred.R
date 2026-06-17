library(readr)
library(dplyr)
library(purrr)
library(labelled)

# Helper to standardise missing codes
standardise_missing <- function(vec, mapping){
  vec <- as.numeric(vec)
  for(i in seq_along(mapping)){
    vec[vec == as.numeric(names(mapping)[i])] <- as.numeric(mapping[i])
  }
  vec[is.na(vec)] <- -3
  return(vec)
}

# Labels
lbl_detailed <- c("Owned outright" = 1,
                  "Owned with mortgage" = 2,
                  "Shared ownership" = 3,
                  "Rent from council" = 4,
                  "Rent from association" = 5,
                  "Rent privately" = 6,
                  "Rent free" = 7,
                  "Other arrangement" = 8)

lbl_collapsed <- c("Owned outright" = 1,
                    "Owned with mortgage" = 2,
                    "Shared ownership" = 3,
                    "Rent it" = 4,
                    "Rent free" = 5,
                    "Other arrangement" = 6)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Missing code mappings
missing_common <- c("-999" = -2,
                    "-997" = -2,
                    "-92" = -9,
                    "-91" = -1,
                    "-1" = -1)

missing_w5 <- c("-999" = -2,
                "-997" = -2,
                "-92" = -9,
                "-91" = -1,
                "-1" = -1)

missing_w6w7 <- c("-999" = -2,
                  "-997" = -2,
                  "-92" = -9,
                  "-91" = -1,
                  "-1" = -1)

missing_w8 <- c("-9" = -9,
                "-8" = -1,
                "-1" = -1)

missing_w9 <- c("-8" = -9,
                "-1" = -1)

# Detailed variables for ages 14-20
wave1 <- wave1 %>% mutate(hownteen14 = standardise_missing(W1hous12HH, missing_common))
wave2 <- wave2 %>% mutate(hownteen15 = standardise_missing(W2Hous12HH, missing_common))
wave3 <- wave3 %>% mutate(hownteen16 = standardise_missing(W3hous12HH, missing_common))
wave4 <- wave4 %>% mutate(hownteen17 = standardise_missing(W4Hous12HH, missing_common))

# Wave5 detailed (age 18)
wave5 <- wave5 %>% 
  mutate(tenure_type = standardise_missing(W5Hous12HH, missing_w5),
         bhh = standardise_missing(W5Hous12BHH, missing_w5),
         chc = standardise_missing(W5Hous12CHH, missing_w5))

wave5 <- wave5 %>% mutate(hownteen18 = case_when(
  tenure_type==1 & bhh %in% 1:4 ~ bhh,
  tenure_type==1 & bhh==5 ~ 8,
  tenure_type==2 & chc==1 ~ 4,
  tenure_type==2 & chc==2 ~ 5,
  tenure_type==2 & chc==3 ~ 6,
  tenure_type==2 & chc==4 ~ 7,
  tenure_type==2 & chc==5 ~ 8,
  tenure_type==3 ~ 8,
  TRUE ~ -3
))

# Wave6 detailed (age 19)
wave6 <- wave6 %>% 
  mutate(tenure_type = standardise_missing(W6Hous12YP, missing_w6w7),
         byp = standardise_missing(W6Hous12bYP, missing_w6w7),
         cyp = standardise_missing(W6Hous12cYP, missing_w6w7))

wave6 <- wave6 %>% mutate(hownteen19 = case_when(
  tenure_type==1 & byp %in% 1:4 ~ byp,
  tenure_type==1 & byp==5 ~ 8,
  tenure_type==2 & cyp==1 ~ 4,
  tenure_type==2 & cyp==2 ~ 5,
  tenure_type==2 & cyp==3 ~ 6,
  tenure_type==2 & cyp==4 ~ 7,
  tenure_type==2 & cyp==5 ~ 8,
  tenure_type==3 ~ 8,
  TRUE ~ -3
))

# Wave7 detailed (age 20)
wave7 <- wave7 %>% 
  mutate(tenure_type = standardise_missing(W7Hous12YP, missing_w6w7),
         byp = standardise_missing(W7Hous12bYP, missing_w6w7),
         cyp = standardise_missing(W7Hous12cYP, missing_w6w7))

wave7 <- wave7 %>% mutate(hownteen20 = case_when(
  tenure_type==1 & byp %in% 1:4 ~ byp,
  tenure_type==1 & byp==5 ~ 8,
  tenure_type==2 & cyp==1 ~ 4,
  tenure_type==2 & cyp==2 ~ 5,
  tenure_type==2 & cyp==3 ~ 6,
  tenure_type==2 & cyp==4 ~ 7,
  tenure_type==2 & cyp==5 ~ 8,
  tenure_type==3 ~ 8,
  TRUE ~ -3
))

# Collapse detailed to collapsed for 14-20
collapse_func <- function(x){
  ifelse(x %in% 4:6, 4, x)
}

wave1 <- wave1 %>% mutate(hown14 = collapse_func(hownteen14))
wave2 <- wave2 %>% mutate(hown15 = collapse_func(hownteen15))
wave3 <- wave3 %>% mutate(hown16 = collapse_func(hownteen16))
wave4 <- wave4 %>% mutate(hown17 = collapse_func(hownteen17))
wave5 <- wave5 %>% mutate(hown18 = collapse_func(hownteen18))
wave6 <- wave6 %>% mutate(hown19 = collapse_func(hownteen19))
wave7 <- wave7 %>% mutate(hown20 = collapse_func(hownteen20))

# Wave8 collapsed (age 25)
wave8 <- wave8 %>% mutate(hown25 = standardise_missing(W8TENURE, missing_w8))
wave8$hown25 <- ifelse(wave8$hown25 %in% 4:6, 4, wave8$hown25)

# Wave9 collapsed (age 32)
wave9 <- wave9 %>% mutate(hown32 = standardise_missing(W9DTENURE, missing_w9))
wave9$hown32 <- ifelse(wave9$hown32 %in% 4:6, 4, wave9$hown32)

# Keep only final variables
vars1 <- wave1 %>% select(NSID, hownteen14, hown14)
vars2 <- wave2 %>% select(NSID, hownteen15, hown15)
vars3 <- wave3 %>% select(NSID, hownteen16, hown16)
vars4 <- wave4 %>% select(NSID, hownteen17, hown17)
vars5 <- wave5 %>% select(NSID, hownteen18, hown18)
vars6 <- wave6 %>% select(NSID, hownteen19, hown19)
vars7 <- wave7 %>% select(NSID, hownteen20, hown20)
vars8 <- wave8 %>% select(NSID, hown25)
vars9 <- wave9 %>% select(NSID, hown32)

# Merge all waves
cleaned <- reduce(list(vars1, vars2, vars3, vars4, vars5, vars6, vars7, vars8, vars9), full_join, by = "NSID")

# Assign labels
for(v in c("hownteen14","hownteen15","hownteen16","hownteen17","hownteen18","hownteen19","hownteen20")){
  cleaned[[v]] <- as.numeric(cleaned[[v]])
  cleaned[[v]] <- labelled(cleaned[[v]], lbl_detailed)
}
for(v in c("hown14","hown15","hown16","hown17","hown18","hown19","hown20","hown25","hown32")){
  if(v %in% names(cleaned)){
    cleaned[[v]] <- as.numeric(cleaned[[v]])
    cleaned[[v]] <- labelled(cleaned[[v]], lbl_collapsed)
  }
}

# Write CSV
write_csv(cleaned, "data/output/cleaned_data.csv")

