# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Define file paths
file_list <- list(
  wave_one = "wave_one_lsype_young_person_2020.tab",
  wave_four = "wave_four_lsype_young_person_2020.tab",
  wave_two = "wave_two_lsype_family_background_2020.tab",
  wave_three = "wave_three_lsype_family_background_2020.tab",
  ns8 = "ns8_2015_derived.tab",
  ns9_derived = "ns9_2022_derived_variables.tab",
  ns9_main = "ns9_2022_main_interview.tab"
)

# Helper to read files as character columns
read_tab <- function(fname){
  read_delim(paste0("data/input/", fname), delim = "\t", col_types = cols(.default = "c"), na = c("", "NA"))
}

# Read all files
wf_one <- read_tab(file_list$wave_one)
wf_four <- read_tab(file_list$wave_four)
wf_two <- read_tab(file_list$wave_two) %>% rename(urbind_w2 = "urbind", gor_w2 = "gor")
wf_three <- read_tab(file_list$wave_three) %>% rename(urbind_w3 = "urbind", gor_w3 = "gor")
ns8 <- read_tab(file_list$ns8)
ns9_derived <- read_tab(file_list$ns9_derived)
ns9_main <- read_tab(file_list$ns9_main)

# Merge all datasets by NSID using full_join
merged <- wf_one %>%
  full_join(wf_four, by = "NSID") %>%
  full_join(wf_two, by = "NSID") %>%
  full_join(wf_three, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Helper to convert to numeric safely

to_num <- function(x){
  suppressWarnings(as.numeric(x))
}

# Recode functions
recode_urbind <- function(x){
  x_num <- to_num(x)
  case_when(
    !is.na(x_num) & x_num %in% 1:8 ~ x_num,
    !is.na(x_num) & x_num == -94 ~ -8,
    !is.na(x_num) & x_num %in% c(-999,-998,-997,-995) ~ -2,
    !is.na(x_num) & x_num == -1 ~ -1,
    TRUE ~ -3
  )
}

recode_gor <- function(x){
  x_num <- to_num(x)
  case_when(
    !is.na(x_num) & x_num %in% 1:9 ~ x_num,
    !is.na(x_num) & x_num == -94 ~ -8,
    !is.na(x_num) & x_num %in% c(-999,-998,-997,-995) ~ -2,
    !is.na(x_num) & x_num == -1 ~ -1,
    TRUE ~ -3
  )
}

recode_w8dgor <- function(x){
  x_num <- to_num(x)
  case_when(
    !is.na(x_num) & x_num %in% 1:12 ~ x_num,
    !is.na(x_num) & x_num == 13 ~ -2,
    !is.na(x_num) & x_num %in% c(-9,-8,-1) ~ x_num,
    !is.na(x_num) & x_num %in% c(-999,-998,-997,-995) ~ -2,
    !is.na(x_num) & x_num == -94 ~ -8,
    TRUE ~ -3
  )
}

recode_w9drgn <- function(x){
  x_num <- to_num(x)
  case_when(
    !is.na(x_num) & x_num %in% 1:12 ~ x_num,
    !is.na(x_num) & x_num == 13 ~ -2,
    !is.na(x_num) & x_num %in% c(-9,-8,-1) ~ x_num,
    !is.na(x_num) & x_num %in% c(-999,-998,-997,-995) ~ -2,
    !is.na(x_num) & x_num == -94 ~ -8,
    TRUE ~ -3
  )
}

recode_regint32 <- function(x){
  x_num <- to_num(x)
  case_when(
    !is.na(x_num) & x_num %in% 1:4 ~ 1,
    !is.na(x_num) & x_num == 5 ~ 2,
    !is.na(x_num) & x_num %in% c(-9,-8,-3,-1) ~ x_num,
    !is.na(x_num) & x_num %in% c(-999,-998,-997,-995) ~ -2,
    !is.na(x_num) & x_num == -94 ~ -8,
    TRUE ~ -3
  )
}

# Derive variables
out <- merged %>%
  mutate(
    regub15 = recode_urbind(urbind_w2),
    regov15 = recode_gor(gor_w2),
    regub16 = recode_urbind(urbind_w3),
    regov16 = recode_gor(gor_w3),
    regor25 = recode_w8dgor(W8DGOR),
    regor32 = recode_w9drgn(W9DRGN),
    regint32 = recode_regint32(W9NATIONRES)
  ) %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write output CSV
write_csv(out, "data/output/cleaned_data.csv")

# End of script
