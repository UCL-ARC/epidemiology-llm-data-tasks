library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(tidyr)

# Define path
input_path <- "data/input/"

# Helper to load and select ONLY NSID and relevant variables to save memory
load_relevant <- function(filename, vars) {
  df <- read_delim(paste0(input_path, filename), delim = "\t", col_types = cols(.default = "numeric"), guess_max = 10000)
  df <- df %>% mutate(NSID = as.character(NSID))
  df <- df %>% select(NSID, all_of(vars))
  return(df)
}

# Define variables to keep for each file
vars_w1 <- c("W1hous12HH")
vars_w2 <- c("W2Hous12HH")
vars_w3 <- c("W3hous12HH")
vars_w4 <- c("W4Hous12HH")
vars_w5 <- c("W5Hous12HH", "W5Hous12BHH", "W5Hous12CHH")
vars_w6 <- c("W6Hous12YP", "W6Hous12bYP", "W6Hous12cYP")
vars_w7 <- c("W7Hous12YP", "W7Hous12bYP", "W7Hous12cYP")
vars_w8 <- c("W8TENURE")
vars_w9 <- c("W9DTENURE")

# Load selectively
df1 <- load_relevant("wave_one_lsype_family_background_2020.tab", vars_w1)
df2 <- load_relevant("wave_two_lsype_family_background_2020.tab", vars_w2)
df3 <- load_relevant("wave_three_lsype_family_background_2020.tab", vars_w3)
df4 <- load_relevant("wave_four_lsype_family_background_2020.tab", vars_w4)
df5 <- load_relevant("wave_five_lsype_family_background_2020.tab", vars_w5)
df6 <- load_relevant("wave_six_lsype_young_person_2020.tab", vars_w6)
df7 <- load_relevant("wave_seven_lsype_young_person_2020.tab", vars_w7)
df8 <- load_relevant("ns8_2015_main_interview.tab", vars_w8)
df9 <- load_relevant("ns9_2022_derived_variables.tab", vars_w9)

# Remove duplicates from NSID
df1 <- df1 %>% distinct(NSID, .keep_all = TRUE)
df2 <- df2 %>% distinct(NSID, .keep_all = TRUE)
df3 <- df3 %>% distinct(NSID, .keep_all = TRUE)
df4 <- df4 %>% distinct(NSID, .keep_all = TRUE)
df5 <- df5 %>% distinct(NSID, .keep_all = TRUE)
df6 <- df6 %>% distinct(NSID, .keep_all = TRUE)
df7 <- df7 %>% distinct(NSID, .keep_all = TRUE)
df8 <- df8 %>% distinct(NSID, .keep_all = TRUE)
df9 <- df9 %>% distinct(NSID, .keep_all = TRUE)

# Merge all
full_data <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID") %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

map_missing <- function(val) {
  res <- case_when(
    is.na(val) ~ -3,
    val == -92 ~ -9,   
    val == -1 ~ -8,    
    val == -91 ~ -1,   
    val == -999 ~ -2,  
    val == -997 ~ -2,  
    val == -998 ~ -2,  
    val == -995 ~ -2,  
    val == -99 ~ -3,   
    TRUE ~ val
  )
  return(res)
}

map_missing_late <- function(val) {
  res <- case_when(
    is.na(val) ~ -3,
    val == -9 ~ -9,    
    val == -8 ~ -8,    
    val == -1 ~ -1,    
    TRUE ~ val
  )
  return(res)
}

full_data <- full_data %>%
  mutate(
    hownteen14 = map_missing(W1hous12HH),
    hown14 = case_when(
      W1hous12HH == 1 ~ 1, W1hous12HH == 2 ~ 2, W1hous12HH == 3 ~ 3, W1hous12HH %in% c(4, 5, 6) ~ 4, W1hous12HH == 7 ~ 5, W1hous12HH == 8 ~ 7, TRUE ~ map_missing(W1hous12HH)
    ),
    hownteen15 = map_missing(W2Hous12HH),
    hown15 = case_when(
      W2Hous12HH == 1 ~ 1, W2Hous12HH == 2 ~ 2, W2Hous12HH == 3 ~ 3, W2Hous12HH %in% c(4, 5, 6) ~ 4, W2Hous12HH == 7 ~ 5, W2Hous12HH == 8 ~ 7, TRUE ~ map_missing(W2Hous12HH)
    ),
    hownteen16 = map_missing(W3hous12HH),
    hown16 = case_when(
      W3hous12HH == 1 ~ 1, W3hous12HH == 2 ~ 2, W3hous12HH == 3 ~ 3, W3hous12HH %in% c(4, 5, 6) ~ 4, W3hous12HH == 7 ~ 5, W3hous12HH == 8 ~ 7, TRUE ~ map_missing(W3hous12HH)
    ),
    hownteen17 = map_missing(W4Hous12HH),
    hown17 = case_when(
      W4Hous12HH == 1 ~ 1, W4Hous12HH == 2 ~ 2, W4Hous12HH == 3 ~ 3, W4Hous12HH %in% c(4, 5, 6) ~ 4, W4Hous12HH == 7 ~ 5, W4Hous12HH == 8 ~ 7, TRUE ~ map_missing(W4Hous12HH)
    ),
    hownteen18 = case_when(
      W5Hous12BHH == 1 ~ 1, W5Hous12BHH == 2 ~ 2, W5Hous12BHH == 3 ~ 3, W5Hous12BHH == 4 ~ 8,
      W5Hous12CHH == 1 ~ 4, W5Hous12CHH == 2 ~ 5, W5Hous12CHH == 3 ~ 6, W5Hous12CHH == 4 ~ 7, W5Hous12CHH == 5 ~ 8,
      TRUE ~ map_missing(W5Hous12HH)
    ),
    hown18 = case_when(
      W5Hous12BHH == 1 ~ 1, W5Hous12BHH == 2 ~ 2, W5Hous12BHH == 3 ~ 3, W5Hous12BHH == 4 ~ 7,
      W5Hous12CHH %in% c(1, 2, 3) ~ 4, W5Hous12CHH == 4 ~ 5, W5Hous12CHH == 5 ~ 7,
      TRUE ~ map_missing(W5Hous12HH)
    ),
    hownteen19 = case_when(
      W6Hous12bYP == 1 ~ 1, W6Hous12bYP == 2 ~ 2, W6Hous12bYP == 3 ~ 3, W6Hous12bYP == 4 ~ 8,
      W6Hous12cYP == 1 ~ 4, W6Hous12cYP == 2 ~ 5, W6Hous12cYP == 3 ~ 6, W6Hous12cYP == 4 ~ 7, W6Hous12cYP == 5 ~ 8,
      TRUE ~ map_missing(W6Hous12YP)
    ),
    hown19 = case_when(
      W6Hous12bYP == 1 ~ 1, W6Hous12bYP == 2 ~ 2, W6Hous12bYP == 3 ~ 3, W6Hous12bYP == 4 ~ 7,
      W6Hous12cYP %in% c(1, 2, 3) ~ 4, W6Hous12cYP == 4 ~ 5, W6Hous12cYP == 5 ~ 7,
      TRUE ~ map_missing(W6Hous12YP)
    ),
    hownteen20 = case_when(
      W7Hous12bYP == 1 ~ 1, W7Hous12bYP == 2 ~ 2, W7Hous12bYP == 3 ~ 3, W7Hous12bYP == 4 ~ 8,
      W7Hous12cYP == 1 ~ 4, W7Hous12cYP == 2 ~ 5, W7Hous12cYP == 3 ~ 6, W7Hous12cYP == 4 ~ 7, W7Hous12cYP == 5 ~ 8,
      TRUE ~ map_missing(W7Hous12YP)
    ),
    hown20 = case_when(
      W7Hous12bYP == 1 ~ 1, W7Hous12bYP == 2 ~ 2, W7Hous12bYP == 3 ~ 3, W7Hous12bYP == 4 ~ 7,
      W7Hous12cYP %in% c(1, 2, 3) ~ 4, W7Hous12cYP == 4 ~ 5, W7Hous12cYP == 5 ~ 7,
      TRUE ~ map_missing(W7Hous12YP)
    ),
    hown25 = map_missing_late(W8TENURE),
    hown32 = map_missing_late(W9DTENURE)
  )

final_cols <- c("NSID", "hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18", "hownteen19", "hownteen20", 
                "hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20", "hown25", "hown32")

final_df <- full_data %>%
  select(all_of(final_cols)) %>%
  mutate(across(where(is.numeric), ~ replace_na(., -3)))

write_csv(final_df, "data/output/cleaned_data.csv")