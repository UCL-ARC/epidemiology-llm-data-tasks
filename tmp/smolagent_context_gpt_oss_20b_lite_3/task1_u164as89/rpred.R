# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Ensure output directory exists
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

# Helper to map sex and missing codes to standard set
map_sex <- function(sex, values) {
  sex_num <- suppressWarnings(as.numeric(sex))
  sex_std <- sex_num
  for (code in names(values)) {
    sex_std[sex_num == as.numeric(code)] <- values[[code]]
  }
  return(sex_std)
}

base_path <- "data/input/"

# Wave 1: Age 14
wf1 <- read_delim(paste0(base_path, "wave_one_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_14 = map_sex(W1sexYP, list('-99' = -3, '-92' = -9, '-91' = -1))) %>%
  select(NSID, sex_14)

# Wave 2: Age 15
wf2 <- read_delim(paste0(base_path, "wave_two_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_15 = map_sex(W2SexYP, list('-998' = -2, '-997' = -2, '-995' = -2,
                               '-99' = -3, '-92' = -9, '-91' = -1, '-1' = -8))) %>%
  select(NSID, sex_15)

# Wave 3: Age 16
wf3 <- read_delim(paste0(base_path, "wave_three_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_16 = map_sex(W3sexYP, list('-99' = -3, '-92' = -9, '-91' = -1))) %>%
  select(NSID, sex_16)

# Wave 4: Age 17
wf4 <- read_delim(paste0(base_path, "wave_four_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_17 = map_sex(W4SexYP, list('-99' = -3, '-92' = -9, '-91' = -1, '-1' = -8))) %>%
  select(NSID, sex_17)

# Wave 5: Age 18
wf5 <- read_delim(paste0(base_path, "wave_five_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_18 = map_sex(W5SexYP, list('-1' = -8))) %>%
  select(NSID, sex_18)

# Wave 6: Age 19
wf6 <- read_delim(paste0(base_path, "wave_six_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_19 = map_sex(W6Sex, list('-92' = -9, '-91' = -1))) %>%
  select(NSID, sex_19)

# Wave 7: Age 20
wf7 <- read_delim(paste0(base_path, "wave_seven_lsype_young_person_2020.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_20 = map_sex(W7Sex, list('-91' = -1))) %>%
  select(NSID, sex_20)

# Wave 8: Age 25
wf8 <- read_delim(paste0(base_path, "ns8_2015_main_interview.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_25 = map_sex(W8CMSEX, list('-9' = -9, '-8' = -8, '-1' = -1))) %>%
  select(NSID, sex_25)

# Wave 9: Age 32
wf9 <- read_delim(paste0(base_path, "ns9_2022_main_interview.tab"),
                   delim="\t", col_types = cols(.default = col_character())) %>%
  mutate(sex_32 = map_sex(W9DSEX, list())) %>%
  select(NSID, sex_32)

# Merge all waves by NSID
cohort <- wf1 %>%
  full_join(wf2, by = "NSID") %>%
  full_join(wf3, by = "NSID") %>%
  full_join(wf4, by = "NSID") %>%
  full_join(wf5, by = "NSID") %>%
  full_join(wf6, by = "NSID") %>%
  full_join(wf7, by = "NSID") %>%
  full_join(wf8, by = "NSID") %>%
  full_join(wf9, by = "NSID")

# Convert to numeric and keep only 1 or 2
sex_cols <- c("sex_32", "sex_25", "sex_20", "sex_19", "sex_18", "sex_17", "sex_16", "sex_15", "sex_14")
cohort <- cohort %>%
  mutate(across(all_of(sex_cols), ~as.numeric(.))) %>%
  mutate(across(all_of(sex_cols), ~ifelse(. %in% c(1,2), ., NA_real_)))

# Consolidated sex (most recent valid first)
cohort <- cohort %>%
  mutate(sex = coalesce(sex_32, sex_25, sex_20, sex_19, sex_18, sex_17, sex_16, sex_15, sex_14))

# Replace remaining NA with -3 (standard missing)
cohort <- cohort %>%
  mutate(sex = ifelse(is.na(sex), -3, sex))

# Keep only NSID and sex
final_df <- cohort %>% select(NSID, sex)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
