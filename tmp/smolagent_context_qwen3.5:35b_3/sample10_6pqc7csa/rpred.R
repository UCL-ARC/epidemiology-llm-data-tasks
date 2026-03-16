# Load required libraries
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_five <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", 
                         delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", 
                  delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                  delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave_one, wave_four, by = "NSID")
df <- full_join(df, wave_five, by = "NSID")
df <- full_join(df, wave_six, by = "NSID")
df <- full_join(df, wave_seven, by = "NSID")
df <- full_join(df, ns8, by = "NSID")
df <- full_join(df, ns9, by = "NSID")

# Age 17: W4empsYP -> ecoact17 (harmonized 6 categories)
df <- df %>%
  mutate(
    ecoact17_raw = W4empsYP,
    ecoact17 = case_when(
      ecoact17_raw == -999 | ecoact17_raw == -998 | ecoact17_raw == -997 | ecoact17_raw == -995 ~ -2,
      ecoact17_raw == -94 ~ -8,
      ecoact17_raw == -92 ~ -9,
      ecoact17_raw == -91 ~ -3,
      ecoact17_raw %in% c(1, 2) ~ 1,
      ecoact17_raw == 3 ~ 2,
      ecoact17_raw == 4 ~ 3,
      ecoact17_raw == 5 ~ 4,
      ecoact17_raw == 6 ~ 5,
      ecoact17_raw == 7 ~ 6,
      ecoact17_raw == 8 ~ 7,
      ecoact17_raw == 9 ~ 8,
      TRUE ~ NA_real_
    )
  )

# Age 18: W5mainactYP -> ecoact18 (harmonized 6 categories)
df <- df %>%
  mutate(
    ecoact18_raw = W5mainactYP,
    ecoact18 = case_when(
      ecoact18_raw == -999 | ecoact18_raw == -998 | ecoact18_raw == -997 | ecoact18_raw == -995 ~ -2,
      ecoact18_raw == -94 ~ -8,
      ecoact18_raw == -92 ~ -9,
      ecoact18_raw == -91 ~ -3,
      ecoact18_raw %in% c(1, 2) ~ 1,
      ecoact18_raw == 3 ~ 2,
      ecoact18_raw == 4 ~ 3,
      ecoact18_raw %in% c(5, 6) ~ 4,
      ecoact18_raw == 7 ~ 5,
      ecoact18_raw == 8 ~ 6,
      ecoact18_raw %in% c(9, 10, 11) ~ 7,
      TRUE ~ NA_real_
    )
  )

# Age 19: W6TCurrentAct -> ecoact19 (harmonized 6 categories)
df <- df %>%
  mutate(
    ecoact19_raw = W6TCurrentAct,
    ecoact19 = case_when(
      ecoact19_raw == -999 | ecoact19_raw == -998 | ecoact19_raw == -997 | ecoact19_raw == -995 ~ -2,
      ecoact19_raw == -91 ~ -3,
      ecoact19_raw %in% c(1, 2) ~ 1,
      ecoact19_raw == 3 ~ 2,
      ecoact19_raw == 4 ~ 3,
      ecoact19_raw == 5 ~ 4,
      ecoact19_raw %in% c(6, 9) ~ 5,
      ecoact19_raw == 7 ~ 6,
      ecoact19_raw == 8 ~ 7,
      ecoact19_raw == 10 ~ 2,
      ecoact19_raw == 11 ~ 8,
      TRUE ~ NA_real_
    )
  )

# Age 20: W7TCurrentAct -> ecoact20 (harmonized 6 categories)
df <- df %>%
  mutate(
    ecoact20_raw = W7TCurrentAct,
    ecoact20 = case_when(
      ecoact20_raw == -999 | ecoact20_raw == -998 | ecoact20_raw == -997 | ecoact20_raw == -995 ~ -2,
      ecoact20_raw == -91 ~ -3,
      ecoact20_raw %in% c(1, 2) ~ 1,
      ecoact20_raw == 3 ~ 2,
      ecoact20_raw == 4 ~ 3,
      ecoact20_raw == 5 ~ 4,
      ecoact20_raw %in% c(6, 9) ~ 5,
      ecoact20_raw == 7 ~ 6,
      ecoact20_raw == 8 ~ 7,
      ecoact20_raw == 10 ~ 8,
      ecoact20_raw %in% c(11, 12, 13, 14, 15) ~ 9,
      TRUE ~ NA_real_
    )
  )

# Age 25: W8DACTIVITYC -> ecoact25 (harmonized 6 categories) + ecoactadu25 (detailed 10 categories)
df <- df %>%
  mutate(
    ecoactadu25_raw = W8DACTIVITYC,
    ecoactadu25 = case_when(
      ecoactadu25_raw == -9 ~ -9,
      ecoactadu25_raw == -8 ~ -8,
      ecoactadu25_raw == -1 ~ -3,
      ecoactadu25_raw == 1 ~ 1,
      ecoactadu25_raw == 2 ~ 2,
      ecoactadu25_raw == 3 ~ 3,
      ecoactadu25_raw == 4 ~ 4,
      ecoactadu25_raw == 5 ~ 5,
      ecoactadu25_raw == 6 ~ 6,
      ecoactadu25_raw == 7 ~ 7,
      ecoactadu25_raw == 8 ~ 8,
      ecoactadu25_raw == 9 ~ 9,
      ecoactadu25_raw == 10 ~ 10,
      TRUE ~ NA_real_
    ),
    ecoact25 = case_when(
      ecoactadu25_raw == -9 ~ -9,
      ecoactadu25_raw == -8 ~ -8,
      ecoactadu25_raw == -1 ~ -3,
      ecoactadu25_raw %in% c(1, 2) ~ 1,
      ecoactadu25_raw == 3 ~ 2,
      ecoactadu25_raw == 4 ~ 3,
      ecoactadu25_raw %in% c(6, 7) ~ 4,
      ecoactadu25_raw == 5 ~ 5,
      ecoactadu25_raw == 8 ~ 6,
      ecoactadu25_raw == 9 ~ 7,
      ecoactadu25_raw == 10 ~ 8,
      TRUE ~ NA_real_
    )
  )

# Age 32: W9DACTIVITYC -> ecoact32 (harmonized 6 categories) + ecoactadu32 (detailed 10 categories)
df <- df %>%
  mutate(
    ecoactadu32_raw = W9DACTIVITYC,
    ecoactadu32 = case_when(
      ecoactadu32_raw == -9 ~ -9,
      ecoactadu32_raw == -8 ~ -8,
      ecoactadu32_raw == -1 ~ -3,
      ecoactadu32_raw == 1 ~ 1,
      ecoactadu32_raw == 2 ~ 2,
      ecoactadu32_raw == 3 ~ 3,
      ecoactadu32_raw == 4 ~ 4,
      ecoactadu32_raw == 5 ~ 5,
      ecoactadu32_raw == 6 ~ 6,
      ecoactadu32_raw == 7 ~ 7,
      ecoactadu32_raw == 8 ~ 8,
      ecoactadu32_raw == 9 ~ 9,
      ecoactadu32_raw == 10 ~ 10,
      TRUE ~ NA_real_
    ),
    ecoact32 = case_when(
      ecoactadu32_raw == -9 ~ -9,
      ecoactadu32_raw == -8 ~ -8,
      ecoactadu32_raw == -1 ~ -3,
      ecoactadu32_raw %in% c(1, 2) ~ 1,
      ecoactadu32_raw == 3 ~ 2,
      ecoactadu32_raw == 4 ~ 3,
      ecoactadu32_raw %in% c(6, 7) ~ 4,
      ecoactadu32_raw == 5 ~ 5,
      ecoactadu32_raw == 8 ~ 6,
      ecoactadu32_raw == 9 ~ 7,
      ecoactadu32_raw == 10 ~ 8,
      TRUE ~ NA_real_
    )
  )

# Convert to factors using base R factor() with named levels
df$ecoact17 <- factor(df$ecoact17, 
    levels = c(-2, -8, -9, -3, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Schedule not applicable", "Don't know", "Refused", "Not asked",
               "In paid work", "Unemployed", "In training", "In full-time education",
               "Looking after family/household", "Retired", "Sick/disabled", "Other"))
df$ecoact18 <- factor(df$ecoact18,
    levels = c(-2, -8, -9, -3, 1, 2, 3, 4, 5, 6, 7),
    labels = c("Schedule not applicable", "Don't know", "Refused", "Not asked",
               "In training/apprenticeship", "In paid work", "In full-time education",
               "In training", "Unemployed", "Looking after family/household", "Waiting for course/job"))
df$ecoact19 <- factor(df$ecoact19,
    levels = c(-2, -3, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Schedule not applicable", "Not asked", "In full-time education",
               "In paid work", "In training", "In training/apprenticeship",
               "Waiting for course/job", "Looking after family/household", "Unemployed",
               "In unpaid/voluntary work"))
df$ecoact20 <- factor(df$ecoact20,
    levels = c(-2, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c("Schedule not applicable", "Not asked", "In full-time education",
               "In paid work", "In training", "In training/apprenticeship",
               "Waiting for course/job", "Looking after family/household", "Unemployed",
               "In unpaid/voluntary work", "Other"))
df$ecoact25 <- factor(df$ecoact25,
    levels = c(-2, -8, -9, -3, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Schedule not applicable", "Don't know", "Refused", "Not asked",
               "In paid work", "In unpaid/voluntary work", "Unemployed",
               "In training/apprenticeship", "In full-time education", "Sick/disabled",
               "Looking after family/household", "Other"))
df$ecoact32 <- factor(df$ecoact32,
    levels = c(-2, -8, -9, -3, 1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("Schedule not applicable", "Don't know", "Refused", "Not asked",
               "In paid work", "In unpaid/voluntary work", "Unemployed",
               "In training/apprenticeship", "In full-time education", "Sick/disabled",
               "Looking after family/household", "Other"))
df$ecoactadu25 <- factor(df$ecoactadu25,
    levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c("Refused", "Don't know", "Not asked",
               "Employee - in paid work", "Self employed", "In unpaid/voluntary work",
               "Unemployed", "Education: School/college/university", "Apprenticeship",
               "On gov't scheme for employment training", "Sick or disabled",
               "Looking after home or family", "Something else"))
df$ecoactadu32 <- factor(df$ecoactadu32,
    levels = c(-9, -8, -3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c("Refused", "Don't know", "Not asked",
               "Employee - in paid work", "Self employed", "In unpaid/voluntary work",
               "Unemployed", "Education: School/college/university", "Apprenticeship",
               "On gov't scheme for employment training", "Sick or disabled",
               "Looking after home or family", "Something else"))

# Select final variables
df_final <- df %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoact32, ecoactadu25, ecoactadu32)

# Write output
write_csv(df_final, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")