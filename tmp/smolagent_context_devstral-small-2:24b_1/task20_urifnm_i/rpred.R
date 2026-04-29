library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Load each dataset
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8_sc <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select and rename variables
wave1 <- wave1 %>% select(NSID, W1alceverYP, W1alcmonYP) %>% rename(alcever14 = W1alceverYP, alcmon14 = W1alcmonYP)
wave2 <- wave2 %>% select(NSID, W2alceverYP) %>% rename(alcever15 = W2alceverYP)
wave3 <- wave3 %>% select(NSID, W3alceverYP) %>% rename(alcever16 = W3alceverYP)
wave4 <- wave4 %>% select(NSID, W4AlcEverYP) %>% rename(alcever17 = W4AlcEverYP)
wave6 <- wave6 %>% select(NSID, W6AlcEverYP) %>% rename(alcever19 = W6AlcEverYP)
wave7 <- wave7 %>% select(NSID, W7AlcEverYP) %>% rename(alcever20 = W7AlcEverYP)
wave8_sc <- wave8_sc %>% select(NSID, W8AUDIT1) %>% rename(alcfreq25 = W8AUDIT1)
wave9 <- wave9 %>% select(NSID, W9AUDIT1) %>% rename(alcfreq32 = W9AUDIT1)

# Merge datasets
merged_data <- full_join(wave1, wave2, by = "NSID")
merged_data <- full_join(merged_data, wave3, by = "NSID")
merged_data <- full_join(merged_data, wave4, by = "NSID")
merged_data <- full_join(merged_data, wave6, by = "NSID")
merged_data <- full_join(merged_data, wave7, by = "NSID")
merged_data <- full_join(merged_data, wave8_sc, by = "NSID")
merged_data <- full_join(merged_data, wave9, by = "NSID")

# Create drinker flags for each age
merged_data <- merged_data %>% 
  mutate(
    drinker14 = (alcever14 == 1 & alcmon14 == 1),
    drinker15 = (alcever15 == 1),
    drinker16 = (alcever16 == 1),
    drinker17 = (alcever17 == 1),
    drinker19 = (alcever19 == 1),
    drinker20 = (alcever20 == 1),
    drinker25 = (alcfreq25 > 1),
    drinker32 = (alcfreq32 > 1)
  )

# Determine first age of drinking
merged_data <- merged_data %>% 
  mutate(
    alcfst = case_when(
      drinker14 ~ 14,
      drinker15 ~ 15,
      drinker16 ~ 16,
      drinker17 ~ 17,
      drinker19 ~ 19,
      drinker20 ~ 20,
      drinker25 ~ 25,
      drinker32 ~ 32,
      TRUE ~ NA_real_
    )
  )

# Determine never drinker status
merged_data <- merged_data %>% 
  mutate(
    never_drinker = case_when(
      (is.na(drinker14) | is.na(drinker15) | is.na(drinker16) | is.na(drinker17) | 
       is.na(drinker19) | is.na(drinker20) | is.na(drinker25) | is.na(drinker32)) & 
      !any(c(drinker14, drinker15, drinker16, drinker17, drinker19, drinker20, drinker25, drinker32), na.rm = TRUE) ~ 1,
      !any(c(drinker14, drinker15, drinker16, drinker17, drinker19, drinker20, drinker25, drinker32), na.rm = TRUE) ~ 1,
      TRUE ~ 0
    )
  )

# Final alcfst assignment
merged_data <- merged_data %>% 
  mutate(
    alcfst = case_when(
      never_drinker == 1 ~ 99,
      is.na(alcfst) ~ -8,
      TRUE ~ alcfst
    )
  )

# Convert alcfst to factor with labels
merged_data <- merged_data %>% mutate(
  alcfst = factor(alcfst, levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                  labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information"))
)

# Output only NSID and alcfst
output_data <- merged_data %>% select(NSID, alcfst)

# Write to CSV
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)