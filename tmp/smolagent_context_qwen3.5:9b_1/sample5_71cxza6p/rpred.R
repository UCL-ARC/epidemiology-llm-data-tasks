library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Read all files from data/input/
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Add wave identifier
wave_one <- wave_one %>% mutate(wave = "wave_one_age14")
wave_four <- wave_four %>% mutate(wave = "wave_four_age17")
wave_six <- wave_six %>% mutate(wave = "wave_six_age19")
ns8 <- ns8 %>% mutate(wave = "ns8_age25")
ns9 <- ns9 %>% mutate(wave = "ns9_age32")

# Merge all files by NSID
combined <- full_join(wave_one, wave_four, by = "NSID")
combined <- full_join(combined, wave_six, by = "NSID")
combined <- full_join(combined, ns8, by = "NSID")
combined <- full_join(combined, ns9, by = "NSID")

# Create harmonized marital status variable
combined <- combined %>%
  mutate(
    marstat_w6 = case_when(
      wave == "wave_six_age19" & W6MarStatYP %in% c(-999, -97, -92, -91, -1) ~ -3,
      wave == "wave_six_age19" & W6MarStatYP == 1 ~ 1,
      wave == "wave_six_age19" & W6MarStatYP == 2 ~ 2,
      wave == "wave_six_age19" & W6MarStatYP == 3 ~ 3,
      wave == "wave_six_age19" & W6MarStatYP == 4 ~ 4,
      wave == "wave_six_age19" & W6MarStatYP == 5 ~ 5,
      TRUE ~ NA_real_
    ),
    marstat_w8 = case_when(
      wave == "ns8_age25" & W8DMARSTAT %in% c(-9, -1) ~ -1,
      wave == "ns8_age25" & W8DMARSTAT == 1 ~ 1,
      wave == "ns8_age25" & W8DMARSTAT == 2 ~ 2,
      wave == "ns8_age25" & W8DMARSTAT == 3 ~ 3,
      wave == "ns8_age25" & W8DMARSTAT == 4 ~ 4,
      wave == "ns8_age25" & W8DMARSTAT == 5 ~ 5,
      wave == "ns8_age25" & W8DMARSTAT %in% c(6, 7, 8, 9) ~ -3,
      TRUE ~ NA_real_
    ),
    marstat_w9 = case_when(
      wave == "ns9_age32" & W9DMARSTAT %in% c(-9, -8) ~ -8,
      wave == "ns9_age32" & W9DMARSTAT == 1 ~ 1,
      wave == "ns9_age32" & W9DMARSTAT == 2 ~ 2,
      wave == "ns9_age32" & W9DMARSTAT == 3 ~ 4,
      wave == "ns9_age32" & W9DMARSTAT == 4 ~ 3,
      wave == "ns9_age32" & W9DMARSTAT == 5 ~ 5,
      wave == "ns9_age32" & W9DMARSTAT %in% c(6, 7, 8) ~ -3,
      TRUE ~ NA_real_
    )
  )

# Output cleaned data to CSV
write_csv(combined, "data/output/cleaned_data.csv")

cat("Data cleaning complete.\n")