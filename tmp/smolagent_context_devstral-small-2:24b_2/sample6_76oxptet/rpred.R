library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each dataset
wave_one <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_2015_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_2022_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
ns9_2022_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_2015_derived, by = "NSID") %>%
  full_join(ns9_2022_derived, by = "NSID") %>%
  full_join(ns9_2022_main, by = "NSID")

# Check variable names in merged_data
print(names(merged_data))

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-7" = "Prefer not to say"
)

# Harmonize missing value codes
merged_data <- merged_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -3, .)))

# Create derived variables
merged_data <- merged_data %>%
  mutate(
    regub14 = case_when(
      urbind.x == 1 ~ "Urban >= 10k - sparse",
      urbind.x == 2 ~ "Town & Fringe - sparse",
      urbind.x == 3 ~ "Village - sparse",
      urbind.x == 4 ~ "Hamlet and Isolated Dwelling - sparse",
      urbind.x == 5 ~ "Urban >= 10k - less sparse",
      urbind.x == 6 ~ "Town & Fringe - less sparse",
      urbind.x == 7 ~ "Village - less sparse",
      urbind.x == 8 ~ "Hamlet & Isolated Dwelling",
      TRUE ~ as.character(-94)
    ),
    regov14 = case_when(
      gor.x == 1 ~ "North East",
      gor.x == 2 ~ "North West",
      gor.x == 3 ~ "Yorkshire and The Humber",
      gor.x == 4 ~ "East Midlands",
      gor.x == 5 ~ "West Midlands",
      gor.x == 6 ~ "East of England",
      gor.x == 7 ~ "London",
      gor.x == 8 ~ "South East",
      gor.x == 9 ~ "South West",
      TRUE ~ as.character(-94)
    ),
    regov15 = case_when(
      gor.y == 1 ~ "North East",
      gor.y == 2 ~ "North West",
      gor.y == 3 ~ "Yorkshire and the Humber",
      gor.y == 4 ~ "East Midlands",
      gor.y == 5 ~ "West Midlands",
      gor.y == 6 ~ "East of England",
      gor.y == 7 ~ "London",
      gor.y == 8 ~ "South East",
      gor.y == 9 ~ "South West",
      TRUE ~ as.character(-94)
    ),
    regov16 = case_when(
      gor.y == 1 ~ "North East",
      gor.y == 2 ~ "North West",
      gor.y == 3 ~ "Yorkshire and the Humber",
      gor.y == 4 ~ "East Midlands",
      gor.y == 5 ~ "West Midlands",
      gor.y == 6 ~ "East of England",
      gor.y == 7 ~ "London",
      gor.y == 8 ~ "South East",
      gor.y == 9 ~ "South West",
      TRUE ~ as.character(-94)
    ),
    regov17 = case_when(
      gor.y == 1 ~ "North East",
      gor.y == 2 ~ "North West",
      gor.y == 3 ~ "Yorkshire and the Humber",
      gor.y == 4 ~ "East Midlands",
      gor.y == 5 ~ "West Midlands",
      gor.y == 6 ~ "East of England",
      gor.y == 7 ~ "London",
      gor.y == 8 ~ "South East",
      gor.y == 9 ~ "South West",
      TRUE ~ as.character(-94)
    ),
    regov32 = case_when(
      W8DGOR == 1 ~ "North East",
      W8DGOR == 2 ~ "North West",
      W8DGOR == 3 ~ "Yorkshire and the Humber",
      W8DGOR == 4 ~ "East Midlands",
      W8DGOR == 5 ~ "West Midlands",
      W8DGOR == 6 ~ "East of England",
      W8DGOR == 7 ~ "London",
      W8DGOR == 8 ~ "South East",
      W8DGOR == 9 ~ "South West",
      W8DGOR == 10 ~ "Wales",
      W8DGOR == 11 ~ "Scotland",
      W8DGOR == 12 ~ "Northern Ireland",
      W8DGOR == 13 ~ "Unknown due to faulty/missing postcode",
      TRUE ~ as.character(-94)
    ),
    regov32_derived = case_when(
      W9DRGN == 1 ~ "North East",
      W9DRGN == 2 ~ "North West",
      W9DRGN == 3 ~ "Yorkshire and the Humber",
      W9DRGN == 4 ~ "East Midlands",
      W9DRGN == 5 ~ "West Midlands",
      W9DRGN == 6 ~ "East of England",
      W9DRGN == 7 ~ "London",
      W9DRGN == 8 ~ "South East",
      W9DRGN == 9 ~ "South West",
      W9DRGN == 10 ~ "Wales",
      W9DRGN == 11 ~ "Scotland",
      W9DRGN == 12 ~ "Northern Ireland",
      W9DRGN == 13 ~ "Unknown due to faulty/missing postcode",
      TRUE ~ as.character(-94)
    ),
    regint32 = case_when(
      W9NATIONRES == 1 ~ "England",
      W9NATIONRES == 2 ~ "Scotland",
      W9NATIONRES == 3 ~ "Wales",
      W9NATIONRES == 4 ~ "Northern Ireland",
      W9NATIONRES == 5 ~ "Outside of UK or unknown",
      TRUE ~ as.character(-94)
    )
  )

# Convert derived variables to factors with labels
merged_data <- merged_data %>%
  mutate(
    regub14 = factor(regub14, levels = c("-94", "Urban >= 10k - sparse", "Town & Fringe - sparse", "Village - sparse", "Hamlet and Isolated Dwelling - sparse", "Urban >= 10k - less sparse", "Town & Fringe - less sparse", "Village - less sparse", "Hamlet & Isolated Dwelling"), labels = c("Insufficient information", "Urban >= 10k - sparse", "Town & Fringe - sparse", "Village - sparse", "Hamlet and Isolated Dwelling - sparse", "Urban >= 10k - less sparse", "Town & Fringe - less sparse", "Village - less sparse", "Hamlet & Isolated Dwelling")),
    regov14 = factor(regov14, levels = c("-94", "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regov15 = factor(regov15, levels = c("-94", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regov16 = factor(regov16, levels = c("-94", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regov17 = factor(regov17, levels = c("-94", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West")),
    regov32 = factor(regov32, levels = c("-94", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown due to faulty/missing postcode"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown due to faulty/missing postcode")),
    regov32_derived = factor(regov32_derived, levels = c("-94", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown due to faulty/missing postcode"), labels = c("Insufficient information", "North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Unknown due to faulty/missing postcode")),
    regint32 = factor(regint32, levels = c("-94", "England", "Scotland", "Wales", "Northern Ireland", "Outside of UK or unknown"), labels = c("Insufficient information", "England", "Scotland", "Wales", "Northern Ireland", "Outside of UK or unknown"))
  )

# Select only the derived variables and NSID
cleaned_data <- merged_data %>%
  select(NSID, regub14, regov14, regov15, regov16, regov17, regov32, regov32_derived, regint32)

# Write the cleaned data to a CSV file
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)