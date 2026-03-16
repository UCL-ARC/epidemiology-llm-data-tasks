library(haven)
library(dplyr)
library(purrr)
library(readr)

# Create output directory
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Load all files with proper tab delimiter
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

print("Files loaded successfully")

# Merge all waves by NSID
final_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

print(paste("Merged data rows:", nrow(final_data)))
print(paste("Merged data columns:", ncol(final_data)))

# Create harmonized marital status collapsed variables
final_data <- final_data %>%
  mutate(
    # Wave 6 marital status
    marstat_wave6 = case_when(
      W6MarStatYP == 1 ~ "Single",
      W6MarStatYP == 2 ~ "Married",
      W6MarStatYP == 3 ~ "Separated",
      W6MarStatYP == 4 ~ "Divorced",
      W6MarStatYP == 5 ~ "Widowed",
      TRUE ~ NA_character_
    ),
    # Wave 8 marital status
    marstat_wave8 = case_when(
      W8DMARSTAT == 1 ~ "Single",
      W8DMARSTAT == 2 ~ "Married",
      W8DMARSTAT == 3 ~ "Separated",
      W8DMARSTAT == 4 ~ "Divorced",
      W8DMARSTAT == 5 ~ "Widowed",
      W8DMARSTAT == 6 ~ "Civil Partner",
      W8DMARSTAT == 7 ~ "Separated CP",
      W8DMARSTAT == 8 ~ "Former CP",
      W8DMARSTAT == 9 ~ "Surviving CP",
      TRUE ~ NA_character_
    ),
    # Wave 9 marital status
    marstat_wave9 = case_when(
      W9DMARSTAT == 1 ~ "Single",
      W9DMARSTAT == 2 ~ "Married",
      W9DMARSTAT == 3 ~ "Divorced",
      W9DMARSTAT == 4 ~ "Separated",
      W9DMARSTAT == 5 ~ "Widowed",
      W9DMARSTAT == 6 ~ "Civil Partner",
      W9DMARSTAT == 7 ~ "Former CP",
      W9DMARSTAT == 8 ~ "Surviving CP",
      TRUE ~ NA_character_
    )
  )

# Convert to factors WITHOUT NA in levels (NA is the default for missing)
final_data$marstat_wave6 <- factor(final_data$marstat_wave6, 
  levels = c("Single", "Married", "Separated", "Divorced", "Widowed"))

final_data$marstat_wave8 <- factor(final_data$marstat_wave8,
  levels = c("Single", "Married", "Separated", "Divorced", "Widowed", "Civil Partner", "Separated CP", "Former CP", "Surviving CP"))

final_data$marstat_wave9 <- factor(final_data$marstat_wave9,
  levels = c("Single", "Married", "Separated", "Divorced", "Widowed", "Civil Partner", "Former CP", "Surviving CP"))

# Save to CSV
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)

print("Data cleaning complete!")
print(paste("Output file: data/output/cleaned_data.csv"))
print(paste("Number of rows:", nrow(final_data)))
print(paste("Number of columns:", ncol(final_data)))
print("Column names:")
print(names(final_data))