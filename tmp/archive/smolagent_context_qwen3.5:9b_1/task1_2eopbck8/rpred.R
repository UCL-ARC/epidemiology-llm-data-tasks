library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory
setwd("data")

# Define input files
files <- list(
  wave_one = "input/wave_one_lsype_young_person_2020.tab",
  wave_two = "input/wave_two_lsype_young_person_2020.tab",
  wave_three = "input/wave_three_lsype_young_person_2020.tab",
  wave_four = "input/wave_four_lsype_young_person_2020.tab",
  wave_five = "input/wave_five_lsype_young_person_2020.tab",
  wave_six = "input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "input/wave_seven_lsype_young_person_2020.tab",
  ns8 = "input/ns8_2015_main_interview.tab",
  ns9 = "input/ns9_2022_main_interview.tab"
)

# Load all files
wave_one <- read_delim(files$wave_one, delim = "\t")
wave_two <- read_delim(files$wave_two, delim = "\t")
wave_three <- read_delim(files$wave_three, delim = "\t")
wave_four <- read_delim(files$wave_four, delim = "\t")
wave_five <- read_delim(files$wave_five, delim = "\t")
wave_six <- read_delim(files$wave_six, delim = "\t")
wave_seven <- read_delim(files$wave_seven, delim = "\t")
ns8 <- read_delim(files$ns8, delim = "\t")
ns9 <- read_delim(files$ns9, delim = "\t")

# Standard sex values
male_val <- 1
female_val <- 2

# Standard missing codes
refusal <- -9
don_know <- -8
not_applicable <- -1
not_asked <- -3

# Start with wave_one data
result <- wave_one

# Add wave_one sex variable
result <- result %>%
  mutate(
    sex14 = case_when(
      W1sexYP == -99 ~ refusal,
      W1sexYP == -92 ~ refusal,
      W1sexYP == -91 ~ not_applicable,
      W1sexYP == -999 ~ not_asked,
      W1sexYP == male_val ~ male_val,
      W1sexYP == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex14_label = case_when(
      W1sexYP == -99 ~ "Refused",
      W1sexYP == -92 ~ "Refused",
      W1sexYP == -91 ~ "Not applicable",
      W1sexYP == -999 ~ "Not asked",
      W1sexYP == male_val ~ "Male",
      W1sexYP == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_two data
result <- full_join(result, wave_two, by = "NSID")

result <- result %>%
  mutate(
    sex15 = case_when(
      W2SexYP %in% c(-998, -997, -995, -99, -92) ~ refusal,
      W2SexYP == -1 ~ don_know,
      W2SexYP %in% c(-999) ~ not_asked,
      W2SexYP == male_val ~ male_val,
      W2SexYP == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex15_label = case_when(
      W2SexYP %in% c(-998, -997, -995, -99, -92) ~ "Refused",
      W2SexYP == -1 ~ "Don't know",
      W2SexYP %in% c(-999) ~ "Not asked",
      W2SexYP == male_val ~ "Male",
      W2SexYP == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_three data
result <- full_join(result, wave_three, by = "NSID")

result <- result %>%
  mutate(
    sex16 = case_when(
      W3sexYP %in% c(-99, -92) ~ refusal,
      W3sexYP == -91 ~ not_applicable,
      W3sexYP %in% c(-999) ~ not_asked,
      W3sexYP == male_val ~ male_val,
      W3sexYP == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex16_label = case_when(
      W3sexYP %in% c(-99, -92) ~ "Refused",
      W3sexYP == -91 ~ "Not applicable",
      W3sexYP %in% c(-999) ~ "Not asked",
      W3sexYP == male_val ~ "Male",
      W3sexYP == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_four data
result <- full_join(result, wave_four, by = "NSID")

result <- result %>%
  mutate(
    sex17 = case_when(
      W4SexYP %in% c(-99, -92) ~ refusal,
      W4SexYP == -91 ~ not_applicable,
      W4SexYP == -1 ~ don_know,
      W4SexYP %in% c(-999) ~ not_asked,
      W4SexYP == male_val ~ male_val,
      W4SexYP == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex17_label = case_when(
      W4SexYP %in% c(-99, -92) ~ "Refused",
      W4SexYP == -91 ~ "Not applicable",
      W4SexYP == -1 ~ "Don't know",
      W4SexYP %in% c(-999) ~ "Not asked",
      W4SexYP == male_val ~ "Male",
      W4SexYP == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_five data
result <- full_join(result, wave_five, by = "NSID")

result <- result %>%
  mutate(
    sex18 = case_when(
      W5SexYP == -1 ~ don_know,
      W5SexYP == male_val ~ male_val,
      W5SexYP == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex18_label = case_when(
      W5SexYP == -1 ~ "Don't know",
      W5SexYP == male_val ~ "Male",
      W5SexYP == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_six data
result <- full_join(result, wave_six, by = "NSID")

result <- result %>%
  mutate(
    sex19 = case_when(
      W6Sex %in% c(-92) ~ refusal,
      W6Sex %in% c(-91) ~ not_applicable,
      W6Sex %in% c(-999) ~ not_asked,
      W6Sex == male_val ~ male_val,
      W6Sex == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex19_label = case_when(
      W6Sex %in% c(-92) ~ "Refused",
      W6Sex %in% c(-91) ~ "Not applicable",
      W6Sex %in% c(-999) ~ "Not asked",
      W6Sex == male_val ~ "Male",
      W6Sex == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add wave_seven data
result <- full_join(result, wave_seven, by = "NSID")

result <- result %>%
  mutate(
    sex20 = case_when(
      W7Sex == -91 ~ not_applicable,
      W7Sex %in% c(-999) ~ not_asked,
      W7Sex == male_val ~ male_val,
      W7Sex == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex20_label = case_when(
      W7Sex == -91 ~ "Not applicable",
      W7Sex %in% c(-999) ~ "Not asked",
      W7Sex == male_val ~ "Male",
      W7Sex == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add ns8_2015 data
result <- full_join(result, ns8, by = "NSID")

result <- result %>%
  mutate(
    sex21 = case_when(
      W8CMSEX == -9 ~ refusal,
      W8CMSEX == -8 ~ don_know,
      W8CMSEX == -1 ~ not_applicable,
      W8CMSEX %in% c(-999) ~ not_asked,
      W8CMSEX == male_val ~ male_val,
      W8CMSEX == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex21_label = case_when(
      W8CMSEX == -9 ~ "Refused",
      W8CMSEX == -8 ~ "Don't know",
      W8CMSEX == -1 ~ "Not applicable",
      W8CMSEX %in% c(-999) ~ "Not asked",
      W8CMSEX == male_val ~ "Male",
      W8CMSEX == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Add ns9_2022 data
result <- full_join(result, ns9, by = "NSID")

result <- result %>%
  mutate(
    sex32 = case_when(
      W9DSEX %in% c(-999) ~ not_asked,
      W9DSEX == male_val ~ male_val,
      W9DSEX == female_val ~ female_val,
      TRUE ~ NA_real_
    ),
    sex32_label = case_when(
      W9DSEX %in% c(-999) ~ "Not asked",
      W9DSEX == male_val ~ "Male",
      W9DSEX == female_val ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Create consolidated sex variable (time-invariant)
# Prioritize most recent valid response
result <- result %>%
  mutate(
    sex_consolidated = case_when(
      !is.na(sex32) & sex32 %in% c(male_val, female_val) ~ sex32,
      !is.na(sex21) & sex21 %in% c(male_val, female_val) ~ sex21,
      !is.na(sex20) & sex20 %in% c(male_val, female_val) ~ sex20,
      !is.na(sex19) & sex19 %in% c(male_val, female_val) ~ sex19,
      !is.na(sex18) & sex18 %in% c(male_val, female_val) ~ sex18,
      !is.na(sex17) & sex17 %in% c(male_val, female_val) ~ sex17,
      !is.na(sex16) & sex16 %in% c(male_val, female_val) ~ sex16,
      !is.na(sex15) & sex15 %in% c(male_val, female_val) ~ sex15,
      !is.na(sex14) & sex14 %in% c(male_val, female_val) ~ sex14,
      TRUE ~ NA_real_
    )
  )

# Define the levels and labels explicitly
consolidated_levels <- c(male_val, female_val, refusal, don_know, not_applicable, not_asked)
consolidated_labels <- c("Male", "Female", "Refused", "Don't know", "Not applicable", "Not asked")

# Convert to factor with explicit levels and labels
result <- result %>%
  mutate(
    sex_consolidated = factor(sex_consolidated, 
                              levels = consolidated_levels,
                              labels = consolidated_labels)
  )

# Write output to data/output/cleaned_data.csv
if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}
write_csv(result, "output/cleaned_data.csv")

print("Script completed successfully")
print(paste("Total rows:", nrow(result)))
print(paste("Total columns:", ncol(result)))
print(paste("Missing sex_consolidated:", sum(is.na(sex_consolidated))))
print(paste("Levels in sex_consolidated:", paste(unique(sex_consolidated), collapse = ", ")))