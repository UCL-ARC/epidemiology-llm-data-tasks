library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files
cat("Loading wave 1 (Age 14)...\n")
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 2 (Age 15)...\n")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 3 (Age 16)...\n")
wave3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 4 (Age 17)...\n")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 5 (Age 18)...\n")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 6 (Age 19)...\n")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 7 (Age 20)...\n")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 8 (Age 25)...\n")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
cat("Loading wave 9 (Age 32)...\n")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

cat("Merging all waves...\n")
# Merge all waves using full_join by NSID
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

cat("Harmonizing sex variables...\n")
# Create a function to harmonize sex variables across waves
harmonize_sex_var <- function(var, wave_name) {
  case_when(
    # Wave 1 (Age 14) - W1sexYP
    wave_name == "wave1" ~ case_when(
      var %in% c(-99, -99.0) ~ -3,  # YP not interviewed
      var %in% c(-92, -92.0) ~ -9,  # Refused
      var %in% c(-91, -91.0) ~ -1,  # Not applicable
      var %in% c(1, 1.0) ~ 1,       # Male
      var %in% c(2, 2.0) ~ 2,       # Female
      TRUE ~ NA_real_
    ),
    # Wave 2 (Age 15) - W2SexYP
    wave_name == "wave2" ~ case_when(
      var %in% c(-998, -998.0) ~ -2, # Interviewer missed question
      var %in% c(-997, -997.0) ~ -2, # Script error
      var %in% c(-995, -995.0) ~ -2, # Missing history section data
      var %in% c(-99, -99.0) ~ -3,   # YP not interviewed
      var %in% c(-92, -92.0) ~ -9,   # Refused
      var %in% c(-91, -91.0) ~ -1,   # Not applicable
      var %in% c(-1, -1.0) ~ -8,     # Don't know
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 3 (Age 16) - W3sexYP
    wave_name == "wave3" ~ case_when(
      var %in% c(-99, -99.0) ~ -3,   # YP not interviewed
      var %in% c(-92, -92.0) ~ -9,   # Refused
      var %in% c(-91, -91.0) ~ -1,   # Not applicable
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 4 (Age 17) - W4SexYP
    wave_name == "wave4" ~ case_when(
      var %in% c(-99, -99.0) ~ -3,   # YP not interviewed
      var %in% c(-92, -92.0) ~ -9,   # Refused
      var %in% c(-91, -91.0) ~ -1,   # Not applicable
      var %in% c(-1, -1.0) ~ -8,     # Don't know
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 5 (Age 18) - W5SexYP
    wave_name == "wave5" ~ case_when(
      var %in% c(-1, -1.0) ~ -8,     # Don't know
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 6 (Age 19) - W6Sex
    wave_name == "wave6" ~ case_when(
      var %in% c(-92, -92.0) ~ -9,   # Refused
      var %in% c(-91, -91.0) ~ -1,   # Not applicable
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 7 (Age 20) - W7Sex
    wave_name == "wave7" ~ case_when(
      var %in% c(-91, -91.0) ~ -1,   # Not applicable
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 8 (Age 25) - W8CMSEX
    wave_name == "wave8" ~ case_when(
      var %in% c(-999, -999.0) ~ -2, # General error
      var %in% c(-998, -998.0) ~ -2, # General error
      var %in% c(-997, -997.0) ~ -2, # General error
      var %in% c(-995, -995.0) ~ -2, # General error
      var %in% c(-100, -100.0) ~ -2, # General error
      var %in% c(-9, -9.0) ~ -9,     # Refused
      var %in% c(-8, -8.0) ~ -8,     # Don't know
      var %in% c(-1, -1.0) ~ -1,     # Not applicable
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    # Wave 9 (Age 32) - W9DSEX
    wave_name == "wave9" ~ case_when(
      var %in% c(-999, -999.0) ~ -2, # General error
      var %in% c(1, 1.0) ~ 1,        # Male
      var %in% c(2, 2.0) ~ 2,        # Female
      TRUE ~ NA_real_
    ),
    TRUE ~ NA_real_
  )
}

# Harmonize all sex variables
merged$W1sexYP_h <- harmonize_sex_var(merged$W1sexYP, "wave1")
merged$W2SexYP_h <- harmonize_sex_var(merged$W2SexYP, "wave2")
merged$W3sexYP_h <- harmonize_sex_var(merged$W3sexYP, "wave3")
merged$W4SexYP_h <- harmonize_sex_var(merged$W4SexYP, "wave4")
merged$W5SexYP_h <- harmonize_sex_var(merged$W5SexYP, "wave5")
merged$W6Sex_h <- harmonize_sex_var(merged$W6Sex, "wave6")
merged$W7Sex_h <- harmonize_sex_var(merged$W7Sex, "wave7")
merged$W8CMSEX_h <- harmonize_sex_var(merged$W8CMSEX, "wave8")
merged$W9DSEX_h <- harmonize_sex_var(merged$W9DSEX, "wave9")

cat("Creating consolidated sex variable (most-recent-valid-first)...\n")
# Create consolidated sex variable using most-recent-valid-first
merged$sex <- case_when(
  merged$W9DSEX_h %in% c(1, 2) ~ merged$W9DSEX_h,
  merged$W8CMSEX_h %in% c(1, 2) ~ merged$W8CMSEX_h,
  merged$W7Sex_h %in% c(1, 2) ~ merged$W7Sex_h,
  merged$W6Sex_h %in% c(1, 2) ~ merged$W6Sex_h,
  merged$W5SexYP_h %in% c(1, 2) ~ merged$W5SexYP_h,
  merged$W4SexYP_h %in% c(1, 2) ~ merged$W4SexYP_h,
  merged$W3sexYP_h %in% c(1, 2) ~ merged$W3sexYP_h,
  merged$W2SexYP_h %in% c(1, 2) ~ merged$W2SexYP_h,
  merged$W1sexYP_h %in% c(1, 2) ~ merged$W1sexYP_h,
  TRUE ~ NA_real_
)

cat("Mapping missing value codes...\n")
# Map consolidated missing codes to standard values
merged$sex <- case_when(
  merged$sex == -9 ~ -9,      # Refused
  merged$sex == -8 ~ -8,      # Don't know
  merged$sex == -7 ~ -7,      # Prefer not to say
  merged$sex == -3 ~ -3,      # Not asked
  merged$sex == -2 ~ -2,      # Not applicable
  merged$sex == -1 ~ -1,      # Item not applicable
  merged$sex %in% c(1, 2) ~ merged$sex,
  TRUE ~ NA_real_
)

cat("Converting to labelled factor...\n")
# Convert sex to labelled factor
merged$sex <- factor(merged$sex, 
                      levels = c(1, 2, -9, -8, -7, -3, -2, -1),
                      labels = c("Male", "Female", "Refused", 
                                 "Don't know", "Prefer not to say", 
                                 "Not asked", "Not applicable", 
                                 "Item not applicable"))

cat("Selecting final variables...\n")
# Keep only final variables
final <- merged %>% select(NSID, sex)

cat("Writing output...\n")
# Write output
write_csv(final, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")
cat("Final dataset dimensions:", nrow(final), "rows,", ncol(final), "columns\n")
cat("Summary of sex variable:\n")
print(table(final$sex, useNA = "ifany"))
