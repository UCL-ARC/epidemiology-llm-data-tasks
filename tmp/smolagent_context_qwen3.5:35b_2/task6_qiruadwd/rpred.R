library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Function to convert wave-specific missing codes to standard codes
convert_missing_codes <- function(x, wave_specific_codes, standard_code) {
  # Replace wave-specific codes with standard code
  x[x %in% wave_specific_codes] <- standard_code
  
  # Convert any remaining NA to -3 (Not asked)
  x[is.na(x)] <- -3
  
  return(x)
}

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", 
                        delim = "\t", show_col_types = FALSE)
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", 
                       delim = "\t", show_col_types = FALSE)
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", 
                         delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_derived.tab", 
                  delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", 
                          delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", 
                       delim = "\t", show_col_types = FALSE)

# Rename columns before merging to avoid conflicts
wave_two <- wave_two %>%
  rename(urbind15 = urbind, gor15 = gor)

wave_three <- wave_three %>%
  rename(urbind16 = urbind, gor16 = gor)

# Convert missing codes in wave_two
wave_two$urbind15 <- convert_missing_codes(wave_two$urbind15, c(-999, -94), -8)
wave_two$gor15 <- convert_missing_codes(wave_two$gor15, c(-999, -94), -8)

# Convert missing codes in wave_three
wave_three$urbind16 <- convert_missing_codes(wave_three$urbind16, c(-999, -94), -8)
wave_three$gor16 <- convert_missing_codes(wave_three$gor16, c(-999, -94), -8)

# Convert missing codes in ns8 W8DGOR
ns8$W8DGOR <- convert_missing_codes(ns8$W8DGOR, c(-9), -9)
ns8$W8DGOR <- convert_missing_codes(ns8$W8DGOR, c(-8), -8)
ns8$W8DGOR <- convert_missing_codes(ns8$W8DGOR, c(-1), -1)

# Convert missing codes in ns9_derived W9DRGN
ns9_derived$W9DRGN <- convert_missing_codes(ns9_derived$W9DRGN, c(-9), -9)
ns9_derived$W9DRGN <- convert_missing_codes(ns9_derived$W9DRGN, c(-8), -8)
ns9_derived$W9DRGN <- convert_missing_codes(ns9_derived$W9DRGN, c(-1), -1)

# Convert missing codes in ns9_main W9NATIONRES
ns9_main$W9NATIONRES <- convert_missing_codes(ns9_main$W9NATIONRES, c(-9), -9)
ns9_main$W9NATIONRES <- convert_missing_codes(ns9_main$W9NATIONRES, c(-8), -8)
ns9_main$W9NATIONRES <- convert_missing_codes(ns9_main$W9NATIONRES, c(-3), -3)
ns9_main$W9NATIONRES <- convert_missing_codes(ns9_main$W9NATIONRES, c(-1), -1)

# Merge all datasets using full_join by NSID
cleaned_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# Create age-specific variables with standardized naming
# Urban/Rural at age 15 (wave_two) - already renamed as urbind15
cleaned_data$urb15 <- cleaned_data$urbind15

# Urban/Rural at age 16 (wave_three) - already renamed as urbind16
cleaned_data$urb16 <- cleaned_data$urbind16

# Government Office Region at age 15 (wave_two) - already renamed as gor15
cleaned_data$gor15 <- cleaned_data$gor15

# Government Office Region at age 16 (wave_three) - already renamed as gor16
cleaned_data$gor16 <- cleaned_data$gor16

# Government Office Region at age 32 (wave 9)
cleaned_data$gor32 <- cleaned_data$W9DRGN

# Nation of UK at age 32
cleaned_data$nation32 <- cleaned_data$W9NATIONRES

# Create collapsed harmonized urban/rural variable (UK/abroad binary)
cleaned_data$urb_binary <- case_when(
  cleaned_data$urb15 %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ cleaned_data$urb15,
  cleaned_data$urb15 %in% c(-9, -8, -1, -3, -2, -7) ~ cleaned_data$urb15,
  TRUE ~ -3
)

# Create collapsed harmonized GOR variable (UK regions)
cleaned_data$gor_binary <- case_when(
  cleaned_data$gor15 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ cleaned_data$gor15,
  cleaned_data$gor15 %in% c(10, 11, 12) ~ cleaned_data$gor15,
  cleaned_data$gor15 %in% c(-9, -8, -1, -3, -2, -7) ~ cleaned_data$gor15,
  TRUE ~ -3
)

# Select only the variables we need for output
output_vars <- c("NSID", "urb15", "urb16", "gor15", "gor16", "gor32", "nation32", "urb_binary", "gor_binary")

# Create final output with only required variables
final_output <- cleaned_data %>%
  select(all_of(output_vars))

# Write to CSV
write_csv(final_output, "data/output/cleaned_data.csv")

cat("Data cleaning complete. Output saved to data/output/cleaned_data.csv\n")
