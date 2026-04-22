library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define mapping for standard missing values based on instruction 3
# -9 = Refusal, -8 = Don't know, -1 = N/A, -3 = Not asked/Null, -2 = Schedule error, -7 = Prefer not to say

# Helper function to harmonize parental employment
# Detailed classification: full-time, part-time, unemployed, training, education, home, retired, sick/disabled, other
harmonize_parental_emp <- function(x) {
  # Map based on metadata labels
  # 1: Full-time (30+), 2: Part-time (<30), 3: Unemployed, 4: Training, 5: Education, 6: Home, 7: Retired, 8: Sick, 9: Other
  
  # Standard missing values mapping
  res <- case_when(
    x == 1 ~ 1, # full-time
    x == 2 ~ 2, # part-time
    x == 3 ~ 3, # unemployed
    x == 4 ~ 4, # training
    x == 5 ~ 5, # education
    x == 6 ~ 6, # home
    x == 7 ~ 7, # retired
    x == 8 ~ 8, # sick/disabled
    x == 9 ~ 9, # other
    # Missing codes from metadata
    x == -999 | x == -996 ~ -2, # Schedule not applicable/info lost
    x == -99 | x == -98 ~ -3,   # Not asked/participated (not interviewed/not present)
    x == -94 ~ -8,               # Insufficient info / Don't know
    x == -92 ~ -9,              # Refusal
    is.na(x) ~ -3,              # Null to -3
    TRUE ~ -3
  )
  
  # Apply factor labels
  res <- factor(res, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -9, -8, -1, -3, -2, -7),
                labels = c('full-time', 'part-time', 'unemployed', 'training', 'education', 'home', 'retired', 'sick/disabled', 'other', 
                            'Refusal', 'Don\'t know', 'Item not applicable', 'Not asked', 'Schedule error', 'Prefer not to say'))
  return(res)
}

# 1. File Loading
file1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
file3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
file4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge using full_join by NSID
merged_data <- file1 %>% 
  full_join(file2, by = "NSID") %>% 
  full_join(file3, by = "NSID") %>% 
  full_join(file4, by = "NSID")

# 11. Parental Economic Activity Harmonization
# W1 (Age 14), W2 (Age 15), W3 (Age 16), W4 (Age 17)
# Variable names for mother: W1empsmum, W2empsmum, W3empsmum, w4empsmum
# Variable names for father: W1empsdad, W2empsdad, W3empsdad, w4empsdad

final_data <- merged_data %>% 
  mutate(
    # Mother
    ecoactdtma14 = harmonize_parental_emp(W1empsmum),
    ecoactdtma15 = harmonize_parental_emp(W2empsmum),
    ecoactdtma16 = harmonize_parental_emp(W3empsmum),
    ecoactdtma17 = harmonize_parental_emp(w4empsmum),
    # Father
    ecoactdtpa14 = harmonize_parental_emp(W1empsdad),
    ecoactdtpa15 = harmonize_parental_emp(W2empsdad),
    ecoactdtpa16 = harmonize_parental_emp(W3empsdad),
    ecoactdtpa17 = harmonize_parental_emp(w4empsdad)
  ) %>% 
  select(NSID, ecoactdtma14, ecoactdtma15, ecoactdtma16, ecoactdtma17, ecoactdtpa14, ecoactdtpa15, ecoactdtpa16, ecoactdtpa17)

# 13. Output Requirements
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)
