library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
merged_data <- wave1 %>% 
  full_join(wave2, by = "NSID") %>% 
  full_join(wave3, by = "NSID") %>% 
  full_join(wave4, by = "NSID") %>% 
  full_join(wave5, by = "NSID")

# Rename variables
merged_data <- merged_data %>% 
  rename(
    nssecma14 = W1nsseccatmum,
    nssecpa14 = W1nsseccatdad,
    nssecma15 = W2nsseccatmum,
    nssecpa15 = W2nsseccatdad,
    nssecma16 = W3cnsseccatmum,
    nssecpa16 = W3cnsseccatdad,
    nssecma17 = w4cnsseccatmum,
    nssecpa17 = w4cnsseccatdad,
    nssecma18 = w5Cnsseccatmum,
    nssecpa18 = w5Cnsseccatdad
  )

# Harmonize NS-SEC classification
nssec_vars <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")

for (var in nssec_vars) {
  merged_data[[var]] <- ifelse(merged_data[[var]] %% 1 != 0, NA, merged_data[[var]])
  merged_data[[var]] <- ifelse(!is.na(merged_data[[var]]) & (merged_data[[var]] < 1 | merged_data[[var]] > 17), NA, merged_data[[var]])
}

# Harmonize missing value codes
merged_data <- merged_data %>% 
  mutate(
    across(all_of(nssec_vars), ~ case_when(
      .x == -999 ~ -2,
      .x == -94 ~ -8,
      .x == -99 | .x == -98 | is.na(.x) ~ -3,
      TRUE ~ .x
    ))
  )

# Select and output required variables
output_data <- merged_data %>% 
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)