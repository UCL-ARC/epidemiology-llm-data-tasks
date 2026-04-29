library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

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

# Function to clean NS-SEC variables
clean_nssec <- function(x) {
  # Convert to character then numeric, handling non-numeric values
  x <- as.character(x)
  x <- suppressWarnings(as.numeric(x))
  
  # Handle missing values
  x[is.na(x)] <- -3
  
  # Floor values and handle invalid ranges
  x <- floor(x)
  x[x < 1 | x > 17] <- -3
  
  return(x)
}

# Apply cleaning function to all NS-SEC variables
nssec_vars <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")

for (var in nssec_vars) {
  merged_data[[var]] <- clean_nssec(merged_data[[var]])
}

# Harmonize missing value codes
merged_data <- merged_data %>% 
  mutate(
    across(all_of(nssec_vars), ~ case_when(
      . == -999 ~ -2,
      . == -94 ~ -8,
      . == -99 | . == -98 | is.na(.) ~ -3,
      TRUE ~ .
    ))
  )

# Select and output required variables
output_data <- merged_data %>% 
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")