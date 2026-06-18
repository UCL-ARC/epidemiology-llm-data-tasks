library(readr)
library(dplyr)

# Read IMD data for each wave
# Wave 15 (age 15) – Family Background
wave15 <- read_delim(
  "data/input/wave_two_lsype_family_background_2020.tab",
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    IMDRSCORE = col_double()
  )
)

# Wave 16 (age 16) – Family Background
wave16 <- read_delim(
  "data/input/wave_three_lsype_family_background_2020.tab",
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    IMDRSCORE = col_double()
  )
)

# Wave 32 (age 32) – Derived Variables
wave32 <- read_delim(
  "data/input/ns9_2022_derived_variables.tab",
  delim = "\t",
  col_types = cols(
    NSID = col_character(),
    W9DIMDD = col_double()
  )
)

# Merge by NSID
merged <- wave15 %>%
  rename(imd15 = IMDRSCORE) %>%
  full_join(wave16 %>% rename(imd16 = IMDRSCORE), by = "NSID") %>%
  full_join(wave32 %>% rename(imd32 = W9DIMDD), by = "NSID")

# Harmonise missing values according to the specified scheme
cleaned <- merged %>%
  mutate(
    imd15 = case_when(
      is.na(imd15) ~ -3,                    # Not asked/unknown
      imd15 == -94 ~ -8,                    # Insufficient information
      imd15 %in% c(-999, -998, -997, -995) ~ -2, # Schedule not applicable / error
      imd15 == -1 ~ -1,                    # Item not applicable
      imd15 < 0 ~ -2,                      # Other negative codes
      TRUE ~ imd15
    ),
    imd16 = case_when(
      is.na(imd16) ~ -3,
      imd16 == -94 ~ -8,
      imd16 %in% c(-999, -998, -997, -995) ~ -2,
      imd16 == -1 ~ -1,
      imd16 < 0 ~ -2,
      TRUE ~ imd16
    ),
    imd32 = case_when(
      is.na(imd32) ~ -3,
      imd32 == -8 ~ -8,
      TRUE ~ imd32
    )
  ) %>%
  select(NSID, imd15, imd16, imd32)

# Write to CSV
write_csv(cleaned, "data/output/cleaned_data.csv")
