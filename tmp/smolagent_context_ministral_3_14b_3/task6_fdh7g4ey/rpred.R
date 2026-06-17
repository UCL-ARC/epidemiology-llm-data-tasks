
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Load datasets
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave_nine_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")
wave_nine_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Function to map missing values
map_missing <- function(x) {
  if (is.numeric(x)) {
    x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
    x[x == -94] <- -8
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -99] <- -3
    x[x == -100 | x == -97] <- -2
    x[x == 13] <- -2
  }
  return(x)
}

# Create derived variables in separate data frames
wave_two <- wave_two %>%
  mutate(
    regub15 = map_missing(urbind),
    regov15 = map_missing(gor)
  ) %>%
  mutate(
    regub15 = if_else(is.na(regub15), -3, regub15),
    regov15 = if_else(is.na(regov15), -3, regov15)
  )

wave_three <- wave_three %>%
  mutate(
    regub16 = map_missing(urbind),
    regov16 = map_missing(gor)
  ) %>%
  mutate(
    regub16 = if_else(is.na(regub16), -3, regub16),
    regov16 = if_else(is.na(regov16), -3, regov16)
  )

wave_eight <- wave_eight %>%
  mutate(
    regor25 = map_missing(W8DGOR)
  ) %>%
  mutate(
    regor25 = if_else(is.na(regor25), -3, regor25)
  )

wave_nine_derived <- wave_nine_derived %>%
  mutate(
    regor32 = map_missing(W9DRGN)
  ) %>%
  mutate(
    regor32 = if_else(is.na(regor32), -3, regor32)
  )

wave_nine_main <- wave_nine_main %>%
  mutate(
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,
      W9NATIONRES == 5 ~ 2,
      TRUE ~ NA_integer_
    )
  ) %>%
  mutate(
    regint32 = if_else(is.na(regint32), -3, regint32)
  )

# Merge all datasets
merged_data <- wave_two %>%
  select(NSID, regub15, regov15) %>%
  left_join(wave_three %>%
              select(NSID, regub16, regov16),
            by = "NSID") %>%
  left_join(wave_eight %>%
              select(NSID, regor25),
            by = "NSID") %>%
  left_join(wave_nine_derived %>%
              select(NSID, regor32),
            by = "NSID") %>%
  left_join(wave_nine_main %>%
              select(NSID, regint32),
            by = "NSID")

# Select only the required variables
final_data <- merged_data %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write the final data to CSV
write_csv(final_data, "data/output/cleaned_data.csv")
