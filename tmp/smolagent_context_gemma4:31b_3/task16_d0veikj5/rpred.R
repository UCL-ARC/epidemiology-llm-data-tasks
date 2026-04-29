library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked/interviewed
# -2 = Script error/lost

# Helper to map missing values for continuous income (Ages 14, 15)
map_missing_cont <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  
  res <- x
  res[x == -999.0] <- -2
  res[x == -992.0] <- -9
  res[x == -99.0]  <- -3
  res[x == -94.0]  <- -8
  res[x == -92.0]  <- -9
  res[x == -91.0]  <- -1
  res[x == -3.0]    <- -1
  res[x == -1.0]    <- -8
  return(res)
}

# Helper to map missing values for banded income (Ages 16, 17)
map_missing_band <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  
  res <- x
  res[x == -99.0]  <- -3
  res[x == -92.0]  <- -9
  res[x == -1.0]   <- -8
  res[x == -996.0] <- -3
  return(res)
}

# Load datasets
df14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
df15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
df16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
df17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))

# Merge datasets
merged_df <- df14 %>%
  full_join(df15, by = "NSID") %>%
  full_join(df16, by = "NSID") %>%
  full_join(df17, by = "NSID")

# Process Age 14
merged_df <- merged_df %>%
  mutate(
    incwhhcnt14 = map_missing_cont(W1GrsswkHH),
    incwhh14_num = case_when(
      incwhhcnt14 >= 0 & incwhhcnt14 < 50 ~ 1,
      incwhhcnt14 >= 50 & incwhhcnt14 < 100 ~ 2,
      incwhhcnt14 >= 100 & incwhhcnt14 < 200 ~ 3,
      incwhhcnt14 >= 200 & incwhhcnt14 < 300 ~ 4,
      incwhhcnt14 >= 300 ~ 5,
      TRUE ~ incwhhcnt14
    )
  )

# Process Age 15
merged_df <- merged_df %>%
  mutate(
    incwhhcnt15 = map_missing_cont(W2GrsswkHH),
    incwhh15_num = case_when(
      incwhhcnt15 >= 0 & incwhhcnt15 < 50 ~ 1,
      incwhhcnt15 >= 50 & incwhhcnt15 < 100 ~ 2,
      incwhhcnt15 >= 100 & incwhhcnt15 < 200 ~ 3,
      incwhhcnt15 >= 200 & incwhhcnt15 < 300 ~ 4,
      incwhhcnt15 >= 300 ~ 5,
      TRUE ~ incwhhcnt15
    )
  )

# Process Age 16
merged_df <- merged_df %>%
  mutate(incwhh16_num = map_missing_band(W3incestw))

# Process Age 17
merged_df <- merged_df %>%
  mutate(incwhh17_num = map_missing_band(w4IncEstW))

# Define labels for factors
band_labels <- c(
  "1" = "Band 1", "2" = "Band 2", "3" = "Band 3", "4" = "Band 4", "5" = "Band 5",
  "6" = "Band 6", "7" = "Band 7", "8" = "Band 8", "9" = "Band 9", "10" = "Band 10",
  "11" = "Band 11", "12" = "Band 12",
  "-9" = "Refusal", "-8" = "Don't know", "-3" = "Not asked", "-2" = "Script error", "-1" = "Not applicable"
)

# Convert to factors and clean final selection
# Using labelled::set_variable_labels for the numeric columns to avoid set_attr error
final_df <- merged_df %>%
  mutate(
    incwhh14 = factor(incwhh14_num, levels = names(band_labels), labels = band_labels),
    incwhh15 = factor(incwhh15_num, levels = names(band_labels), labels = band_labels),
    incwhh16 = factor(incwhh16_num, levels = names(band_labels), labels = band_labels),
    incwhh17 = factor(incwhh17_num, levels = names(band_labels), labels = band_labels)
  ) %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Apply labels to continuous variables
var_label(final_df$incwhhcnt14) <- "Continuous Weekly HH Income Age 14"
var_label(final_df$incwhhcnt15) <- "Continuous Weekly HH Income Age 15"

write_csv(final_df, "data/output/cleaned_data.csv")