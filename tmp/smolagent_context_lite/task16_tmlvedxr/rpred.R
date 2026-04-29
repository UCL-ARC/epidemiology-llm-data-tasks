library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file_path_w1 <- 'data/input/wave_one_lsype_family_background_2020.tab'
file_path_w2 <- 'data/input/wave_two_lsype_family_background_2020.tab'
file_path_w3 <- 'data/input/wave_three_lsype_family_background_2020.tab'
file_path_w4 <- 'data/input/wave_four_lsype_family_background_2020.tab'

data_w1 <- read_delim(file_path_w1, delim = '\t', col_types = cols(.default = 'c'))
data_w2 <- read_delim(file_path_w2, delim = '\t', col_types = cols(.default = 'c'))
data_w3 <- read_delim(file_path_w3, delim = '\t', col_types = cols(.default = 'c'))
data_w4 <- read_delim(file_path_w4, delim = '\t', col_types = cols(.default = 'c'))

# Convert relevant columns to numeric
data_w1 <- data_w1 %>% mutate(W1GrsswkHH = as.numeric(W1GrsswkHH))
data_w2 <- data_w2 %>% mutate(W2GrsswkHH = as.numeric(W2GrsswkHH))
data_w3 <- data_w3 %>% mutate(W3incestw = as.numeric(W3incestw))
data_w4 <- data_w4 %>% mutate(w4IncEstW = as.numeric(w4IncEstW))

# Merge datasets
full_df <- data_w1 %>%
  full_join(data_w2, by = 'NSID') %>%
  full_join(data_w3, by = 'NSID') %>%
  full_join(data_w4, by = 'NSID')

# Missing value mapping helper
map_missing <- function(val, mapping) {
  res <- as.numeric(mapping[as.character(val)])
  res[is.na(res)] <- ifelse(is.na(val), -3, val)
  return(res)
}

# Standard mapping
missing_map <- c(
  "-999" = "-2",
  "-992" = "-2",
  "-99" = "-3",
  "-94" = "-8",
  "-92" = "-9",
  "-91" = "-1",
  "-3" = "-1",
  "-1" = "-8"
)

# Wave 4 map
missing_map_w4 <- c(missing_map, "-996" = "-1")

# Continuous variables
full_df <- full_df %>%
  mutate(incwhhcnt14 = map_missing(W1GrsswkHH, missing_map),
         incwhhcnt15 = map_missing(W2GrsswkHH, missing_map))

# Banded variables (16, 17)
full_df <- full_df %>%
  mutate(incwhh16 = case_when(
    W3incestw >= 1 & W3incestw <= 12 ~ W3incestw,
    TRUE ~ map_missing(W3incestw, missing_map)
  ), incwhh17 = case_when(
    w4IncEstW >= 1 & w4IncEstW <= 12 ~ w4IncEstW,
    TRUE ~ map_missing(w4IncEstW, missing_map_w4)
  ))

# Banded variables (14, 15) - source is continuous, so we create them as NA as no banding rules provided
full_df <- full_df %>%
  mutate(incwhh14 = NA_real_,
         incwhh15 = NA_real_)

# Final selection
final_df <- full_df %>%
  select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

# Define labels for banded factors
band_labels <- c(
  "1" = "Up to £49", "2" = "£50 up to £99", "3" = "£100 up to £199",
  "4" = "£200 up to £299", "5" = "£300 up to £399", "6" = "£400 up to £499",
  "7" = "£500 up to £599", "8" = "£600 up to £699", "9" = "£700 up to £799",
  "10" = "£800 up to £899", "11" = "£900 up to £999", "12" = "£1,000 or more",
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say",
  "-3" = "Not asked", "-2" = "Schedule not applicable", "-1" = "Not applicable"
)

# Apply factor labels
for(var in c("incwhh14", "incwhh15", "incwhh16", "incwhh17")) {
  final_df[[var]] <- factor(final_df[[var]], levels = as.numeric(names(band_labels)), labels = band_labels)
}

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
