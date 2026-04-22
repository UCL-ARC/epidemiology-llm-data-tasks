library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Wave 2 (Age 15)
data_w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, W2concenYP, W2nosleepYP, W2usefulYP, W2decideYP, W2strainYP, W2difficYP, W2activYP, W2probsYP, W2depressYP, W2noconfYP, W2wthlessYP, W2happyYP, W2ghq12scr)

# Wave 4 (Age 17)
data_w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, W4ConcenYP, W4NoSleepYP, W4UsefulYP, W4DecideYP, W4StrainYP, W4DifficYP, W4ActivYP, W4ProbsYP, W4DepressYP, W4NoConfYP, W4WthlessYP, W4HappyYP, W4ghq12scr)

# Wave 8 (Age 25)
data_w8_main <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, starts_with("W8GHQ12_"))

data_w8_der <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, W8DGHQSC)

# Wave 9 (Age 32)
data_w9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, starts_with("W9GHQ12_"))

data_w9_der <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c")) %>%
  select(NSID, W9DGHQSC)

# Merge all
full_data <- data_w2 %>%
  full_join(data_w4, by = "NSID") %>%
  full_join(data_w8_main, by = "NSID") %>%
  full_join(data_w8_der, by = "NSID") %>%
  full_join(data_w9_main, by = "NSID") %>%
  full_join(data_w9_der, by = "NSID")

# Helper for GHQ sum scoring
calc_ghq_sum <- function(cols) {
  # Convert to numeric
  df_subset <- map_df(cols, ~as.numeric(.))
  
  # If ALL items are NA -> -3
  all_na <- apply(df_subset, 1, function(x) all(is.na(x)))
  
  # If ANY item is negative -> -8
  any_neg <- apply(df_subset, 1, function(x) any(!is.na(x) & x < 0))
  
  # Sum items
  sums <- rowSums(df_subset, na.rm = TRUE)
  
  res <- sums
  res[any_neg] <- -8
  res[all_na] <- -3
  res[is.na(res)] <- -3
  return(res)
}

# GHQ Items Lists
items_15 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
items_17 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
items_25 <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")
items_32 <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

# Derive ghqtlXX
full_data$ghqtl15 <- calc_ghq_sum(full_data[items_15])
full_data$ghqtl17 <- calc_ghq_sum(full_data[items_17])
full_data$ghqtl25 <- calc_ghq_sum(full_data[items_25])
full_data$ghqtl32 <- calc_ghq_sum(full_data[items_32])

# Harmonise pre-derived GHQ scores
harmonise_early <- function(x) {
  x <- as.numeric(x)
  x[x == -96 | x == -99] <- -3
  x[x == -97 | x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

harmonise_late <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  return(x)
}

full_data$ghq15 <- harmonise_early(full_data$W2ghq12scr)
full_data$ghq17 <- harmonise_early(full_data$W4ghq12scr)
full_data$ghq25 <- harmonise_late(full_data$W8DGHQSC)
full_data$ghq32 <- harmonise_late(full_data$W9DGHQSC)

# Labels
# The error was due to the labels being a named character vector where the names were strings. 
# For labelled::labelled(), the labels argument should be a named vector where names are the labels and values are the codes.
# Actually, the standard is: labels = c("Label1" = value1, "Label2" = value2)

ghq_labels <- c(
  "Item not applicable" = -1,
  "Script error/information lost" = -2,
  "Not asked at the fieldwork stage/participated/interviewed" = -3,
  "Don't know/insufficient information" = -8,
  "Refusal" = -9
)

derived_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")

for(var in derived_vars) {
  full_data[[var]] <- labelled::labelled(full_data[[var]], ghq_labels)
}

# Final Selection and Factor conversion
final_data <- full_data %>%
  select(NSID, all_of(derived_vars)) %>%
  mutate(across(all_of(derived_vars), as.factor))

write_csv(final_data, "data/output/cleaned_data.csv")
