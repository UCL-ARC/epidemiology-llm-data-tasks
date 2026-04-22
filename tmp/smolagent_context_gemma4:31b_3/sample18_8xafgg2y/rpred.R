library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define missing value labels as a named vector for labelled package
# In labelled::val_labels(), the labels are a named vector
missing_labels <- c(
  "-1" = "Item not applicable",
  "-2" = "Script error/information lost",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-8" = "Don't know/insufficient information",
  "-9" = "Refusal"
)

# 1. File Loading
# Sweep 2 (Age 15)
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
vars2_items <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
vars2_pre <- "W2ghq12scr"
df2_raw <- read_delim(file2, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars2_items), all_of(vars2_pre)) %>% 
  mutate(across(-NSID, as.numeric))

# Sweep 4 (Age 17)
file4 <- "data/input/wave_four_lsype_young_person_2020.tab"
vars4_items <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
vars4_pre <- "W4ghq12scr"
df4_raw <- read_delim(file4, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars4_items), all_of(vars4_pre)) %>% 
  mutate(across(-NSID, as.numeric))

# Sweep 8 (Age 25)
file8_items <- "data/input/ns8_2015_self_completion.tab"
vars8_items <- paste0("W8GHQ12_", 1:12)
df8_items_raw <- read_delim(file8_items, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars8_items)) %>% 
  mutate(across(-NSID, as.numeric))

file8_pre <- "data/input/ns8_2015_derived.tab"
vars8_pre <- "W8DGHQSC"
df8_pre_raw <- read_delim(file8_pre, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars8_pre)) %>% 
  mutate(across(-NSID, as.numeric))

# Sweep 9 (Age 32)
file9_items <- "data/input/ns9_2022_main_interview.tab"
vars9_items <- paste0("W9GHQ12_", 1:12)
df9_items_raw <- read_delim(file9_items, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars9_items)) %>% 
  mutate(across(-NSID, as.numeric))

file9_pre <- "data/input/ns9_2022_derived_variables.tab"
vars9_pre <- "W9DGHQSC"
df9_pre_raw <- read_delim(file9_pre, delim = "\t", col_types = cols(.default = "c")) %>% 
  select(NSID, all_of(vars9_pre)) %>% 
  mutate(across(-NSID, as.numeric))

# Merge datasets
df <- df2_raw %>%
  full_join(df4_raw, by = "NSID") %>%
  full_join(df8_items_raw, by = "NSID") %>%
  full_join(df8_pre_raw, by = "NSID") %>%
  full_join(df9_items_raw, by = "NSID") %>%
  full_join(df9_pre_raw, by = "NSID")

# 4. Deriving GHQ-12 Sum Scores (ghqtlXX)
compute_ghqtl <- function(data, item_vars) {
  items <- data[, item_vars, drop = FALSE]
  sapply(1:nrow(items), function(i) {
    row <- items[i, ]
    if (all(is.na(row))) return(-3)
    if (any(row < 0, na.rm = TRUE)) return(-8)
    sum(row, na.rm = TRUE)
  })
}

df$ghqtl15 <- compute_ghqtl(df, vars2_items)
df$ghqtl17 <- compute_ghqtl(df, vars4_items)
df$ghqtl25 <- compute_ghqtl(df, vars8_items)
df$ghqtl32 <- compute_ghqtl(df, vars9_items)

# 5. Harmonising Pre-Derived GHQ Scores (ghqXX)
harmonise_early <- function(x) {
  x <- ifelse(is.na(x), -3, x)
  x <- ifelse(x == -96 | x == -99, -3, x)
  x <- ifelse(x == -97 | x == -92, -9, x)
  x
}

harmonise_late <- function(x) {
  ifelse(is.na(x), -3, x)
}

df$ghq15 <- harmonise_early(df[[vars2_pre]])
df$ghq17 <- harmonise_early(df[[vars4_pre]])
df$ghq25 <- harmonise_late(df[[vars8_pre]])
df$ghq32 <- harmonise_late(df[[vars9_pre]])

# 6. Variable Labels
derived_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")
for (var in derived_vars) {
  # The correct way to apply labels in the labelled package is using val_labels()
  df[[var]] <- labelled::labelled(df[[var]], labels = labelled::val_labels(missing_labels))
}

# 7. Output Variables
df_final <- df %>%
  select(NSID, all_of(derived_vars)) %>%
  mutate(across(all_of(derived_vars), as.factor))

# 9. Output Requirements
write_csv(df_final, "data/output/cleaned_data.csv")
