library(dplyr)
library(readr)

files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave2 = "data/input/wave_two_lsype_young_person_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab"
)

# Helper to map missing codes
map_missing <- function(df, var, type) {
  if (type == "YP") {
    df %>% mutate(!!sym(var) := case_when(
      !!sym(var) %in% c(-99, -92, -91, -1) ~ NA_real_,
      !!sym(var) %in% 1:4 ~ !!sym(var),
      TRUE ~ NA_real_
    ))
  } else if (type == "HH") {
    df %>% mutate(!!sym(var) := case_when(
      !!sym(var) %in% c(-999, -997, -99, -92, -91, -1) ~ NA_real_,
      !!sym(var) %in% 1:4 ~ !!sym(var),
      TRUE ~ NA_real_
    ))
  } else {
    df
  }
}

# Load wave 1
w1 <- read_delim(files$wave1, delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
w1 <- w1 %>% rename(w1_var = W1englangYP)
w1 <- map_missing(w1, "w1_var", "YP")

# Load wave 2
w2 <- read_delim(files$wave2, delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
w2 <- w2 %>% rename(w2_var = W2EnglangYP)
w2 <- map_missing(w2, "w2_var", "YP")

# Load wave 3
w3 <- read_delim(files$wave3, delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
w3 <- w3 %>% rename(w3_var = W3englangHH)
w3 <- map_missing(w3, "w3_var", "HH")

# Load wave 4
w4 <- read_delim(files$wave4, delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
w4 <- w4 %>% rename(w4_var = W4EngLangHH)
w4 <- map_missing(w4, "w4_var", "HH")

# Merge all waves by NSID
merged <- full_join(w1, w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Consolidate
merged <- merged %>%
  mutate(lang = case_when(
    !is.na(w1_var) ~ w1_var,
    !is.na(w2_var) ~ w2_var,
    !is.na(w3_var) ~ w3_var,
    !is.na(w4_var) ~ w4_var,
    TRUE ~ -3
  ))
merged <- merged %>% mutate(lang = ifelse(is.na(lang), -3, lang))

final_df <- merged %>% select(NSID, lang)
write_csv(final_df, "data/output/cleaned_data.csv")
