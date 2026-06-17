library(readr)
library(dplyr)
library(purrr)

# Helper to map missing codes for GHQ derived scores
map_missing_ghq <- function(x){
  if(is.null(x)) return(x)
  res <- as.numeric(x)
  # W2/W4
  res[res == -99] <- -3
  res[res == -97 | res == -92] <- -9
  res[res == -96] <- -3
  # W8
  res[res == -9] <- -9
  res[res == -8] <- -8
  res[res == -1] <- -1
  # W9
  res[res == -9] <- -9
  res[res == -8] <- -8
  res[res == -3] <- -3
  res[res == -1] <- -1
  res[res < 0 & !res %in% c(-9,-8,-3,-1)] <- -3
  return(res)
}

# Load data
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim="\t", col_types = cols(.default = col_character()))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim="\t", col_types = cols(.default = col_character()))
wave8d <- read_delim("data/input/ns8_2015_derived.tab", delim="\t", col_types = cols(.default = col_character()))
wave9d <- read_delim("data/input/ns9_2022_derived_variables.tab", delim="\t", col_types = cols(.default = col_character()))

# Convert derived score columns to numeric
wave2 <- wave2 %>% mutate(W2ghq12scr = as.numeric(W2ghq12scr))
wave4 <- wave4 %>% mutate(W4ghq12scr = as.numeric(W4ghq12scr))
wave8d <- wave8d %>% mutate(W8DGHQSC = as.numeric(W8DGHQSC))
wave9d <- wave9d %>% mutate(W9DGHQSC = as.numeric(W9DGHQSC))

# Map missing codes
wave2$W2ghq12scr <- map_missing_ghq(wave2$W2ghq12scr)
wave4$W4ghq12scr <- map_missing_ghq(wave4$W4ghq12scr)
wave8d$W8DGHQSC <- map_missing_ghq(wave8d$W8DGHQSC)
wave9d$W9DGHQSC <- map_missing_ghq(wave9d$W9DGHQSC)

# Caseness calculation
caseness <- function(score){
  res <- score
  res[!is.na(res) & res >= 4] <- 1
  res[!is.na(res) & res < 4] <- 0
  return(res)
}

wave2 <- wave2 %>% mutate(ghq15 = caseness(W2ghq12scr))
wave4 <- wave4 %>% mutate(ghq17 = caseness(W4ghq12scr))
wave8d <- wave8d %>% mutate(ghq25 = caseness(W8DGHQSC))
wave9d <- wave9d %>% mutate(ghq32 = caseness(W9DGHQSC))

# Load GHQ items for waves 8 and 9
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim="\t", col_types = cols(.default = col_character()))
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim="\t", col_types = cols(.default = col_character()))

# Convert items to numeric and collapse
wave8_items <- wave8 %>% select(starts_with("W8GHQ12_")) %>% mutate_all(~as.numeric(.))
wave8_items <- wave8_items %>% mutate_all(~ifelse(. < 1, NA, .))
wave8_sum <- apply(wave8_items, 1, function(x){ if(all(!is.na(x))) sum(x) else NA })
wave8d <- wave8d %>% mutate(ghqtl25 = ifelse(is.na(wave8_sum), -3, wave8_sum))

wave9_items <- wave9 %>% select(starts_with("W9GHQ12_")) %>% mutate_all(~as.numeric(.))
wave9_items <- wave9_items %>% mutate_all(~ifelse(. < 1, NA, .))
wave9_sum <- apply(wave9_items, 1, function(x){ if(all(!is.na(x))) sum(x) else NA })
wave9d <- wave9d %>% mutate(ghqtl32 = ifelse(is.na(wave9_sum), -3, wave9_sum))

# Set ghqtl15 and ghqtl17 to -3
wave2 <- wave2 %>% mutate(ghqtl15 = -3)
wave4 <- wave4 %>% mutate(ghqtl17 = -3)

# Merge by NSID
final_df <- wave2 %>% select(NSID, ghq15, ghqtl15) %>%
  full_join(wave4 %>% select(NSID, ghq17, ghqtl17), by="NSID") %>%
  full_join(wave8d %>% select(NSID, ghq25, ghqtl25), by="NSID") %>%
  full_join(wave9d %>% select(NSID, ghq32, ghqtl32), by="NSID")

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")
