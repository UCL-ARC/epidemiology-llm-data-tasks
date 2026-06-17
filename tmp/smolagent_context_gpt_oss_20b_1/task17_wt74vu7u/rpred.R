library(readr)
library(dplyr)

path_input <- "data/input/"
path_output <- "data/output/"

read_file <- function(filename, col_types) {
  read_delim(paste0(path_input, filename), delim = "\t", col_types = col_types, na = character())
}

wave2_cols <- cols(NSID = col_character(), IMDRSCORE = col_double())
wave2_df <- read_file("wave_two_lsype_family_background_2020.tab", wave2_cols) %>%
  distinct(NSID, .keep_all = TRUE)
wave2_df <- wave2_df %>% rename(imd15 = IMDRSCORE)

wave3_cols <- cols(NSID = col_character(), IMDRSCORE = col_double())
wave3_df <- read_file("wave_three_lsype_family_background_2020.tab", wave3_cols) %>%
  distinct(NSID, .keep_all = TRUE)
wave3_df <- wave3_df %>% rename(imd16 = IMDRSCORE)

ns9_cols <- cols(NSID = col_character(), W9DIMDD = col_double())
ns9_df <- read_file("ns9_2022_derived_variables.tab", ns9_cols) %>%
  distinct(NSID, .keep_all = TRUE)
ns9_df <- ns9_df %>% rename(imd32 = W9DIMDD)

harmonise_missing_continuous <- function(x) {
  ifelse(is.na(x) | x < 0, -3, x)
}

wave2_df$imd15 <- harmonise_missing_continuous(wave2_df$imd15)
wave3_df$imd16 <- harmonise_missing_continuous(wave3_df$imd16)
ns9_df$imd32   <- harmonise_missing_continuous(ns9_df$imd32)

merged_df <- wave2_df %>%
  full_join(wave3_df, by = "NSID") %>%
  full_join(ns9_df, by = "NSID")

final_df <- merged_df %>% select(NSID, imd15, imd16, imd32)

write_csv(final_df, paste0(path_output, "cleaned_data.csv"))