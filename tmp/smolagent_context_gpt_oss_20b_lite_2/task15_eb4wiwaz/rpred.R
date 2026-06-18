library(readr)
library(dplyr)

input_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# file names
files <- list(
  wave_one = "wave_one_lsype_young_person_2020.tab",
  wave_four = "wave_four_lsype_young_person_2020.tab",
  ns8 = "ns8_2015_derived.tab",
  ns9 = "ns9_2022_derived_variables.tab"
)

# read function
read_tab <- function(fname) {
  read_delim(paste0(input_dir, fname), delim = "\t", col_types = cols(.default = "d", NSID = "c"))
}

# load
wave_one_df <- read_tab(files$wave_one)
wave_four_df <- read_tab(files$wave_four)
ns8_df <- read_tab(files$ns8)
ns9_df <- read_tab(files$ns9)

# merge
merged_df <- wave_one_df %>%
  full_join(wave_four_df, by = "NSID") %>%
  full_join(ns8_df, by = "NSID") %>%
  full_join(ns9_df, by = "NSID")

# derive income bands
merged_df <- merged_df %>%
  mutate(
    inc25 = ifelse(is.na(W8DINCB), -3L, ifelse(W8DINCB == -1.0, -1L, as.integer(W8DINCB))),
    inc32 = ifelse(is.na(W9DINCB), -3L, ifelse(W9DINCB == -1.0, -1L, as.integer(W9DINCB)))
  ) %>%
  select(NSID, inc25, inc32)

write_csv(merged_df, output_file)
