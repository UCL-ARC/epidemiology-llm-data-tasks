library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# 1. Read files
input_dir <- "data/input/"
wave_one <- read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = "c"))
wave_four <- read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = "c"))
wave_two <- read_delim(file.path(input_dir, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(.default = "c"))
wave_three <- read_delim(file.path(input_dir, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(.default = "c"))
ns8 <- read_delim(file.path(input_dir, "ns8_2015_derived.tab"), delim = "\t", col_types = cols(.default = "c"))
ns9_derived <- read_delim(file.path(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = cols(.default = "c"))
ns9_main <- read_delim(file.path(input_dir, "ns9_2022_main_interview.tab"), delim = "\t", col_types = cols(.default = "c"))

# 2. Merge
merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID")

# 3. Helper functions
na_to_missing <- function(x){x <- as.numeric(x); x[is.na(x)] <- -3; return(x)}
clean_urbind_gor <- function(x){x <- na_to_missing(x); x[x == -94] <- -8; x[x == -1] <- -1; return(x)}
clean_unknown_postcode <- function(x){x <- na_to_missing(x); x[x == 13] <- -2; return(x)}
clean_nation_res <- function(x){x <- na_to_missing(x); x[x %in% 1:4] <- 1; x[x == 5] <- 2; return(x)}

# 4. Derive variables
cleaned <- merged %>%
  mutate(regub15 = clean_urbind_gor(urbind.x),
         regov15 = clean_urbind_gor(gor.x),
         regub16 = clean_urbind_gor(urbind.y),
         regov16 = clean_urbind_gor(gor.y),
         regor25 = clean_unknown_postcode(W8DGOR),
         regor32 = clean_unknown_postcode(W9DRGN),
         regint32 = clean_nation_res(W9NATIONRES)) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# 5. Write output
output_dir <- "data/output"
write_csv(cleaned, file.path(output_dir, "cleaned_data.csv"))
