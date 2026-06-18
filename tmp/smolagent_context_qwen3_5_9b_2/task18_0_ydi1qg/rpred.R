library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

input_dir <- "data/input"
output_dir <- "data/output"

files <- list(
  wave1 = file.path(input_dir, "wave_one_lsype_young_person_2020.tab"),
  wave2 = file.path(input_dir, "wave_two_lsype_young_person_2020.tab"),
  wave4 = file.path(input_dir, "wave_four_lsype_young_person_2020.tab"),
  wave8_items = file.path(input_dir, "ns8_2015_self_completion.tab"),
  wave8_derived = file.path(input_dir, "ns8_2015_derived.tab"),
  wave9_items = file.path(input_dir, "ns9_2022_main_interview.tab"),
  wave9_derived = file.path(input_dir, "ns9_2022_derived_variables.tab")
)

wave1 <- read_delim(files$wave1, delim = "\t", show_col_types = FALSE)
wave2 <- read_delim(files$wave2, delim = "\t", show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = "\t", show_col_types = FALSE)
wave8_items <- read_delim(files$wave8_items, delim = "\t", show_col_types = FALSE)
wave8_derived <- read_delim(files$wave8_derived, delim = "\t", show_col_types = FALSE)
wave9_items <- read_delim(files$wave9_items, delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim(files$wave9_derived, delim = "\t", show_col_types = FALSE)

data_all <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_items, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_items, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

gHQ15_items <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
gHQ17_items <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")
gHQ25_items <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")
gHQ32_items <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

calc_ghqtl <- function(items, data) {
  item_vals <- data[items]
  neg_check <- any(sapply(item_vals, function(x) !is.na(x) & x < 0))
  if (neg_check) {
    return(-8)
  }
  any_na <- any(sapply(item_vals, is.na))
  if (any_na) {
    all_na <- all(sapply(item_vals, function(x) is.na(x)))
    if (all_na) {
      return(-3)
    } else {
      return(-8)
    }
  }
  return(sum(item_vals))
}

data_all$ghqtl15 <- calc_ghqtl(gHQ15_items, data_all)
data_all$ghqtl17 <- calc_ghqtl(gHQ17_items, data_all)
data_all$ghqtl25 <- calc_ghqtl(gHQ25_items, data_all)
data_all$ghqtl32 <- calc_ghqtl(gHQ32_items, data_all)

data_all$ghq15 <- data_all$W2ghq12scr
data_all$ghq15[data_all$ghq15 %in% c(-97, -92)] <- -9

data_all$ghq17 <- data_all$W4ghq12scr
data_all$ghq17[data_all$ghq17 %in% c(-97, -92)] <- -9

data_all$ghq25 <- data_all$W8DGHQSC
data_all$ghq32 <- data_all$W9DGHQSC

data_final <- data_all %>%
  select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(data_final, file.path(output_dir, "cleaned_data.csv"))

cat("Script completed successfully.\n")
cat(paste("Output written to", file.path(output_dir, "cleaned_data.csv"), "\n"))
cat(paste("Rows:", nrow(data_final), "\n"))
cat(paste("Columns:", ncol(data_final), "\n"))