library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- list(
  w1 = "data/input/wave_one_lsype_young_person_2020.tab",
  w2 = "data/input/wave_two_lsype_family_background_2020.tab",
  w3 = "data/input/wave_three_lsype_family_background_2020.tab",
  w4 = "data/input/wave_four_lsype_young_person_2020.tab",
  w8 = "data/input/ns8_2015_derived.tab",
  w9_der = "data/input/ns9_2022_derived_variables.tab",
  w9_main = "data/input/ns9_2022_main_interview.tab"
)

# Memory-efficient loader: read and select only requested columns
load_vars <- function(path, vars) {
  # Read only the header first
  header <- read_delim(path, delim = "\t", n_max = 0, show_col_types = FALSE)
  all_cols <- colnames(header)
  # Filter vars to those that actually exist in the file to avoid selection errors
  valid_vars <- vars[vars %in% all_cols]
  
  # Read file and select
  # Using col_types = cols(.default = "d") as requested, but NSID is often character
  # We will let read_delim guess or specify NSID as character
  read_delim(path, delim = "\t", col_types = cols(NSID = "c", .default = "d"), show_col_types = FALSE) %>%
    select(all_of(valid_vars))
}

# Load specific variables
d1_id <- load_vars(files$w1, "NSID")
d2_vars <- load_vars(files$w2, c("NSID", "urbind", "gor"))
d3_vars <- load_vars(files$w3, c("NSID", "urbind", "gor"))
d4_id <- load_vars(files$w4, "NSID")
d8_vars <- load_vars(files$w8, c("NSID", "W8DGOR"))
d9_der_vars <- load_vars(files$w9_der, c("NSID", "W9DRGN"))
d9_main_vars <- load_vars(files$w9_main, c("NSID", "W9NATIONRES"))

# Merge
merged_data <- d1_id %>%
  full_join(d2_vars, by = "NSID", relationship = "many-to-many") %>%
  full_join(d3_vars, by = "NSID", relationship = "many-to-many") %>%
  full_join(d4_id, by = "NSID", relationship = "many-to-many") %>%
  full_join(d8_vars, by = "NSID", relationship = "many-to-many") %>%
  full_join(d9_der_vars, by = "NSID", relationship = "many-to-many") %>%
  full_join(d9_main_vars, by = "NSID", relationship = "many-to-many")

# Harmonize missing values
harmonize_missing <- function(x) {
  if (is.null(x)) return(numeric(0))
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -94] <- -8
  x[x %in% c(-100, -97, -999, -998, -997, -995, -92, -91, -99)] <- -3
  return(x)
}

# Create derived variables
# Note: urbind.x is from d2, urbind.y is from d3
merged_data <- merged_data %>%
  mutate(
    regub15 = if ("urbind.x" %in% names(.)) harmonize_missing(urbind.x) else NA,
    regub16 = if ("urbind.y" %in% names(.)) harmonize_missing(urbind.y) else NA,
    regov15 = if ("gor.x" %in% names(.)) harmonize_missing(gor.x) else NA,
    regov16 = if ("gor.y" %in% names(.)) harmonize_missing(gor.y) else NA,
    regov_w8 = if ("W8DGOR" %in% names(.)) harmonize_missing(W8DGOR) else NA,
    regov_w9 = if ("W9DRGN" %in% names(.)) harmonize_missing(W9DRGN) else NA
  )

# Special handling for regint_w9
if ("W9NATIONRES" %in% names(merged_data)) {
  merged_data <- merged_data %>%
    mutate(
      regint_w9_raw = harmonize_missing(W9NATIONRES),
      regint_w9 = case_when(
        regint_w9_raw %in% 1:4 ~ 1, # UK
        regint_w9_raw == 5 ~ 2,      # Abroad
        TRUE ~ regint_w9_raw
      )
    )
} else {
  merged_data$regint_w9 <- NA
}

# Final selection
final_cols <- c("NSID", "regub15", "regub16", "regov15", "regov16", "regov_w8", "regov_w9", "regint_w9")
final_df <- merged_data %>%
  select(all_of(intersect(final_cols, names(merged_data))))

write_csv(final_df, "data/output/cleaned_data.csv")
