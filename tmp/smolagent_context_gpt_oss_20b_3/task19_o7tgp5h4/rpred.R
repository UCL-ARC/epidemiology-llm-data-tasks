library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# 1. Load all data files -------------------------------------------------
wave_one <- read_delim(
  "data/input/wave_one_lsype_young_person_2020.tab",
  delim = "\t",
  show_col_types = FALSE
)

wave_four <- read_delim(
  "data/input/wave_four_lsype_young_person_2020.tab",
  delim = "\t",
  show_col_types = FALSE
)

ns8_derived <- read_delim(
  "data/input/ns8_2015_derived.tab",
  delim = "\t",
  show_col_types = FALSE
)

ns9_derived <- read_delim(
  "data/input/ns9_2022_derived_variables.tab",
  delim = "\t",
  show_col_types = FALSE
)

# 2. Create a master list of all NSID values ---------------------------------
master_ids <- unique(
  c(
    wave_one$NSID,
    wave_four$NSID,
    ns8_derived$NSID,
    ns9_derived$NSID
  )
)

base_df <- tibble(NSID = master_ids)

# 3. Merge BMI variables from the derived files ------------------------------
df <- base_df %>%
  left_join(select(ns8_derived, NSID, W8DBMI), by = "NSID") %>%
  left_join(select(ns9_derived, NSID, W9DBMI), by = "NSID")

# 4. Helper function to map missing codes to the standard scheme ---------
map_missing <- function(x) {
  # Ensure numeric
  x <- as.numeric(x)

  # Replace R NA with -3
  x[is.na(x)] <- -3

  # Handle negative values (missing codes)
  neg_idx <- which(x < 0)
  if (length(neg_idx) > 0) {
    neg_vals <- x[neg_idx]

    # -999 to -995 → -2 (schedule/not applicable)
    rng <- which(neg_vals <= -995 & neg_vals >= -999)
    if (length(rng) > 0) {
      x[neg_idx[rng]] <- -2
      neg_vals[rng] <- -2
    }

    # -92 → -9 (Refusal)
    x[neg_idx[neg_vals == -92]] <- -9

    # -91 → -1 (Not applicable)
    x[neg_idx[neg_vals == -91]] <- -1

    # -99 → -3 (Not asked)
    x[neg_idx[neg_vals == -99]] <- -3

    # All other negative values are already standard codes
  }

  return(x)
}

# 5. Derive BMI variables --------------------------------------------------
df <- df %>%
  mutate(
    bmi25 = map_missing(W8DBMI),
    bmi32 = map_missing(W9DBMI)
  ) %>%
  select(NSID, bmi25, bmi32)

# 6. Write the cleaned data -------------------------------------------------
write_csv(df, "data/output/cleaned_data.csv")
