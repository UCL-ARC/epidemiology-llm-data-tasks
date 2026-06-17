# Load required packages
library(readr)
library(dplyr)
library(labelled)

# Helper to harmonise missing codes according to metadata and standard scheme
map_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -99]  <- -3
  x[x == -98]  <- -3
  x[x == -94]  <- -8
  x[x == -92]  <- -9
  x[x == -91]  <- -1
  x[x == -1]   <- -8
  return(x)
}

# Read input files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", trim_ws = TRUE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", trim_ws = TRUE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", trim_ws = TRUE)

# Apply missing code harmonisation to relevant variables
wave1 <- wave1 %>% mutate(
  W1hiqualmum_std = map_missing(W1hiqualmum),
  W1hiqualdad_std = map_missing(W1hiqualdad)
)

wave2 <- wave2 %>% mutate(
  W2hiqualmum_std = map_missing(W2hiqualmum),
  W2hiqualdad_std = map_missing(W2hiqualdad)
)

wave4 <- wave4 %>% mutate(
  w4hiqualmum_std = map_missing(w4hiqualmum),
  w4hiqualdad_std = map_missing(w4hiqualdad)
)

# Merge all waves by NSID
merged <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to get first value across waves
get_first_value <- function(values) {
  pos_vals <- values[values >= 1 & values <= 20]
  if(length(pos_vals) > 0) {
    return(pos_vals[1])
  } else {
    neg_vals <- values[values <= 0]
    if(length(neg_vals) > 0) {
      return(neg_vals[1])
    } else {
      return(-3)  # no data
    }
  }
}

# Create consolidated detailed variables
merged <- merged %>% rowwise() %>% mutate(
  educdtlma = get_first_value(c(W1hiqualmum_std, W2hiqualmum_std, w4hiqualmum_std)),
  educdtlpa = get_first_value(c(W1hiqualdad_std, W2hiqualdad_std, w4hiqualdad_std))
) %>% ungroup()

# Collapse detailed to 5-level NVQ scheme
collapse_nvq <- function(detailed) {
  case_when(
    detailed == 1 ~ 0,
    detailed == 2 ~ 0,
    detailed == 3 ~ 0,
    detailed == 4 ~ 0,
    detailed %in% 5:17 ~ 1,
    detailed == 18 ~ 2,
    detailed == 19 ~ 3,
    detailed == 20 ~ 4,
    TRUE ~ detailed
  )
}

merged <- merged %>% mutate(
  educma = collapse_nvq(educdtlma),
  educpa = collapse_nvq(educdtlpa)
)

# Select final variables
final_df <- merged %>% select(NSID, educdtlma, educma, educdtlpa, educpa)

# Write output CSV
write_csv(final_df, "data/output/cleaned_data.csv")

# Print success message
cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")