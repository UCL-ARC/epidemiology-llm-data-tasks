library(readr)
library(dplyr)

input_path <- "data/input/"
output_path <- "data/output/"

# Function to replace wave-specific missing codes with standard scheme
standardise_missing <- function(x, wave){
  x <- as.numeric(x)
  case_when(
    is.na(x) ~ NA_real_,
    wave %in% c(1,2) ~ case_when(
      x == -999.0 ~ -2,
      x == -992.0 ~ -2,
      x == -99.0  ~ -3,
      x == -94.0  ~ -8,
      x == -92.0  ~ -9,
      x == -91.0  ~ -1,
      x == -3.0   ~ -3,
      x == -1.0   ~ -8,
      TRUE        ~ x
    ),
    wave == 3 ~ case_when(
      x == -99.0  ~ -3,
      x == -92.0  ~ -9,
      x == -1.0   ~ -8,
      TRUE        ~ x
    ),
    wave == 4 ~ case_when(
      x == -996.0 ~ -3,
      x == -99.0  ~ -3,
      x == -92.0  ~ -9,
      x == -1.0   ~ -8,
      TRUE        ~ x
    ),
    TRUE ~ x
  )
}

# Midpoint mapping for income bands (1‑12)
midpoints <- c(24.5, 74.5, 149.5, 249.5, 349.5, 449.5,
               549.5, 649.5, 749.5, 849.5, 949.5, 1250)

band_to_midpoint <- function(x){
  res <- rep(-3, length(x))
  idx <- which(x > 0 & x <= 12)
  res[idx] <- midpoints[as.integer(x[idx])]
  res
}

# Load the four sweep files
wf1 <- read_delim(paste0(input_path, "wave_one_lsype_family_background_2020.tab"),
                   delim="\t", col_types = cols(), na = c("", "NA"))
wf2 <- read_delim(paste0(input_path, "wave_two_lsype_family_background_2020.tab"),
                   delim="\t", col_types = cols(), na = c("", "NA"))
wf3 <- read_delim(paste0(input_path, "wave_three_lsype_family_background_2020.tab"),
                   delim="\t", col_types = cols(), na = c("", "NA"))
wf4 <- read_delim(paste0(input_path, "wave_four_lsype_family_background_2020.tab"),
                   delim="\t", col_types = cols(), na = c("", "NA"))

# Keep only the income variables and the ID
wf1 <- wf1 %>% select(NSID, inc14_raw = W1GrsswkHH)
wf2 <- wf2 %>% select(NSID, inc15_raw = W2GrsswkHH)
wf3 <- wf3 %>% select(NSID, inc16_raw = W3incestw)
wf4 <- wf4 %>% select(NSID, inc17_raw = w4IncEstW)

# Process each wave
wf1 <- wf1 %>% mutate(
  inc_banded14 = standardise_missing(inc14_raw, 1),
  inc_cont14 = band_to_midpoint(inc_banded14)
)
wf2 <- wf2 %>% mutate(
  inc_banded15 = standardise_missing(inc15_raw, 2),
  inc_cont15 = band_to_midpoint(inc_banded15)
)
wf3 <- wf3 %>% mutate(
  inc_banded16 = standardise_missing(inc16_raw, 3)
)
wf4 <- wf4 %>% mutate(
  inc_banded17 = standardise_missing(inc17_raw, 4)
)

# Merge on NSID
merged <- wf1 %>% full_join(wf2, by="NSID") %>%
  full_join(wf3, by="NSID") %>%
  full_join(wf4, by="NSID")

# Final columns
final_df <- merged %>% select(NSID,
                              inc_banded14, inc_cont14,
                              inc_banded15, inc_cont15,
                              inc_banded16, inc_banded17)

# Write to CSV
write_csv(final_df, file.path(output_path, "cleaned_data.csv"))