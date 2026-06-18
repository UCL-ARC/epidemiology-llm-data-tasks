library(dplyr)
library(readr)
library(haven)
library(tidyr)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Helper function to harmonize missing values
harmonize_missing <- function(x) {
  result <- x
  # Map specific missing codes based on label meaning
  result[result == -999] <- -2  # Missing - household data lost
  result[result == -998] <- -2  # Interviewer missed question
  result[result == -997] <- -2  # Script error
  result[result == -995] <- -2  # Missing history section data
  result[result == -99] <- -3   # YP not interviewed
  result[result == -94] <- -8   # Insufficient information
  result[result == -92] <- -9   # Refused
  result[result == -8] <- -8    # Insufficient information (keep)
  result[result == -9] <- -9    # Refused (keep)
  
  # Handle NA -> -3 (not asked / not interviewed)
  result[is.na(result)] <- -3
  
  return(result)
}

# Process Wave 1 (Age 14)
w1_eth <- w1$W1ethnic2YP
w1_eth <- harmonize_missing(w1_eth)
# In wave 1, -1 means "Don't know" -> -8, -91 means "Not applicable" -> -1
w1_eth[w1_eth == -1] <- -8  # Don't know
w1_eth[w1_eth == -91] <- -1  # Not applicable
w1 <- w1 %>% mutate(eth_w14 = w1_eth)

# Process Wave 2 (Age 15)
w2_eth <- w2$W2ethnicYP
w2_eth <- harmonize_missing(w2_eth)
# In wave 2, -1 means "Don't Know" -> -8, -91 means "Not applicable" -> -1
w2_eth[w2_eth == -1] <- -8  # Don't Know
w2_eth[w2_eth == -91] <- -1  # Not applicable
w2 <- w2 %>% mutate(eth_w15 = w2_eth)

# Process Wave 4 (Age 17)
w4_eth <- w4$w4ethnic2YP
w4_eth <- harmonize_missing(w4_eth)
# In wave 4, -1 means "Don't know" -> -8
w4_eth[w4_eth == -1] <- -8
w4 <- w4 %>% mutate(eth_w17 = w4_eth)

# Process Wave 8 (Age 25)
w8_eth <- w8$W8DETHN15
w8_eth <- harmonize_missing(w8_eth)
# In wave 8, -1 means "Not applicable" -> -1 (keep as is)
w8 <- w8 %>% mutate(eth_w25 = w8_eth)

# Process Wave 9 (Age 32)
w9_eth <- w9$W9DETHN15
w9_eth <- harmonize_missing(w9_eth)
w9 <- w9 %>% mutate(eth_w32 = w9_eth)

# Merge all files by NSID using full_join
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Create consolidated ethnicity variable using earliest-valid-first
# Valid responses are 1-16; missing codes are negative values
# Priority: w14 (14) > w15 (15) > w17 (17) > w25 (25) > w32 (32)

# Convert missing codes to NA for coalesce purposes
df <- df %>%
  mutate(
    eth_w14_na = ifelse(eth_w14 >= 1 & eth_w14 <= 16, eth_w14, NA_real_),
    eth_w15_na = ifelse(eth_w15 >= 1 & eth_w15 <= 16, eth_w15, NA_real_),
    eth_w17_na = ifelse(eth_w17 >= 1 & eth_w17 <= 16, eth_w17, NA_real_),
    eth_w25_na = ifelse(eth_w25 >= 1 & eth_w25 <= 16, eth_w25, NA_real_),
    eth_w32_na = ifelse(eth_w32 >= 1 & eth_w32 <= 16, eth_w32, NA_real_)
  )

# Use coalesce to get first valid value (earliest wave first)
df <- df %>%
  mutate(eth = coalesce(eth_w14_na, eth_w15_na, eth_w17_na, eth_w25_na, eth_w32_na))

# For rows where eth is NA, we need to find the first non-missing (including missing codes)
na_rows <- is.na(df$eth)
if (any(na_rows)) {
  df$eth[na_rows] <- mapply(function(w14, w15, w17, w25, w32) {
    candidates <- c(w14, w15, w17, w25, w32)
    non_na_idx <- which(!is.na(candidates))
    if (length(non_na_idx) > 0) {
      return(candidates[non_na_idx[1]])
    } else {
      return(-3)
    }
  }, df$eth_w14[na_rows], df$eth_w15[na_rows], df$eth_w17[na_rows], df$eth_w25[na_rows], df$eth_w32[na_rows])
}

# Remove intermediate columns
df <- df %>% select(-matches("_na$"))

# Create labels as a named numeric vector for haven::labelled
# Format: names = labels, values = codes
labels <- c(
  "White - British" = 1,
  "White - Irish" = 2,
  "Any other White background" = 3,
  "Mixed - White and Black Caribbean" = 4,
  "Mixed - White and Black African" = 5,
  "Mixed - White and Asian" = 6,
  "Any other mixed background" = 7,
  "Indian" = 8,
  "Pakistani" = 9,
  "Bangladeshi" = 10,
  "Any other Asian background" = 11,
  "Black Caribbean" = 12,
  "Black African" = 13,
  "Any other Black background" = 14,
  "Chinese" = 15,
  "Any other ethnic background" = 16,
  "Not applicable" = -1,
  "Schedule not applicable / script error / information lost" = -2,
  "Not asked at the fieldwork stage / not interviewed" = -3,
  "Don't know / insufficient information" = -8,
  "Refusal" = -9
)

# Convert to labelled using haven
df$eth <- haven::labelled(df$eth, labels = labels)

# Keep only NSID and eth
df_out <- df %>% select(NSID, eth)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Done. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_out), "\n")