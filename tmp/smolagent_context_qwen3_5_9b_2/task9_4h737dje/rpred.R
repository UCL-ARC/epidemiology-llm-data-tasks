library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load the three wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Extract education variables from each wave
W1hiqualmum <- wave1$W1hiqualmum
W1hiqualdad <- wave1$W1hiqualdad
W2hiqualmum <- wave2$W2hiqualmum
W2hiqualdad <- wave2$W2hiqualdad
w4hiqualmum <- wave4$w4hiqualmum
w4hiqualdad <- wave4$w4hiqualdad

# Function to convert missing values based on metadata labels
convert_missing <- function(x) {
  x_char <- as.character(x)
  x_char[x_char == "-999"] <- "-2"
  x_char[x_char == "-998"] <- "-2"
  x_char[x_char == "-997"] <- "-2"
  x_char[x_char == "-995"] <- "-2"
  x_char[x_char == "-94"] <- "-8"
  x_char[x_char == "-92"] <- "-9"
  x_char[x_char == "-91"] <- "-1"
  x_char[x_char == "-99"] <- "-3"
  x_num <- suppressWarnings(as.numeric(x_char))
  x_num[is.na(x_num)] <- -3
  return(x_num)
}

W1hiqualmum <- convert_missing(W1hiqualmum)
W2hiqualmum <- convert_missing(W2hiqualmum)
w4hiqualmum <- convert_missing(w4hiqualmum)
W1hiqualdad <- convert_missing(W1hiqualdad)
W2hiqualdad <- convert_missing(W2hiqualdad)
w4hiqualdad <- convert_missing(w4hiqualdad)

# Create data frames with NSID and education variables for each wave
wave1_edu <- data.frame(NSID = wave1$NSID, W1_mom = W1hiqualmum, W1_dad = W1hiqualdad)
wave2_edu <- data.frame(NSID = wave2$NSID, W2_mom = W2hiqualmum, W2_dad = W2hiqualdad)
wave4_edu <- data.frame(NSID = wave4$NSID, W4_mom = w4hiqualmum, W4_dad = w4hiqualdad)

# Merge all waves by NSID (full join to keep all IDs)
edu_base <- full_join(wave1_edu, wave2_edu, by = "NSID")
edu_base <- full_join(edu_base, wave4_edu, by = "NSID")

# Function to check if value is valid positive (1-20)
is_valid_pos <- function(x) x %in% 1:20
# Function to check if value is valid negative (-9 to -2)
is_valid_neg <- function(x) x >= -9 & x <= -2

# Derive consolidated mother education (earliest valid first: W1, W2, W4)
mom_consolidated <- rep(-3, nrow(edu_base))
mom_pos_W1 <- is_valid_pos(edu_base$W1_mom)
mom_consolidated[mom_pos_W1] <- edu_base$W1_mom[mom_pos_W1]

still_missing <- is.na(mom_consolidated)
if (any(still_missing)) {
  mom_pos_W2 <- is_valid_pos(edu_base$W2_mom)
  mom_consolidated[still_missing & mom_pos_W2] <- edu_base$W2_mom[still_missing & mom_pos_W2]
}
still_missing <- is.na(mom_consolidated)
if (any(still_missing)) {
  mom_pos_W4 <- is_valid_pos(edu_base$W4_mom)
  mom_consolidated[still_missing & mom_pos_W4] <- edu_base$W4_mom[still_missing & mom_pos_W4]
}

# If still missing, try negative values
still_missing <- is.na(mom_consolidated)
if (any(still_missing)) {
  neg_W1 <- is_valid_neg(edu_base$W1_mom)
  mom_consolidated[still_missing & neg_W1] <- edu_base$W1_mom[still_missing & neg_W1]
}
still_missing <- is.na(mom_consolidated)
if (any(still_missing)) {
  neg_W2 <- is_valid_neg(edu_base$W2_mom)
  mom_consolidated[still_missing & neg_W2] <- edu_base$W2_mom[still_missing & neg_W2]
}
still_missing <- is.na(mom_consolidated)
if (any(still_missing)) {
  neg_W4 <- is_valid_neg(edu_base$W4_mom)
  mom_consolidated[still_missing & neg_W4] <- edu_base$W4_mom[still_missing & neg_W4]
}
mom_consolidated[is.na(mom_consolidated)] <- -3

# Derive consolidated father education
dad_consolidated <- rep(-3, nrow(edu_base))
dad_pos_W1 <- is_valid_pos(edu_base$W1_dad)
dad_consolidated[dad_pos_W1] <- edu_base$W1_dad[dad_pos_W1]

still_missing <- is.na(dad_consolidated)
if (any(still_missing)) {
  dad_pos_W2 <- is_valid_pos(edu_base$W2_dad)
  dad_consolidated[still_missing & dad_pos_W2] <- edu_base$W2_dad[still_missing & dad_pos_W2]
}
still_missing <- is.na(dad_consolidated)
if (any(still_missing)) {
  dad_pos_W4 <- is_valid_pos(edu_base$W4_dad)
  dad_consolidated[still_missing & dad_pos_W4] <- edu_base$W4_dad[still_missing & dad_pos_W4]
}
still_missing <- is.na(dad_consolidated)
if (any(still_missing)) {
  neg_W1 <- is_valid_neg(edu_base$W1_dad)
  dad_consolidated[still_missing & neg_W1] <- edu_base$W1_dad[still_missing & neg_W1]
}
still_missing <- is.na(dad_consolidated)
if (any(still_missing)) {
  neg_W2 <- is_valid_neg(edu_base$W2_dad)
  dad_consolidated[still_missing & neg_W2] <- edu_base$W2_dad[still_missing & neg_W2]
}
still_missing <- is.na(dad_consolidated)
if (any(still_missing)) {
  neg_W4 <- is_valid_neg(edu_base$W4_dad)
  dad_consolidated[still_missing & neg_W4] <- edu_base$W4_dad[still_missing & neg_W4]
}
dad_consolidated[is.na(dad_consolidated)] <- -3

# Collapse to NVQ scheme
collapse_to_nvq <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 0,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -7 ~ -7,
    x %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
    x == 18 ~ 2,
    x %in% c(19, 20, -3, -2, -1) ~ -3,
    TRUE ~ NA_integer_
  )
}

educma <- collapse_to_nvq(mom_consolidated)
educpa <- collapse_to_nvq(dad_consolidated)

# Create final dataframe
final_df <- data.frame(
  NSID = edu_base$NSID,
  educdtlma = mom_consolidated,
  educdtlpa = dad_consolidated,
  educma = educma,
  educpa = educpa
)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("Script completed successfully!\n")
cat("Variables:", names(final_df), "\n")
cat("Rows:", nrow(final_df), "\n")
