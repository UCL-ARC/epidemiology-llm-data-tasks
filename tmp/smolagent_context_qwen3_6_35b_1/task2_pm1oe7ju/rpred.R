library(dplyr)
library(readr)
library(labelled)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Load all files from data/input/
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
file4 <- "data/input/wave_four_lsype_young_person_2020.tab"
file8 <- "data/input/ns8_2015_derived.tab"
file9 <- "data/input/ns9_2022_derived_variables.tab"

data1 <- read_delim(file1, delim = "\t", show_col_types = FALSE)
data2 <- read_delim(file2, delim = "\t", show_col_types = FALSE)
data4 <- read_delim(file4, delim = "\t", show_col_types = FALSE)
data8 <- read_delim(file8, delim = "\t", show_col_types = FALSE)
data9 <- read_delim(file9, delim = "\t", show_col_types = FALSE)

cat("File sizes:\n")
cat("File1:", nrow(data1), "\n")
cat("File2:", nrow(data2), "\n")
cat("File4:", nrow(data4), "\n")
cat("File8:", nrow(data8), "\n")
cat("File9:", nrow(data9), "\n")

# Merge all datasets using full_join by NSID
df <- full_join(data1, data2, by = "NSID")
df <- full_join(df, data4, by = "NSID")
df <- full_join(df, data8, by = "NSID")
df <- full_join(df, data9, by = "NSID")

cat("Merged dataset size:", nrow(df), "\n")

# Helper function to map missing values to standard codes
map_missing <- function(x) {
  # Create a copy
  result <- x
  
  # Map specific codes to standard missing codes
  # -999: household data lost -> -2 (schedule not applicable / information lost)
  result[result == -999] <- -2
  # -998: interviewer missed question -> -2
  result[result == -998] <- -2
  # -997: script error -> -2
  result[result == -997] <- -2
  # -995: missing history section data -> -2
  result[result == -995] <- -2
  # -99: not interviewed -> -3 (not asked / not interviewed)
  result[result == -99] <- -3
  # -94: insufficient information -> -8 (don't know / insufficient information)
  result[result == -94] <- -8
  # -92: refused -> -9 (refusal)
  result[result == -92] <- -9
  # -9: refused -> -9 (refusal)
  result[result == -9] <- -9
  # -8: insufficient information -> -8 (don't know / insufficient information)
  result[result == -8] <- -8
  # -1: don't know / not applicable -> -8 for "don't know", -1 for "not applicable"
  # We need to distinguish -1 codes by source
  
  # Convert remaining NAs to -3
  result[is.na(result)] <- -3
  
  return(result)
}

# Function to map missing values with source-specific -1 handling
map_missing_w1 <- function(x) {
  result <- x
  result[result == -999] <- -2
  result[result == -94] <- -8
  result[result == -92] <- -9
  result[result == -91] <- -1  # Not applicable
  result[result == -1] <- -8   # Don't know
  result[is.na(result)] <- -3
  return(result)
}

map_missing_w2 <- function(x) {
  result <- x
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -99] <- -3
  result[result == -92] <- -9
  result[result == -91] <- -1  # Not applicable
  result[result == -1] <- -8   # Don't Know
  result[is.na(result)] <- -3
  return(result)
}

map_missing_w4 <- function(x) {
  result <- x
  result[result == -94] <- -8
  result[result == -1] <- -8   # Don't know
  result[is.na(result)] <- -3
  return(result)
}

map_missing_w8 <- function(x) {
  result <- x
  result[result == -9] <- -9   # Refused
  result[result == -8] <- -8   # Insufficient information
  result[result == -1] <- -1   # Not applicable
  result[is.na(result)] <- -3
  return(result)
}

map_missing_w9 <- function(x) {
  result <- x
  result[result == -8] <- -8   # Insufficient information
  result[is.na(result)] <- -3
  return(result)
}

# Apply missing value mapping
w1_eth <- map_missing_w1(df$W1ethnic2YP)
w2_eth <- map_missing_w2(df$W2ethnicYP)
w4_eth <- map_missing_w4(df$w4ethnic2YP)
w8_eth <- map_missing_w8(df$W8DETHN15)
w9_eth <- map_missing_w9(df$W9DETHN15)

# Create consolidated eth variable using earliest valid positive response (1-16)
# Priority: W1ethnic2YP -> W2ethnicYP -> w4ethnic2YP -> W8DETHN15 -> W9DETHN15
eth <- rep(-3, nrow(df))  # Default: not interviewed

# Start with W1
for (i in seq_len(nrow(df))) {
  if (w1_eth[i] >= 1 && w1_eth[i] <= 16) {
    eth[i] <- w1_eth[i]
  } else if (w2_eth[i] >= 1 && w2_eth[i] <= 16) {
    eth[i] <- w2_eth[i]
  } else if (w4_eth[i] >= 1 && w4_eth[i] <= 16) {
    eth[i] <- w4_eth[i]
  } else if (w8_eth[i] >= 1 && w8_eth[i] <= 16) {
    eth[i] <- w8_eth[i]
  } else if (w9_eth[i] >= 1 && w9_eth[i] <= 16) {
    eth[i] <- w9_eth[i]
  }
}

# Vectorized approach for efficiency
eth <- w1_eth
missing_w1 <- eth < 1 | eth > 16
eth[missing_w1] <- w2_eth[missing_w1]
missing_w2 <- eth[!missing_w1] < 1 | eth[!missing_w1] > 16
# Recalculate properly
for (i in seq_len(nrow(df))) {
  if (w1_eth[i] >= 1 && w1_eth[i] <= 16) {
    eth[i] <- w1_eth[i]
  } else if (w2_eth[i] >= 1 && w2_eth[i] <= 16) {
    eth[i] <- w2_eth[i]
  } else if (w4_eth[i] >= 1 && w4_eth[i] <= 16) {
    eth[i] <- w4_eth[i]
  } else if (w8_eth[i] >= 1 && w8_eth[i] <= 16) {
    eth[i] <- w8_eth[i]
  } else if (w9_eth[i] >= 1 && w9_eth[i] <= 16) {
    eth[i] <- w9_eth[i]
  } else {
    eth[i] <- -3
  }
}

# Create factor with labels
ethnic_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background"
)

# Add missing value labels
missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked / not interviewed",
  "-2" = "Schedule not applicable",
  "-1" = "Item not applicable"
)

all_levels <- c(as.character(1:16), as.character(-9), as.character(-8), as.character(-7), as.character(-3), as.character(-2), as.character(-1))
all_labels <- c(ethnic_labels, missing_labels)

# Set NA values to -3 for proper factor conversion
eth_final <- eth
eth_final[is.na(eth_final)] <- -3

eth_factor <- factor(eth_final, levels = all_levels, labels = all_labels)

# Attach labels
df$eth <- eth_factor

# Select only NSID and eth
df_out <- df %>% select(NSID, eth)

# Write to CSV
write_csv(df_out, "data/output/cleaned_data.csv")

cat("\nOutput written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_out), "\n")
cat("\nDistribution of eth:\n")
print(table(df_out$eth, useNA = "ifany"))
