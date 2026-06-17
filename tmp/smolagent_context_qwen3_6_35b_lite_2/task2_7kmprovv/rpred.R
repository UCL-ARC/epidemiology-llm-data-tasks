library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(labelled)

# Define file paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab"
)

# Load each file
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Keep only NSID and the ethnicity variable from each file
w1 <- w1 %>% select(NSID, W1ethnic2YP)
w2 <- w2 %>% select(NSID, W2ethnicYP)
w4 <- w4 %>% select(NSID, w4ethnic2YP)
w8 <- w8 %>% select(NSID, W8DETHN15)
w9 <- w9 %>% select(NSID, W9DETHN15)

# Merge all files by NSID
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Function to convert missing values to standard codes
convert_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  x[x == -8] <- -8
  x[x == -9] <- -9
  return(x)
}

# Apply missing value conversion to each wave variable
df$W1ethnic2YP <- convert_missing(df$W1ethnic2YP)
df$W2ethnicYP <- convert_missing(df$W2ethnicYP)
df$w4ethnic2YP <- convert_missing(df$w4ethnic2YP)
df$W8DETHN15 <- convert_missing(df$W8DETHN15)
df$W9DETHN15 <- convert_missing(df$W9DETHN15)

# Create consolidated eth variable using earliest-valid-first
is_valid <- function(x) x >= 1 & x <= 16

df$eth <- df$W1ethnic2YP
df$eth[!is_valid(df$W1ethnic2YP)] <- df$W2ethnicYP[!is_valid(df$W1ethnic2YP)]
df$eth[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP)] <- df$w4ethnic2YP[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP)]
df$eth[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP) & !is_valid(df$w4ethnic2YP)] <- df$W8DETHN15[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP) & !is_valid(df$w4ethnic2YP)]
df$eth[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP) & !is_valid(df$w4ethnic2YP) & !is_valid(df$W8DETHN15)] <- df$W9DETHN15[!is_valid(df$W1ethnic2YP) & !is_valid(df$W2ethnicYP) & !is_valid(df$w4ethnic2YP) & !is_valid(df$W8DETHN15)]

# Handle any remaining NAs
df$eth[is.na(df$eth)] <- -3

# Convert to factor with proper labels
label_map <- c(
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
  "16" = "Any other ethnic background",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable",
  "-3" = "Not asked",
  "-8" = "Don't know / insufficient information",
  "-9" = "Refused"
)

# Create factor
df$eth <- as.factor(as.character(df$eth))
levels(df$eth) <- label_map[levels(df$eth)]

# Select only NSID and eth for output
df_out <- df %>% select(NSID, eth)

# Write to CSV
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(df_out), "\n")
