library(dplyr)
library(tidyr)
library(readr)
library(labelled)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab",
  "data/input/wave_five_lsype_family_background_2020.tab",
  "data/input/wave_six_lsype_young_person_2020.tab",
  "data/input/wave_seven_lsype_young_person_2020.tab",
  "data/input/ns8_2015_main_interview.tab",
  "data/input/ns9_2022_derived_variables.tab"
)

# Load all files
s1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
s2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
s3 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
s4 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)
s5 <- read_delim(files[5], delim = "\t", show_col_types = FALSE)
s6 <- read_delim(files[6], delim = "\t", show_col_types = FALSE)
s7 <- read_delim(files[7], delim = "\t", show_col_types = FALSE)
s8 <- read_delim(files[8], delim = "\t", show_col_types = FALSE)
s9 <- read_delim(files[9], delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- s1 %>%
  full_join(s2, by = "NSID") %>%
  full_join(s3, by = "NSID") %>%
  full_join(s4, by = "NSID") %>%
  full_join(s5, by = "NSID") %>%
  full_join(s6, by = "NSID") %>%
  full_join(s7, by = "NSID") %>%
  full_join(s8, by = "NSID") %>%
  full_join(s9, by = "NSID")

# Define helper function to map missing codes
map_missing <- function(x) {
  case_when(
    x == -999 ~ -2,
    x == -997 ~ -2,
    x == -99 ~ -2,
    x == -998 ~ -2,
    x == -995 ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
}

# Define helper function to collapse detailed to collapsed
collapse_tenure <- function(detailed) {
  case_when(
    detailed == 1 ~ 1,
    detailed == 2 ~ 2,
    detailed == 3 ~ 3,
    detailed == 4 ~ 4,
    detailed == 5 ~ 4,
    detailed == 6 ~ 4,
    detailed == 7 ~ 5,
    detailed == 8 ~ 6,
    detailed == -9 ~ -9,
    detailed == -8 ~ -8,
    detailed == -7 ~ -7,
    detailed == -3 ~ -3,
    detailed == -2 ~ -2,
    detailed == -1 ~ -1,
    TRUE ~ NA_real_
  )
}

# Define helper function to collapse sweeps 8-9
collapse_tenure_s89 <- function(source) {
  case_when(
    source == 1 ~ 1,
    source == 2 ~ 2,
    source == 3 ~ 3,
    source == 4 ~ 4,
    source == 5 ~ 5,
    source == 6 ~ 6,
    source == 7 ~ 6,
    source == -9 ~ -9,
    source == -8 ~ -8,
    source == -1 ~ -1,
    TRUE ~ NA_real_
  )
}

# Sweeps 1-4: Single source variable per sweep
# Detailed 8-category and collapsed 6-category

# Sweep 1 (Age 14)
df <- df %>%
  mutate(
    hownteen14 = case_when(
      W1hous12HH >= 1 & W1hous12HH <= 8 ~ W1hous12HH,
      TRUE ~ map_missing(W1hous12HH)
    ),
    hown14 = collapse_tenure(hownteen14)
  )

# Sweep 2 (Age 15)
df <- df %>%
  mutate(
    hownteen15 = case_when(
      W2Hous12HH >= 1 & W2Hous12HH <= 8 ~ W2Hous12HH,
      TRUE ~ map_missing(W2Hous12HH)
    ),
    hown15 = collapse_tenure(hownteen15)
  )

# Sweep 3 (Age 16)
df <- df %>%
  mutate(
    hownteen16 = case_when(
      W3hous12HH >= 1 & W3hous12HH <= 8 ~ W3hous12HH,
      TRUE ~ map_missing(W3hous12HH)
    ),
    hown16 = collapse_tenure(hownteen16)
  )

# Sweep 4 (Age 17)
df <- df %>%
  mutate(
    hownteen17 = case_when(
      W4Hous12HH >= 1 & W4Hous12HH <= 8 ~ W4Hous12HH,
      TRUE ~ map_missing(W4Hous12HH)
    ),
    hown17 = collapse_tenure(hownteen17)
  )

# Sweeps 5-7: Three source variables per sweep
# Detailed 8-category and collapsed 6-category

# Sweep 5 (Age 18)
df <- df %>%
  mutate(
    hownteen18 = case_when(
      W5Hous12HH == 1 ~ case_when(
        W5Hous12BHH == 1 ~ 1,
        W5Hous12BHH == 2 ~ 2,
        W5Hous12BHH == 3 ~ 3,
        W5Hous12BHH == 4 ~ 8,
        TRUE ~ map_missing(W5Hous12BHH)
      ),
      W5Hous12HH == 2 ~ case_when(
        W5Hous12CHH == 1 ~ 4,
        W5Hous12CHH == 2 ~ 5,
        W5Hous12CHH == 3 ~ 6,
        W5Hous12CHH == 4 ~ 7,
        W5Hous12CHH == 5 ~ 8,
        TRUE ~ map_missing(W5Hous12CHH)
      ),
      W5Hous12HH == 3 ~ 8,
      TRUE ~ case_when(
        W5Hous12BHH == 1 ~ 1,
        W5Hous12BHH == 2 ~ 2,
        W5Hous12BHH == 3 ~ 3,
        W5Hous12BHH == 4 ~ 8,
        TRUE ~ map_missing(W5Hous12BHH)
      )
    ),
    hown18 = collapse_tenure(hownteen18)
  )

# Sweep 6 (Age 19)
df <- df %>%
  mutate(
    hownteen19 = case_when(
      W6Hous12YP == 1 ~ case_when(
        W6Hous12bYP == 1 ~ 1,
        W6Hous12bYP == 2 ~ 2,
        W6Hous12bYP == 3 ~ 3,
        W6Hous12bYP == 4 ~ 8,
        TRUE ~ map_missing(W6Hous12bYP)
      ),
      W6Hous12YP == 2 ~ case_when(
        W6Hous12cYP == 1 ~ 4,
        W6Hous12cYP == 2 ~ 5,
        W6Hous12cYP == 3 ~ 6,
        W6Hous12cYP == 4 ~ 7,
        W6Hous12cYP == 5 ~ 8,
        TRUE ~ map_missing(W6Hous12cYP)
      ),
      W6Hous12YP == 3 ~ 8,
      TRUE ~ case_when(
        W6Hous12bYP == 1 ~ 1,
        W6Hous12bYP == 2 ~ 2,
        W6Hous12bYP == 3 ~ 3,
        W6Hous12bYP == 4 ~ 8,
        TRUE ~ map_missing(W6Hous12bYP)
      )
    ),
    hown19 = collapse_tenure(hownteen19)
  )

# Sweep 7 (Age 20)
df <- df %>%
  mutate(
    hownteen20 = case_when(
      W7Hous12YP == 1 ~ case_when(
        W7Hous12bYP == 1 ~ 1,
        W7Hous12bYP == 2 ~ 2,
        W7Hous12bYP == 3 ~ 3,
        W7Hous12bYP == 4 ~ 8,
        TRUE ~ map_missing(W7Hous12bYP)
      ),
      W7Hous12YP == 2 ~ case_when(
        W7Hous12cYP == 1 ~ 4,
        W7Hous12cYP == 2 ~ 5,
        W7Hous12cYP == 3 ~ 6,
        W7Hous12cYP == 4 ~ 7,
        W7Hous12cYP == 5 ~ 8,
        TRUE ~ map_missing(W7Hous12cYP)
      ),
      W7Hous12YP == 3 ~ 8,
      TRUE ~ case_when(
        W7Hous12bYP == 1 ~ 1,
        W7Hous12bYP == 2 ~ 2,
        W7Hous12bYP == 3 ~ 3,
        W7Hous12bYP == 4 ~ 8,
        TRUE ~ map_missing(W7Hous12bYP)
      )
    ),
    hown20 = collapse_tenure(hownteen20)
  )

# Sweeps 8-9: Single source variable per sweep
# Collapsed 6-category only

# Sweep 8 (Age 25)
df <- df %>%
  mutate(
    hown25 = collapse_tenure_s89(W8TENURE)
  )

# Sweep 9 (Age 32)
df <- df %>%
  mutate(
    hown32 = collapse_tenure_s89(W9DTENURE)
  )

# Convert remaining NA to -3
df <- df %>%
  mutate(across(c(hownteen14:hownteen20, hown14:hown32), ~ replace_na(., -3)))

# Define labels for detailed variables
detailed_labels <- c(
  "1" = "Owned outright",
  "2" = "Being bought on a mortgage",
  "3" = "Shared ownership",
  "4" = "Rented from a Council",
  "5" = "Rented from a Housing Association",
  "6" = "Rented privately",
  "7" = "Rent free",
  "8" = "Some other arrangement",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule not applicable",
  "-1" = "Item not applicable"
)

# Define labels for collapsed variables
collapsed_labels <- c(
  "1" = "Own outright",
  "2" = "Own, buying with mortgage",
  "3" = "Part rent, part mortgage",
  "4" = "Rent it",
  "5" = "Live rent-free",
  "6" = "Other",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule not applicable",
  "-1" = "Item not applicable"
)

# Convert to factors
detailed_vars <- c("hownteen14", "hownteen15", "hownteen16", "hownteen17", "hownteen18", "hownteen19", "hownteen20")
collapsed_vars <- c("hown14", "hown15", "hown16", "hown17", "hown18", "hown19", "hown20", "hown25", "hown32")

for (var in detailed_vars) {
  df[[var]] <- factor(df[[var]], levels = c(1:8, -9, -8, -7, -3, -2, -1), labels = detailed_labels)
}

for (var in collapsed_vars) {
  df[[var]] <- factor(df[[var]], levels = c(1:6, -9, -8, -7, -3, -2, -1), labels = collapsed_labels)
}

# Select final variables
df_final <- df %>%
  select(NSID, hownteen14, hownteen15, hownteen16, hownteen17, hownteen18, hownteen19, hownteen20,
         hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write output
write_csv(df_final, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_final), "\n")
cat("Columns:", paste(names(df_final), collapse = ", "), "\n")
