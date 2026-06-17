library(dplyr)
library(readr)
library(labelled)

# Load all files
s1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
s8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
s9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Helper function to map missing value codes to standard codes
map_missing <- function(x) {
  result <- as.numeric(x)
  result[result == -99] <- -3
  result[result == -97] <- -9
  result[result == -96] <- -2
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -8
  result[result == -998] <- -2
  result[result == -997] <- -2
  result[result == -995] <- -2
  result[result == -996] <- -2
  result
}

# Create drinking indicators for each sweep
s1 <- s1 %>%
  mutate(
    alcever_s1 = map_missing(W1alceverYP),
    alcmon_s1 = map_missing(W1alcmonYP),
    alc_drink_s1 = case_when(
      alcever_s1 == 1 & alcmon_s1 == 1 ~ 1,
      alcever_s1 == 2 | alcmon_s1 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s2 <- s2 %>%
  mutate(
    alcever_s2 = map_missing(W2alceverYP),
    alc_drink_s2 = case_when(
      alcever_s2 == 1 ~ 1,
      alcever_s2 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s3 <- s3 %>%
  mutate(
    alcever_s3 = map_missing(W3alceverYP),
    alc_drink_s3 = case_when(
      alcever_s3 == 1 ~ 1,
      alcever_s3 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s4 <- s4 %>%
  mutate(
    AlcEver_s4 = map_missing(W4AlcEverYP),
    alc_drink_s4 = case_when(
      AlcEver_s4 == 1 ~ 1,
      AlcEver_s4 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s6 <- s6 %>%
  mutate(
    AlcEver_s6 = map_missing(W6AlcEverYP),
    alc_drink_s6 = case_when(
      AlcEver_s6 == 1 ~ 1,
      AlcEver_s6 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s7 <- s7 %>%
  mutate(
    AlcEver_s7 = map_missing(W7AlcEverYP),
    alc_drink_s7 = case_when(
      AlcEver_s7 == 1 ~ 1,
      AlcEver_s7 == 2 ~ 2,
      TRUE ~ -3
    )
  )

s8 <- s8 %>%
  mutate(
    AUDIT1_s8 = map_missing(W8AUDIT1),
    alc_drink_s8 = case_when(
      AUDIT1_s8 > 1 ~ 1,
      AUDIT1_s8 == 1 ~ 2,
      TRUE ~ -3
    )
  )

s9 <- s9 %>%
  mutate(
    AUDIT1_s9 = map_missing(W9AUDIT1),
    alc_drink_s9 = case_when(
      AUDIT1_s9 > 1 ~ 1,
      AUDIT1_s9 == 1 ~ 2,
      TRUE ~ -3
    )
  )

# Merge all datasets
full_data <- s1 %>%
  full_join(s2, by = "NSID") %>%
  full_join(s3, by = "NSID") %>%
  full_join(s4, by = "NSID") %>%
  full_join(s6, by = "NSID") %>%
  full_join(s7, by = "NSID") %>%
  full_join(s8, by = "NSID") %>%
  full_join(s9, by = "NSID")

# Create vectors for drinking indicators
drink_vals <- data.frame(
  age14 = full_data$alc_drink_s1,
  age15 = full_data$alc_drink_s2,
  age16 = full_data$alc_drink_s3,
  age17 = full_data$alc_drink_s4,
  age19 = full_data$alc_drink_s6,
  age20 = full_data$alc_drink_s7,
  age25 = full_data$alc_drink_s8,
  age32 = full_data$alc_drink_s9
)

# Check if any drinking is observed (value = 1)
any_drinking <- rowSums(drink_vals == 1, na.rm = TRUE) > 0

# Create age vectors where drinking=1, NA otherwise
age_vectors <- data.frame(
  age14 = ifelse(drink_vals$age14 == 1, 14, NA_real_),
  age15 = ifelse(drink_vals$age15 == 1, 15, NA_real_),
  age16 = ifelse(drink_vals$age16 == 1, 16, NA_real_),
  age17 = ifelse(drink_vals$age17 == 1, 17, NA_real_),
  age19 = ifelse(drink_vals$age19 == 1, 19, NA_real_),
  age20 = ifelse(drink_vals$age20 == 1, 20, NA_real_),
  age25 = ifelse(drink_vals$age25 == 1, 25, NA_real_),
  age32 = ifelse(drink_vals$age32 == 1, 32, NA_real_)
)

# Find minimum age (earliest drinking)
min_ages <- apply(age_vectors, 1, function(x) min(x, na.rm = TRUE))

# Check if all observed values are not drinking (2) and none are missing
all_not_drinking <- rowSums(drink_vals == 2, na.rm = TRUE) > 0
no_missing <- !rowSums(is.na(drink_vals)) > 0

# Check if any indicator is missing (codes -3, -2, -1, -9, -8)
missing_codes <- c(-3, -2, -1, -9, -8)
any_missing <- rowSums(sapply(drink_vals, function(col) col %in% missing_codes), na.rm = TRUE) > 0

# Determine alcfst_raw
alcfst_raw <- ifelse(
  any_drinking,
  min_ages,
  ifelse(
    all_not_drinking & no_missing,
    99,
    -8
  )
)

# Create factor with specified levels and labels
alc_factor <- factor(
  alcfst_raw,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19", "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")
)

# Add labelled class
alc_factor <- labelled::labelled(
  alc_factor,
  labels = c(
    "Age 14" = 14,
    "Age 15" = 15,
    "Age 16" = 16,
    "Age 17" = 17,
    "Age 19" = 19,
    "Age 20" = 20,
    "Age 25" = 25,
    "Age 32" = 32,
    "Never had alcohol" = 99,
    "Don't know/insufficient information" = -8
  )
)

# Create final output
output <- data.frame(
  NSID = full_data$NSID,
  alcfst = alc_factor
)

# Write output
write_csv(output, "data/output/cleaned_data.csv")

# Check the output
cat("Output file written successfully.\n")
cat("Number of rows:", nrow(output), "\n")
cat("\nDistribution of alcfst:\n")
print(table(output$alcfst))
cat("\nUnique alcfst_raw values:", sort(unique(alcfst_raw)), "\n")
