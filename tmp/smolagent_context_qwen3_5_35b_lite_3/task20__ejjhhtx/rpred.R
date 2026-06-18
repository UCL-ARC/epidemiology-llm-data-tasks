# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df3 <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
df9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
merged <- full_join(df1, df2, by = "NSID")
merged <- full_join(merged, df3, by = "NSID")
merged <- full_join(merged, df4, by = "NSID")
merged <- full_join(merged, df6, by = "NSID")
merged <- full_join(merged, df7, by = "NSID")
merged <- full_join(merged, df8, by = "NSID")
merged <- full_join(merged, df9, by = "NSID")

# Create wave-specific ever-drink variables with age codes
# W1alceverYP (age 14)
alcfst14 <- rep(NA_integer_, nrow(merged))
var <- merged$W1alceverYP
alcfst14[var == 1] <- 14
alcfst14[var == 2] <- NA
alcfst14[var == -99] <- -3
alcfst14[var == -97] <- -9
alcfst14[var == -96] <- -2
alcfst14[var == -92] <- -9
alcfst14[var == -91] <- -1
alcfst14[var == -1] <- -8

# W2alceverYP (age 15)
alcfst15 <- rep(NA_integer_, nrow(merged))
var <- merged$W2alceverYP
alcfst15[var == 1] <- 15
alcfst15[var == 2] <- NA
alcfst15[var == -998] <- -2
alcfst15[var == -997] <- -2
alcfst15[var == -995] <- -2
alcfst15[var == -99] <- -3
alcfst15[var == -97] <- -9
alcfst15[var == -96] <- -2
alcfst15[var == -92] <- -9
alcfst15[var == -91] <- -1
alcfst15[var == -1] <- -8

# W3alceverYP (age 16)
alcfst16 <- rep(NA_integer_, nrow(merged))
var <- merged$W3alceverYP
alcfst16[var == 1] <- 16
alcfst16[var == 2] <- NA
alcfst16[var == -99] <- -3
alcfst16[var == -97] <- -9
alcfst16[var == -96] <- -2
alcfst16[var == -92] <- -9
alcfst16[var == -91] <- -1
alcfst16[var == -1] <- -8

# W4AlcEverYP (age 17)
alcfst17 <- rep(NA_integer_, nrow(merged))
var <- merged$W4AlcEverYP
alcfst17[var == 1] <- 17
alcfst17[var == 2] <- NA
alcfst17[var == -99] <- -3
alcfst17[var == -97] <- -9
alcfst17[var == -96] <- -2
alcfst17[var == -92] <- -9
alcfst17[var == -91] <- -1
alcfst17[var == -1] <- -8

# W6AlcEverYP (age 19)
alcfst19 <- rep(NA_integer_, nrow(merged))
var <- merged$W6AlcEverYP
alcfst19[var == 1] <- 19
alcfst19[var == 2] <- NA
alcfst19[var == -997] <- -2
alcfst19[var == -97] <- -9
alcfst19[var == -92] <- -9
alcfst19[var == -91] <- -1
alcfst19[var == -1] <- -8

# W7AlcEverYP (age 20)
alcfst20 <- rep(NA_integer_, nrow(merged))
var <- merged$W7AlcEverYP
alcfst20[var == 1] <- 20
alcfst20[var == 2] <- NA
alcfst20[var == -996] <- -2
alcfst20[var == -97] <- -9
alcfst20[var == -92] <- -9
alcfst20[var == -91] <- -1
alcfst20[var == -1] <- -8

# Derive alcfst: earliest age at first alcohol consumption
merged$alcfst <- rep(NA_integer_, nrow(merged))

# Check each wave in order (earliest to latest)
# If valid age found, keep it and don't override with later waves
for (age in c(14, 15, 16, 17, 19, 20)) {
  age_var <- get(paste0("alcfst", age))
  # Only set if alcfst is still NA (earliest valid wins)
  idx <- is.na(merged$alcfst) & !is.na(age_var) & age_var > 0
  merged$alcfst[idx] <- age_var[idx]
}

# For those with no valid ever-drink response, set alcfst = 99 (never drank)
merged$alcfst[is.na(merged$alcfst)] <- 99

# Select only final derived variables: NSID and alcfst
output <- merged %>% select(NSID, alcfst)

# Write output
dir.create("data/output", recursive = TRUE, showWarnings = FALSE)
write_csv(output, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(output), "\n")
cat("Summary of alcfst:\n")
print(table(output$alcfst, useNA = "ifany"))
