library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load each file
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Rename variables
wave1 <- wave1 %>%
  rename(nssecma14 = W1nsseccatmum,
         nssecpa14 = W1nsseccatdad)
wave2 <- wave2 %>%
  rename(nssecma15 = W2nsseccatmum,
         nssecpa15 = W2nsseccatdad)
wave3 <- wave3 %>%
  rename(nssecma16 = W3cnsseccatmum,
         nssecpa16 = W3cnsseccatdad)
wave4 <- wave4 %>%
  rename(nssecma17 = w4cnsseccatmum,
         nssecpa17 = w4cnsseccatdad)
wave5 <- wave5 %>%
  rename(nssecma18 = w5Cnsseccatmum,
         nssecpa18 = w5Cnsseccatdad)

# Merge datasets
data_merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Define variables to process
vars_to_process <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")

# Process each variable in a loop
for (var in vars_to_process) {
  x <- data_merged[[var]]
  # Recode specific missing codes
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x %in% c(-99, -98)] <- -3
  x[is.na(x)] <- -3
  # Identify non-missing codes (not in standard missing codes)
  non_missing <- !x %in% c(-9, -8, -7, -3, -2, -1)
  # For non-missing values, take integer part
  x[non_missing] <- as.integer(x[non_missing])
  # Check if between 1-17; if not, set to NA
  x[non_missing & (x < 1 | x > 17)] <- NA
  data_merged[[var]] <- x
}

# Define labels for all categories (names = labels, values = codes)
labels_vec <- c(
  "Employers in large organisations" = 1,
  "Higher managerial occupations" = 2,
  "Higher professional" = 3,
  "Lower professional" = 4,
  "Lower managerial occupations" = 5,
  "Higher supervisory occupations" = 6,
  "Intermediate" = 7,
  "Employers in small orgs" = 8,
  "Own account workers" = 9,
  "Lower supervisory occupations" = 10,
  "Lower technical" = 11,
  "Semi routine" = 12,
  "Routine" = 13,
  "Never worked/Long-term unemployed/Not currently working" = 14,
  "Full-time students" = 15,
  "Not classified or inadequately stated" = 16,
  "Not classifiable for other reasons" = 17,
  "Refusal" = -9,
  "Don't know" = -8,
  "Prefer not to say" = -7,
  "Not asked/not interviewed" = -3,
  "Script error/information lost" = -2,
  "Not applicable" = -1,
  "Invalid" = NA
)

# Convert to labelled vectors using haven::labelled
for (var in vars_to_process) {
  data_merged[[var]] <- haven::labelled(data_merged[[var]], labels = labels_vec)
}

# Select only required variables
data_final <- data_merged %>%
  select(NSID, all_of(vars_to_process))

# Write to CSV
write_csv(data_final, "data/output/cleaned_data.csv")