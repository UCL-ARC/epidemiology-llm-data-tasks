
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Define file paths
file_paths <- list(
  wave_one = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five = "data/input/wave_five_lsype_family_background_2020.tab"
)

# Load and merge datasets
load_data <- function(file_path) {
  read_delim(file_path, delim = "\t")
}

wave_one <- load_data(file_paths$wave_one)
wave_two <- load_data(file_paths$wave_two)
wave_three <- load_data(file_paths$wave_three)
wave_four <- load_data(file_paths$wave_four)
wave_five <- load_data(file_paths$wave_five)

# Merge datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID")

# Function to harmonize NS-SEC codes
harmonize_nssec <- function(x) {
  if (!is.numeric(x)) return(x)

  x <- ifelse(x == -999, -2, x)
  x <- ifelse(x == -99 | x == -98, -3, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(is.na(x), -3, x)

  x <- ifelse(x >= 3.1 & x <= 4.4, as.integer(x),
              ifelse(x >= 7.1 & x <= 12.7, as.integer(x),
                     ifelse(x >= 13.1 & x <= 13.5, as.integer(x), x)))

  x <- ifelse(x >= 1 & x <= 17, x, NA_integer_)
  return(x)
}

# Create labeled factors with custom labels
create_labeled_factor <- function(x, levels, labels) {
  x <- harmonize_nssec(x)
  factor(x, levels = levels, labels = labels)
}

# Define NS-SEC levels and labels
nssec_levels <- c(-9, -8, -7, -3, -2, -1, 1:17)
nssec_labels <- c(
  "Refusal", "Don't know", "Prefer not to say", "Not asked/not interviewed",
  "Script error/information lost", "Not applicable",
  "Employers in large organisations", "Higher managerial occupations",
  "Higher professional occupations", "Lower professional occupations",
  "Lower managerial occupations", "Higher supervisory occupations",
  "Intermediate occupations", "Employers in small organisations",
  "Own account workers", "Lower supervisory occupations",
  "Lower technical occupations", "Semi-routine occupations",
  "Routine occupations", "Never worked/long-term unemployed",
  "Full-time students", "Not classified or inadequately stated",
  "Not classifiable for other reasons"
)

# Process each variable
merged_data$nssecma14 <- create_labeled_factor(merged_data$W1nsseccatmum, nssec_levels, nssec_labels)
merged_data$nssecpa14 <- create_labeled_factor(merged_data$W1nsseccatdad, nssec_levels, nssec_labels)
merged_data$nssecma15 <- create_labeled_factor(merged_data$W2nsseccatmum, nssec_levels, nssec_labels)
merged_data$nssecpa15 <- create_labeled_factor(merged_data$W2nsseccatdad, nssec_levels, nssec_labels)
merged_data$nssecma16 <- create_labeled_factor(merged_data$W3cnsseccatmum, nssec_levels, nssec_labels)
merged_data$nssecpa16 <- create_labeled_factor(merged_data$W3cnsseccatdad, nssec_levels, nssec_labels)
merged_data$nssecma17 <- create_labeled_factor(merged_data$w4cnsseccatmum, nssec_levels, nssec_labels)
merged_data$nssecpa17 <- create_labeled_factor(merged_data$w4cnsseccatdad, nssec_levels, nssec_labels)
merged_data$nssecma18 <- create_labeled_factor(merged_data$w5Cnsseccatmum, nssec_levels, nssec_labels)
merged_data$nssecpa18 <- create_labeled_factor(merged_data$w5Cnsseccatdad, nssec_levels, nssec_labels)

# Select required variables
final_data <- merged_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15,
         nssecma16, nssecpa16, nssecma17, nssecpa17,
         nssecma18, nssecpa18)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
