
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Suppress specific warnings
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(labelled)
})

# Define the mapping for collapsing fractional NS-SEC codes to major categories
collapse_nssec <- function(x) {
  if (is.numeric(x)) {
    as.integer(floor(x))
  } else {
    x
  }
}

# Define the mapping for missing values
map_missing <- function(x) {
  replace(x, x == -98, -3)
}

# Define the standard NS-SEC labels
nssec_labels <- c(
  `1` = "Employers in large organisations",
  `2` = "Higher managerial occupations",
  `3` = "Higher professional occupations",
  `4` = "Lower professional occupations",
  `5` = "Lower managerial occupations",
  `6` = "Higher supervisory occupations",
  `7` = "Intermediate occupations",
  `8` = "Employers in small organisations",
  `9` = "Own account workers",
  `10` = "Lower supervisory occupations",
  `11` = "Lower technical occupations",
  `12` = "Semi-routine occupations",
  `13` = "Routine occupations",
  `14` = "Never worked / long-term unemployed",
  `15` = "Full-time students",
  `16` = "Not classified or inadequately stated",
  `17` = "Not classifiable for other reasons"
)

# Missing value labels
missing_labels <- c(
  `-3` = "Parent not present (or partner at wave 5)",
  `-2` = "Schedule not applicable/script error/information lost",
  `-1` = "Item not applicable",
  `-9` = "Refusal",
  `-8` = "Don't know/insufficient information",
  `-7` = "Prefer not to say"
)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

# Load files with explicit column types
wave_one_data <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two_data <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three_data <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four_data <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_five_data <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Process each wave with explicit column selection
wave_one_data <- wave_one_data %>%
  mutate(nssecma14 = collapse_nssec(W1nsseccatmum)) %>%
  mutate(nssecma14 = map_missing(nssecma14)) %>%
  mutate(nssecpa14 = collapse_nssec(W1nsseccatdad)) %>%
  mutate(nssecpa14 = map_missing(nssecpa14))

wave_two_data <- wave_two_data %>%
  mutate(nssecma15 = collapse_nssec(W2nsseccatmum)) %>%
  mutate(nssecma15 = map_missing(nssecma15)) %>%
  mutate(nssecpa15 = collapse_nssec(W2nsseccatdad)) %>%
  mutate(nssecpa15 = map_missing(nssecpa15))

wave_three_data <- wave_three_data %>%
  mutate(nssecma16 = collapse_nssec(W3cnsseccatmum)) %>%
  mutate(nssecma16 = map_missing(nssecma16)) %>%
  mutate(nssecpa16 = collapse_nssec(W3cnsseccatdad)) %>%
  mutate(nssecpa16 = map_missing(nssecpa16))

wave_four_data <- wave_four_data %>%
  mutate(nssecma17 = collapse_nssec(w4cnsseccatmum)) %>%
  mutate(nssecma17 = map_missing(nssecma17)) %>%
  mutate(nssecpa17 = collapse_nssec(w4cnsseccatdad)) %>%
  mutate(nssecpa17 = map_missing(nssecpa17))

wave_five_data <- wave_five_data %>%
  mutate(nssecma18 = collapse_nssec(w5Cnsseccatmum)) %>%
  mutate(nssecma18 = map_missing(nssecma18)) %>%
  mutate(nssecpa18 = collapse_nssec(w5Cnsseccatdad)) %>%
  mutate(nssecpa18 = map_missing(nssecpa18))

# Merge all datasets by NSID
merged_data <- full_join(
  wave_one_data %>% select(NSID, nssecma14, nssecpa14),
  wave_two_data %>% select(NSID, nssecma15, nssecpa15),
  by = "NSID"
) %>%
  full_join(
    wave_three_data %>% select(NSID, nssecma16, nssecpa16),
    by = "NSID"
  ) %>%
  full_join(
    wave_four_data %>% select(NSID, nssecma17, nssecpa17),
    by = "NSID"
  ) %>%
  full_join(
    wave_five_data %>% select(NSID, nssecma18, nssecpa18),
    by = "NSID"
  )

# Create labelled factors with proper handling
nssec_vars <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15",
                "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
                "nssecma18", "nssecpa18")

for (var in nssec_vars) {
  # Get all unique values in the variable
  all_levels <- sort(unique(merged_data[[var]]))
  all_levels <- all_levels[!is.na(all_levels)]

  # Create complete level vector including missing values
  complete_levels <- c(as.numeric(names(nssec_labels)), -3, -2, -1, -9, -8, -7)

  # Create factor with all possible levels
  merged_data[[var]] <- factor(
    merged_data[[var]],
    levels = complete_levels,
    labels = c(nssec_labels, missing_labels)
  )
}

# Write output with explicit path
output_path <- "data/output/cleaned_data.csv"
write_csv(merged_data, output_path)

# Verify output was created
if (file.exists(output_path)) {
  cat("Successfully created cleaned_data.csv with NS-SEC variables\n")
} else {
  stop("Failed to create output file")
}
