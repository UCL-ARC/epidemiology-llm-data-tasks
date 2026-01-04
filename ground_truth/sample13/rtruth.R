options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('haven', 'dplyr', 'purrr', 'here', 'labelled', 'readr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(haven)  # for reading SPSS/Stata files
  library(dplyr)  # for data manipulation
  library(purrr)  # for functional programming (map, reduce)
  library(here)  # for file paths
  library(labelled)  # for handling labelled data
  library(readr)  # for reading delimited files
})

# Data path
# Set folder path (change as needed)
data_path <- 'data/input/'

S1 <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)

S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)

S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)

S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)

S5 <- read_delim(file.path(data_path, "wave_five_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)

nssec_parents_all <- reduce(list(S1, S2, S3, S4, S5), full_join, by = "NSID")

# Harmonise values (preserve decimals, apply missing codes)
recode_nssec_detail <- function(x) {
  case_when(
    floor(x) %in% 1:17 ~ floor(x),
    x %in% c(-999, -94) ~ -2,
    x %in% c(-99, -98) | is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Apply recode and assign to derived variables
nssec_parents_all <- nssec_parents_all %>%
  mutate(
    nssecma14 = recode_nssec_detail(nssecma14),
    nssecpa14 = recode_nssec_detail(nssecpa14),
    nssecma15 = recode_nssec_detail(nssecma15),
    nssecpa15 = recode_nssec_detail(nssecpa15),
    nssecma16 = recode_nssec_detail(nssecma16),
    nssecpa16 = recode_nssec_detail(nssecpa16),
    nssecma17 = recode_nssec_detail(nssecma17),
    nssecpa17 = recode_nssec_detail(nssecpa17),
    nssecma18 = recode_nssec_detail(nssecma18),
    nssecpa18 = recode_nssec_detail(nssecpa18)
  ) %>%
  mutate(across(
    c(starts_with("nssecma"), starts_with("nssecpa")),
    ~ factor(
      .x,
      levels = c(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        -1,
        -2,
        -3,
        -8,
        -9
      ),
      labels = c(
        "Employers in large organisations",
        "Higher managerial and administrative occupations",
        "Higher professional occupations",
        "Lower professional and higher technical occupations",
        "Lower managerial and administrative occupations",
        "Higher supervisory occupations",
        "Intermediate occupations",
        "Employers in small establishments",
        "Own account workers",
        "Lower supervisory occupations",
        "Lower technical occupations",
        "Semi-routine occupations",
        "Routine occupations",
        "Never worked and long-term unemployed",
        "Full-time student",
        "Not classified or inadequately stated",
        "Not classifiable for other reasons",
        "Item not applicable",
        "Script error/information lost",
        "Not asked at the fieldwork stage/participated/interviewed",
        "Don’t know/insufficient information",
        "Refusal"
      )
    )
  )) %>%
  select(
    NSID,
    nssecma14,
    nssecpa14,
    nssecma15,
    nssecpa15,
    nssecma16,
    nssecpa16,
    nssecma17,
    nssecpa17,
    nssecma18,
    nssecpa18
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(nssec_parents_all, output_data_path, row.names = FALSE)