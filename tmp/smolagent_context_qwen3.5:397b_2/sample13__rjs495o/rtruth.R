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


# Merge all parental NS-SEC variables by NSID
nssec_parents_all <- reduce(list(S1, S2, S3, S4, S5), full_join, by = "NSID") %>%
  # Add '_raw' suffix to all 'nssec*' variable names for simpler re-coding & cross-checks
  rename_with(
    ~ stringr::str_c(.x, "_raw"),
    contains("nssec")
  )



# Fix labels
nssec_labels_core <- c(
  `Employers in large organisations` = 1,
  `Higher managerial occupations` = 2,
  `Higher professional traditional employee` = 3.1,
  `Higher professional new employee` = 3.2,
  `Higher professional traditional self emp` = 3.3,
  `Higher professional new self emp` = 3.4,
  `Lower professional traditional employee` = 4.1,
  `Lower professional new employee` = 4.2,
  `Lower professional traditional self emp` = 4.3,
  `Lower professional new self emp` = 4.4,
  `Lower managerial occupations` = 5,
  `Higher supervisory occupations` = 6,
  `Intermediate clerical and administrative` = 7.1,
  `Intermediate sales and service` = 7.2,
  `Intermediate technical and auxiliary` = 7.3,
  `Intermediate engineering` = 7.4,
  `Employers in small orgs non-professional` = 8.1,
  `Employers in small orgs agriculture` = 8.2,
  `Own account workers non professional` = 9.1,
  `Own account workers agriculture` = 9.2,
  `Lower supervisory occupations` = 10,
  `Lower technical craft` = 11.1,
  `Lower technical process operative` = 11.2,
  `Semi routine sales` = 12.1,
  `Semi routine services` = 12.2,
  `Semi routine technical` = 12.3,
  `Semi routine operative` = 12.4,
  `Semi routine agricultural` = 12.5,
  `Semi routine clerical` = 12.6,
  `Semi routine childcare` = 12.7,
  `Routine sales and service` = 13.1,
  `Routine production` = 13.2,
  `Routine technical` = 13.3,
  `Routine operative` = 13.4,
  `Routine agricultural` = 13.5,
  `Never worked` = 14.1,
  `Long-term unemployed` = 14.2,
  `Not working` = 14.3,
  `Full-time students` = 15,
  `Not classified or inadequately stated` = 16,
  `Not classifiable for other reasons` = 17
)


## Missing labels
nssec_parents_labels_missing <- c(
  "Missing - household data lost" = -999,
  "Parent not interviewed" = -99,
  "Parent not present" = -98,
  "Insufficient information" = -94
)

common_missing_labels <- c(
  "Item not applicable" = -1L,
  "Script error/information lost" = -2L,
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed" = -3L,
  "Data not available" = -5L,
  "Prefer not to say" = -7L,
  "Don’t know/insufficient information" = -8L,
  "Refusal" = -9L
)


nssec_parents_all <- nssec_parents_all %>%
  mutate(
    # Sweeps with common labels
    across(
      -NSID,
      ~ labelled(
        .x,
        ## Re-using nssec_labels_core as the occupation codes/labels are the same for parent variables as well.
        labels = c(nssec_parents_labels_missing, nssec_labels_core)
      )
    )
  )

# Function to harmonise values (handle decimals, recode missing codes)
recode_nssec_parents <- function(x) {
  case_when(
    floor(x) %in% 1:17 ~ floor(x),
    x == -999 ~ -2,
    x %in% c(-99, -98) | is.na(x) ~ -3,
    x == -94 ~ -8,
    TRUE ~ x
  )
}

# Apply recode and assign to derived variables
nssec_parents_rec <- nssec_parents_all %>%
  mutate(
    nssecma14 = recode_nssec_parents(nssecma14_raw),
    nssecpa14 = recode_nssec_parents(nssecpa14_raw),
    nssecma15 = recode_nssec_parents(nssecma15_raw),
    nssecpa15 = recode_nssec_parents(nssecpa15_raw),
    nssecma16 = recode_nssec_parents(nssecma16_raw),
    nssecpa16 = recode_nssec_parents(nssecpa16_raw),
    nssecma17 = recode_nssec_parents(nssecma17_raw),
    nssecpa17 = recode_nssec_parents(nssecpa17_raw),
    nssecma18 = recode_nssec_parents(nssecma18_raw),
    nssecpa18 = recode_nssec_parents(nssecpa18_raw)
  ) %>%
  mutate(
    across(
      c(starts_with("nssec") & !ends_with("raw")),
      ~ labelled(
        .x,
        labels = c(
          "Employers in large organisations" = 1,
          "Higher managerial and administrative occupations" = 2,
          "Higher professional occupations" = 3,
          "Lower professional and higher technical occupations" = 4,
          "Lower managerial and administrative occupations" = 5,
          "Higher supervisory occupations" = 6,
          "Intermediate occupations" = 7,
          "Employers in small establishments" = 8,
          "Own account workers" = 9,
          "Lower supervisory occupations" = 10,
          "Lower technical occupations" = 11,
          "Semi-routine occupations" = 12,
          "Routine occupations" = 13,
          "Never worked and long-term unemployed" = 14,
          "Full-time student" = 15,
          "Not classified or inadequately stated" = 16,
          "Not classifiable for other reasons" = 17,
          common_missing_labels
        )
      )
    )
  )


# Extract derived variables
nssec_parents_all <- nssec_parents_rec %>%
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
  ) %>%
  mutate(
    across(
      -NSID,
      ~ labelled::to_factor(.x, levels = "labels")
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(nssec_parents_all, output_data_path, row.names = FALSE)