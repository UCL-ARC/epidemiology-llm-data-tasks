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

S1 <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE) %>% select(NSID, hown14 = W1hous12HH)
S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>% select(NSID, hown15 = W2Hous12HH)
S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>% select(NSID, hown16 = W3hous12HH)
S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>% select(NSID, hown17 = W4Hous12HH)
S5 <- read_delim(file.path(data_path, "wave_five_lsype_family_background_2020.tab"), show_col_types = FALSE) %>% select(NSID, W5Hous12HH, W5Hous12BHH, W5Hous12CHH)
S6 <- read_delim(file.path(data_path, "wave_six_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, W6Hous12YP, W6Hous12bYP, W6Hous12cYP)
S7 <- read_delim(file.path(data_path, "wave_seven_lsype_young_person_2020.tab"), show_col_types = FALSE) %>% select(NSID, W7Hous12YP, W7Hous12bYP, W7Hous12cYP)
S8 <- read_delim(file.path(data_path, "ns8_2015_main_interview.tab"), show_col_types = FALSE) %>% select(NSID, hown25 = W8TENURE)
S9 <- read_delim(file.path(data_path, "ns9_2022_derived_variables.tab"), show_col_types = FALSE) %>% select(NSID, hown32 = W9DTENURE)

hown_all <- reduce(list(S1, S2, S3, S4, S5, S6, S7, S8, S9), full_join, by = "NSID")

# Derive harmonised variables
hown_all <- hown_all %>%
  mutate(
    # Detailed versions for S1-S7
    hownteen14 = case_when(
      hown14 > 0 ~ hown14,
      hown14 == -999 ~ -2,
      hown14 == -92 ~ -9,
      hown14 == -91 ~ -1,
      hown14 == -1 ~ -8,
      is.na(hown14) ~ -3
    ),
    hownteen15 = case_when(
      hown15 > 0 ~ hown15,
      hown15 %in% c(-998, -997, -995, -99) ~ -2,
      hown15 == -92 ~ -9,
      hown15 == -91 ~ -1,
      hown15 == -1 ~ -8,
      is.na(hown15) ~ -3
    ),
    hownteen16 = case_when(
      hown16 > 0 ~ hown16,
      hown16 == -999 ~ -2,
      hown16 == -92 ~ -9,
      hown16 == -91 ~ -1,
      hown16 == -1 ~ -8,
      is.na(hown16) ~ -3
    ),
    hownteen17 = case_when(
      hown17 > 0 ~ hown17,
      hown17 %in% c(-999, -997) ~ -2,
      hown17 == -92 ~ -9,
      hown17 == -91 ~ -1,
      hown17 == -1 ~ -8,
      is.na(hown17) ~ -3
    ),
    hownteen18 = case_when(
      W5Hous12BHH == 1 ~ 1, # Owned outright
      W5Hous12BHH == 2 ~ 2, # Being bought on a mortgage/bank loan
      W5Hous12BHH == 3 ~ 3, # Shared ownership (owns & rents property)
      W5Hous12CHH == 1 ~ 4, # Rented from a Council or New Town
      W5Hous12CHH == 2 ~ 5, # Rented from a Housing Association
      W5Hous12CHH == 3 ~ 6, # Rented privately
      W5Hous12CHH == 4 ~ 7, # Rent free
      W5Hous12HH == 3 | W5Hous12BHH == 4 | W5Hous12CHH == 5 ~ 8, # Other
      W5Hous12BHH %in% c(-999, -92) | W5Hous12CHH == -92 ~ -9,
      W5Hous12BHH == -91 | W5Hous12CHH == -91 ~ -1,
      W5Hous12BHH == -1 | W5Hous12CHH == -1 ~ -8,
      is.na(W5Hous12BHH) & is.na(W5Hous12CHH) ~ -3
    ),
    hownteen19 = case_when(
      W6Hous12bYP == 1 ~ 1,
      W6Hous12bYP == 2 ~ 2,
      W6Hous12bYP == 3 ~ 3,
      W6Hous12cYP == 1 ~ 4,
      W6Hous12cYP == 2 ~ 5,
      W6Hous12cYP == 3 ~ 6,
      W6Hous12cYP == 4 ~ 7,
      W6Hous12YP == 3 | W6Hous12bYP == 4 | W6Hous12cYP == 5 ~ 8,
      W6Hous12bYP %in% c(-999, -92) | W6Hous12cYP == -92 ~ -9,
      W6Hous12bYP == -91 | W6Hous12cYP == -91 ~ -1,
      W6Hous12bYP == -1 | W6Hous12cYP == -1 ~ -8,
      is.na(W6Hous12bYP) & is.na(W6Hous12cYP) ~ -3
    ),
    hownteen20 = case_when(
      W7Hous12bYP == 1 ~ 1,
      W7Hous12bYP == 2 ~ 2,
      W7Hous12bYP == 3 ~ 3,
      W7Hous12cYP == 1 ~ 4,
      W7Hous12cYP == 2 ~ 5,
      W7Hous12cYP == 3 ~ 6,
      W7Hous12cYP == 4 ~ 7,
      W7Hous12YP == 3 | W7Hous12bYP == 4 | W7Hous12cYP == 5 ~ 8,
      W7Hous12bYP %in% c(-999, -92) | W7Hous12cYP == -92 ~ -9,
      W7Hous12bYP == -91 | W7Hous12cYP == -91 ~ -1,
      W7Hous12bYP == -1 | W7Hous12cYP == -1 ~ -8,
      is.na(W7Hous12bYP) & is.na(W7Hous12cYP) ~ -3
    )
  ) %>%
  mutate(
    hown14 = case_when(
      hown14 == 1 ~ 1, # own outright
      hown14 == 2 ~ 2, # own, buying with help of mortgage/loan
      hown14 == 3 ~ 3, # part rent, part mortgage
      hown14 %in% 4:6 ~ 4, # rent it
      hown14 == 7 ~ 5, # live-in rent free
      hown14 == 8 ~ 6, # other
      hown14 == -999 ~ -2,
      hown14 == -92 ~ -9,
      hown14 == -91 ~ -1,
      hown14 == -1 ~ -8,
      is.na(hown14) ~ -3
    ),
    hown15 = case_when(
      hown15 == 1 ~ 1,
      hown15 == 2 ~ 2,
      hown15 == 3 ~ 3,
      hown15 %in% 4:6 ~ 4,
      hown15 == 7 ~ 5,
      hown15 == 8 ~ 6,
      hown15 %in% c(-998, -997, -995, -99) ~ -2,
      hown15 == -92 ~ -9,
      hown15 == -91 ~ -1,
      hown15 == -1 ~ -8,
      is.na(hown15) ~ -3
    ),
    hown16 = case_when(
      hown16 == 1 ~ 1,
      hown16 == 2 ~ 2,
      hown16 == 3 ~ 3,
      hown16 %in% 4:6 ~ 4,
      hown16 == 7 ~ 5,
      hown16 == 8 ~ 6,
      hown16 == -999 ~ -2,
      hown16 == -92 ~ -9,
      hown16 == -91 ~ -1,
      hown16 == -1 ~ -8,
      is.na(hown16) ~ -3
    ),
    hown17 = case_when(
      hown17 == 1 ~ 1,
      hown17 == 2 ~ 2,
      hown17 == 3 ~ 3,
      hown17 %in% 4:6 ~ 4,
      hown17 == 7 ~ 5,
      hown17 == 8 ~ 6,
      hown17 %in% c(-999, -997) ~ -2,
      hown17 == -92 ~ -9,
      hown17 == -91 ~ -1,
      hown17 == -1 ~ -8,
      is.na(hown17) ~ -3
    ),
    hown18 = case_when(
      W5Hous12BHH == 1 ~ 1,
      W5Hous12BHH == 2 ~ 2,
      W5Hous12BHH == 3 ~ 3,
      W5Hous12CHH %in% 1:3 ~ 4,
      W5Hous12CHH == 4 ~ 5,
      W5Hous12BHH == 4 | W5Hous12CHH == 5 ~ 6,
      W5Hous12BHH %in% c(-999, -92) | W5Hous12CHH == -92 ~ -9,
      W5Hous12BHH == -91 | W5Hous12CHH == -91 ~ -1,
      W5Hous12BHH == -1 | W5Hous12CHH == -1 ~ -8,
      is.na(W5Hous12BHH) & is.na(W5Hous12CHH) ~ -3
    ),
    hown19 = case_when(
      W6Hous12bYP == 1 ~ 1,
      W6Hous12bYP == 2 ~ 2,
      W6Hous12bYP == 3 ~ 3,
      W6Hous12cYP %in% 1:3 ~ 4,
      W6Hous12cYP == 4 ~ 5,
      W6Hous12bYP == 4 | W6Hous12cYP == 5 ~ 6,
      W6Hous12bYP %in% c(-999, -92) | W6Hous12cYP == -92 ~ -9,
      W6Hous12bYP == -91 | W6Hous12cYP == -91 ~ -1,
      W6Hous12bYP == -1 | W6Hous12cYP == -1 ~ -8,
      is.na(W6Hous12bYP) & is.na(W6Hous12cYP) ~ -3
    ),
    hown20 = case_when(
      W7Hous12bYP == 1 ~ 1,
      W7Hous12bYP == 2 ~ 2,
      W7Hous12bYP == 3 ~ 3,
      W7Hous12cYP %in% 1:3 ~ 4,
      W7Hous12cYP == 4 ~ 5,
      W7Hous12bYP == 4 | W7Hous12cYP == 5 ~ 6,
      W7Hous12bYP %in% c(-999, -92) | W7Hous12cYP == -92 ~ -9,
      W7Hous12bYP == -91 | W7Hous12cYP == -91 ~ -1,
      W7Hous12bYP == -1 | W7Hous12cYP == -1 ~ -8,
      is.na(W7Hous12bYP) & is.na(W7Hous12cYP) ~ -3
    ),
    hown25 = case_when(
      hown25 == 1 ~ 1,
      hown25 == 2 ~ 2,
      hown25 == 3 ~ 3,
      hown25 == 4 ~ 4,
      hown25 == 5 ~ 5,
      hown25 %in% 6:7 ~ 6,
      hown25 == -9 ~ -9,
      hown25 == -8 ~ -8,
      hown25 == -1 ~ -1,
      is.na(hown25) ~ -3
    ),
    hown32 = case_when(
      hown32 == 1 ~ 1,
      hown32 == 2 ~ 2,
      hown32 == 3 ~ 3,
      hown32 == 4 ~ 4,
      hown32 == 5 ~ 5,
      hown32 %in% 6:7 ~ 6,
      hown32 == -9 ~ -9,
      hown32 == -8 ~ -8,
      hown32 == -1 ~ -1,
      is.na(hown32) ~ -3
    )
  ) %>%
  mutate(
    across(
      c(
        hownteen14,
        hownteen15,
        hownteen16,
        hownteen17,
        hownteen18,
        hownteen19,
        hownteen20
      ),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -3, -8, -9),
        labels = c(
          "Owned outright",
          "Being bought on a mortgage/bank loan",
          "Shared ownership (owns & rents property)",
          "Rented from a Council or New Town",
          "Rented from a Housing Association",
          "Rented privately",
          "Rent free",
          "Some other arrangement",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    ),
    across(
      c(hown14, hown15, hown16, hown17, hown18, hown19, hown20, hown25, hown32),
      ~ factor(
        .x,
        levels = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -8, -9),
        labels = c(
          "Owned outright",
          "Owned, buying with help of mortgage/loan",
          "Spart rent, part mortgage",
          "Rent it",
          "live rent-free",
          "Other",
          "Item not applicable",
          "Script error/information lost",
          "Not asked at the fieldwork stage/participated/interviewed",
          "Don’t know/insufficient information",
          "Refusal"
        )
      )
    )
  ) %>%
  select(
    NSID,
    hown14,
    hown15,
    hown16,
    hown17,
    hown18,
    hown19,
    hown20,
    hown25,
    hown32,
    hownteen14,
    hownteen15,
    hownteen16,
    hownteen17,
    hownteen18,
    hownteen19,
    hownteen20
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output","output.csv")
write.csv(hown_all, output_data_path, row.names = FALSE)