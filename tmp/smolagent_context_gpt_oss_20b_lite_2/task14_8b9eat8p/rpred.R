library(readr)
library(dplyr)

# Function to map wave‑specific missing codes to the standard scheme
map_missing <- function(x) {
  x <- case_when(
    x %in% c(-999, -998, -997, -995, -100, -97) ~ -2,
    x == -94 ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -99 ~ -3,
    TRUE ~ x
  )
  ifelse(is.na(x), -3, x)
}

# Load all datasets
files <- list(
  wave_one   = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two   = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four  = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five  = "data/input/wave_five_lsype_family_background_2020.tab",
  wave_six   = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_main_interview.tab",
  wave_nine  = "data/input/ns9_2022_derived_variables.tab"
)

read_tab <- function(path) {
  read_delim(path, delim = "\t", col_types = cols(.default = "c"))
}

all_data <- lapply(files, read_tab)
merged <- Reduce(function(x, y) full_join(x, y, by = "NSID"), all_data)

# Identify tenure columns per wave
cols <- list(
  W1hous12HH = "wave_one",
  W2Hous12HH = "wave_two",
  W3hous12HH = "wave_three",
  W4Hous12HH = "wave_four",
  W5Hous12HH = "wave_five",
  W6Hous12YP = "wave_six",
  W7Hous12YP = "wave_seven",
  W8TENURE   = "wave_eight",
  W9DTENURE  = "wave_nine"
)

# Convert to numeric and harmonise missing values
for(col in names(cols)) {
  merged[[col]] <- as.numeric(merged[[col]])
  merged[[col]] <- map_missing(merged[[col]])
}

# Recoding functions
rec_detail_14_17 <- function(x) { x }
rec_detail_18_20 <- function(x) { x }
rec_collapsed_14_17 <- function(x) {
  case_when(
    x %in% c(1,2,3) ~ x,
    x %in% c(4,5,6) ~ 4,
    x == 7 ~ 5,
    x == 8 ~ 7,
    TRUE ~ x
  )
}
rec_collapsed_18_20 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 4,
    x == 3 ~ 7,
    TRUE ~ x
  )
}
rec_collapsed_25 <- function(x) { x }
rec_collapsed_32 <- function(x) { x }

# Create derived variables
merged <- merged %>%
  mutate(
    # Detailed, age‑specific variables (14‑17)
    hownteen14 = rec_detail_14_17(W1hous12HH),
    hownteen15 = rec_detail_14_17(W2Hous12HH),
    hownteen16 = rec_detail_14_17(W3hous12HH),
    hownteen17 = rec_detail_14_17(W4Hous12HH),
    # Detailed, age‑specific variables (18‑20)
    hownteen18 = rec_detail_18_20(W5Hous12HH),
    hownteen19 = rec_detail_18_20(W6Hous12YP),
    hownteen20 = rec_detail_18_20(W7Hous12YP),
    # Collapsed, age‑specific variables (14‑17)
    hown14  = rec_collapsed_14_17(W1hous12HH),
    hown15  = rec_collapsed_14_17(W2Hous12HH),
    hown16  = rec_collapsed_14_17(W3hous12HH),
    hown17  = rec_collapsed_14_17(W4Hous12HH),
    # Collapsed, age‑specific variables (18‑20)
    hown19  = rec_collapsed_18_20(W6Hous12YP),
    hown20  = rec_collapsed_18_20(W7Hous12YP),
    # Collapsed, ages 25 & 32 (no change to categories)
    hown25  = rec_collapsed_25(W8TENURE),
    hown32  = rec_collapsed_32(W9DTENURE)
  )

# Helper to create a labelled factor (plain factor with labels)
make_labelled_factor <- function(vec, levels, labels) {
  factor(vec, levels = levels, labels = labels)
}

# Define levels and labels for each construct
# Detailed 14‑17
lvl_d14_17 <- c(1:8, -9, -8, -1, -2, -3)
lbl_d14_17 <- c(
  "Owned outright",
  "Owned buying with mortgage",
  "Shared ownership",
  "Rented from council",
  "Rented from housing association",
  "Rented privately",
  "Rent free",
  "Some other arrangement",
  "Refused",
  "Don\'t know / insufficient information",
  "Not applicable",
  "Schedule not applicable",
  "Not asked"
)

# Detailed 18‑20
lvl_d18_20 <- c(1:3, -9, -8, -1, -2, -3)
lbl_d18_20 <- c(
  "Owned",
  "Rented",
  "Something else",
  "Refused",
  "Don\'t know / insufficient information",
  "Not applicable",
  "Schedule not applicable",
  "Not asked"
)

# Collapsed (1‑7)
lvl_collapsed <- c(1:7, -9, -8, -1, -2, -3)
lbl_collapsed <- c(
  "Owned outright",
  "Owned buying with mortgage",
  "Shared ownership",
  "Rent it",
  "Rent free",
  "Squatting",
  "Other",
  "Refused",
  "Don\'t know / insufficient information",
  "Not applicable",
  "Schedule not applicable",
  "Not asked"
)

# Apply labelled factors to derived variables
merged <- merged %>%
  mutate(
    hownteen14 = make_labelled_factor(hownteen14, lvl_d14_17, lbl_d14_17),
    hownteen15 = make_labelled_factor(hownteen15, lvl_d14_17, lbl_d14_17),
    hownteen16 = make_labelled_factor(hownteen16, lvl_d14_17, lbl_d14_17),
    hownteen17 = make_labelled_factor(hownteen17, lvl_d14_17, lbl_d14_17),
    hownteen18 = make_labelled_factor(hownteen18, lvl_d18_20, lbl_d18_20),
    hownteen19 = make_labelled_factor(hownteen19, lvl_d18_20, lbl_d18_20),
    hownteen20 = make_labelled_factor(hownteen20, lvl_d18_20, lbl_d18_20),
    hown14 = make_labelled_factor(hown14,  lvl_collapsed, lbl_collapsed),
    hown15 = make_labelled_factor(hown15,  lvl_collapsed, lbl_collapsed),
    hown16 = make_labelled_factor(hown16,  lvl_collapsed, lbl_collapsed),
    hown17 = make_labelled_factor(hown17,  lvl_collapsed, lbl_collapsed),
    hown19 = make_labelled_factor(hown19,  lvl_collapsed, lbl_collapsed),
    hown20 = make_labelled_factor(hown20,  lvl_collapsed, lbl_collapsed),
    hown25 = make_labelled_factor(hown25,  lvl_collapsed, lbl_collapsed),
    hown32 = make_labelled_factor(hown32,  lvl_collapsed, lbl_collapsed)
  )

# Keep only final derived variables and the ID
final_df <- merged %>% select(NSID, hownteen14:hownteen20, hown14:hown32)

# Write the cleaned file
write_csv(final_df, "data/output/cleaned_data.csv")
