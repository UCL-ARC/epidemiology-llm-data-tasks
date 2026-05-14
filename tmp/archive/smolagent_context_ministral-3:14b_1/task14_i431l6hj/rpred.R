library(haven)
library(dplyr)
library(readr)

# Load all datasets
files <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab",
  wave5 = "data/input/wave_five_lsype_family_background_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_main_interview.tab",
  wave9 = "data/input/ns9_2022_derived_variables.tab"
)

# Read all files with explicit column types
data_list <- lapply(files, function(file) {
  read_delim(file, delim = "\t", 
              col_types = cols(
                NSID = col_character(),
                .default = col_double()
              ),
              show_col_types = FALSE)
})

# Merge all datasets
merged_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  merged_data <- full_join(merged_data, data_list[[i]], by = "NSID")
}

# Process wave 1 (age 14)
merged_data <- merged_data %>%
  mutate(
    hownteen14 = case_when(
      !is.na(W1hous12HH) ~ W1hous12HH,
      TRUE ~ -3
    ),
    hown14 = case_when(
      W1hous12HH == 1 ~ 1,
      W1hous12HH == 2 ~ 2,
      W1hous12HH == 3 ~ 3,
      W1hous12HH == 4 ~ 4,
      W1hous12HH == 5 ~ 4,
      W1hous12HH == 6 ~ 4,
      W1hous12HH == 7 ~ 5,
      W1hous12HH == 8 ~ 6,
      TRUE ~ -3
    )
  )

# Process wave 2 (age 15)
merged_data <- merged_data %>%
  mutate(
    hownteen15 = case_when(
      !is.na(W2Hous12HH) ~ W2Hous12HH,
      TRUE ~ -3
    ),
    hown15 = case_when(
      W2Hous12HH == 1 ~ 1,
      W2Hous12HH == 2 ~ 2,
      W2Hous12HH == 3 ~ 3,
      W2Hous12HH == 4 ~ 4,
      W2Hous12HH == 5 ~ 4,
      W2Hous12HH == 6 ~ 4,
      W2Hous12HH == 7 ~ 5,
      W2Hous12HH == 8 ~ 6,
      TRUE ~ -3
    )
  )

# Process wave 3 (age 16)
merged_data <- merged_data %>%
  mutate(
    hownteen16 = case_when(
      !is.na(W3hous12HH) ~ W3hous12HH,
      TRUE ~ -3
    ),
    hown16 = case_when(
      W3hous12HH == 1 ~ 1,
      W3hous12HH == 2 ~ 2,
      W3hous12HH == 3 ~ 3,
      W3hous12HH == 4 ~ 4,
      W3hous12HH == 5 ~ 4,
      W3hous12HH == 6 ~ 4,
      W3hous12HH == 7 ~ 5,
      W3hous12HH == 8 ~ 6,
      TRUE ~ -3
    )
  )

# Process wave 4 (age 17)
merged_data <- merged_data %>%
  mutate(
    hownteen17 = case_when(
      !is.na(W4Hous12HH) ~ W4Hous12HH,
      TRUE ~ -3
    ),
    hown17 = case_when(
      W4Hous12HH == 1 ~ 1,
      W4Hous12HH == 2 ~ 2,
      W4Hous12HH == 3 ~ 3,
      W4Hous12HH == 4 ~ 4,
      W4Hous12HH == 5 ~ 4,
      W4Hous12HH == 6 ~ 4,
      W4Hous12HH == 7 ~ 5,
      W4Hous12HH == 8 ~ 6,
      TRUE ~ -3
    )
  )

# Process wave 5 (age 18) - split variables
merged_data <- merged_data %>%
  mutate(
    hownteen18 = case_when(
      !is.na(W5Hous12HH) & W5Hous12HH == 1 & !is.na(W5Hous12BHH) ~ W5Hous12BHH,
      !is.na(W5Hous12HH) & W5Hous12HH == 1 & is.na(W5Hous12BHH) ~ -3,
      !is.na(W5Hous12HH) & W5Hous12HH == 2 & !is.na(W5Hous12CHH) ~ W5Hous12CHH + 3,
      !is.na(W5Hous12HH) & W5Hous12HH == 2 & is.na(W5Hous12CHH) ~ -3,
      !is.na(W5Hous12HH) & W5Hous12HH == 3 ~ 8,
      TRUE ~ -3
    ),
    hown18 = case_when(
      !is.na(W5Hous12HH) & W5Hous12HH == 1 & !is.na(W5Hous12BHH) ~ case_when(
        W5Hous12BHH == 1 ~ 1,
        W5Hous12BHH == 2 ~ 2,
        W5Hous12BHH == 3 ~ 3,
        TRUE ~ 6
      ),
      !is.na(W5Hous12HH) & W5Hous12HH == 2 & !is.na(W5Hous12CHH) ~ case_when(
        W5Hous12CHH == 1 ~ 4,
        W5Hous12CHH == 2 ~ 4,
        W5Hous12CHH == 3 ~ 4,
        W5Hous12CHH == 4 ~ 5,
        TRUE ~ 6
      ),
      TRUE ~ -3
    )
  )

# Process wave 6 (age 19) - split variables
merged_data <- merged_data %>%
  mutate(
    hownteen19 = case_when(
      !is.na(W6Hous12YP) & W6Hous12YP == 1 & !is.na(W6Hous12bYP) ~ W6Hous12bYP,
      !is.na(W6Hous12YP) & W6Hous12YP == 1 & is.na(W6Hous12bYP) ~ -3,
      !is.na(W6Hous12YP) & W6Hous12YP == 2 & !is.na(W6Hous12cYP) ~ W6Hous12cYP + 3,
      !is.na(W6Hous12YP) & W6Hous12YP == 2 & is.na(W6Hous12cYP) ~ -3,
      !is.na(W6Hous12YP) & W6Hous12YP == 3 ~ 8,
      TRUE ~ -3
    ),
    hown19 = case_when(
      !is.na(W6Hous12YP) & W6Hous12YP == 1 & !is.na(W6Hous12bYP) ~ case_when(
        W6Hous12bYP == 1 ~ 1,
        W6Hous12bYP == 2 ~ 2,
        W6Hous12bYP == 3 ~ 3,
        TRUE ~ 6
      ),
      !is.na(W6Hous12YP) & W6Hous12YP == 2 & !is.na(W6Hous12cYP) ~ case_when(
        W6Hous12cYP == 1 ~ 4,
        W6Hous12cYP == 2 ~ 4,
        W6Hous12cYP == 3 ~ 4,
        W6Hous12cYP == 4 ~ 5,
        TRUE ~ 6
      ),
      TRUE ~ -3
    )
  )

# Process wave 7 (age 20) - split variables
merged_data <- merged_data %>%
  mutate(
    hownteen20 = case_when(
      !is.na(W7Hous12YP) & W7Hous12YP == 1 & !is.na(W7Hous12bYP) ~ W7Hous12bYP,
      !is.na(W7Hous12YP) & W7Hous12YP == 1 & is.na(W7Hous12bYP) ~ -3,
      !is.na(W7Hous12YP) & W7Hous12YP == 2 & !is.na(W7Hous12cYP) ~ W7Hous12cYP + 3,
      !is.na(W7Hous12YP) & W7Hous12YP == 2 & is.na(W7Hous12cYP) ~ -3,
      !is.na(W7Hous12YP) & W7Hous12YP == 3 ~ 8,
      TRUE ~ -3
    ),
    hown20 = case_when(
      !is.na(W7Hous12YP) & W7Hous12YP == 1 & !is.na(W7Hous12bYP) ~ case_when(
        W7Hous12bYP == 1 ~ 1,
        W7Hous12bYP == 2 ~ 2,
        W7Hous12bYP == 3 ~ 3,
        TRUE ~ 6
      ),
      !is.na(W7Hous12YP) & W7Hous12YP == 2 & !is.na(W7Hous12cYP) ~ case_when(
        W7Hous12cYP == 1 ~ 4,
        W7Hous12cYP == 2 ~ 4,
        W7Hous12cYP == 3 ~ 4,
        W7Hous12cYP == 4 ~ 5,
        TRUE ~ 6
      ),
      TRUE ~ -3
    )
  )

# Process wave 8 (age 25)
merged_data <- merged_data %>%
  mutate(
    hownteen25 = case_when(
      !is.na(W8TENURE) ~ W8TENURE,
      TRUE ~ -3
    ),
    hown25 = case_when(
      W8TENURE == 1 ~ 1,
      W8TENURE == 2 ~ 2,
      W8TENURE == 3 ~ 3,
      W8TENURE == 4 ~ 4,
      W8TENURE == 5 ~ 5,
      W8TENURE == 6 ~ 6,
      W8TENURE == 7 ~ 6,
      TRUE ~ -3
    )
  )

# Process wave 9 (age 32)
merged_data <- merged_data %>%
  mutate(
    hownteen32 = case_when(
      !is.na(W9DTENURE) ~ W9DTENURE,
      TRUE ~ -3
    ),
    hown32 = case_when(
      W9DTENURE == 1 ~ 1,
      W9DTENURE == 2 ~ 2,
      W9DTENURE == 3 ~ 3,
      W9DTENURE == 4 ~ 4,
      W9DTENURE == 5 ~ 5,
      W9DTENURE == 6 ~ 6,
      W9DTENURE == 7 ~ 6,
      TRUE ~ -3
    )
  )

# Select only required variables
required_vars <- c("NSID")
ages <- c(14:20, 25, 32)
for (age in ages) {
  required_vars <- c(required_vars, paste0("hownteen", age), paste0("hown", age))
}

cleaned_data <- merged_data %>%
  select(all_of(required_vars))

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)