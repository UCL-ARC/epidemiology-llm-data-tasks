# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define input directory
input_dir <- "data/input/"

# Helper function to read a file selecting only the NSID and required variables
read_wave <- function(file, cols_spec){
  read_delim(file.path(input_dir, file), delim = "\t", col_types = cols_spec)
}

# Read each file with only the needed columns
# Wave 1: W1alceverYP, W1alcmonYP
w1 <- read_wave("wave_one_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W1alceverYP = col_double(),
  W1alcmonYP  = col_double()
))
# Wave 2: W2alceverYP
w2 <- read_wave("wave_two_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W2alceverYP = col_double()
))
# Wave 3: W3alceverYP
w3 <- read_wave("wave_three_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W3alceverYP = col_double()
))
# Wave 4: W4AlcEverYP
w4 <- read_wave("wave_four_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W4AlcEverYP = col_double()
))
# Wave 6: W6AlcEverYP
w6 <- read_wave("wave_six_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W6AlcEverYP = col_double()
))
# Wave 7: W7AlcEverYP
w7 <- read_wave("wave_seven_lsype_young_person_2020.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W7AlcEverYP = col_double()
))
# Wave 8: W8AUDIT1
w8 <- read_wave("ns8_2015_self_completion.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W8AUDIT1 = col_double()
))
# Wave 9: W9AUDIT1
w9 <- read_wave("ns9_2022_main_interview.tab", cols = cols(
  .default = col_skip(),
  NSID = col_character(),
  W9AUDIT1 = col_double()
))

# Merge all datasets on NSID
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Helper to convert alcever variables to 1=Yes, 0=No, NA for others
convert_alcever <- function(x){
  ifelse(x == 1, 1, ifelse(x == 2, 0, NA_real_))
}

# Wave 1 special rule: both ever and in last 12 months must be 1 for Yes
age14_drink <- with(merged, {
  ifelse(
    !is.na(W1alceverYP) & !is.na(W1alcmonYP) & W1alceverYP == 1 & W1alcmonYP == 1,
    1,
    ifelse(
      !is.na(W1alceverYP) & !is.na(W1alcmonYP) & W1alceverYP == 2 & W1alcmonYP == 2,
      0,
      NA_real_
    )
  )
})

# Waves 2-4,6,7
age15_drink <- convert_alcever(merged$W2alceverYP)
age16_drink <- convert_alcever(merged$W3alceverYP)
age17_drink <- convert_alcever(merged$W4AlcEverYP)
age19_drink <- convert_alcever(merged$W6AlcEverYP)
age20_drink <- convert_alcever(merged$W7AlcEverYP)

# Waves 8 & 9: AUDIT1 >1 indicates drinking
age25_drink <- ifelse(merged$W8AUDIT1 > 1, 1, ifelse(merged$W8AUDIT1 == 1, 0, NA_real_))
age32_drink <- ifelse(merged$W9AUDIT1 > 1, 1, ifelse(merged$W9AUDIT1 == 1, 0, NA_real_))

# Combine indicators into a tibble
indicators <- tibble(
  age14 = age14_drink,
  age15 = age15_drink,
  age16 = age16_drink,
  age17 = age17_drink,
  age19 = age19_drink,
  age20 = age20_drink,
  age25 = age25_drink,
  age32 = age32_drink
)

# Compute earliest drinking age per person
alcfst_num <- indicators %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "age_col", values_to = "drink") %>%
  mutate(age = as.integer(gsub("age", "", age_col))) %>%
  group_by(row_id) %>%
  summarise(
    earliest = if(any(drink == 1, na.rm = TRUE)) {
      min(age[drink == 1])
    } else {
      if(all(!is.na(drink) & drink == 0)) {
        99
      } else {
        -8
      }
    }
  ) %>%
  ungroup()

# Combine with NSID
final_df <- merged %>%
  select(NSID) %>%
  mutate(row_id = row_number()) %>%
  left_join(alcfst_num, by = "row_id") %>%
  select(NSID, alcfst = earliest)

# Define factor levels and labels
levels_vec <- c(14,15,16,17,19,20,25,32,99,-8)
labels_vec <- c("Age 14","Age 15","Age 16","Age 17","Age 19","Age 20","Age 25","Age 32","Never had alcohol","Don\'t know/insufficient information")

final_df <- final_df %>%
  mutate(alcfst = factor(alcfst, levels = levels_vec, labels = labels_vec, ordered = FALSE))

# Write to CSV
output_path <- "data/output/cleaned_data.csv"
write_csv(final_df, output_path)

cat("Cleaning complete. Output written to", output_path, "\n")
