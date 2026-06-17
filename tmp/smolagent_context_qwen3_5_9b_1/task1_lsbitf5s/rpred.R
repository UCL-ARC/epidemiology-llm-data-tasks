library(readr)
library(dplyr)

# Define file paths
input_dir <- "data/input"
output_dir <- "data/output"

files <- list(
  "wave_one_lsype_young_person_2020.tab" = file.path(input_dir, "wave_one_lsype_young_person_2020.tab"),
  "wave_two_lsype_young_person_2020.tab" = file.path(input_dir, "wave_two_lsype_young_person_2020.tab"),
  "wave_three_lsype_young_person_2020.tab" = file.path(input_dir, "wave_three_lsype_young_person_2020.tab"),
  "wave_four_lsype_young_person_2020.tab" = file.path(input_dir, "wave_four_lsype_young_person_2020.tab"),
  "wave_five_lsype_young_person_2020.tab" = file.path(input_dir, "wave_five_lsype_young_person_2020.tab"),
  "wave_six_lsype_young_person_2020.tab" = file.path(input_dir, "wave_six_lsype_young_person_2020.tab"),
  "wave_seven_lsype_young_person_2020.tab" = file.path(input_dir, "wave_seven_lsype_young_person_2020.tab"),
  "ns8_2015_main_interview.tab" = file.path(input_dir, "ns8_2015_main_interview.tab"),
  "ns9_2022_main_interview.tab" = file.path(input_dir, "ns9_2022_main_interview.tab")
)

# Load all files
load_list <- list()
for (i in seq_along(files)) {
  fname <- names(files)[i]
  fpath <- files[[i]]
  load_list[[fname]] <- read_delim(fpath, delim = "\t", col_types = cols(.default = "c"))
}

# Get base dataframes
wave1 <- load_list[["wave_one_lsype_young_person_2020.tab"]]
wave2 <- load_list[["wave_two_lsype_young_person_2020.tab"]]
wave3 <- load_list[["wave_three_lsype_young_person_2020.tab"]]
wave4 <- load_list[["wave_four_lsype_young_person_2020.tab"]]
wave5 <- load_list[["wave_five_lsype_young_person_2020.tab"]]
wave6 <- load_list[["wave_six_lsype_young_person_2020.tab"]]
wave7 <- load_list[["wave_seven_lsype_young_person_2020.tab"]]
wave8 <- load_list[["ns8_2015_main_interview.tab"]]
wave9 <- load_list[["ns9_2022_main_interview.tab"]]

# Create full dataset
full_data <- wave1
full_data <- full_data %>%
  full_join(wave2, by = "NSID")
full_data <- full_data %>%
  full_join(wave3, by = "NSID")
full_data <- full_data %>%
  full_join(wave4, by = "NSID")
full_data <- full_data %>%
  full_join(wave5, by = "NSID")
full_data <- full_data %>%
  full_join(wave6, by = "NSID")
full_data <- full_data %>%
  full_join(wave7, by = "NSID")
full_data <- full_data %>%
  full_join(wave8, by = "NSID")
full_data <- full_data %>%
  full_join(wave9, by = "NSID")

print("Full data created")
print("Columns:")
print(names(full_data))

# Convert all sex variables to numeric with proper missing code handling
convert_sex <- function(val) {
  if (is.na(val) || val %in% c(-999, -998, -997, -995, -99, -92, -91, -1, -8, -3)) return(NA)
  if (val %in% c(1, 2)) return(val)
  return(NA)
}

full_data <- full_data %>%
  mutate(
    W1sexYP_num = convert_sex(W1sexYP),
    W2SexYP_num = convert_sex(W2SexYP),
    W3sexYP_num = convert_sex(W3sexYP),
    W4SexYP_num = convert_sex(W4SexYP),
    W5SexYP_num = convert_sex(W5SexYP),
    W6Sex_num = convert_sex(W6Sex),
    W7Sex_num = convert_sex(W7Sex),
    W8CMSEX_num = convert_sex(W8CMSEX),
    W9DSEX_num = convert_sex(W9DSEX)
  ) %>%
  select(-ends_with("sex"), -ends_with("Sex"))

print("Sex variables converted")

# Derive consolidated sex variable (most recent valid first)
full_data <- full_data %>%
  mutate(
    sex = coalesce(
      W9DSEX_num,
      W8CMSEX_num,
      W7Sex_num,
      W6Sex_num,
      W5SexYP_num,
      W4SexYP_num,
      W3sexYP_num,
      W2SexYP_num,
      W1sexYP_num
    )
  )

print("Consolidated sex variable created")

# Create output directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Write output
write_csv(full_data, file.path(output_dir, "cleaned_data.csv"))
print("Output written to", file.path(output_dir, "cleaned_data.csv"))
print(head(full_data))
