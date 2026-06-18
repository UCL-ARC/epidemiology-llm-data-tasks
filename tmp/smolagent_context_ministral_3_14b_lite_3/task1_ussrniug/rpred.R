# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the files to load
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "ns8_2015_main_interview.tab",
  "ns9_2022_main_interview.tab"
)

# Load each file into separate data frames
load_file <- function(file) {
  read_delim(paste0("data/input/", file), delim = "\t")
}

# Load all files
loaded_files <- map(files, load_file)

# Assign each loaded file to a named object for clarity
wave1 <- loaded_files[[1]]
wave2 <- loaded_files[[2]]
wave3 <- loaded_files[[3]]
wave4 <- loaded_files[[4]]
wave5 <- loaded_files[[5]]
wave6 <- loaded_files[[6]]
wave7 <- loaded_files[[7]]
wave8 <- loaded_files[[8]]
wave9 <- loaded_files[[9]]

# Merge all datasets by NSID
merged_data <- wave1
for (i in 2:length(loaded_files)) {
  merged_data <- merged_data %>%
    full_join(loaded_files[[i]], by = "NSID")
}

# Function to recode missing values
recode_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x %in% c(-999, -998, -997, -995)] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -1] <- -1
  return(x)
}

# Apply missing value recoding to each sex variable
sex_vars <- c("W1sexYP", "W2SexYP", "W3sexYP", "W4SexYP", "W5SexYP", "W6Sex", "W7Sex", "W8CMSEX", "W9DSEX")

for (var in sex_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- recode_missing(merged_data[[var]])
  }
}

# Derive consolidated sex variable
merged_data <- merged_data %>%
  mutate(
    sex = coalesce(
      ifelse(W9DSEX %in% c(1, 2), W9DSEX, NA),
      ifelse(W8CMSEX %in% c(1, 2), W8CMSEX, NA),
      ifelse(W7Sex %in% c(1, 2), W7Sex, NA),
      ifelse(W6Sex %in% c(1, 2), W6Sex, NA),
      ifelse(W5SexYP %in% c(1, 2), W5SexYP, NA),
      ifelse(W4SexYP %in% c(1, 2), W4SexYP, NA),
      ifelse(W3sexYP %in% c(1, 2), W3sexYP, NA),
      ifelse(W2SexYP %in% c(1, 2), W2SexYP, NA),
      ifelse(W1sexYP %in% c(1, 2), W1sexYP, NA),
      NA_real_
    )
  )

# Convert sex to a labelled factor
sex_levels <- c(-9, -8, -7, -3, -2, -1, 1, 2)
sex_labels <- c("Refusal", "Don't know", "Prefer not to say", "Not interviewed", 
                "Schedule not applicable", "Not applicable", "Male", "Female")

merged_data$sex <- factor(merged_data$sex, levels = sex_levels, labels = sex_labels)

# Select only NSID and the final derived sex variable
final_data <- merged_data %>% select(NSID, sex)

# Write the final output to CSV
write_csv(final_data, "data/output/cleaned_data.csv")