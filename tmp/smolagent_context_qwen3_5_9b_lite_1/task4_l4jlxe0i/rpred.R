library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_self_completion.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

waves_data <- list()
for (name in names(files)) {
  filepath <- files[[name]]
  waves_data[[name]] <- read_delim(filepath, delim = "\t", col_types = cols(.default = col_character()))
}

cohort <- waves_data$wave1
cohort <- full_join(cohort, waves_data$wave4, by = "NSID")
cohort <- full_join(cohort, waves_data$wave6, by = "NSID")
cohort <- full_join(cohort, waves_data$wave7, by = "NSID")
cohort <- full_join(cohort, waves_data$wave8, by = "NSID")
cohort <- full_join(cohort, waves_data$wave9, by = "NSID")

cohort <- cohort %>%
  mutate(
    sori19 = case_when(
      W6SexualityYP %in% c(-999, -97) ~ -2L,
      W6SexualityYP %in% c(-92) ~ -9L,
      W6SexualityYP %in% c(-91) ~ -1L,
      W6SexualityYP %in% c(-1) ~ -8L,
      W6SexualityYP %in% c(1L, 2L, 3L, 4L) ~ as.integer(W6SexualityYP),
      TRUE ~ NA_integer_
    ),
    .keep = "all"
  )

cohort <- cohort %>%
  mutate(
    sori20 = case_when(
      W7SexualityYP %in% c(-100, -97) ~ -2L,
      W7SexualityYP %in% c(-92) ~ -9L,
      W7SexualityYP %in% c(-91) ~ -1L,
      W7SexualityYP %in% c(-1) ~ -8L,
      W7SexualityYP %in% c(1L, 2L, 3L, 4L) ~ as.integer(W7SexualityYP),
      TRUE ~ NA_integer_
    ),
    .keep = "all"
  )

cohort <- cohort %>%
  mutate(
    sori25 = case_when(
      W8SEXUALITY %in% c(-9, -8, -1) ~ as.integer(W8SEXUALITY),
      W8SEXUALITY %in% c(1L, 2L, 3L, 4L) ~ as.integer(W8SEXUALITY),
      TRUE ~ NA_integer_
    ),
    .keep = "all"
  )

cohort <- cohort %>%
  mutate(
    sori32 = case_when(
      W9SORI %in% c(-9, -8, -3, -1) ~ as.integer(W9SORI),
      W9SORI %in% c(1L, 2L, 3L, 4L, 5L) ~ as.integer(W9SORI),
      TRUE ~ NA_integer_
    ),
    .keep = "all"
  )

# Simple factor labels approach
valid_labels <- c(
  "Het" = "Heterosexual / Straight",
  "GL" = "Gay / Lesbian",
  "Bis" = "Bisexual",
  "Other" = "Other",
  "PN" = "Prefer not to say"
)

miss_labels <- c(
  "Refused" = "Refused",
  "DK" = "Don't know",
  "NAS" = "Not asked at fieldwork stage",
  "NAP" = "Not applicable",
  "SNA" = "Schedule not applicable"
)

cohort <- cohort %>%
  mutate(
    sori19 = factor(sori19, levels = c(1,2,3,4,-2,-9,-1,-8), 
                    labels = c(valid_labels["Het"], valid_labels["GL"], 
                             valid_labels["Bis"], valid_labels["Other"],
                             miss_labels["SNA"], miss_labels["Refused"],
                             miss_labels["NAP"], miss_labels["DK"])),
    sori20 = factor(sori20, levels = c(1,2,3,4,-2,-9,-1,-8), 
                    labels = c(valid_labels["Het"], valid_labels["GL"], 
                             valid_labels["Bis"], valid_labels["Other"],
                             miss_labels["SNA"], miss_labels["Refused"],
                             miss_labels["NAP"], miss_labels["DK"])),
    sori25 = factor(sori25, levels = c(1,2,3,4,-9,-8,-1), 
                    labels = c(valid_labels["Het"], valid_labels["GL"], 
                             valid_labels["Bis"], valid_labels["Other"],
                             miss_labels["Refused"], miss_labels["DK"],
                             miss_labels["NAP"])),
    sori32 = factor(sori32, levels = c(1,2,3,4,5,-9,-8,-3,-1), 
                    labels = c(valid_labels["Het"], valid_labels["GL"], 
                             valid_labels["Bis"], valid_labels["Other"],
                             valid_labels["PN"],
                             miss_labels["Refused"], miss_labels["DK"],
                             miss_labels["NAS"], miss_labels["NAP"]))
  )

final_vars <- c("NSID", "sori19", "sori20", "sori25", "sori32")
cohort_final <- cohort %>% select(all_of(final_vars))

write_csv(cohort_final, "data/output/cleaned_data.csv")

cat("Script completed successfully.\n")
cat(paste("Output written to: data/output/cleaned_data.csv\n"))
cat(paste("Total rows:", nrow(cohort_final), "\n"))
cat(paste("Total columns:", ncol(cohort_final), "\n"))