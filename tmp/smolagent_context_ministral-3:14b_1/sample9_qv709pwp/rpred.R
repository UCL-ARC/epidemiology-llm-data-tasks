library(dplyr)
library(readr)

# Load and process data
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Convert NSID to character
wave_one$NSID <- as.character(wave_one$NSID)
wave_two$NSID <- as.character(wave_two$NSID)
wave_four$NSID <- as.character(wave_four$NSID)

# Merge datasets
merged <- full_join(wave_one, wave_two, by = "NSID") %>%
  full_join(wave_four, by = "NSID")

# Create final dataset
result <- merged %>%
  select(NSID, W1hiqualmum, W2hiqualmum, w4hiqualmum, W1hiqualdad, W2hiqualdad, w4hiqualdad) %>%
  mutate(
    educdtlma = coalesce(w4hiqualmum, W2hiqualmum, W1hiqualmum),
    educdtlpa = coalesce(w4hiqualdad, W2hiqualdad, W1hiqualdad),
    educma = case_when(
      educdtlma %in% c(1, 2, 3, 4) ~ 0,
      educdtlma %in% c(5:17) ~ 1,
      educdtlma == 18 ~ 2,
      educdtlma == 19 ~ 3,
      educdtlma == 20 ~ 4,
      TRUE ~ educdtlma
    ),
    educpa = case_when(
      educdtlpa %in% c(1, 2, 3, 4) ~ 0,
      educdtlpa %in% c(5:17) ~ 1,
      educdtlpa == 18 ~ 2,
      educdtlpa == 19 ~ 3,
      educdtlpa == 20 ~ 4,
      TRUE ~ educdtlpa
    )
  ) %>%
  mutate_at(vars(educdtlma, educdtlpa, educma, educpa), ~ifelse(is.na(.), -3, .)) %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Create output directory and save file
if (!dir.exists("data/output")) dir.create("data/output")
write.csv(result, "data/output/cleaned_data.csv", row.names = FALSE)
TRUE