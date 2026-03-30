library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

files <- c(
  "wave_one_lsype_young_person_2020.tab" = 14,
  "wave_four_lsype_young_person_2020.tab" = 17,
  "wave_six_lsype_young_person_2020.tab" = 19,
  "wave_seven_lsype_young_person_2020.tab" = 20,
  "ns8_2015_self_completion.tab" = 21,
  "ns9_2022_main_interview.tab" = 32
)

data_list <- map(names(files), function(file) {
  file_path <- paste0("data/input/", file)
  df <- read_delim(file_path, delim = "\t")
  df <- df %>% rename_with(~ tolower(.x))
  age <- files[file]

  if (file == "wave_six_lsype_young_person_2020.tab" && "w6sexualityyp" %in% colnames(df)) {
    df <- df %>% rename(sori19 = w6sexualityyp) %>%
      mutate(sori19 = case_when(
        sori19 == -97 ~ -9,
        sori19 == -92 ~ -9,
        sori19 == -91 ~ -1,
        sori19 == -1 ~ -8,
        TRUE ~ sori19
      ))
  } else if (file == "wave_seven_lsype_young_person_2020.tab" && "w7sexualityyp" %in% colnames(df)) {
    df <- df %>% rename(sori20 = w7sexualityyp) %>%
      mutate(sori20 = case_when(
        sori20 == -100 ~ -9,
        sori20 == -97 ~ -9,
        sori20 == -92 ~ -9,
        sori20 == -91 ~ -1,
        sori20 == -1 ~ -8,
        TRUE ~ sori20
      ))
  } else if (file == "ns8_2015_self_completion.tab" && "w8sexuality" %in% colnames(df)) {
    df <- df %>% rename(sori21 = w8sexuality) %>%
      mutate(sori21 = case_when(
        sori21 == -9 ~ -9,
        sori21 == -8 ~ -8,
        sori21 == -1 ~ -1,
        TRUE ~ sori21
      ))
  } else if (file == "ns9_2022_main_interview.tab" && "w9sori" %in% colnames(df)) {
    df <- df %>% rename(sori32 = w9sori) %>%
      mutate(sori32 = case_when(
        sori32 == -9 ~ -9,
        sori32 == -8 ~ -8,
        sori32 == -3 ~ -3,
        sori32 == -1 ~ -1,
        sori32 == 5 ~ -7,
        TRUE ~ sori32
      ))
  }
  df
})

merged_data <- data_list %>% reduce(full_join, by = "nsid")

cleaned_data <- merged_data %>% select(nsid, starts_with("sori"))

write_csv(cleaned_data, "data/output/cleaned_data.csv")