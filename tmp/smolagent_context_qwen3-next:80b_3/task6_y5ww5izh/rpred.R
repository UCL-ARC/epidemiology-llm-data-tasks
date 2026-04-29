library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

wave2_selected <- wave2 %>% select(NSID, gor, urbind) %>% rename(gor15 = gor, urbind15 = urbind)
wave3_selected <- wave3 %>% select(NSID, gor, urbind) %>% rename(gor16 = gor, urbind16 = urbind)
wave8_selected <- wave8 %>% select(NSID, W8DGOR) %>% rename(gor25 = W8DGOR)
wave9_derived_selected <- wave9_derived %>% select(NSID, W9DRGN) %>% rename(gor32 = W9DRGN)
wave9_main_selected <- wave9_main %>% select(NSID, W9NATIONRES) %>% rename(nation32 = W9NATIONRES)

merged_data <- wave1 %>% select(NSID) %>%
  full_join(wave2_selected, by = "NSID") %>%
  full_join(wave3_selected, by = "NSID") %>%
  full_join(wave8_selected, by = "NSID") %>%
  full_join(wave9_derived_selected, by = "NSID") %>%
  full_join(wave9_main_selected, by = "NSID")

merged_data <- merged_data %>% mutate(across(everything(), ~ifelse(is.na(.), -3, .)))

merged_data <- merged_data %>% mutate(
  gor15 = case_when(gor15 == -94 ~ -8, gor15 %in% c(-999, -97, -100) ~ -3, TRUE ~ gor15),
  gor16 = case_when(gor16 == -94 ~ -8, gor16 %in% c(-999, -97, -100) ~ -3, TRUE ~ gor16),
  urbind15 = case_when(urbind15 == -94 ~ -8, urbind15 %in% c(-999, -97, -100) ~ -3, TRUE ~ urbind15),
  urbind16 = case_when(urbind16 == -94 ~ -8, urbind16 %in% c(-999, -97, -100) ~ -3, TRUE ~ urbind16),
  gor25 = case_when(gor25 %in% c(-97, -100) ~ -3, TRUE ~ gor25),
  gor32 = case_when(gor32 %in% c(-97, -100) ~ -3, TRUE ~ gor32),
  nation32 = case_when(nation32 %in% c(-97, -100) ~ -3, TRUE ~ nation32)
)

merged_data <- merged_data %>% mutate(
  uk_abroad32 = case_when(nation32 %in% c(1, 2, 3, 4) ~ 1, nation32 == 5 ~ 2, nation32 < 0 ~ nation32, TRUE ~ -3)
)

gor_labels_early <- c("Refused"=-9, "Insufficient information"=-8, "Not asked at fieldwork stage"=-3, "Not applicable"=-1, "North East"=1, "North West"=2, "Yorkshire and the Humber"=3, "East Midlands"=4, "West Midlands"=5, "East of England"=6, "London"=7, "South East"=8, "South West"=9)
gor_labels_late <- c("Refused"=-9, "Insufficient information"=-8, "Not asked at fieldwork stage"=-3, "Not applicable"=-1, "North East"=1, "North West"=2, "Yorkshire and the Humber"=3, "East Midlands"=4, "West Midlands"=5, "East of England"=6, "London"=7, "South East"=8, "South West"=9, "Wales"=10, "Scotland"=11, "Northern Ireland"=12, "Unknown due to faulty/missing postcode"=13)
urbind_labels <- c("Refused"=-9, "Insufficient information"=-8, "Not asked at fieldwork stage"=-3, "Not applicable"=-1, "Urban >= 10k - sparse"=1, "Town & Fringe - sparse"=2, "Village - sparse"=3, "Hamlet and Isolated Dwelling - sparse"=4, "Urban >= 10k - less sparse"=5, "Town & Fringe - less sparse"=6, "Village - less sparse"=7, "Hamlet & Isolated Dwelling"=8)
nation_labels <- c("Refused"=-9, "Don't know"=-8, "Not asked at fieldwork stage"=-3, "Not applicable"=-1, "England"=1, "Scotland"=2, "Wales"=3, "Northern Ireland"=4, "Outside of UK or unknown"=5)
uk_abroad_labels <- c("Refused"=-9, "Don't know"=-8, "Not asked at fieldwork stage"=-3, "Not applicable"=-1, "UK"=1, "Abroad"=2)

merged_data <- merged_data %>% mutate(
  gor15 = labelled(gor15, labels = gor_labels_early),
  gor16 = labelled(gor16, labels = gor_labels_early),
  gor25 = labelled(gor25, labels = gor_labels_late),
  gor32 = labelled(gor32, labels = gor_labels_late),
  urbind15 = labelled(urbind15, labels = urbind_labels),
  urbind16 = labelled(urbind16, labels = urbind_labels),
  nation32 = labelled(nation32, labels = nation_labels),
  uk_abroad32 = labelled(uk_abroad32, labels = uk_abroad_labels)
)

cleaned_data <- merged_data %>% select(NSID, gor15, gor16, gor25, gor32, urbind15, urbind16, nation32, uk_abroad32)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
cleaned_data_csv <- cleaned_data %>% mutate(across(where(is.labelled), ~as.numeric(.)))
write_csv(cleaned_data_csv, "data/output/cleaned_data.csv")

cat("Cleaned data successfully written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")
cat("Column names:", paste(names(cleaned_data), collapse = ", "), "\n")
