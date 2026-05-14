library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

wave1 <- wave1 %>% select(NSID)

wave4_nssec <- wave4 %>% select(NSID, nssec17 = W4nsseccatYP)
wave4_nssec <- wave4_nssec %>% mutate(nssec17 = case_when(
    is.na(nssec17) ~ -3,
    nssec17 == -99 ~ -3,
    nssec17 == -91 ~ -1,
    TRUE ~ nssec17
)) %>% mutate(nssec17 = ifelse(nssec17 >= 0, floor(nssec17), nssec17))

wave5_nssec <- wave5 %>% select(NSID, nssec18 = W5nsseccatYP)
wave5_nssec <- wave5_nssec %>% mutate(nssec18 = case_when(
    is.na(nssec18) ~ -3,
    nssec18 == -91 ~ -1,
    TRUE ~ nssec18
)) %>% mutate(nssec18 = ifelse(nssec18 >= 0, floor(nssec18), nssec18))

wave6_nssec <- wave6 %>% select(NSID, nssec19 = w6nsseccatYP)
wave6_nssec <- wave6_nssec %>% mutate(nssec19 = case_when(
    is.na(nssec19) ~ -3,
    nssec19 == -91 ~ -1,
    TRUE ~ nssec19
)) %>% mutate(nssec19 = ifelse(nssec19 >= 0, floor(nssec19), nssec19))

wave7_nssec <- wave7 %>% select(NSID, nssec20 = W7NSSECCat)
wave7_nssec <- wave7_nssec %>% mutate(nssec20 = case_when(
    is.na(nssec20) ~ -3,
    nssec20 == -91 ~ -1,
    TRUE ~ nssec20
)) %>% mutate(nssec20 = ifelse(nssec20 >= 0, floor(nssec20), nssec20))

wave8_nssec <- wave8 %>% select(NSID, nssec25 = W8DNSSEC17, W8DACTIVITYC)
wave8_nssec <- wave8_nssec %>% mutate(nssec25 = case_when(
    is.na(nssec25) ~ -3,
    nssec25 == -9 ~ -9,
    nssec25 == -8 ~ -8,
    nssec25 == -1 ~ -1,
    TRUE ~ nssec25
)) %>% mutate(nssec25 = ifelse(nssec25 >= 0, floor(nssec25), nssec25)) %>%
mutate(nssec25 = case_when(
    !is.na(W8DACTIVITYC) & W8DACTIVITYC == 5.0 ~ 15,
    TRUE ~ nssec25
))

wave9_nssec <- wave9 %>% select(NSID, nssec32 = W9NSSEC)
wave9_nssec <- wave9_nssec %>% mutate(nssec32 = case_when(
    is.na(nssec32) ~ -3,
    nssec32 == -1 ~ -1,
    TRUE ~ nssec32
)) %>% mutate(nssec32 = ifelse(nssec32 >= 0, floor(nssec32), nssec32))

all_data <- wave1 %>%
  full_join(wave4_nssec, by = "NSID") %>%
  full_join(wave5_nssec, by = "NSID") %>%
  full_join(wave6_nssec, by = "NSID") %>%
  full_join(wave7_nssec, by = "NSID") %>%
  full_join(wave8_nssec, by = "NSID") %>%
  full_join(wave9_nssec, by = "NSID")

category_labels <- setNames(
  1:17,
  c(
    "Employers in large organisations",
    "Higher managerial and administrative occupations",
    "Higher professional occupations",
    "Lower professional and higher technical occupations",
    "Lower managerial and administrative occupations",
    "Higher supervisory occupations",
    "Intermediate occupations",
    "Employers in small establishments",
    "Own account workers",
    "Lower supervisory occupations",
    "Lower technical occupations",
    "Semi-routine occupations",
    "Routine occupations",
    "Never worked and Long-term unemployed",
    "Full-time students",
    "Occupations not stated or inadequately described",
    "Not classifiable for other reasons"
  )
)

missing_labels <- setNames(
  c(-9, -8, -7, -3, -2, -1),
  c(
    "Refusal",
    "Don't know/insufficient information",
    "Prefer not to say",
    "Not asked at the fieldwork stage/participated/interviewed",
    "Schedule not applicable/Script error/information lost",
    "Item not applicable"
  )
)

all_labels <- c(category_labels, missing_labels)

all_data <- all_data %>% mutate(
  nssec17 = haven::labelled(nssec17, labels = all_labels),
  nssec18 = haven::labelled(nssec18, labels = all_labels),
  nssec19 = haven::labelled(nssec19, labels = all_labels),
  nssec20 = haven::labelled(nssec20, labels = all_labels),
  nssec25 = haven::labelled(nssec25, labels = all_labels),
  nssec32 = haven::labelled(nssec32, labels = all_labels)
)

all_data <- all_data %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

write.csv(all_data, "data/output/cleaned_data.csv", row.names = FALSE)