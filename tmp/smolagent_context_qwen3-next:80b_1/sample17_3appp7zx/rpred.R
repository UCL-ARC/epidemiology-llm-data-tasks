library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

df1 <- wave1 %>% select(NSID)
df2 <- wave2 %>% select(NSID, imd15 = IMDRSCORE)
df3 <- wave3 %>% select(NSID, imd16 = IMDRSCORE)
df4 <- wave4 %>% select(NSID)
df9 <- ns9 %>% select(NSID, imd32 = W9DIMDD)

merged_df <- full_join(df1, df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID") %>%
  full_join(df9, by = "NSID")

merged_df <- merged_df %>%
  mutate(across(c(imd15, imd16, imd32), ~ ifelse(is.na(.), -3, .))) %>%
  mutate(across(c(imd15, imd16), ~ ifelse(. == -94, -8, .)))

merged_df <- merged_df %>%
  mutate(imd15 = labelled(imd15, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1)),
         imd16 = labelled(imd16, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1)),
         imd32 = labelled(imd32, labels = c("Refusal" = -9, "Don't know / insufficient information" = -8, "Not asked / not interviewed" = -3, "Script error / information lost" = -2, "Not applicable" = -1)))

cleaned_data <- merged_df %>% select(NSID, imd15, imd16, imd32)

write_csv(cleaned_data, "data/output/cleaned_data.csv")