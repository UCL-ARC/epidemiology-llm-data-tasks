library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
df9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

df8 <- df8 %>% select(NSID, W8DINCB) %>% rename(inc25 = W8DINCB)
df9 <- df9 %>% select(NSID, W9DINCB) %>% rename(inc32 = W9DINCB)

df_merged <- full_join(df8, df9, by = "NSID")

df_merged$inc25[is.na(df_merged$inc25)] <- -3
df_merged$inc32[is.na(df_merged$inc32)] <- -3

labels_list <- c(
  "less than 25" = 1,
  "25 to 50" = 2,
  "50 to 90" = 3,
  "90 to 140" = 4,
  "140 to 240" = 5,
  "240 to 300" = 6,
  "300 to 350" = 7,
  "350 to 400" = 8,
  "400 to 500" = 9,
  "500 to 600" = 10,
  "600 to 700" = 11,
  "700 to 800" = 12,
  "800 to 900" = 13,
  "900 to 1200" = 14,
  "1200 to 1400" = 15,
  "more than 1400" = 16,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Item not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

df_merged$inc25 <- labelled(df_merged$inc25, labels = labels_list)
df_merged$inc32 <- labelled(df_merged$inc32, labels = labels_list)

write_csv(df_merged %>% select(NSID, inc25, inc32), "data/output/cleaned_data.csv")