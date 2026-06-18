#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

df <- full_join(wave1, wave4, by = "NSID")
df <- full_join(df, wave6, by = "NSID")
df <- full_join(df, wave7, by = "NSID")
df <- full_join(df, ns8, by = "NSID")
df <- full_join(df, ns9, by = "NSID")

df$educaim17 <- case_when(
  df$w4saim %in% c(1, 5, 9) ~ 1,
  df$w4saim == 10 ~ 2,
  df$w4saim %in% c(4, 7, 11, 12, 13) ~ 3,
  df$w4saim %in% c(2, 3, 6, 8) ~ 4,
  df$w4saim == 14 ~ 5,
  df$w4saim %in% c(-999, -998, -997, -995) ~ -2,
  df$w4saim == -94 ~ -8,
  df$w4saim == -92 ~ -9,
  df$w4saim == -91 ~ -1,
  df$w4saim == -99 ~ -3,
  df$w4saim == -1 ~ -1,
  is.na(df$w4saim) ~ -3,
  TRUE ~ -3
)

df$educaim19 <- case_when(
  df$W6Saim %in% c(1, 2, 3, 4) ~ 0,
  df$W6Saim %in% c(5, 9, 12) ~ 1,
  df$W6Saim %in% c(8, 10, 13, 14, 15) ~ 3,
  df$W6Saim %in% c(6, 7, 11) ~ 4,
  df$W6Saim == 16 ~ 5,
  df$W6Saim %in% c(-999, -998, -997, -995) ~ -2,
  df$W6Saim == -94 ~ -8,
  df$W6Saim == -92 ~ -9,
  df$W6Saim == -91 ~ -1,
  df$W6Saim == -99 ~ -3,
  df$W6Saim == -1 ~ -1,
  is.na(df$W6Saim) ~ -3,
  TRUE ~ -3
)

df$educaim20 <- case_when(
  df$W7SAim %in% c(10, 11, 12, 13) ~ 0,
  df$W7SAim %in% c(1, 6, 3) ~ 1,
  df$W7SAim %in% c(2, 5, 9, 14) ~ 3,
  df$W7SAim %in% c(4, 7, 8) ~ 4,
  df$W7SAim == -91 ~ 5,
  df$W7SAim == -94 ~ -8,
  df$W7SAim %in% c(-999, -998, -997, -995) ~ -2,
  df$W7SAim == -92 ~ -9,
  df$W7SAim == -99 ~ -3,
  df$W7SAim == -1 ~ -1,
  is.na(df$W7SAim) ~ -3,
  TRUE ~ -3
)

recodew8 <- function(W8act, W8NVQ45, W8NVQ13, W8entry, W8other, W8none, W8dk, W8ref) {
  ifelse(!is.na(W8act) & W8act == 0, 5,
    ifelse(!is.na(W8act) & W8act == -9, -9,
      ifelse(!is.na(W8act) & W8act == -8, -8,
        ifelse(!is.na(W8act) & W8act == -1, -3,
          ifelse(is.na(W8act), -3,
            ifelse(W8NVQ45, 0,
              ifelse(W8NVQ13, 1,
                ifelse(W8entry, 2,
                  ifelse(W8other, 3,
                    ifelse(W8none, 4,
                      ifelse(W8dk, -8,
                        ifelse(W8ref, -9, -3)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

recodew9 <- function(W9act, W9NVQ45, W9NVQ13, W9entry, W9other, W9none, W9dk, W9ref) {
  ifelse(!is.na(W9act) & W9act %in% c(6, 7),
    ifelse(W9NVQ45, 0,
      ifelse(W9NVQ13, 1,
        ifelse(W9entry, 2,
          ifelse(W9other, 3,
            ifelse(W9none, 4,
              ifelse(W9dk, -8,
                ifelse(W9ref, -9, -3)
              )
            )
          )
        )
      )
    ),
    ifelse(!is.na(W9act) & W9act == -9, -9,
      ifelse(!is.na(W9act) & W9act == -8, -8,
        ifelse(!is.na(W9act) & W9act %in% c(-3, -1), -3,
          ifelse(is.na(W9act), -3, 5)
        )
      )
    )
  )
}

W8act <- df$W8ACTIVITY05
W8NVQ45 <- (!is.na(df$W8ACQUC0A) & df$W8ACQUC0A == 1) | (!is.na(df$W8ACQUC0B) & df$W8ACQUC0B == 1) | (!is.na(df$W8ACQUC0C) & df$W8ACQUC0C == 1) | (!is.na(df$W8VCQUC0J) & df$W8VCQUC0J == 1) | (!is.na(df$W8VCQUC0K) & df$W8VCQUC0K == 1)
W8NVQ13 <- (!is.na(df$W8VCQUC0A) & df$W8VCQUC0A == 1) | (!is.na(df$W8VCQUC0B) & df$W8VCQUC0B == 1) | (!is.na(df$W8VCQUC0C) & df$W8VCQUC0C == 1) | (!is.na(df$W8VCQUC0E) & df$W8VCQUC0E == 1)
W8entry <- !is.na(df$W8VCQUC0D) & df$W8VCQUC0D == 1
W8other <- (!is.na(df$W8ACQUC0F) & df$W8ACQUC0F == 1) | (!is.na(df$W8ACQUC0G) & df$W8ACQUC0G == 1) | (!is.na(df$W8ACQUC0H) & df$W8ACQUC0H == 1) | (!is.na(df$W8ACQUC0I) & df$W8ACQUC0I == 1) | (!is.na(df$W8ACQUC0J) & df$W8ACQUC0J == 1) | (!is.na(df$W8ACQUC0K) & df$W8ACQUC0K == 1) | (!is.na(df$W8ACQUC0L) & df$W8ACQUC0L == 1) | (!is.na(df$W8ACQUC0M) & df$W8ACQUC0M == 1) | (!is.na(df$W8ACQUC0N) & df$W8ACQUC0N == 1)
W8none <- !is.na(df$W8ACQUC0O) & df$W8ACQUC0O == 1
W8dk <- !is.na(df$W8ACQUC0P) & df$W8ACQUC0P == 1
W8ref <- !is.na(df$W8ACQUC0Q) & df$W8ACQUC0Q == 1

df$educaim25 <- recodew8(W8act, W8NVQ45, W8NVQ13, W8entry, W8other, W8none, W8dk, W8ref)

W9act <- df$W9ECONACT2
W9NVQ45 <- (!is.na(df$W9ACQUC0A) & df$W9ACQUC0A == 1) | (!is.na(df$W9ACQUC0B) & df$W9ACQUC0B == 1) | (!is.na(df$W9ACQUC0C) & df$W9ACQUC0C == 1) | (!is.na(df$W9ACQUC0D) & df$W9ACQUC0D == 1) | (!is.na(df$W9ACQUC0E) & df$W9ACQUC0E == 1) | (!is.na(df$W9VCQUC0C) & df$W9VCQUC0C == 1) | (!is.na(df$W9VCQUC0R) & df$W9VCQUC0R == 1) | (!is.na(df$W9VCQUC0S) & df$W9VCQUC0S == 1) | (!is.na(df$W9VCQUCAC) & df$W9VCQUCAC == 1)
W9NVQ13 <- (!is.na(df$W9ACQUC0I) & df$W9ACQUC0I == 1) | (!is.na(df$W9VCQUC0D) & df$W9VCQUC0D == 1) | (!is.na(df$W9VCQUC0I) & df$W9VCQUC0I == 1) | (!is.na(df$W9VCQUC0E) & df$W9VCQUC0E == 1) | (!is.na(df$W9VCQUC0J) & df$W9VCQUC0J == 1) | (!is.na(df$W9VCQUC0F) & df$W9VCQUC0F == 1) | (!is.na(df$W9VCQUC0P) & df$W9VCQUC0P == 1) | (!is.na(df$W9VCQUC0Q) & df$W9VCQUC0Q == 1) | (!is.na(df$W9VCQUC0O) & df$W9VCQUC0O == 1)
W9entry <- !is.na(df$W9VCQUC0K) & df$W9VCQUC0K == 1
W9other <- (!is.na(df$W9ACQUC0F) & df$W9ACQUC0F == 1) | (!is.na(df$W9ACQUC0G) & df$W9ACQUC0G == 1) | (!is.na(df$W9ACQUC0H) & df$W9ACQUC0H == 1) | (!is.na(df$W9ACQUC0J) & df$W9ACQUC0J == 1) | (!is.na(df$W9ACQUC0K) & df$W9ACQUC0K == 1) | (!is.na(df$W9ACQUC0L) & df$W9ACQUC0L == 1) | (!is.na(df$W9ACQUC0M) & df$W9ACQUC0M == 1) | (!is.na(df$W9ACQUC0N) & df$W9ACQUC0N == 1) | (!is.na(df$W9ACQUC0O) & df$W9ACQUC0O == 1) | (!is.na(df$W9ACQUC0P) & df$W9ACQUC0P == 1) | (!is.na(df$W9ACQUC0Q) & df$W9ACQUC0Q == 1) | (!is.na(df$W9ACQUC0R) & df$W9ACQUC0R == 1) | (!is.na(df$W9VCQUC0B) & df$W9VCQUC0B == 1) | (!is.na(df$W9VCQUC0G) & df$W9VCQUC0G == 1) | (!is.na(df$W9VCQUC0H) & df$W9VCQUC0H == 1) | (!is.na(df$W9VCQUC0M) & df$W9VCQUC0M == 1) | (!is.na(df$W9VCQUC0N) & df$W9VCQUC0N == 1) | (!is.na(df$W9VCQUC0T) & df$W9VCQUC0T == 1) | (!is.na(df$W9VCQUC0U) & df$W9VCQUC0U == 1) | (!is.na(df$W9VCQUC0V) & df$W9VCQUC0V == 1) | (!is.na(df$W9VCQUC0W) & df$W9VCQUC0W == 1) | (!is.na(df$W9VCQUC0X) & df$W9VCQUC0X == 1) | (!is.na(df$W9VCQUC0Y) & df$W9VCQUC0Y == 1) | (!is.na(df$W9VCQUC0Z) & df$W9VCQUC0Z == 1) | (!is.na(df$W9VCQUCAA) & df$W9VCQUCAA == 1) | (!is.na(df$W9VCQUCAB) & df$W9VCQUCAB == 1) | (!is.na(df$W9VCQUCAD) & df$W9VCQUCAD == 1) | (!is.na(df$W9VCQUCAE) & df$W9VCQUCAE == 1) | (!is.na(df$W9VCQUCAF) & df$W9VCQUCAF == 1)
W9none <- (!is.na(df$W9ACQUC0S) & df$W9ACQUC0S == 1) | (!is.na(df$W9VCQUCAG) & df$W9VCQUCAG == 1)
W9dk <- (!is.na(df$W9ACQUC0T) & df$W9ACQUC0T == 1) | (!is.na(df$W9VCQUCAH) & df$W9VCQUCAH == 1)
W9ref <- (!is.na(df$W9ACQUC0U) & df$W9ACQUC0U == 1) | (!is.na(df$W9VCQUCAI) & df$W9VCQUCAI == 1)

df$educaim32 <- recodew9(W9act, W9NVQ45, W9NVQ13, W9entry, W9other, W9none, W9dk, W9ref)

educaim_levels <- c("0", "1", "2", "3", "4", "5", "-1", "-2", "-3", "-8", "-9")
educaim_labels <- c("0"="NVQ 4-5 equivalent (higher / HE-level qualifications)", "1"="NVQ 1-3 equivalent (lower / mid-level qualifications)", "2"="None / entry level", "3"="Other (level unknown or unclassifiable)", "4"="None of these qualifications", "5"="Not currently studying", "-1"="Item not applicable", "-2"="Schedule not applicable / script error / information lost", "-3"="Not asked at the fieldwork stage / not interviewed", "-8"="Dont know / insufficient information", "-9"="Refusal")

for (var in c("educaim17", "educaim19", "educaim20", "educaim25", "educaim32")) {
  df[[var]] <- factor(df[[var]], levels = educaim_levels, labels = educaim_labels)
}

df <- df %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

write_csv(df, "data/output/cleaned_data.csv")

cat("Output saved to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df), "\n")
print(table(df$educaim17, useNA = "ifany"))
print(table(df$educaim19, useNA = "ifany"))
print(table(df$educaim20, useNA = "ifany"))
print(table(df$educaim25, useNA = "ifany"))
print(table(df$educaim32, useNA = "ifany"))

