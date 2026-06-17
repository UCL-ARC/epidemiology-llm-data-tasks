library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

collapse_nssec <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x %in% c(3.1, 3.2, 3.3, 3.4) ~ 3,
    x %in% c(4.1, 4.2, 4.3, 4.4) ~ 4,
    x == 5 ~ 5,
    x == 6 ~ 6,
    x %in% c(7.1, 7.2, 7.3, 7.4) ~ 7,
    x %in% c(8.1, 8.2) ~ 8,
    x %in% c(9.1, 9.2) ~ 9,
    x == 10 ~ 10,
    x %in% c(11.1, 11.2) ~ 11,
    x %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7) ~ 12,
    x %in% c(13.1, 13.2, 13.3, 13.4, 13.5) ~ 13,
    x %in% c(14.1, 14.2, 14.3) ~ 14,
    x == 15 ~ 15,
    x == 16 ~ 16,
    x == 17 ~ 17,
    TRUE ~ NA_real_
  )
}

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

data <- full_join(wave1, wave4, by = 'NSID')
data <- full_join(data, wave5, by = 'NSID')
data <- full_join(data, wave6, by = 'NSID')
data <- full_join(data, wave7, by = 'NSID')
data <- full_join(data, wave8, by = 'NSID')
data <- full_join(data, wave9, by = 'NSID')

code_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -2
  x[x == -995] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x == -100] <- -2
  x[x == -97] <- -2
  x[x == -9] <- -9
  x[x == -8] <- -8
  x[x == -7] <- -7
  x[x == -3] <- -3
  x[x == -2] <- -2
  x[x == -1] <- -1
  x
}

data$W4nsseccatYP <- code_missing(data$W4nsseccatYP)
data$W5nsseccatYP <- code_missing(data$W5nsseccatYP)
data$w6nsseccatYP <- code_missing(data$w6nsseccatYP)
data$W7NSSECCat <- code_missing(data$W7NSSECCat)
data$W8DNSSEC17 <- code_missing(data$W8DNSSEC17)
data$W9NSSEC <- code_missing(data$W9NSSEC)

data$nssec17 <- collapse_nssec(data$W4nsseccatYP)
data$nssec18 <- collapse_nssec(data$W5nsseccatYP)
data$nssec19 <- collapse_nssec(data$w6nsseccatYP)
data$nssec20 <- collapse_nssec(data$W7NSSECCat)
data$nssec25 <- collapse_nssec(data$W8DNSSEC17)
data$nssec32 <- collapse_nssec(data$W9NSSEC)

data$nssec17[is.na(data$nssec17)] <- -3
data$nssec18[is.na(data$nssec18)] <- -3
data$nssec19[is.na(data$nssec19)] <- -3
data$nssec20[is.na(data$nssec20)] <- -3
data$nssec25[is.na(data$nssec25)] <- -3
data$nssec32[is.na(data$nssec32)] <- -3

output <- data %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

write_csv(output, 'data/output/cleaned_data.csv')

cat('Successfully created cleaned_data.csv\n')