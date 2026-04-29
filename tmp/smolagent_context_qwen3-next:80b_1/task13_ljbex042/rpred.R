library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

wave1 <- wave1 %>% select(NSID, W1nsseccatmum, W1nsseccatdad) %>% rename(nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)
wave1 <- wave1 %>% mutate(across(c(nssecma14, nssecpa14), ~ {
  case_when(
    . == -999 ~ -2,
    . == -94 ~ -8,
    . %in% c(-99, -98) ~ -3,
    is.na(.) ~ -3,
    . %in% c(-9, -8, -7, -3, -2, -1) ~ .,
    TRUE ~ {
      int_part <- as.integer(.)
      ifelse(int_part >= 1 & int_part <= 17, int_part, -3)
    }
  )
}))

wave2 <- wave2 %>% select(NSID, W2nsseccatmum, W2nsseccatdad) %>% rename(nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)
wave2 <- wave2 %>% mutate(across(c(nssecma15, nssecpa15), ~ {
  case_when(
    . == -999 ~ -2,
    . == -94 ~ -8,
    . %in% c(-99, -98) ~ -3,
    is.na(.) ~ -3,
    . %in% c(-9, -8, -7, -3, -2, -1) ~ .,
    TRUE ~ {
      int_part <- as.integer(.)
      ifelse(int_part >= 1 & int_part <= 17, int_part, -3)
    }
  )
}))

wave3 <- wave3 %>% select(NSID, W3cnsseccatmum, W3cnsseccatdad) %>% rename(nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)
wave3 <- wave3 %>% mutate(across(c(nssecma16, nssecpa16), ~ {
  case_when(
    . == -999 ~ -2,
    . == -94 ~ -8,
    . %in% c(-99, -98) ~ -3,
    is.na(.) ~ -3,
    . %in% c(-9, -8, -7, -3, -2, -1) ~ .,
    TRUE ~ {
      int_part <- as.integer(.)
      ifelse(int_part >= 1 & int_part <= 17, int_part, -3)
    }
  )
}))

wave4 <- wave4 %>% select(NSID, w4cnsseccatmum, w4cnsseccatdad) %>% rename(nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)
wave4 <- wave4 %>% mutate(across(c(nssecma17, nssecpa17), ~ {
  case_when(
    . == -999 ~ -2,
    . == -94 ~ -8,
    . %in% c(-99, -98) ~ -3,
    is.na(.) ~ -3,
    . %in% c(-9, -8, -7, -3, -2, -1) ~ .,
    TRUE ~ {
      int_part <- as.integer(.)
      ifelse(int_part >= 1 & int_part <= 17, int_part, -3)
    }
  )
}))

wave5 <- wave5 %>% select(NSID, w5Cnsseccatmum, w5Cnsseccatdad) %>% rename(nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)
wave5 <- wave5 %>% mutate(across(c(nssecma18, nssecpa18), ~ {
  case_when(
    . == -999 ~ -2,
    . == -94 ~ -8,
    . %in% c(-99, -98) ~ -3,
    is.na(.) ~ -3,
    . %in% c(-9, -8, -7, -3, -2, -1) ~ .,
    TRUE ~ {
      int_part <- as.integer(.)
      ifelse(int_part >= 1 & int_part <= 17, int_part, -3)
    }
  )
}))

merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

labels_vec <- c(
  "Employers in large organisations" = 1,
  "Higher managerial occupations" = 2,
  "Higher professional" = 3,
  "Lower professional" = 4,
  "Lower managerial occupations" = 5,
  "Higher supervisory occupations" = 6,
  "Intermediate" = 7,
  "Employers in small organisations" = 8,
  "Own account workers" = 9,
  "Lower supervisory occupations" = 10,
  "Lower technical" = 11,
  "Semi-routine" = 12,
  "Routine" = 13,
  "Never worked / Long-term unemployed" = 14,
  "Full-time students" = 15,
  "Not classified or inadequately stated" = 16,
  "Not classifiable for other reasons" = 17,
  "Refusal" = -9,
  "Don't know" = -8,
  "Prefer not to say" = -7,
  "Not asked/not interviewed" = -3,
  "Script error/information lost" = -2,
  "Not applicable" = -1
)

vars_to_label <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")
for (var in vars_to_label) {
  merged_data[[var]] <- labelled(merged_data[[var]], labels = labels_vec)
}

write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)