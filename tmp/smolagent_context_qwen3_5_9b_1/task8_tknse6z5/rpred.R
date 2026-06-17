library(haven)
library(dplyr)
library(tidyr)
library(readr)

if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

full_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

voc_tiers <- data.frame(var = c("W8VCQU0A","W8VCQU0B","W8VCQU0C","W8VCQU0D","W8VCQU0E","W8VCQU0F","W8VCQU0G","W8VCQU0H","W8VCQU0I","W8VCQU0J","W8VCQU0K","W8VCQU0L","W8VCQU0M","W8VCQU0N","W8VCQU0O","W8VCQU0P","W8VCQU0Q","W8VCQU0R"), tier = c(2,2,2,2,2,2,2,2,2,1,1,2,1,1,1,2,-8,-9), stringsAsFactors = FALSE)

for (i in 1:nrow(voc_tiers)) {
  v <- voc_tiers$var[i]; t <- voc_tiers$tier[i]
  full_data[[v]] <- as.integer(full_data[[v]])
  full_data[[v]] <- ifelse(full_data[[v]] %in% c(-9,-8), full_data[[v]], ifelse(full_data[[v]] %in% c(-1,0,1), t, full_data[[v]]))
}

full_data <- full_data %>% mutate(educ25_academic = case_when(W8DHANVQH %in% c(4,5) ~ 0, W8DHANVQH %in% c(1,2,3) ~ 1, W8DHANVQH == 95 ~ 3, W8DHANVQH == 96 ~ 4, TRUE ~ NA_integer_))

voc_subs <- c("W8VCQU0A","W8VCQU0B","W8VCQU0C","W8VCQU0D","W8VCQU0E","W8VCQU0F","W8VCQU0G","W8VCQU0H","W8VCQU0I","W8VCQU0J","W8VCQU0K","W8VCQU0L","W8VCQU0M","W8VCQU0N","W8VCQU0O","W8VCQU0P")

calc_educ25 <- function(row) {
  acad <- row["educ25_academic"]
  voc_vals <- row[voc_subs]
  all_valid <- c(acad[!is.na(acad) & acad >= 0 & acad <= 4], voc_vals[!is.na(voc_vals) & voc_vals >= 0 & voc_vals <= 4])
  if (length(all_valid) > 0) return(min(all_valid))
  else return(-3)
}

educ25_vec <- apply(full_data, 1, calc_educ25)
full_data$educ25 <- factor(educ25_vec, levels = c(0,1,2,3,4,-3,-9,-8,-1,-2))

full_data <- full_data %>% mutate(educ32 = case_when(
  is.na(W9DANVQH) & is.na(W9DVNVQH) ~ -3,
  is.na(W9DANVQH) & !is.na(W9DVNVQH) & W9DVNVQH >= 0 & W9DVNVQH %in% c(0:5,95,96) ~ W9DVNVQH,
  !is.na(W9DANVQH) & is.na(W9DVNVQH) & W9DANVQH >= 0 & W9DANVQH %in% c(0:5,95,96) ~ W9DANVQH,
  !is.na(W9DANVQH) & !is.na(W9DVNVQH) & W9DANVQH %in% c(0:5,95,96) & W9DVNVQH %in% c(0:5,95,96) ~ pmax(W9DANVQH, W9DVNVQH),
  TRUE ~ -3
))

full_data$educ32 <- factor(full_data$educ32, levels = c(0:5, 95, 96, -1, -2, -3, -8, -9))

academic_vars <- c("W9ACQU0A","W9ACQU0B","W9ACQU0C","W9ACQU0D","W9ACQU0E","W9ACQU0F","W9ACQU0G","W9ACQU0H","W9ACQU0I","W9ACQU0J","W9ACQU0K","W9ACQU0L","W9ACQU0M","W9ACQU0N","W9ACQU0O","W9ACQU0P","W9ACQU0Q","W9ACQU0R","W9ACQU0S","W9ACQU0T","W9ACQU0U","W9ACQU0V")
calc_educadtl32 <- function(row) {
  vals <- as.integer(row[academic_vars])
  yes_idx <- which(vals == 1)
  if (length(yes_idx) > 0) return(yes_idx[1])
  non_sub <- which(vals %in% c(-3,-1,-8,-9,-2))
  if (length(non_sub) > 0) return(-1)
  return(4)
}

educadtl32_vec <- apply(full_data, 1, calc_educadtl32)
full_data$educadtl32 <- factor(educadtl32_vec, levels = c(1,2,3,4,-1,-2,-3,-8,-9))

voc_vars_32 <- c("W9VCQU0A","W9VCQU0B","W9VCQU0C","W9VCQU0D","W9VCQU0E","W9VCQU0F","W9VCQU0G","W9VCQU0H","W9VCQU0I","W9VCQU0J","W9VCQU0K","W9VCQU0L","W9VCQU0M","W9VCQU0N","W9VCQU0O","W9VCQU0P","W9VCQU0Q","W9VCQU0R","W9VCQU0S","W9VCQU0T","W9VCQU0U","W9VCQU0V","W9VCQU0W","W9VCQU0X","W9VCQU0Y","W9VCQU0Z","W9VCQUAA","W9VCQUAB","W9VCQUAC","W9VCQUAD","W9VCQUAE","W9VCQUAF","W9VCQUAG","W9VCQUAH","W9VCQUAI")
calc_educvdtl32 <- function(row) {
  vals <- as.integer(row[voc_vars_32])
  yes_idx <- which(vals == 1)
  if (length(yes_idx) > 0) return(yes_idx[1])
  non_sub <- which(vals %in% c(-3,-1,-8,-9))
  if (length(non_sub) > 0) return(-1)
  return(37)
}

educvdtl32_vec <- apply(full_data, 1, calc_educvdtl32)
full_data$educvdtl32 <- factor(educvdtl32_vec, levels = c(1:37, -1, -2, -3, -8, -9))

write_csv(full_data %>% select(NSID, educ25, educ32, educadtl32, educvdtl32), "data/output/cleaned_data.csv")
cat("Done.\n")
