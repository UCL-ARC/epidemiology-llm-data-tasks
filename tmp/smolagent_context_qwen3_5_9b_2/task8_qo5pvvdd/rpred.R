library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

all_data <- wave1 %>% full_join(wave4, by = "NSID") %>% full_join(ns8_main, by = "NSID") %>% full_join(ns8_derived, by = "NSID") %>% full_join(ns9_main, by = "NSID") %>% full_join(ns9_derived, by = "NSID")

w8_ac_tier <- function(v) {
  if (!is.numeric(v) || length(v) != 1) return(NA_real_)
  if (is.na(v)) return(NA_real_)
  if (v == -9) return(-9)
  if (v == -8) return(-8)
  if (v == -1) return(NA_real_)
  if (v >= 1 && v <= 5) return(1)
  if (v == 95) return(0)
  if (v == 96) return(4)
  return(NA_real_)
}

all_data <- all_data %>% mutate(educ25_ac = w8_ac_tier(W8DHANVQH))

w8_vc_list <- c("W8VCQU0A","W8VCQU0B","W8VCQU0C","W8VCQU0D","W8VCQU0E","W8VCQU0F","W8VCQU0G","W8VCQU0H","W8VCQU0I","W8VCQU0J","W8VCQU0K","W8VCQU0L","W8VCQU0M","W8VCQU0N","W8VCQU0O","W8VCQU0P","W8VCQU0Q","W8VCQU0R")
w8_vc_tiers <- c(0,0,0,2,3,3,1,1,1,1,0,2,0,0,2,3,-8,-9)

voc_cols <- list()
for (i in seq_along(w8_vc_list)) {
  v <- w8_vc_list[i]
  t <- w8_vc_tiers[i]
  val <- all_data[[v]]
  if (t %in% c(-8,-9)) {
    all_data[[paste0("w8_vc_",v)]] <- ifelse(val == -9, -9, ifelse(val == -8, -8, NA_real_))
  } else if (is.na(t)) {
    all_data[[paste0("w8_vc_",v)]] <- ifelse(val == 1, 0, NA_real_)
  } else {
    all_data[[paste0("w8_vc_",v)]] <- ifelse(val == 1, t, NA_real_)
  }
  voc_cols[[v]] <- all_data[[paste0("w8_vc_",v)]]
}

w8_mv <- NA_real_
for (v in names(voc_cols)) {
  w8_mv <- pmax(w8_mv, voc_cols[[v]], na.rm = TRUE)
}

all_data <- all_data %>% mutate(educ25 = pmax(educ25_ac, w8_mv, na.rm = TRUE)) %>% select(-educ25_ac)

all_data <- all_data %>% mutate(w9_ac = W9DANVQH, w9_vc = W9DVNVQH)
all_data <- all_data %>% mutate(educ32_r = case_when(!is.na(w9_ac)&!is.na(w9_vc) ~ pmax(w9_ac,w9_vc,na.rm=TRUE), !is.na(w9_ac) ~ w9_ac, !is.na(w9_vc) ~ w9_vc, TRUE ~ NA_real_)) 
all_data <- all_data %>% mutate(educ32 = case_when(educ32_r %in% 1:5 ~ 1, educ32_r == 0 ~ 0, educ32_r == 95 ~ 0, educ32_r == 96 ~ 4, educ32_r == -9 ~ -9, educ32_r == -8 ~ -8, TRUE ~ NA_real_)) 
all_data <- all_data %>% select(-w9_ac, -w9_vc, -educ32_r)

w9_ac_vars <- c("W9ACQU0A","W9ACQU0B","W9ACQU0C","W9ACQU0D","W9ACQU0E","W9ACQU0F","W9ACQU0G","W9ACQU0H","W9ACQU0I","W9ACQU0J","W9ACQU0K","W9ACQU0L","W9ACQU0M","W9ACQU0N","W9ACQU0O","W9ACQU0P","W9ACQU0Q","W9ACQU0R","W9ACQU0S")
w9_ac_labels <- c("Doctorate or equivalent","Masters or equivalent","Undergraduate or equivalent","Post-graduate Diplomas and Certificates","Diplomas in higher education","Teaching qualifications for schools","A/AS Levels or equivalent","Grade A-C, Level 4-9","Grade D-G, Level 1-3","SCE Higher","Scottish Certificate Sixth Year Studies","SCE Standard","National 4 and 5","National 2 and 3","Leaving Certificate","Junior Certificate grade A-C","Junior Certificate grade D and below","Other academic qualifications","None of these qualifications")

all_data <- all_data %>% mutate(educadtl32 = 19, first = -999)
for (i in seq_along(w9_ac_vars)) {
  v <- w9_ac_vars[i]
  all_data <- all_data %>% mutate(temp = ifelse(!!sym(v) == 1 & first == -999, i, first))
}
all_data <- all_data %>% mutate(educadtl32 = case_when(first == -999 ~ 19, first == -3 ~ -3, first == -1 ~ -1, first == -9 ~ -9, TRUE ~ first)) %>% select(-temp, -first)

acad_labels_full <- c(w9_ac_labels, "Not asked", "Not applicable", "Refused")
all_data <- all_data %>% mutate(educadtl32 = factor(educadtl32, levels = c(1:19,-3,-1,-9), labels = acad_labels_full))

w9_vc_vars <- c("W9VCQU0A","W9VCQU0B","W9VCQU0C","W9VCQU0D","W9VCQU0E","W9VCQU0F","W9VCQU0G","W9VCQU0H","W9VCQU0I","W9VCQU0J","W9VCQU0K","W9VCQU0L","W9VCQU0M","W9VCQU0N","W9VCQU0O","W9VCQU0P","W9VCQU0Q","W9VCQU0R","W9VCQU0S","W9VCQU0T","W9VCQU0U","W9VCQU0V","W9VCQU0W","W9VCQU0X","W9VCQU0Y","W9VCQU0Z","W9VCQUAA","W9VCQUAB","W9VCQUAC","W9VCQUAD","W9VCQUAE","W9VCQUAF","W9VCQUAG")
w9_vc_labels <- c("Professional qualifications at degree level","Nursing or other medical qualifications","Level 4 or 5","Level 3","Level 2","Level 1","GNVQ Advanced","GNVQ Intermediate","Level 3","Level 2","Level Foundation","Advanced Craft, Part III","Craft, Part II","Craft, Part I","Level 3","Level 2","Level 1","Advanced Diploma","Higher Diploma","RSA Diploma","RSA Stage I, II,III","Higher Level BTEC","BTEC National","BTEC First","SCOTVEC National Certificate","SCOTVEC first or general diploma","SCOTVEC general diploma","SCOTVEC modules","HND or HNC","OND or ONCM","Junior certificate","Other vocational qualifications","None of these qualifications")

all_data <- all_data %>% mutate(educvdtl32 = 35, first = -999)
for (i in seq_along(w9_vc_vars)) {
  v <- w9_vc_vars[i]
  all_data <- all_data %>% mutate(temp = ifelse(!!sym(v) == 1 & first == -999, i, first))
}
all_data <- all_data %>% mutate(educvdtl32 = case_when(first == -999 ~ 35, first == -3 ~ -3, first == -9 ~ -9, first == -8 ~ -8, TRUE ~ first)) %>% select(-temp, -first)

vc_labels_full <- c(w9_vc_labels, "Not asked", "Refused")
all_data <- all_data %>% mutate(educvdtl32 = factor(educvdtl32, levels = c(1:35,-3,-9), labels = vc_labels_full))

all_data <- all_data %>% mutate(educ25 = factor(educ25, levels = c(0,1,2,3,4,-9,-8,-1), labels = c("NVQ 4-5 equivalent","NVQ 1-3 equivalent","Entry level or no qualifications","Other qualifications not mappable","None of these qualifications","Refused","Don\'t know","Not applicable"))) %>% mutate(educ32 = factor(educ32, levels = c(0,1,2,3,4,-9,-8,-1), labels = c("NVQ 4-5 equivalent","NVQ 1-3 equivalent","Entry level or no qualifications","Other qualifications not mappable","None of these qualifications","Refused","Missing information","Not applicable")))

write_csv(all_data, "data/output/cleaned_data.csv")
cat("Done\n")
