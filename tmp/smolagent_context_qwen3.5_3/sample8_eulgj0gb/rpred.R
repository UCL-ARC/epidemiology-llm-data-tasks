library(haven); library(dplyr); library(purrr); library(labelled); library(readr)
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge by NSID
data <- wave1 %>% select(NSID) %>%
  full_join(wave4 %>% select(NSID), by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")
print(paste("Merged:", nrow(data), "rows"))

# W8VCQU classification
w8_v45 <- c("W8VCQU0J", "W8VCQU0K")  # NVQ 4-5
w8_v13 <- c("W8VCQU0A","W8VCQU0B","W8VCQU0C","W8VCQU0D","W8VCQU0E","W8VCQU0F","W8VCQU0G","W8VCQU0H","W8VCQU0I","W8VCQU0L","W8VCQU0M","W8VCQU0N")
w8_vnone <- "W8VCQU0P"
w8_vother <- "W8VCQU0O"

# Map W8DHANVQH
map_w8_acad <- function(v) {
  if(is.na(v)) return(NA_real_)
  if(v %in% c(-9,-8,-1)) return(v)
  if(v %in% c(4,5)) return(0)
  if(v %in% c(1,2,3)) return(1)
  if(v==95) return(3)
  if(v==96) return(4)
  NA_real_
}

# Get W8 voc NVQ for row
get_w8voc <- function(r) {
  for(v in c(w8_v45,w8_v13,w8_vnone,w8_vother)) {
    if(v %in% names(r) && !is.na(r[[v]])) {
      if(r[[v]] %in% c(-9,-8)) return(-9)
      if(r[[v]] == -1) return(-1)
    }
  }
  for(v in w8_v45) if(v %in% names(r) && !is.na(r[[v]]) && r[[v]]==1) return(0)
  for(v in w8_v13) if(v %in% names(r) && !is.na(r[[v]]) && r[[v]]==1) return(1)
  if("W8VCQU0P" %in% names(r) && !is.na(r[["W8VCQU0P"]]) && r[["W8VCQU0P"]]==1) return(4)
  if("W8VCQU0O" %in% names(r) && !is.na(r[["W8VCQU0O"]]) && r[["W8VCQU0O"]]==1) return(3)
  NA_real_
}

print("Creating educ25...")
data$w8_acad <- sapply(data$W8DHANVQH, map_w8_acad)
data$w8_voc <- sapply(1:nrow(data), function(i) get_w8voc(data[i,]))

# Combine NVQ levels
comb_nvq <- function(a, v) {
  am <- !is.na(a) && a %in% c(-9,-8,-1)
  vm <- !is.na(v) && v %in% c(-9,-8,-1)
  if(am && vm) { vals <- c(a,v); vals <- vals[!is.na(vals)]; if(-9 %in% vals) return(-9); if(-8 %in% vals) return(-8); return(-1) }
  if(am) return(v)
  if(vm) return(a)
  if(is.na(a) && is.na(v)) return(NA_real_)
  if(is.na(a)) return(v)
  if(is.na(v)) return(a)
  min(a, v)
}

data$educ25_raw <- mapply(comb_nvq, data$w8_acad, data$w8_voc)
data$educ25_raw <- ifelse(is.na(data$educ25_raw), -3, data$educ25_raw)

lvls <- c(0,1,2,3,4,-1,-3,-8,-9)
labs <- c("NVQ 4-5 equivalent qualifications","NVQ 1-3 equivalent qualifications","Entry level or no qualifications","Other qualifications not mappable to NVQ framework","None of these qualifications","Not applicable","No answer","Don't know/Insufficient information","Refused")
data$educ25 <- factor(data$educ25_raw, levels=lvls, labels=labs)
print("educ25 done")

# educ32
print("Creating educ32...")
map_w9 <- function(v) {
  if(is.na(v)) return(NA_real_)
  if(v %in% c(-9,-8,-1)) return(v)
  if(v %in% c(4,5)) return(0)
  if(v %in% c(1,2,3)) return(1)
  if(v==0) return(2)
  if(v==95) return(3)
  if(v==96) return(4)
  NA_real_
}
data$w9_acad <- sapply(data$W9DANVQH, map_w9)
data$w9_voc <- sapply(data$W9DVNVQH, map_w9)
data$educ32_raw <- mapply(comb_nvq, data$w9_acad, data$w9_voc)
data$educ32_raw <- ifelse(is.na(data$educ32_raw), -3, data$educ32_raw)
data$educ32 <- factor(data$educ32_raw, levels=lvls, labels=labs)
print("educ32 done")

# educadtl32
print("Creating educadtl32...")
w9aq <- c("W9ACQU0A","W9ACQU0B","W9ACQU0C","W9ACQU0D","W9ACQU0E","W9ACQU0F","W9ACQU0G","W9ACQU0H","W9ACQU0I","W9ACQU0J","W9ACQU0K","W9ACQU0L","W9ACQU0M","W9ACQU0N","W9ACQU0O","W9ACQU0P","W9ACQU0Q","W9ACQU0R","W9ACQU0S","W9ACQU0T","W9ACQU0U","W9ACQU0V")
w9aq_lab <- c("Doctorate or equivalent","Masters or equivalent","Undergraduate or equivalent","Post-graduate Diplomas and Certificates","Diplomas in higher education and other higher education qualifications","Teaching qualifications for schools or further education (below degree level)","A/AS Levels or equivalent","Grade A-C, Level 4-9","Grade D-G, Level 1-3","SCE Higher","Scottish Certificate Sixth Year Studies","SCE Standard","National 4 and 5","National 2 and 3","Leaving Certificate","Junior Certificate grade A-C","Junior Certificate grade D and below","Other academic qualifications (including overseas)","None of these qualifications","Don't know","Refused","No answer")
names(w9aq_lab) <- w9aq

get_adtl <- function(r) {
  for(v in c("W9ACQU0T","W9ACQU0U","W9ACQU0V")) {
    if(v %in% names(r) && !is.na(r[[v]]) && r[[v]]==1) return(w9aq_lab[v])
  }
  for(v in w9aq) { if(v %in% names(r) && !is.na(r[[v]])) { if(r[[v]]==-3) return("Not asked at fieldwork stage"); if(r[[v]]==-1) return("Not applicable") } }
  for(v in w9aq[1:18]) { if(v %in% names(r) && !is.na(r[[v]]) && r[[v]]==1) return(w9aq_lab[v]) }
  if("W9ACQU0S" %in% names(r) && !is.na(r[["W9ACQU0S"]]) && r[["W9ACQU0S"]]==1) return("None of these qualifications")
  "None of these qualifications"
}
data$educadtl32 <- sapply(1:nrow(data), function(i) get_adtl(data[i,]))
print("educadtl32 done")

# educvdtl32
print("Creating educvdtl32...")
w9vq <- c("W9VCQU0A","W9VCQU0B","W9VCQU0C","W9VCQU0D","W9VCQU0E","W9VCQU0F","W9VCQU0G","W9VCQU0H","W9VCQU0I","W9VCQU0J","W9VCQU0K","W9VCQU0L","W9VCQU0M","W9VCQU0N","W9VCQU0O","W9VCQU0P","W9VCQU0Q","W9VCQU0R","W9VCQU0S","W9VCQU0T","W9VCQU0U","W9VCQU0V","W9VCQU0W","W9VCQU0X","W9VCQU0Y","W9VCQU0Z","W9VCQUAA","W9VCQUAB","W9VCQUAC","W9VCQUAD","W9VCQUAE","W9VCQUAF","W9VCQUAG","W9VCQUAH","W9VCQUAI")
w9vq_lab <- c("Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor","Nursing or other medical qualifications (below degree level)","Level 4 or 5","Level 3","Level 2","Level 1","GNVQ Advanced","GNVQ Intermediate","Level 3","Level 2","Level Foundation","Advanced Craft, Part III","Craft, Part II","Craft, Part I","Level 3","Level 2","Level 1","Advanced Diploma","Higher Diploma","RSA Diploma","RSA Stage I, II,III","Higher Level BTEC","BTEC National","BTEC First","SCOTVEC National Certificate","SCOTVEC first or general diploma","SCOTVEC general diploma","SCOTVEC modules","HND or  HNC","OND or ONCM","Junior certificate","Other vocational qualifications (including some overseas)","None of these qualifications","Don't know","Refused")
names(w9vq_lab) <- w9vq

get_vdtl <- function(r) {
  if("W9VCQUAH" %in% names(r) && !is.na(r[["W9VCQUAH"]]) && r[["W9VCQUAH"]]==1) return("Don't know")
  if("W9VCQUAI" %in% names(r) && !is.na(r[["W9VCQUAI"]]) && r[["W9VCQUAI"]]==1) return("Refused")
  for(v in w9vq) { if(v %in% names(r) && !is.na(r[[v]])) { if(r[[v]]==-3) return("Not asked at fieldwork stage"); if(r[[v]]==-1) return("Not applicable") } }
  for(v in w9vq[1:32]) { if(v %in% names(r) && !is.na(r[[v]]) && r[[v]]==1) return(w9vq_lab[v]) }
  if("W9VCQUAG" %in% names(r) && !is.na(r[["W9VCQUAG"]]) && r[["W9VCQUAG"]]==1) return("None of these qualifications")
  "None of these qualifications"
}
data$educvdtl32 <- sapply(1:nrow(data), function(i) get_vdtl(data[i,]))
print("educvdtl32 done")

# Final output
final_data <- data %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)
print(paste("Final:", nrow(final_data), "x", ncol(final_data)))
write_csv(final_data, "data/output/cleaned_data.csv")
print("Done!")
