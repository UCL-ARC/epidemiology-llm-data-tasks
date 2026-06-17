library(readr)
library(dplyr)
library(tidyr)
library(labelled)

input_dir <- "data/input"

# read files
wave_one <- read_delim(file.path(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_character()))
wave_four <- read_delim(file.path(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = cols(.default = col_character()))
ns8_main <- read_delim(file.path(input_dir, "ns8_2015_main_interview.tab"), delim = "\t", col_types = cols(.default = col_character()))
ns8_derived <- read_delim(file.path(input_dir, "ns8_2015_derived.tab"), delim = "\t", col_types = cols(.default = col_character()))
ns9_main <- read_delim(file.path(input_dir, "ns9_2022_main_interview.tab"), delim = "\t", col_types = cols(.default = col_character()))
ns9_derived <- read_delim(file.path(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = cols(.default = col_character()))

# merge by NSID
merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# helper to convert to numeric
num_vars <- c(
  "W8DHANVQH",
  paste0("W8VCQU0", LETTERS[1:18]),
  "W9DANVQH", "W9DVNVQH",
  paste0("W9ACQU0", LETTERS[1:20]),
  paste0("W9VCQU0", LETTERS[1:23])
)
merged <- merged %>% mutate(across(all_of(num_vars), as.numeric))

# ----- mapping for VW vocational to NVQ tier -----
vc_vars <- paste0("W8VCQU0", LETTERS[1:18])
vc_tier_map <- setNames(c(
  1, 1, 1, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 3, 3, -9, -9
), vc_vars)

# function to get NVQ collapsed tier from vocational tick
get_vc_tier <- function(val, varname){
  if(is.na(val)) return(NA)
  if(val %in% c(-9,-8,-1)) return(val)
  if(val==1){return(vc_tier_map[[varname]])}
  return(NA)
}

# compute educ25 collapsed
educ25_tier <- apply(merged, 1, function(row){
  # iterate over vars
  for(v in vc_vars){
    val <- as.numeric(row[[v]])
    tier <- get_vc_tier(val, v)
    if(!is.na(tier) && tier < 0){
      return(tier) # negative code wins
    }
    if(!is.na(tier)){
      return(tier) # first positive tier
    }
  }
  return(NA)
})

# academic NVQ collapsed mapping
map_academic_to_collapsed <- function(val){
  if(is.na(val)) return(NA)
  if(val %in% c(-9,-8,-1)) return(val)
  if(val %in% 1:3) return(1)
  if(val %in% 4:5) return(0)
  if(val==0) return(2)
  if(val==95) return(3)
  if(val==96) return(4)
  return(NA)
}
educ25_academic <- sapply(merged$W8DHANVQH, map_academic_to_collapsed)
# choose higher NVQ: lower numeric code unless negative
educ25_collapsed <- vector("numeric", nrow(merged))
for(i in seq_len(nrow(merged))){
  vt <- educ25_tier[i]
  at <- educ25_academic[i]
  if(!is.na(vt) && vt < 0) educ25_collapsed[i] <- vt
  else if(!is.na(at) && at < 0) educ25_collapsed[i] <- at
  else {
    tiers <- c(vt, at)
    tiers <- tiers[!is.na(tiers)]
    if(length(tiers)==0) educ25_collapsed[i] <- NA
    else educ25_collapsed[i] <- min(tiers)
  }
}
educ25_collapsed[is.na(educ25_collapsed)] <- -3

# ----- Educ32 collapsed -----
map_academic_to_collapsed32 <- function(val){
  if(is.na(val)) return(NA)
  if(val %in% c(-9,-8,-1)) return(val)
  if(val %in% 1:3) return(1)
  if(val %in% 4:5) return(0)
  if(val==0) return(2)
  if(val==95) return(3)
  if(val==96) return(4)
  return(NA)
}
educ32_academic <- sapply(merged$W9DANVQH, map_academic_to_collapsed32)
educ32_voc <- sapply(merged$W9DVNVQH, map_academic_to_collapsed32)
educ32_collapsed <- vector("numeric", nrow(merged))
for(i in seq_len(nrow(merged))){
  at <- educ32_academic[i]
  vt <- educ32_voc[i]
  if(!is.na(at) && at < 0) educ32_collapsed[i] <- at
  else if(!is.na(vt) && vt < 0) educ32_collapsed[i] <- vt
  else {
    tiers <- c(at, vt)
    tiers <- tiers[!is.na(tiers)]
    if(length(tiers)==0) educ32_collapsed[i] <- NA
    else educ32_collapsed[i] <- min(tiers)
  }
}
educ32_collapsed[is.na(educ32_collapsed)] <- -3

# ----- Educadtl32 ----
acad_vars <- paste0("W9ACQU0", LETTERS[1:20])
compute_acad_code <- function(row){
  code <- NULL
  last_sub <- 0
  for(v in acad_vars){
    val <- as.numeric(row[[v]])
    if(is.na(val)) next
    if(val %in% c(-3,-1)) next
    if(val == -8){code <- -8; return(code)}
    if(val == -9){code <- -9; return(code)}
    if(val == -2){code <- -2; return(code)}
    if(val == 1){last_sub <- last_sub + 1; return(last_sub)}
    if(val == 2){last_sub <- last_sub + 1}
  }
  if(last_sub == 0) return(-3)
  else return(last_sub + 1)
}
educadtl32_code <- apply(merged, 1, compute_acad_code)

# ----- Educvdtl32 -----
voc_vars <- paste0("W9VCQU0", LETTERS[1:23])
compute_voc_code <- function(row){
  code <- NULL
  last_sub <- 0
  for(v in voc_vars){
    val <- as.numeric(row[[v]])
    if(is.na(val)) next
    if(val %in% c(-3,-1)) next
    if(val == -8){code <- -8; return(code)}
    if(val == -9){code <- -9; return(code)}
    if(val == -2){code <- -2; return(code)}
    if(val == 1){last_sub <- last_sub + 1; return(last_sub)}
    if(val == 2){last_sub <- last_sub + 1}
  }
  if(last_sub == 0) return(-3)
  else return(last_sub + 1)
}
educvdtl32_code <- apply(merged, 1, compute_voc_code)

# final dataframe
final_df <- merged %>% select(NSID) %>%
  mutate(educ25 = educ25_collapsed,
         educ32 = educ32_collapsed,
         educadtl32 = educadtl32_code,
         educvdtl32 = educvdtl32_code)

# write output
output_dir <- "data/output"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
write_csv(final_df, file.path(output_dir, "cleaned_data.csv"))

cat("done")
