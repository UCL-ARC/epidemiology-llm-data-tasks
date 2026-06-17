# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# Helper function to map generic missing codes
map_missing <- function(x){
  x <- as.numeric(x)
  # Map specific negative codes to standard ones
  x[x %in% c(-999,-998,-997,-995)] <- -2
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x
}

# Load files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave_eight <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave_nine <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
full_df <- full_join(wave_one, wave_four, by = 'NSID') %>%
  full_join(wave_six, by = 'NSID') %>%
  full_join(wave_seven, by = 'NSID') %>%
  full_join(wave_eight, by = 'NSID') %>%
  full_join(wave_nine, by = 'NSID')

# -------------------------------------------------
# educaim17 from w4saim (age 17)
# -------------------------------------------------
full_df$w4saim <- map_missing(full_df$w4saim)
educaim17_map <- function(val){
  if(is.na(val)) return(-3)
  # NVQ1-3 equivalents (codes 1,3,4,5,7,9,10,11,12,13)
  if(val %in% c(1,3,4,5,7,9,10,11,12,13)) return(1)
  # Entry-level (codes 8,14,15)
  if(val %in% c(8,14,15)) return(2)
  # Other (codes 2,6,8,11,12,13) already covered
  # Not studying (16)
  if(val == 16) return(5)
  # Other category (2,6,8,11,12,13) -> 3
  if(val %in% c(2,6,8,11,12,13)) return(3)
  return(-3)
}
educaim17 <- sapply(full_df$w4saim, educaim17_map)

# -------------------------------------------------
# educaim19 from W6Saim (age 19)
# -------------------------------------------------
full_df$W6Saim <- map_missing(full_df$W6Saim)
educaim19_map <- function(val){
  if(is.na(val)) return(-3)
  if(val %in% c(1,2,3,4)) return(0)  # NVQ4-5
  if(val %in% c(5,6,7,8,9,10,11,12,13)) return(1)  # NVQ1-3
  if(val %in% c(11)) return(2)  # GCSE
  if(val %in% c(14,15)) return(3)  # Other/Unknown
  if(val == 16) return(5)  # Not studying
  return(-3)
}
educaim19 <- sapply(full_df$W6Saim, educaim19_map)

# -------------------------------------------------
# educaim20 from W7SAim (age 20)
# -------------------------------------------------
full_df$W7SAim <- map_missing(full_df$W7SAim)
educaim20_map <- function(val){
  if(is.na(val)) return(-3)
  if(val %in% c(10,11,12,13)) return(0)  # NVQ4-5
  if(val %in% c(1,2,3,5,6,7,8,9)) return(1)  # NVQ1-3
  if(val == 4) return(2)  # GCSE
  if(val == 14) return(3)  # Other
  if(val == -91) return(5)  # Not studying (not applicable)
  if(val == -94) return(-8)  # Don’t know / insufficient info
  if(val == -92) return(-9)  # Refusal
  return(-3)
}
educaim20 <- sapply(full_df$W7SAim, educaim20_map)

# -------------------------------------------------
# educaim25 from wave eight variables (age 25)
# -------------------------------------------------
# Activity variable
full_df$W8ACTIVITY05 <- map_missing(full_df$W8ACTIVITY05)
# Tick‑box columns (qualifications)
qual_cols <- c('W8ACQUC0A','W8ACQUC0B','W8ACQUC0C','W8ACQUC0D','W8ACQUC0E','W8ACQUC0F','W8ACQUC0G','W8ACQUC0H','W8ACQUC0I','W8ACQUC0J','W8ACQUC0K','W8ACQUC0L','W8ACQUC0M','W8ACQUC0N','W8ACQUC0O','W8ACQUC0P')
# Ensure columns exist
qual_cols <- qual_cols[qual_cols %in% names(full_df)]
# Map missing for tick boxes
for(col in qual_cols){ full_df[[col]] <- map_missing(full_df[[col]]) }

educaim25 <- numeric(nrow(full_df))
for(i in seq_len(nrow(full_df))){
  act <- full_df$W8ACTIVITY05[i]
  if(is.na(act) || act %in% c(-3,-1)){ educaim25[i] <- -3; next }
  if(act == 0){ educaim25[i] <- 5; next }
  if(act == 1){
    vals <- full_df[i, qual_cols] %>% unlist()
    # Priority 0: NVQ4-5 ticked (cols A,B,C)
    if(any(vals[c('W8ACQUC0A','W8ACQUC0B','W8ACQUC0C')] == 1, na.rm = TRUE)){ educaim25[i] <- 0; next }
    # Priority 1: NVQ1-3 ticked (cols D–P)
    if(any(vals[c('W8ACQUC0D','W8ACQUC0E','W8ACQUC0F','W8ACQUC0G','W8ACQUC0H','W8ACQUC0I','W8ACQUC0J','W8ACQUC0K','W8ACQUC0L','W8ACQUC0M','W8ACQUC0N','W8ACQUC0O','W8ACQUC0P')] == 1, na.rm = TRUE)){ educaim25[i] <- 1; next }
    # Priority 2: entry‑level ticked (L–P)
    if(any(vals[c('W8ACQUC0L','W8ACQUC0M','W8ACQUC0N','W8ACQUC0O','W8ACQUC0P')] == 1, na.rm = TRUE)){ educaim25[i] <- 2; next }
    # Priority 3: other ticked (any remaining 1)
    if(any(vals == 1, na.rm = TRUE)){ educaim25[i] <- 3; next }
    # Priority 4: none ticked
    if(all(is.na(vals) | vals == 0, na.rm = TRUE)){ educaim25[i] <- 4; next }
    # Only refusals (-9)
    if(all(vals %in% c(-9, NA), na.rm = TRUE)){ educaim25[i] <- -9; next }
    # Only don't knows (-8)
    if(all(vals %in% c(-8, NA), na.rm = TRUE)){ educaim25[i] <- -8; next }
    educaim25[i] <- -3
  } else {
    # For any other activity code (e.g., -9 or -8), map accordingly
    if(act == -9){ educaim25[i] <- -9 }
    else if(act == -8){ educaim25[i] <- -8 }
    else{ educaim25[i] <- -3 }
  }
}

# -------------------------------------------------
# educaim32 from wave nine variables (age 32)
# -------------------------------------------------
full_df$W9ECONACT2 <- map_missing(full_df$W9ECONACT2)
# Academic tick boxes (A–P)
ad_ids <- paste0('W9ACQUC0', LETTERS[1:16])
ad_ids <- ad_ids[ad_ids %in% names(full_df)]
# Vocational tick boxes (A–T)
voc_ids <- paste0('W9VCQUC0', LETTERS[1:20])
voc_ids <- voc_ids[voc_ids %in% names(full_df)]
# Map missing for tick boxes
for(col in c(ad_ids, voc_ids)){ full_df[[col]] <- map_missing(full_df[[col]]) }

educaim32 <- numeric(nrow(full_df))
for(i in seq_len(nrow(full_df))){
  act <- full_df$W9ECONACT2[i]
  if(is.na(act) || act %in% c(-3,-1)){ educaim32[i] <- -3; next }
  if(act %in% c(6,7)){  # studying
    vals_acad <- full_df[i, ad_ids] %>% unlist()
    vals_voc <- full_df[i, voc_ids] %>% unlist()
    vals_all <- c(vals_acad, vals_voc)
    # Priority 0: NVQ4-5 ticked (A–E)
    if(any(vals_acad[c('W9ACQUC0A','W9ACQUC0B','W9ACQUC0C','W9ACQUC0D','W9ACQUC0E')] == 1, na.rm = TRUE)){ educaim32[i] <- 0; next }
    # Priority 1: NVQ1-3 ticked (G,H,I)
    if(any(vals_acad[c('W9ACQUC0G','W9ACQUC0H','W9ACQUC0I')] == 1, na.rm = TRUE)){ educaim32[i] <- 1; next }
    # Priority 2: entry‑level ticked (L,M,N,O,P)
    if(any(vals_acad[c('W9ACQUC0L','W9ACQUC0M','W9ACQUC0N','W9ACQUC0O','W9ACQUC0P')] == 1, na.rm = TRUE)){ educaim32[i] <- 2; next }
    # Priority 3: other ticked (any 1 in all tick boxes)
    if(any(vals_all == 1, na.rm = TRUE)){ educaim32[i] <- 3; next }
    # Priority 4: none ticked
    if(all(is.na(vals_all) | vals_all == 0, na.rm = TRUE)){ educaim32[i] <- 4; next }
    # Only refusals (-9)
    if(all(vals_all %in% c(-9, NA), na.rm = TRUE)){ educaim32[i] <- -9; next }
    # Only don’t knows (-8)
    if(all(vals_all %in% c(-8, NA), na.rm = TRUE)){ educaim32[i] <- -8; next }
    educaim32[i] <- -3
  } else if(act %in% c(1:5,8:15)){
    educaim32[i] <- 5
  } else {
    if(act == -9){ educaim32[i] <- -9 }
    else if(act == -8){ educaim32[i] <- -8 }
    else{ educaim32[i] <- -3 }
  }
}

# -------------------------------------------------
# Construct final dataset
# -------------------------------------------------
final_df <- full_df %>% select(NSID) %>%
  mutate(educaim17 = educaim17,
         educaim19 = educaim19,
         educaim20 = educaim20,
         educaim25 = educaim25,
         educaim32 = educaim32)

# Write CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

cat('Script executed successfully.\n')
