library(readr)
library(dplyr)

# Recode negative missing values to standard codes
recode_missing <- function(x) {
  x_num <- as.numeric(x)
  mapping <- list('-999' = -2, '-998' = -2, '-997' = -2, '-995' = -2,
                   '-1' = -1, '-8' = -8, '-92' = -9, '-94' = -8,
                   '-9' = -9, '-99' = -3, '-91' = -1)
  res <- x_num
  for (i in seq_along(x_num)) {
    val <- x_num[i]
    if (!is.na(val) && val < 0) {
      key <- as.character(val)
      if (key %in% names(mapping)) res[i] <- mapping[[key]] else res[i] <- -3
    }
  }
  res
}

# Map full NS-SEC to 5 major categories
map_major_nssec <- function(vals) {
  sapply(vals, function(v) {
    if (is.na(v)) return(NA_real_)
    if (v %in% c(1,2,3,4,5,6,10)) return(1)
    if (v %in% c(5,6,10)) return(2)
    if (v %in% c(7,8,9)) return(3)
    if (v %in% c(11,12,13)) return(4)
    if (v %in% c(14,15,16)) return(5)
    if (v %in% c(3.1,3.2,3.3,3.4,4.1,4.2,4.3,4.4)) return(1)
    if (v %in% c(5.0,6.0,10.0)) return(2)
    if (v %in% c(7.1,7.2,7.3,7.4,8.1,8.2,9.1,9.2)) return(3)
    if (v %in% c(11.1,11.2,12.1,12.2,12.3,12.4,12.5,12.6,12.7,13.1,13.2,13.3,13.4,13.5)) return(4)
    if (v %in% c(14.1,14.2,14.3,15.0,16.0)) return(5)
    NA_real_
  })
}

read_wave <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(read_delim(path, delim='\t', col_types=cols(), na=c('','NA')), error=function(e) NULL)
  if (is.null(df) || nrow(df)==0) return(NULL)
  df
}

# Load waves
wave4 <- read_wave('data/input/wave_four_lsype_young_person_2020.tab')
wave5 <- read_wave('data/input/wave_five_lsype_young_person_2020.tab')
wave6 <- read_wave('data/input/wave_six_lsype_young_person_2020.tab')
wave7 <- read_wave('data/input/wave_seven_lsype_young_person_2020.tab')
wave8 <- read_wave('data/input/ns8_2015_derived.tab')
wave9 <- read_wave('data/input/ns9_2022_main_interview.tab')

# Process each wave
if (!is.null(wave4)) wave4 <- wave4 %>% mutate(nssec17_raw=recode_missing(W4nsseccatYP), nssec17=map_major_nssec(nssec17_raw))
if (!is.null(wave5)) wave5 <- wave5 %>% mutate(nssec18_raw=recode_missing(W5nsseccatYP), nssec18=map_major_nssec(nssec18_raw))
if (!is.null(wave6)) wave6 <- wave6 %>% mutate(nssec19_raw=recode_missing(w6nsseccatYP), nssec19=map_major_nssec(nssec19_raw))
if (!is.null(wave7)) wave7 <- wave7 %>% mutate(nssec20_raw=recode_missing(W7NSSECCat), nssec20=map_major_nssec(nssec20_raw))
if (!is.null(wave8)) wave8 <- wave8 %>% mutate(nssec25_raw=recode_missing(W8DNSSEC17), nssec25=map_major_nssec(nssec25_raw))
if (!is.null(wave9)) wave9 <- wave9 %>% mutate(nssec32_raw=recode_missing(W9NSSEC), nssec32=map_major_nssec(nssec32_raw))

make_empty <- function(var) data.frame(NSID=character(), setNames(data.frame(x=numeric()), var), stringsAsFactors=FALSE)

out4 <- if (!is.null(wave4)) wave4 %>% select(NSID, nssec17) else make_empty('nssec17')
out5 <- if (!is.null(wave5)) wave5 %>% select(NSID, nssec18) else make_empty('nssec18')
out6 <- if (!is.null(wave6)) wave6 %>% select(NSID, nssec19) else make_empty('nssec19')
out7 <- if (!is.null(wave7)) wave7 %>% select(NSID, nssec20) else make_empty('nssec20')
out8 <- if (!is.null(wave8)) wave8 %>% select(NSID, nssec25) else make_empty('nssec25')
out9 <- if (!is.null(wave9)) wave9 %>% select(NSID, nssec32) else make_empty('nssec32')

merged <- full_join(out4, out5, by='NSID') %>% full_join(., out6, by='NSID') %>% full_join(., out7, by='NSID') %>% full_join(., out8, by='NSID') %>% full_join(., out9, by='NSID')
merged[is.na(merged)] <- -3

write_csv(merged, 'data/output/cleaned_data.csv')
