library(readr)
library(dplyr)
library(purrr)

# load function
load_tab <- function(file){
  read_delim(paste0('data/input/',file), delim='\t', col_types = cols(.default='c'))
}

# load files
wf4  <- load_tab('wave_four_lsype_young_person_2020.tab')
wf6  <- load_tab('wave_six_lsype_young_person_2020.tab')
wf7  <- load_tab('wave_seven_lsype_young_person_2020.tab')
wf8  <- load_tab('ns8_2015_main_interview.tab')
wf9  <- load_tab('ns9_2022_main_interview.tab')

# ensure key exists
required_key <- 'NSID'
frames <- list(wf4,wf6,wf7,wf8,wf9)
for(f in frames){ if(!(required_key %in% colnames(f))) stop('Key missing') }

# merge all with full_join
full <- frames %>% reduce(full_join, by=required_key)

# convert numeric columns
full <- full %>% mutate(
  w4saim = as.numeric(w4saim),
  W6Saim = as.numeric(W6Saim),
  W7SAim = as.numeric(W7SAim)
)

# map NVQ levels
educaim17 <- full %>% mutate(educaim17 = case_when(
  w4saim==9 ~ 1L,
  w4saim==5 ~ 2L,
  w4saim==1 ~ 3L,
  w4saim==6 ~ 4L,
  TRUE ~ NA_integer_
)) %>% pull(educaim17)

educaim19 <- full %>% mutate(educaim19 = case_when(
  W6Saim==12 ~ 1L,
  W6Saim==9 ~ 2L,
  W6Saim==5 ~ 3L,
  W6Saim==3 ~ 4L,
  W6Saim==1 ~ 5L,
  TRUE ~ NA_integer_
)) %>% pull(educaim19)

educaim20 <- full %>% mutate(educaim20 = case_when(
  W7SAim==1 ~ 1L,
  W7SAim==3 ~ 2L,
  W7SAim==6 ~ 3L,
  W7SAim==10 ~ 4L,
  W7SAim==13 ~ 5L,
  TRUE ~ NA_integer_
)) %>% pull(educaim20)

# binary qualifier helper
get_nvq_binary <- function(row, prefix){
  nvq5 <- paste0(prefix,'J')
  nvq4 <- paste0(prefix,'E')
  nvq3 <- paste0(prefix, LETTERS[2:6])
  nvq2 <- paste0(prefix,'G')
  nvq1 <- paste0(prefix,'A')
  if(any(row[nvq5]=='1', na.rm=TRUE)) return(5L)
  if(any(row[nvq4]=='1', na.rm=TRUE)) return(4L)
  if(any(row[nvq3]=='1', na.rm=TRUE)) return(3L)
  if(any(row[nvq2]=='1', na.rm=TRUE)) return(2L)
  if(any(row[nvq1]=='1', na.rm=TRUE)) return(1L)
  NA_integer_
}

educaim25 <- apply(full,1,function(r){ get_nvq_binary(r,'W8ACQUC0') })
educaim32 <- apply(full,1,function(r){ get_nvq_binary(r,'W9ACQUC0') })

clean <- full %>% select(NSID) %>% mutate(
  educaim17 = ifelse(is.na(educaim17),-3,educaim17),
  educaim19 = ifelse(is.na(educaim19),-3,educaim19),
  educaim20 = ifelse(is.na(educaim20),-3,educaim20),
  educaim25 = ifelse(is.na(educaim25),-3,educaim25),
  educaim32 = ifelse(is.na(educaim32),-3,educaim32)
)

write_csv(clean,'data/output/cleaned_data.csv')