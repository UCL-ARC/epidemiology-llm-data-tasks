library(readr)
library(dplyr)

# Function to convert negative to -3
convert_missing <- function(x){x[x<0] <- -3; x}

# Map for each wave
map_w4 <- function(x){case_when(x==14~0,
                               x%in%c(1,2,4)~3,
                               x%in%c(5,6,7)~2,
                               x%in%c(3,8,9,10,11,12)~1,
                               x==13~-3,
                               TRUE~-3)}
map_w6 <- function(x){case_when(x==16~0,
                               x%in%c(1,2)~5,
                               x%in%c(3,4)~4,
                               x%in%c(5,6,8)~3,
                               x==9~2,
                               x%in%c(10,7)~2,
                               x%in%c(11,12,13)~1,
                               TRUE~-3)}
map_w7 <- function(x){case_when(x==14~0,
                               x==-91~0,
                               x==-94~-8,
                               x==1~1,
                               x==2~1,
                               x==3~2,
                               x==4~1,
                               x==5~2,
                               x==6~3,
                               x==7~1,
                               x==8~3,
                               x==9~3,
                               x==10~4,
                               x==11~5,
                               x==12~4,
                               x==13~5,
                               TRUE~-3)}
map_w8 <- function(df){act <- df$w8activity05; act[act<0] <- -3; case_when(act%in%c(6,7)~1, act%in%c(0,1)~0, TRUE~-3)}
map_w9 <- function(df){nvq5_vars <- c('w9acquc0a','w9acquc0b','w9acquc0c','w9vcquc0a');
                    nvq4_vars <- c('w9acquc0d','w9acquc0e');
                    nvq3_vars <- c('w9acquc0f','w9acquc0g','w9acquc0h','w9acquc0i','w9acquc0j','w9acquc0k','w9acquc0l','w9acquc0m');
                    all_vars <- c(nvq5_vars,nvq4_vars,nvq3_vars);
                    for(v in all_vars) if(v %in% names(df)) df[[v]][df[[v]]<0] <- NA;
                    nvq5_present <- rowSums(df[nvq5_vars]==1, na.rm=TRUE) > 0;
                    nvq4_present <- rowSums(df[nvq4_vars]==1, na.rm=TRUE) > 0;
                    nvq3_present <- rowSums(df[nvq3_vars]==1, na.rm=TRUE) > 0;
                    qual_any <- rowSums(df[all_vars]==1, na.rm=TRUE) > 0;
                    res <- NA_real_;
                    res[nvq5_present] <- 5;
                    res[nvq4_present & !nvq5_present] <- 4;
                    res[nvq3_present & !nvq5_present & !nvq4_present] <- 3;
                    nvq1_present <- qual_any & !nvq5_present & !nvq4_present & !nvq3_present;
                    res[nvq1_present] <- 1;
                    none_present <- !(nvq5_present | nvq4_present | nvq3_present | qual_any);
                    res[none_present] <- 0;
                    all_na <- rowSums(is.na(df[all_vars]), na.rm=FALSE) == length(all_vars);
                    res[all_na] <- -3;
                    res}

# Define file paths
file_paths <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  ns8 = 'data/input/ns8_2015_main_interview.tab',
  ns9 = 'data/input/ns9_2022_main_interview.tab')

# Load and lowercase column names
for(name in names(file_paths)){
  df <- read_delim(file_paths[[name]], delim='\t', show_col_types=FALSE)
  names(df) <- tolower(names(df))
  assign(name, df)
}

# Derive variables
wave_four$educaim17  <- map_w4(wave_four$w4saim)
wave_six$educaim19  <- map_w6(wave_six$w6saim)
wave_seven$educaim20 <- map_w7(wave_seven$w7saim)
ns8$educaim25       <- map_w8(ns8)
ns9$educaim32       <- map_w9(ns9)

# Merge
merged <- wave_one %>%
  full_join(select(wave_four, nsid, educaim17), by='nsid') %>%
  full_join(select(wave_six, nsid, educaim19), by='nsid') %>%
  full_join(select(wave_seven, nsid, educaim20), by='nsid') %>%
  full_join(select(ns8, nsid, educaim25), by='nsid') %>%
  full_join(select(ns9, nsid, educaim32), by='nsid')

output <- merged %>% select(nsid, educaim17, educaim19, educaim20, educaim25, educaim32)
if (!dir.exists('data/output')) dir.create('data/output', recursive=TRUE)
write_csv(output, 'data/output/cleaned_data.csv')
