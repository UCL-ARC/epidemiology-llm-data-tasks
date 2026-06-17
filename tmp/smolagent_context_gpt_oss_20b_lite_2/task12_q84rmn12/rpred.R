library(readr)
library(dplyr)
library(tidyr)
library(labelled)

read_file <- function(path){
  tryCatch(read_delim(path, delim="\t", show_col_types = FALSE), error=function(e) tibble())
}

files_names <- c(
  "wave_one_lsype_young_person_2020.tab",
  "ns9_2022_main_interview.tab",
  "ns8_2015_derived.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab"
)
files <- lapply(files_names, function(f) read_file(file.path("data/input", f)))
names(files) <- sub(".tab$", "", files_names)

# keep only those with NSID
valid_files <- lapply(files, function(df){ if("NSID" %in% names(df)) df else tibble(NSID=character()) })

merged <- Reduce(function(x,y) full_join(x,y, by="NSID"), valid_files)

# vectorised recode function
recode_nssec <- function(x){
  case_when(
    x == 1.0 ~ 1,
    x == 2.0 ~ 2,
    x %in% c(3.1,3.2,3.3,3.4) ~ 3,
    x %in% c(4.1,4.2,4.3,4.4) ~ 4,
    x == 5.0 ~ 5,
    x == 6.0 ~ 6,
    x %in% c(7.1,7.2,7.3,7.4) ~ 7,
    x %in% c(8.1,8.2) ~ 8,
    x %in% c(9.1,9.2) ~ 9,
    x == 10.0 ~ 10,
    x %in% c(11.1,11.2) ~ 11,
    x %in% c(12.1,12.2,12.3,12.4,12.5,12.6,12.7) ~ 12,
    x %in% c(13.1,13.2,13.3,13.4,13.5) ~ 13,
    x %in% c(14.1,14.2,14.3) ~ 14,
    x == 15.0 ~ 15,
    x == 16.0 ~ 16,
    x == 17.0 ~ 17,
    TRUE ~ NA_real_
  )
}

vars_needed <- c("W4nsseccatYP","W5nsseccatYP","w6nsseccatYP","W7NSSECCat","W8DNSSEC17","W9NSSEC")
cols_to_keep <- c("NSID", vars_needed)
merged <- merged %>% select(any_of(cols_to_keep))

for(v in vars_needed){
  if(v %in% names(merged)){
    merged[[v]] <- recode_nssec(merged[[v]])
    merged[[v]][is.na(merged[[v]])] <- -3
  }
}

write_csv(merged, "data/output/cleaned_data.csv")
cat("Done\n")
