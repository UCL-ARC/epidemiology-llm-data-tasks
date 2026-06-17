library(readr)
library(dplyr)
library(labelled)

# Helper: map missing codes to standard scheme
map_missing <- function(x){
  x_mapped <- case_when(
    x == -999 ~ -2,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x < 0 ~ -3,
    TRUE ~ x)
  x_mapped[is.na(x_mapped)] <- -3
  return(x_mapped)
}

# Helper: collapse detailed education to a 5‑level NVQ scheme
recode_nvq <- function(x){
  xm <- map_missing(x)
  nvq <- case_when(
    xm == 1 ~ 1,
    xm == 2 ~ 2,
    xm == 3 ~ 3,
    xm %in% c(4,9,14,17) ~ 4,
    xm %in% c(-9,-8,-7,-3,-2,-1,5:20) ~ 5,
    TRUE ~ xm)
  return(nvq)
}

input_path <- 'data/input/'
output_path <- 'data/output/'

w1 <- read_delim(paste0(input_path,'wave_one_lsype_family_background_2020.tab'), delim='\t', col_types=cols(.default=col_guess()))
w2 <- read_delim(paste0(input_path,'wave_two_lsype_family_background_2020.tab'), delim='\t', col_types=cols(.default=col_guess()))
w4 <- read_delim(paste0(input_path,'wave_four_lsype_family_background_2020.tab'), delim='\t', col_types=cols(.default=col_guess()))

names(w1) <- tolower(names(w1))
names(w2) <- tolower(names(w2))
names(w4) <- tolower(names(w4))

w1$nsid <- as.character(w1$nsid)
w2$nsid <- as.character(w2$nsid)
w4$nsid <- as.character(w4$nsid)

w1 <- w1 %>% mutate(educdtlma_w14=map_missing(w1hiqualmum), educma_w14=recode_nvq(w1hiqualmum))
w2 <- w2 %>% mutate(educdtlma_w15=map_missing(w2hiqualmum), educma_w15=recode_nvq(w2hiqualmum))
w4 <- w4 %>% mutate(educdtlma_w17=map_missing(w4hiqualmum), educma_w17=recode_nvq(w4hiqualmum))

w1 <- w1 %>% mutate(educdtlpa_w14=map_missing(w1hiqualdad), educpa_w14=recode_nvq(w1hiqualdad))
w2 <- w2 %>% mutate(educdtlpa_w15=map_missing(w2hiqualdad), educpa_w15=recode_nvq(w2hiqualdad))
w4 <- w4 %>% mutate(educdtlpa_w17=map_missing(w4hiqualdad), educpa_w17=recode_nvq(w4hiqualdad))

merged <- w1 %>% full_join(w2, by='nsid') %>% full_join(w4, by='nsid') %>% distinct(nsid, .keep_all=TRUE)

merged <- merged %>% mutate(
  educdtlma = coalesce(educdtlma_w14, educdtlma_w15, educdtlma_w17),
  educdtlpa = coalesce(educdtlpa_w14, educdtlpa_w15, educdtlpa_w17),
  educma    = coalesce(educma_w14,    educma_w15,    educma_w17),
  educpa    = coalesce(educpa_w14,    educpa_w15,    educpa_w17)
)

final_df <- merged %>% select(nsid, educdtlma, educdtlpa, educma, educpa)

write_csv(final_df, paste0(output_path,'cleaned_data.csv'))