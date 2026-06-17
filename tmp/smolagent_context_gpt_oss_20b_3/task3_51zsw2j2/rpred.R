library(readr)
library(dplyr)

# Load files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim='\t', col_types=cols(.default='c'), show_col_types=FALSE)
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim='\t', col_types=cols(.default='c'), show_col_types=FALSE)
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim='\t', col_types=cols(.default='c'), show_col_types=FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim='\t', col_types=cols(.default='c'), show_col_types=FALSE)

# Function to map codes
map_lang_codes <- function(x){
  num <- as.numeric(x)
  num[is.na(num)] <- -3
  num[num==-999] <- -2
  num[num==-998] <- -2
  num[num==-997] <- -2
  num[num==-995] <- -2
  num[num==-99]  <- -3
  num[num==-92]  <- -9
  num[num==-91]  <- -1
  num[num==-94]  <- -2
  num[num==-1]  <- -8
  return(num)
}

# Map variables per wave
w1 <- w1 %>% mutate(W1englangYP_mapped = map_lang_codes(W1englangYP))
w2 <- w2 %>% mutate(W2EnglangYP_mapped = map_lang_codes(W2EnglangYP))
w3 <- w3 %>% mutate(W3englangHH_mapped = map_lang_codes(W3englangHH))
w4 <- w4 %>% mutate(W4EngLangHH_mapped = map_lang_codes(W4EngLangHH))

# Merge
merged <- w1 %>% full_join(w2, by='NSID') %>% full_join(w3, by='NSID') %>% full_join(w4, by='NSID')

# Derive consolidated lang
merged <- merged %>% mutate(lang = coalesce(W1englangYP_mapped, W2EnglangYP_mapped, W3englangHH_mapped, W4EngLangHH_mapped))

# Keep only NSID and lang
final_df <- merged %>% select(NSID, lang)

# Write CSV
write_csv(final_df, 'data/output/cleaned_data.csv')

cat('Finished writing cleaned_data.csv\n')
