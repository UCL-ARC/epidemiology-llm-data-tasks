library(readr)
library(dplyr)
library(labelled)

# File paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave_two = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave_three = 'data/input/wave_three_lsype_family_background_2020.tab',
  wave_four = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Read required columns
wave1 <- read_delim(files$wave_one, delim = '\t', col_select = c('NSID', 'W1GrsswkHH'), col_types = cols(NSID = col_character(), W1GrsswkHH = col_double()))
wave2 <- read_delim(files$wave_two, delim = '\t', col_select = c('NSID', 'W2GrsswkHH'), col_types = cols(NSID = col_character(), W2GrsswkHH = col_double()))
wave3 <- read_delim(files$wave_three, delim = '\t', col_select = c('NSID', 'W3incestw'), col_types = cols(NSID = col_character(), W3incestw = col_double()))
wave4 <- read_delim(files$wave_four, delim = '\t', col_select = c('NSID', 'w4IncEstW'), col_types = cols(NSID = col_character(), w4IncEstW = col_double()))

# Harmonise missing codes
map_missing_wave1_2 <- function(x){
  x <- replace(x, x == -999, -2)
  x <- replace(x, x == -992, -9)
  x <- replace(x, x == -99, -3)
  x <- replace(x, x == -94, -8)
  x <- replace(x, x == -92, -9)
  x <- replace(x, x == -91, -1)
  x <- replace(x, x == -3, -3)
  x <- replace(x, x == -1, -8)
  x
}
map_missing_wave3 <- function(x){
  x <- replace(x, x == -999, -2)
  x <- replace(x, x == -92, -9)
  x <- replace(x, x == -1, -8)
  x
}
map_missing_wave4 <- function(x){
  x <- replace(x, x == -996, -1)
  x <- replace(x, x == -99, -3)
  x <- replace(x, x == -92, -9)
  x <- replace(x, x == -1, -8)
  x
}

# Clean waves
wave1_clean <- wave1 %>% mutate(W1GrsswkHH = map_missing_wave1_2(W1GrsswkHH)) %>% distinct(NSID, .keep_all = TRUE)
wave2_clean <- wave2 %>% mutate(W2GrsswkHH = map_missing_wave1_2(W2GrsswkHH)) %>% distinct(NSID, .keep_all = TRUE)
wave3_clean <- wave3 %>% mutate(W3incestw = map_missing_wave3(W3incestw)) %>% distinct(NSID, .keep_all = TRUE)
wave4_clean <- wave4 %>% mutate(w4IncEstW = map_missing_wave4(w4IncEstW)) %>% distinct(NSID, .keep_all = TRUE)

# Merge
merged <- wave1_clean %>% full_join(wave2_clean, by = 'NSID') %>% full_join(wave3_clean, by = 'NSID') %>% full_join(wave4_clean, by = 'NSID')

# Labels and midpoints
band_labels <- c('Up to £49','£50 up to £99','£100 up to £199','£200 up to £299','£300 up to £399','£400 up to £499','£500 up to £599','£600 up to £699','£700 up to £799','£800 up to £899','£900 up to £999','£1,000 or more')
midpoints <- c(24.5, 74.5, 149.5, 249.5, 349.5, 449.5, 549.5, 649.5, 749.5, 849.5, 949.5, 1100)

make_labelled <- function(vec){
  labels <- c('Up to £49'=1,'£50 up to £99'=2,'£100 up to £199'=3,'£200 up to £299'=4,'£300 up to £399'=5,'£400 up to £499'=6,'£500 up to £599'=7,'£600 up to £699'=8,'£700 up to £799'=9,'£800 up to £899'=10,'£900 up to £999'=11,'£1,000 or more'=12,'Refusal'=-9,'Don\'t know / insufficient information'=-8,'Not asked'=-3,'Schedule not applicable / script error / information lost'=-2,'Item not applicable'=-1)
  labelled(vec, labels)
}

# Final variables
final <- merged %>% mutate(
  income_band14 = make_labelled(W1GrsswkHH),
  income_band15 = make_labelled(W2GrsswkHH),
  income_band16 = make_labelled(W3incestw),
  income_band17 = make_labelled(w4IncEstW),
  income_cont14 = midpoints[match(W1GrsswkHH, 1:12)],
  income_cont15 = midpoints[match(W2GrsswkHH, 1:12)]
) %>% select(NSID, income_band14, income_band15, income_band16, income_band17, income_cont14, income_cont15)

write_csv(final, 'data/output/cleaned_data.csv')