library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Check the actual values and structure of the income variables
print('Wave 1 W1GrsswkHH:')
print(summary(wave1$W1GrsswkHH))
print('Unique values:')
print(sort(unique(wave1$W1GrsswkHH)))

print('\nWave 2 W2GrsswkHH:')
print(summary(wave2$W2GrsswkHH))
print('Unique values:')
print(sort(unique(wave2$W2GrsswkHH)))

print('\nWave 3 W3incestw:')
print(summary(wave3$W3incestw))
print('Unique values:')
print(sort(unique(wave3$W3incestw)))

print('\nWave 4 w4IncEstW:')
print(summary(wave4$w4IncEstW))
print('Unique values:')
print(sort(unique(wave4$w4IncEstW)))

# Check if there are continuous versions
print('\nChecking for continuous income variables...')
print('Wave 1 W1GrssyrHH (if exists):')
print(summary(wave1$W1GrssyrHH))
print('Wave 2 W2GrssyrHH (if exists):')
print(summary(wave2$W2GrssyrHH))