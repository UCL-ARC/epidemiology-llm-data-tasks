
# Load required packages
library(haven)
library(dplyr)
library(readr)

# Load datasets
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Get all unique NSIDs
all_nsids <- unique(c(wave2$NSID, wave3$NSID, wave8$NSID, wave9$NSID))

# Create base dataframe
result <- data.frame(NSID = all_nsids, stringsAsFactors = FALSE)

# Function to handle missing values
handle_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x[x %in% c(-999, -998, -997, -995)] <- -2
  return(x)
}

# Create a complete solution using base R
result$regub15 <- handle_missing(ifelse(result$NSID %in% wave2$NSID,
                                       wave2$urbind[match(result$NSID, wave2$NSID)],
                                       -3))

result$regov15 <- handle_missing(ifelse(result$NSID %in% wave2$NSID,
                                       wave2$gor[match(result$NSID, wave2$NSID)],
                                       -3))

result$regub16 <- handle_missing(ifelse(result$NSID %in% wave3$NSID,
                                       wave3$urbind[match(result$NSID, wave3$NSID)],
                                       -3))

result$regov16 <- handle_missing(ifelse(result$NSID %in% wave3$NSID,
                                       wave3$gor[match(result$NSID, wave3$NSID)],
                                       -3))

result$regor25 <- handle_missing(ifelse(result$NSID %in% wave8$NSID,
                                       wave8$W8DGOR[match(result$NSID, wave8$NSID)],
                                       -3))

result$regint32 <- handle_missing(ifelse(result$NSID %in% wave9$NSID,
                                       wave9$W9NATIONRES[match(result$NSID, wave9$NSID)],
                                       -3))

# Create factor variables with labels
result$regub15 <- factor(result$regub15,
                       levels = c(-8, 1, 2, 3, 4, 5, 6, 7, 8),
                       labels = c('Insufficient information',
                                 'Urban >= 10k - sparse',
                                 'Town & Fringe - sparse',
                                 'Village - sparse',
                                 'Hamlet and Isolated Dwelling - sparse',
                                 'Urban >= 10k - less sparse',
                                 'Town & Fringe - less sparse',
                                 'Village - less sparse',
                                 'Hamlet & Isolated Dwelling'))

result$regub16 <- factor(result$regub16,
                       levels = c(-8, 1, 2, 3, 4, 5, 6, 7, 8),
                       labels = c('Insufficient information',
                                 'Urban >= 10k - sparse',
                                 'Town & Fringe - sparse',
                                 'Village - sparse',
                                 'Hamlet and Isolated Dwelling - sparse',
                                 'Urban >= 10k - less sparse',
                                 'Town & Fringe - less sparse',
                                 'Village - less sparse',
                                 'Hamlet & Isolated Dwelling'))

result$regov15 <- factor(result$regov15,
                       levels = c(-8, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels = c('Insufficient information',
                                 'North East', 'North West',
                                 'Yorkshire and The Humber',
                                 'East Midlands', 'West Midlands',
                                 'East of England', 'London',
                                 'South East', 'South West'))

result$regov16 <- factor(result$regov16,
                       levels = c(-8, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                       labels = c('Insufficient information',
                                 'North East', 'North West',
                                 'Yorkshire and The Humber',
                                 'East Midlands', 'West Midlands',
                                 'East of England', 'London',
                                 'South East', 'South West'))

result$regor25 <- factor(result$regor25,
                       levels = c(-9, -8, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                       labels = c('Refused', 'Insufficient information', 'Not applicable',
                                 'North East', 'North West',
                                 'Yorkshire and The Humber', 'East Midlands',
                                 'West Midlands', 'East of England', 'London',
                                 'South East', 'South West', 'Wales',
                                 'Scotland', 'Northern Ireland',
                                 'Unknown due to faulty/missing postcode'))

result$regint32 <- factor(result$regint32,
                        levels = c(-9, -8, -3, -1, 1, 2, 3, 4, 5),
                        labels = c('Refused', 'Don\'t know',
                                  'Not asked at fieldwork stage',
                                  'Not applicable', 'England',
                                  'Scotland', 'Wales', 'Northern Ireland',
                                  'Outside of UK or unknown'))

# Write to CSV
write.csv(result[, c('NSID', 'regub15', 'regub16', 'regov15', 'regov16', 'regor25', 'regint32')],
          'data/output/cleaned_data.csv', row.names = FALSE)
