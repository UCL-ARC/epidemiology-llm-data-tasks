
# Load data using base R
wave_six <- read.table('data/input/wave_six_lsype_young_person_2020.tab', header=TRUE, sep='\t', stringsAsFactors=FALSE)
wave_seven <- read.table('data/input/wave_seven_lsype_young_person_2020.tab', header=TRUE, sep='\t', stringsAsFactors=FALSE)
wave_eight <- read.table('data/input/ns8_2015_self_completion.tab', header=TRUE, sep='\t', stringsAsFactors=FALSE)
wave_nine <- read.table('data/input/ns9_2022_main_interview.tab', header=TRUE, sep='\t', stringsAsFactors=FALSE)

# Function to map missing values
map_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999] <- NA
  x[x == -998] <- NA
  x[x == -997] <- NA
  x[x == -97] <- NA
  x[x == -92] <- NA
  x[x == -91] <- NA
  x[x == -99] <- NA
  x[x == -100] <- NA
  return(x)
}

# Get all unique NSIDs
all_nsids <- unique(unlist(mget(c('wave_six$NSID', 'wave_seven$NSID',
                                  'wave_eight$NSID', 'wave_nine$NSID'))))

# Initialize output data frame
output <- data.frame(NSID=all_nsids,
                    sori19=rep(NA, length(all_nsids)),
                    sori20=rep(NA, length(all_nsids)),
                    sori25=rep(NA, length(all_nsids)),
                    sori32=rep(NA, length(all_nsids)))

# Map wave 6 data
if ('W6SexualityYP' %in% names(wave_six)) {
  idx <- match(wave_six$NSID, output$NSID)
  output$sori19[idx] <- map_missing(wave_six$W6SexualityYP)
}

# Map wave 7 data
if ('W7SexualityYP' %in% names(wave_seven)) {
  idx <- match(wave_seven$NSID, output$NSID)
  output$sori20[idx] <- map_missing(wave_seven$W7SexualityYP)
}

# Map wave 8 data
if ('W8SEXUALITY' %in% names(wave_eight)) {
  idx <- match(wave_eight$NSID, output$NSID)
  output$sori25[idx] <- map_missing(wave_eight$W8SEXUALITY)
}

# Map wave 9 data
if ('W9SORI' %in% names(wave_nine)) {
  idx <- match(wave_nine$NSID, output$NSID)
  output$sori32[idx] <- map_missing(wave_nine$W9SORI)
}

# Write output to CSV using base R
write.table(output, 'data/output/cleaned_data.csv',
            row.names=FALSE, sep=',', quote=FALSE)
