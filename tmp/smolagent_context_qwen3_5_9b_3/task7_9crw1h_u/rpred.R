library(haven)
library(dplyr)
library(readr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets
all_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Age 17 (wave4 - w4saim)
educaim17 <- sapply(all_data$w4saim, function(x) {
  if (is.na(x) || (x < -1 || x > -999)) return(NA_integer_)
  if (x == 13) return(3L)
  if (x == 14) return(5L)
  if (x %in% c(1,2,3,5,6,7,9)) return(1L)
  if (x %in% c(8,10,11)) return(2L)
  if (x %in% c(4,12)) return(3L)
  NA_integer_
})

# Age 19 (wave6 - W6Saim)
educaim19 <- sapply(all_data$W6Saim, function(x) {
  if (is.na(x) || (x < -1 || x > -999)) return(NA_integer_)
  if (x == 16) return(5L)
  if (x == 15) return(3L)
  if (x == 14) return(3L)
  if (x == 13) return(2L)
  if (x %in% c(1,3,5)) return(0L)
  if (x %in% c(2,4)) return(0L)
  if (x %in% c(6,7,8)) return(1L)
  if (x %in% c(9,10)) return(1L)
  if (x %in% c(11,12)) return(2L)
  NA_integer_
})

# Age 20 (wave7 - W7SAim)
educaim20 <- sapply(all_data$W7SAim, function(x) {
  if (is.na(x) || (x < -1 || x > -999)) return(NA_integer_)
  if (x == -94) return(-8L)
  if (x == -91) return(-3L)
  if (x %in% c(11,12,13)) return(0L)
  if (x %in% c(1,2,3,6,8,9,10)) return(1L)
  if (x %in% c(4,5,7)) return(2L)
  if (x == 14) return(3L)
  NA_integer_
})

# Age 25 (wave8)
in_edu_25 <- all_data$W8ACTIVITY05 == 1L
he_vars_25 <- c("W8ACQUC0A","W8ACQUC0B","W8ACQUC0C","W8ACQUC0D","W8ACQUC0E")
mid_vars_25 <- c("W8ACQUC0F","W8ACQUC0G","W8ACQUC0H","W8ACQUC0I","W8VCQUC0J","W8VCQUC0K")
entry_vars_25 <- c("W8ACQUC0L","W8ACQUC0M","W8ACQUC0N","W8ACQUC0O")
other_vars_25 <- c("W8ACQUC0P","W8ACQUC0Q")

he_any_25 <- apply(all_data[, he_vars_25], 1, function(row) any(row == 1L, na.rm=TRUE))
mid_any_25 <- apply(all_data[, mid_vars_25], 1, function(row) any(row == 1L, na.rm=TRUE))
entry_any_25 <- apply(all_data[, entry_vars_25], 1, function(row) any(row == 1L, na.rm=TRUE))
other_any_25 <- apply(all_data[, other_vars_25], 1, function(row) any(row == 1L, na.rm=TRUE))

educaim25 <- rep(NA_integer_, nrow(all_data))
idx <- which(in_edu_25)
for (i in idx) {
  if (he_any_25[i]) educaim25[i] <- 0L
  else if (mid_any_25[i]) educaim25[i] <- 1L
  else if (entry_any_25[i]) educaim25[i] <- 2L
  else if (other_any_25[i]) educaim25[i] <- 3L
  else educaim25[i] <- 4L
}
educaim25[!in_edu_25] <- 5L

# Age 32 (wave9)
in_edu_32 <- all_data$W9ECONACT2 %in% c(6L, 7L)
he_vars_32 <- c("W9ACQUC0A","W9ACQUC0B","W9ACQUC0C","W9ACQUC0D","W9ACQUC0E")
mid_vars_32 <- c("W9ACQUC0G","W9ACQUC0H","W9ACQUC0I","W9ACQUC0J","W9ACQUC0K","W9ACQUC0L","W9ACQUC0M","W9ACQUC0N")
entry_vars_32 <- c("W9ACQUC0O","W9ACQUC0P","W9ACQUC0Q","W9ACQUC0R","W9ACQUC0S")
other_vars_32 <- c("W9ACQUC0T","W9ACQUC0U","W9ACQUC0V")

he_any_32 <- apply(all_data[, he_vars_32], 1, function(row) any(row == 1L, na.rm=TRUE))
mid_any_32 <- apply(all_data[, mid_vars_32], 1, function(row) any(row == 1L, na.rm=TRUE))
entry_any_32 <- apply(all_data[, entry_vars_32], 1, function(row) any(row == 1L, na.rm=TRUE))
other_any_32 <- apply(all_data[, other_vars_32], 1, function(row) any(row == 1L, na.rm=TRUE))

educaim32 <- rep(NA_integer_, nrow(all_data))
idx <- which(in_edu_32)
for (i in idx) {
  if (he_any_32[i]) educaim32[i] <- 0L
  else if (mid_any_32[i]) educaim32[i] <- 1L
  else if (entry_any_32[i]) educaim32[i] <- 2L
  else if (other_any_32[i]) educaim32[i] <- 3L
  else educaim32[i] <- 4L
}
educaim32[!in_edu_32] <- 5L

# Create output
output <- all_data
output$educaim17 <- educaim17
output$educaim19 <- educaim19
output$educaim20 <- educaim20
output$educaim25 <- educaim25
output$educaim32 <- educaim32

output <- output %>%
  select(NSID, all_of(c("educaim17", "educaim19", "educaim20", "educaim25", "educaim32")))

write_csv(output, "data/output/cleaned_data.csv")
