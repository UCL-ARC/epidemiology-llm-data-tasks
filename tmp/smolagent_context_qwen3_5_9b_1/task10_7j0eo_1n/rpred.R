library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# ============ HELPER FUNCTIONS ============
map_wave4_to_6 <- function(x) {
  x[x == 1] <- 1
  x[x == 2] <- 1
  x[x == 3] <- 4
  x[x == 4] <- 2
  x[x == 5] <- 3
  x[x == 6] <- 5
  x[x %in% c(7,8,9)] <- 6
  return(x)
}

map_wave5_to_6 <- function(x) {
  x[x == 1] <- 2
  x[x == 2] <- 3
  x[x == 3] <- 1
  x[x == 4] <- 3
  x[x == 5] <- 2
  x[x == 6] <- 2
  x[x == 7] <- 4
  x[x == 8] <- 5
  x[x %in% c(9,10,11)] <- 6
  return(x)
}

map_wave6_to_6 <- function(x) {
  x[x == 1] <- 3
  x[x == 2] <- 3
  x[x == 3] <- 1
  x[x == 4] <- 2
  x[x == 5] <- 2
  x[x == 6] <- 6
  x[x == 7] <- 5
  x[x == 8] <- 4
  x[x %in% c(9,10,11)] <- 6
  return(x)
}

map_wave7_to_6 <- function(x) {
  x[x == 1] <- 3
  x[x == 2] <- 3
  x[x == 3] <- 1
  x[x == 4] <- 2
  x[x == 5] <- 2
  x[x == 6] <- 6
  x[x == 7] <- 5
  x[x == 8] <- 4
  x[x == 9] <- 3
  x[x == 10] <- 6
  x[x == 11] <- 2
  x[x %in% c(12,13,14,15)] <- 6
  return(x)
}

map_wave8_to_6 <- function(x) {
  x[x == 1] <- 1
  x[x == 2] <- 1
  x[x == 3] <- 6
  x[x == 4] <- 4
  x[x == 5] <- 3
  x[x == 6] <- 2
  x[x == 7] <- 2
  x[x == 8] <- 6
  x[x == 9] <- 5
  x[x == 10] <- 6
  return(x)
}

map_wave9_to_6 <- function(x) {
  x[x == 1] <- 1
  x[x == 2] <- 1
  x[x == 3] <- 6
  x[x == 4] <- 4
  x[x == 5] <- 3
  x[x == 6] <- 2
  x[x == 7] <- 2
  x[x == 8] <- 6
  x[x == 9] <- 5
  x[x == 10] <- 6
  return(x)
}

# ============ FILE LOADING ============
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# ============ CREATE COLLAPSED VARIABLES FOR EACH WAVE ============
# ecoact17 (wave4)
wave4$ecoact17 <- wave4$W4empsYP
wave4$ecoact17[wave4$ecoact17 == -999] <- -2
wave4$ecoact17[wave4$ecoact17 == -94] <- -8
wave4$ecoact17[wave4$ecoact17 == -92] <- -9
wave4$ecoact17[wave4$ecoact17 == -91] <- -1
wave4$ecoact17[wave4$ecoact17 %in% c(1:9)] <- map_wave4_to_6(wave4$ecoact17[wave4$ecoact17 %in% c(1:9)])
wave4$ecoact17[is.na(wave4$ecoact17)] <- -3
wave4$ecoact17 <- as.integer(wave4$ecoact17)

# ecoact18 (wave5)
wave5$ecoact18 <- wave5$W5mainactYP
wave5$ecoact18[wave5$ecoact18 == -999] <- -2
wave5$ecoact18[wave5$ecoact18 == -94] <- -8
wave5$ecoact18[wave5$ecoact18 %in% c(-999:-1)] <- NA
wave5$ecoact18 <- map_wave5_to_6(wave5$ecoact18)
wave5$ecoact18[is.na(wave5$ecoact18)] <- -3
wave5$ecoact18 <- as.integer(wave5$ecoact18)

# ecoact19 (wave6)
wave6$ecoact19 <- wave6$W6TCurrentAct
wave6$ecoact19[wave6$ecoact19 == -91] <- -1
wave6$ecoact19[wave6$ecoact19 %in% c(-999:-1)] <- NA
wave6$ecoact19 <- map_wave6_to_6(wave6$ecoact19)
wave6$ecoact19[is.na(wave6$ecoact19)] <- -3
wave6$ecoact19 <- as.integer(wave6$ecoact19)

# ecoact20 (wave7)
wave7$ecoact20 <- wave7$W7TCurrentAct
wave7$ecoact20[wave7$ecoact20 == -91] <- -1
wave7$ecoact20[wave7$ecoact20 %in% c(-999:-1)] <- NA
wave7$ecoact20 <- map_wave7_to_6(wave7$ecoact20)
wave7$ecoact20[is.na(wave7$ecoact20)] <- -3
wave7$ecoact20 <- as.integer(wave7$ecoact20)

# ecoact25 (wave8)
wave8$ecoact25 <- wave8$W8DACTIVITYC
wave8$ecoact25[wave8$ecoact25 == -9] <- -9
wave8$ecoact25[wave8$ecoact25 == -8] <- -8
wave8$ecoact25[wave8$ecoact25 == -1] <- -1
wave8$ecoact25[wave8$ecoact25 %in% c(-9:-1)] <- NA
wave8$ecoact25 <- map_wave8_to_6(wave8$ecoact25)
wave8$ecoact25[is.na(wave8$ecoact25)] <- -3
wave8$ecoact25 <- as.integer(wave8$ecoact25)

# ecoact32 (wave9)
wave9$ecoact32 <- wave9$W9DACTIVITYC
wave9$ecoact32[wave9$ecoact32 == -9] <- -9
wave9$ecoact32[wave9$ecoact32 == -8] <- -8
wave9$ecoact32[wave9$ecoact32 == -1] <- -1
wave9$ecoact32[wave9$ecoact32 %in% c(-9:-1)] <- NA
wave9$ecoact32 <- map_wave9_to_6(wave9$ecoact32)
wave9$ecoact32[is.na(wave9$ecoact32)] <- -3
wave9$ecoact32 <- as.integer(wave9$ecoact32)

# ============ CREATE DETAILED VARIABLES ============
wave8$ecoactadu25 <- wave8$W8DACTIVITYC
wave8$ecoactadu25[wave8$ecoactadu25 %in% c(-9, -8, -1)] <- NA
wave8$ecoactadu25[is.na(wave8$ecoactadu25)] <- -3
wave8$ecoactadu25 <- as.integer(wave8$ecoactadu25)

wave9$ecoactadu32 <- wave9$W9DACTIVITYC
wave9$ecoactadu32[wave9$ecoactadu32 %in% c(-9, -8, -1)] <- NA
wave9$ecoactadu32[is.na(wave9$ecoactadu32)] <- -3
wave9$ecoactadu32 <- as.integer(wave9$ecoactadu32)

# ============ MERGE ALL DATASETS ============
data <- wave1
data <- full_join(data, wave4, by = "NSID")
data <- full_join(data, wave5, by = "NSID")
data <- full_join(data, wave6, by = "NSID")
data <- full_join(data, wave7, by = "NSID")
data <- full_join(data, wave8, by = "NSID")
data <- full_join(data, wave9, by = "NSID")

# ============ ASSEMBLE FINAL OUTPUT ============
output <- data %>%
  select(NSID,
         all_of(c("ecoact17", "ecoact18", "ecoact19", "ecoact20", "ecoact25", "ecoact32")),
         all_of(c("ecoactadu25", "ecoactadu32")))

# Create factors with proper levels
output$ecoact17 <- factor(output$ecoact17, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoact18 <- factor(output$ecoact18, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoact19 <- factor(output$ecoact19, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoact20 <- factor(output$ecoact20, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoact25 <- factor(output$ecoact25, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoact32 <- factor(output$ecoact32, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6))
output$ecoactadu25 <- factor(output$ecoactadu25, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6,7,8,9,10))
output$ecoactadu32 <- factor(output$ecoactadu32, levels = c(-9,-8,-7,-3,-2,-1,1,2,3,4,5,6,7,8,9,10))

# Write output as CSV
write_csv(output, "data/output/cleaned_data.csv")

print("Script completed successfully!")
print(paste("Output written to data/output/cleaned_data.csv"))
print(paste("Rows:", nrow(output), "Columns:", ncol(output)))
print("Variables:", paste(names(output), collapse = ", "))
}