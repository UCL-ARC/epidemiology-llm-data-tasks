library(readr)
library(dplyr)

# Load all datasets
s4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
s6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
s7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets
cohort <- s4 %>%
  full_join(s6, by = "NSID") %>%
  full_join(s7, by = "NSID") %>%
  full_join(ns8, by = "NSID") %>%
  full_join(ns9, by = "NSID")

cat("Cohort size:", nrow(cohort), "\n")

# Age 17 - wave_four: w4saim
# Values 1-11: NVQ 1-3 -> category 1
# Values 12, 13: Other/No detail -> category 3
# Value 14: Not studying -> category 5
# Missing: -999, -1, -8 -> -8; -9 -> -9

educaim17 <- cohort$w4saim
if (!is.null(educaim17)) {
  educaim17[educaim17 %in% c(-999, -1, -8)] <- -8
  educaim17[educaim17 == -9] <- -9
  educaim17[educaim17 >= 1 & educaim17 <= 11] <- 1
  educaim17[educaim17 == 12 | educaim17 == 13] <- 3
  educaim17[educaim17 == 14] <- 5
}

# Age 19 - wave_six: W6Saim
# Values 1-4: NVQ 4-5/HE level -> category 0
# Values 5-13: NVQ 1-3/mid level -> category 1
# Values 14, 15: Other/No detail -> category 3
# Value 16: Not studying -> category 5

educaim19 <- cohort$W6Saim
if (!is.null(educaim19)) {
  educaim19[educaim19 %in% c(-999, -1, -8)] <- -8
  educaim19[educaim19 == -9] <- -9
  educaim19[educaim19 >= 1 & educaim19 <= 4] <- 0
  educaim19[educaim19 >= 5 & educaim19 <= 13] <- 1
  educaim19[educaim19 == 14 | educaim19 == 15] <- 3
  educaim19[educaim19 == 16] <- 5
}

# Age 20 - wave_seven: W7SAim
# -94: Insufficient information -> -8
# -91, -999, -1: Not applicable -> -1
# Values 1-9: NVQ 1-3 -> category 1
# Values 10-13: NVQ 4-5/HE -> category 0
# Value 14: Other/unknown -> category 3

educaim20 <- cohort$W7SAim
if (!is.null(educaim20)) {
  educaim20[educaim20 == -94] <- -8
  educaim20[educaim20 %in% c(-91, -999, -1)] <- -1
  educaim20[educaim20 >= 1 & educaim20 <= 9] <- 1
  educaim20[educaim20 >= 10 & educaim20 <= 13] <- 0
  educaim20[educaim20 == 14] <- 3
}

# Age 25 - ns8
educaim25 <- cohort$W8ACTIVITY05
if (!is.null(educaim25)) {
  for (i in 1:nrow(cohort)) {
    act <- cohort$W8ACTIVITY05[i]
    if (is.na(act)) {
      educaim25[i] <- NA
    } else if (act == 1) {
      # Studying - check qualifications
      acqA <- cohort$W8ACQUC0A[i]
      acqB <- cohort$W8ACQUC0B[i]
      acqC <- cohort$W8ACQUC0C[i]
      acqD <- cohort$W8ACQUC0D[i]
      acqE <- cohort$W8ACQUC0E[i]
      
      if (acqA == 1 || acqB == 1 || acqC == 1 || acqD == 1 || acqE == 1) {
        educaim25[i] <- 0
      } else {
        vcqJ <- cohort$W8VCQUC0J[i]
        vcqK <- cohort$W8VCQUC0K[i]
        
        if (vcqJ == 1 || vcqK == 1) {
          educaim25[i] <- 0
        } else {
          vcqA <- cohort$W8VCQUC0A[i]
          vcqB <- cohort$W8VCQUC0B[i]
          vcqC <- cohort$W8VCQUC0C[i]
          vcqD <- cohort$W8VCQUC0D[i]
          vcqE <- cohort$W8VCQUC0E[i]
          
          if (vcqA == 1 || vcqB == 1 || vcqC == 1 || vcqD == 1 || vcqE == 1) {
            educaim25[i] <- 1
          } else {
            educaim25[i] <- 4
          }
        }
      }
    } else if (act == 0) {
      educaim25[i] <- 5
    } else {
      if (act == -9) {
        educaim25[i] <- -9
      } else if (act == -8) {
        educaim25[i] <- -8
      } else if (act == -1) {
        educaim25[i] <- -1
      } else {
        educaim25[i] <- NA
      }
    }
  }
}

# Age 32 - ns9
educaim32 <- cohort$W9ECONACT2
if (!is.null(educaim32)) {
  for (i in 1:nrow(cohort)) {
    econ <- cohort$W9ECONACT2[i]
    if (is.na(econ)) {
      educaim32[i] <- NA
    } else if (econ == 6 || econ == 7) {
      # Studying - check qualifications
      acqA <- cohort$W9ACQUC0A[i]
      acqB <- cohort$W9ACQUC0B[i]
      acqC <- cohort$W9ACQUC0C[i]
      acqD <- cohort$W9ACQUC0D[i]
      acqE <- cohort$W9ACQUC0E[i]
      acqF <- cohort$W9ACQUC0F[i]
      
      if (acqA == 1 || acqB == 1 || acqC == 1 || acqD == 1 || acqE == 1 || acqF == 1) {
        educaim32[i] <- 0
      } else {
        vcqV <- cohort$W9VCQUC0V[i]
        
        if (vcqV == 1) {
          educaim32[i] <- 0
        } else {
          vcqC <- cohort$W9VCQUC0C[i]
          vcqD <- cohort$W9VCQUC0D[i]
          vcqE <- cohort$W9VCQUC0E[i]
          vcqF <- cohort$W9VCQUC0F[i]
          vcqG <- cohort$W9VCQUC0G[i]
          vcqH <- cohort$W9VCQUC0H[i]
          vcqO <- cohort$W9VCQUC0O[i]
          
          if (vcqC == 1 || vcqD == 1 || vcqE == 1 || vcqF == 1 || vcqG == 1 || vcqH == 1 || vcqO == 1) {
            educaim32[i] <- 1
          } else {
            educaim32[i] <- 4
          }
        }
      }
    } else if (econ == -1) {
      educaim32[i] <- -1
    } else if (econ == -8) {
      educaim32[i] <- -8
    } else if (econ == -3) {
      educaim32[i] <- -3
    } else if (econ == -9) {
      educaim32[i] <- -9
    } else {
      educaim32[i] <- NA
    }
  }
}

# Create final dataset
result <- cohort %>%
  mutate(
    educaim17 = educaim17,
    educaim19 = educaim19,
    educaim20 = educaim20,
    educaim25 = educaim25,
    educaim32 = educaim32
  ) %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(result, "data/output/cleaned_data.csv")
