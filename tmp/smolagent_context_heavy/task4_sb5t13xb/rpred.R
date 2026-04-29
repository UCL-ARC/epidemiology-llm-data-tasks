library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load input files
path_prefix <- "data/input/"

file_w1 <- read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_w4 <- read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_w6 <- read_delim(paste0(path_prefix, "wave_six_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_w7 <- read_delim(paste0(path_prefix, "wave_seven_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_w8 <- read_delim(paste0(path_prefix, "ns8_2015_self_completion.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_w9 <- read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t", col_types = readr::cols(.default = "c"))

file_w6 <- file_w6 %>% mutate(W6SexualityYP = as.numeric(W6SexualityYP))
file_w7 <- file_w7 %>% mutate(W7SexualityYP = as.numeric(W7SexualityYP))
file_w8 <- file_w8 %>% mutate(W8SEXUALITY = as.numeric(W8SEXUALITY))
file_w9 <- file_w9 %>% mutate(W9SORI = as.numeric(W9SORI))

full_frame <- file_w1 %>%
  full_join(file_w4, by = "NSID") %>%
  full_join(file_w6, by = "NSID") %>%
  full_join(file_w7, by = "NSID") %>%
  full_join(file_w8, by = "NSID") %>%
  full_join(file_w9, by = "NSID")

process_sori <- function(val, wave) {
  val[is.na(val)] <- -3
  res <- val
  if (wave == "W6") {
    res[val == -97] <- -9
    res[val == -92] <- -9
    res[val == -91] <- -1
    res[val == -1] <- -8
  } else if (wave == "W7") {
    res[val == -100] <- -9
    res[val == -97] <- -9
    res[val == -92] <- -9
    res[val == -91] <- -1
    res[val == -1] <- -8
  } else if (wave == "W8") {
    res[val == -9] <- -9
    res[val == -8] <- -8
    res[val == -1] <- -1
  } else if (wave == "W9") {
    res[val == 5] <- -7
    res[val == -9] <- -9
    res[val == -8] <- -8
    res[val == -3] <- -3
    res[val == -1] <- -1
  }
  return(res)
}

full_frame <- full_frame %>%
  mutate(
    sori19 = process_sori(W6SexualityYP, "W6"),
    sori20 = process_sori(W7SexualityYP, "W7"),
    sori25 = process_sori(W8SEXUALITY, "W8"),
    sori32 = process_sori(W9SORI, "W9")
  )

# To avoid the vec_cast_named error, we define labels as a named vector
# where the NAMES are the labels and the VALUES are the codes.
# According to labelled::set_value_labels documentation for numeric vectors:
# it takes a named vector where names = labels and values = codes.
sori_labels_vec <- c(
  "Heterosexual/straight" = 1, 
  "Gay/lesbian" = 2, 
  "Bisexual" = 3, 
  "Other" = 4, 
  "Refusal" = -9, 
  "Don't know" = -8, 
  "Prefer not to say" = -7, 
  "Not asked" = -3, 
  "Schedule not applicable" = -2, 
  "Item not applicable" = -1
)

final_cols <- c("NSID", "sori19", "sori20", "sori25", "sori32")
output_df <- full_frame %>% select(all_of(final_cols))

for(col in c("sori19", "sori20", "sori25", "sori32")) {
  output_df[[col]] <- set_value_labels(output_df[[col]], sori_labels_vec)
}

write_csv(output_df, "data/output/cleaned_data.csv")