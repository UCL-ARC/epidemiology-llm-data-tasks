library(dplyr)
library(readr)
library(labelled)
library(purrr)

# Helper to read only needed columns based on positions
read_by_pos <- function(file, pos_df) {
  # Read whole file as character columns
  df <- read_delim(
    file.path("data/input", file),
    delim = "\t",
    col_names = FALSE,
    show_col_types = FALSE
  )
  # Create generic column names V1..Vn
  ncol <- ncol(df)
  names(df) <- paste0("V", 1:ncol)
  # Select needed columns by position
  df <- df %>% select(all_of(pos_df$pos))
  # Rename to variable names
  names(df) <- pos_df$var
  # Convert to numeric where not NSID
  df <- df %>% mutate(across(-NSID, ~ as.numeric(.)))
  return(df)
}

# Variable positions for each file
w1pos <- data.frame(var = c("NSID","W1alceverYP","W1alcmonYP"), pos = c(1,272,273))
w2pos <- data.frame(var = c("NSID","W2alceverYP"), pos = c(1,452))
w3pos <- data.frame(var = c("NSID","W3alceverYP"), pos = c(1,576))
w4pos <- data.frame(var = c("NSID","W4AlcEverYP"), pos = c(1,884))
w6pos <- data.frame(var = c("NSID","W6AlcEverYP"), pos = c(1,546))
w7pos <- data.frame(var = c("NSID","W7AlcEverYP"), pos = c(1,616))
w8pos <- data.frame(var = c("NSID","W8AUDIT1"), pos = c(1,41))
w9pos <- data.frame(var = c("NSID","W9AUDIT1"), pos = c(1,1582))

# Load each sweep
f1 <- read_by_pos("wave_one_lsype_young_person_2020.tab", w1pos)
f2 <- read_by_pos("wave_two_lsype_young_person_2020.tab", w2pos)
f3 <- read_by_pos("wave_three_lsype_young_person_2020.tab", w3pos)
f4 <- read_by_pos("wave_four_lsype_young_person_2020.tab", w4pos)
f6 <- read_by_pos("wave_six_lsype_young_person_2020.tab", w6pos)
f7 <- read_by_pos("wave_seven_lsype_young_person_2020.tab", w7pos)
f8 <- read_by_pos("ns8_2015_self_completion.tab", w8pos)
f9 <- read_by_pos("ns9_2022_main_interview.tab", w9pos)

# Merge all sweeps by ID
merged <- f1 %>%
  full_join(f2, by = "NSID") %>%
  full_join(f3, by = "NSID") %>%
  full_join(f4, by = "NSID") %>%
  full_join(f6, by = "NSID") %>%
  full_join(f7, by = "NSID") %>%
  full_join(f8, by = "NSID") %>%
  full_join(f9, by = "NSID")

# Mapping functions
map_alc_ever <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}

map_alc_audit <- function(x) {
  case_when(
    x == 1 ~ 0,
    x >= 2 & x <= 5 ~ 1,
    TRUE ~ NA_real_
  )
}

# Compute drinking indicators per age
merged <- merged %>% mutate(
  drink14 = with(., {
    ever <- map_alc_ever(W1alceverYP)
    mon  <- map_alc_ever(W1alcmonYP)
    ifelse(is.na(ever) | is.na(mon), NA_real_,
           ifelse(ever == 1 & mon == 1, 1, 0))
  }),
  drink15 = map_alc_ever(W2alceverYP),
  drink16 = map_alc_ever(W3alceverYP),
  drink17 = map_alc_ever(W4AlcEverYP),
  drink19 = map_alc_ever(W6AlcEverYP),
  drink20 = map_alc_ever(W7AlcEverYP),
  drink25 = map_alc_audit(W8AUDIT1),
  drink32 = map_alc_audit(W9AUDIT1)
)

ages <- c(14,15,16,17,19,20,25,32)
mat <- as.matrix(merged %>% select(drink14:drink32))

first_age <- apply(mat, 1, function(v) {
  idx <- which(v == 1)
  if (length(idx) > 0) ages[idx[1]] else NA_real_
})

has_missing <- apply(mat, 1, function(v) any(is.na(v)))

final_age <- ifelse(is.na(first_age),
                    ifelse(has_missing, -8, 99),
                    first_age)

alcfst <- factor(final_age,
  levels = c(14,15,16,17,19,20,25,32,99,-8),
  labels = c("Age 14","Age 15","Age 16","Age 17","Age 19","Age 20","Age 25","Age 32","Never had alcohol","Don\'t know/insufficient information"))

output_df <- merged %>% select(NSID) %>% mutate(alcfst = alcfst)
write_csv(output_df, "data/output/cleaned_data.csv")