# Load required packages
library(readr)
library(dplyr)

# File paths
files <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab",
  wave5 = "data/input/wave_five_lsype_family_background_2020.tab"
)

# Helper: read only needed columns and keep unique NSID rows
read_and_select <- function(path, raw_cols){
  df <- read_delim(
    path,
    delim="\t",
    col_types = cols(.default = col_double()),
    na = c("", "NA"),
    col_select = all_of(c("NSID", raw_cols))
  )
  df %>% distinct(NSID, .keep_all = TRUE)
}

# Load waves
wf1 <- read_and_select(files$wave1, c("W1nsseccatmum", "W1nsseccatdad"))
wf2 <- read_and_select(files$wave2, c("W2nsseccatmum", "W2nsseccatdad"))
wf3 <- read_and_select(files$wave3, c("W3cnsseccatmum", "W3cnsseccatdad"))
wf4 <- read_and_select(files$wave4, c("w4cnsseccatmum", "w4cnsseccatdad"))
wf5 <- read_and_select(files$wave5, c("w5Cnsseccatmum", "w5Cnsseccatdad"))

# Map raw NS-SEC to major categories and standard missing codes
map_nssec <- function(x){
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x %in% c(-99, -98)] <- -3
  x[x == -94] <- -8
  pos <- which(x > 0)
  if(length(pos) > 0) x[pos] <- floor(x[pos])
  return(x)
}

# Derive variables per wave
wf1 <- wf1 %>% mutate(
  nssecma14 = map_nssec(W1nsseccatmum),
  nssecpa14 = map_nssec(W1nsseccatdad)
) %>% select(NSID, nssecma14, nssecpa14)

wf2 <- wf2 %>% mutate(
  nssecma15 = map_nssec(W2nsseccatmum),
  nssecpa15 = map_nssec(W2nsseccatdad)
) %>% select(NSID, nssecma15, nssecpa15)

wf3 <- wf3 %>% mutate(
  nssecma16 = map_nssec(W3cnsseccatmum),
  nssecpa16 = map_nssec(W3cnsseccatdad)
) %>% select(NSID, nssecma16, nssecpa16)

wf4 <- wf4 %>% mutate(
  nssecma17 = map_nssec(w4cnsseccatmum),
  nssecpa17 = map_nssec(w4cnsseccatdad)
) %>% select(NSID, nssecma17, nssecpa17)

wf5 <- wf5 %>% mutate(
  nssecma18 = map_nssec(w5Cnsseccatmum),
  nssecpa18 = map_nssec(w5Cnsseccatdad)
) %>% select(NSID, nssecma18, nssecpa18)

# Merge waves
full_df <- wf1 %>%
  full_join(wf2, by = "NSID") %>%
  full_join(wf3, by = "NSID") %>%
  full_join(wf4, by = "NSID") %>%
  full_join(wf5, by = "NSID")

# Output
write_csv(full_df, "data/output/cleaned_data.csv")
