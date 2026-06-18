library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files from data/input/
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Process wave 1 (age 14) continuous income from W1GrsswkHH
w1$incwhhcnt14 <- w1$W1GrsswkHH
# Map missing values for age 14 first
w1$incwhhcnt14 <- recode(w1$incwhhcnt14, "-3" = "-1", "-1" = "-8", "-992" = "-9")
w1$incwhhcnt14 <- as.numeric(w1$incwhhcnt14)
w1$incwhhcnt14[is.na(w1$incwhhcnt14)] <- -3

# Band the continuous income for age 14
w1$incwhh14 <- cut(w1$incwhhcnt14, breaks = c(-Inf, 49.5, 99.5, 199.5, 299.5, 399.5, 499.5, 599.5, 699.5, 799.5, 899.5, 999.5, Inf), right = FALSE, include.lowest = TRUE)
valid_w14 <- w1$incwhhcnt14[!is.na(w1$incwhhcnt14) & w1$incwhhcnt14 > 0]
w1$incwhh14[!is.na(w1$incwhhcnt14) & w1$incwhhcnt14 > 0] <- cut(valid_w14, breaks = c(0, 49.5, 99.5, 199.5, 299.5, 399.5, 499.5, 599.5, 699.5, 799.5, 899.5, 999.5, Inf), right = FALSE)
w1$incwhh14 <- as.integer(w1$incwhh14)
w1$incwhh14[is.na(w1$incwhh14)] <- -1

# Process wave 2 (age 15) continuous income from W2GrsswkHH
w2$incwhhcnt15 <- w2$W2GrsswkHH
# Map missing values for age 15 first
w2$incwhhcnt15 <- recode(w2$incwhhcnt15, "-3" = "-1", "-1" = "-8", "-992" = "-9")
w2$incwhhcnt15 <- as.numeric(w2$incwhhcnt15)
w2$incwhhcnt15[is.na(w2$incwhhcnt15)] <- -3

# Band the continuous income for age 15
w2$incwhh15 <- cut(w2$incwhhcnt15, breaks = c(-Inf, 49.5, 99.5, 199.5, 299.5, 399.5, 499.5, 599.5, 699.5, 799.5, 899.5, 999.5, Inf), right = FALSE, include.lowest = TRUE)
valid_w15 <- w2$incwhhcnt15[!is.na(w2$incwhhcnt15) & w2$incwhhcnt15 > 0]
w2$incwhh15[!is.na(w2$incwhhcnt15) & w2$incwhhcnt15 > 0] <- cut(valid_w15, breaks = c(0, 49.5, 99.5, 199.5, 299.5, 399.5, 499.5, 599.5, 699.5, 799.5, 899.5, 999.5, Inf), right = FALSE)
w2$incwhh15 <- as.integer(w2$incwhh15)
w2$incwhh15[is.na(w2$incwhh15)] <- -1

# Process wave 3 (age 16) - W3incestw is already banded
w3$incwhh16 <- w3$W3incestw
# Map missing values according to metadata
w3$incwhh16 <- recode(w3$incwhh16, "-99" = "-3", "-92" = "-9", "-1" = "-8", "-999" = "-3")
w3$incwhh16 <- as.numeric(w3$incwhh16)
w3$incwhh16[is.na(w3$incwhh16)] <- -3

# Process wave 4 (age 17) - w4IncEstW is already banded
w4$incwhh17 <- w4$w4IncEstW
# Map missing values according to metadata
w4$incwhh17 <- recode(w4$incwhh17, "-996" = "-3", "-99" = "-3", "-92" = "-9", "-1" = "-8", "-999" = "-3")
w4$incwhh17 <- as.numeric(w4$incwhh17)
w4$incwhh17[is.na(w4$incwhh17)] <- -3

# Full join all datasets by NSID
final <- full_join(w1, w2, by = "NSID")
final <- full_join(final, w3, by = "NSID")
final <- full_join(final, w4, by = "NSID")

# Remove raw source variables, keep only NSID and final derived income variables
final <- select(final, NSID, incwhhcnt14, incwhh14, incwhhcnt15, incwhh15, incwhh16, incwhh17)

# Write output
write_csv(final, "data/output/cleaned_data.csv")