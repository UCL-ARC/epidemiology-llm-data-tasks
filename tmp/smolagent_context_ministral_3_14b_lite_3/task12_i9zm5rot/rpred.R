library(readr);

# Load all required files
wave_four <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t");
wave_five <- readr::read_delim("data/input/wave_five_lsype_young_person_2020.tab", delim = "\t");
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t");
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t");
ns8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t");
ns9_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t");

# Create NS-SEC mapping
nssec_mapping <- data.frame(
  detailed = c(1, 2, 5, 6, 8, 10, 3.1, 3.2, 3.3, 3.4, 4.1, 4.2, 4.3, 4.4,
               7.1, 7.2, 7.3, 7.4, 11.1, 11.2, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7,
               13.1, 13.2, 13.3, 13.4, 13.5, 14.1, 14.2, 14.3, 15, 16, 17),
  major = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6),
  stringsAsFactors = FALSE
);

# Function to handle missing values
handle_missing <- function(x) {
  x <- as.numeric(x);
  x[x >= -999 & x <= -1] <- NA;
  x <- ifelse(x == -91, -1, x);
  x <- ifelse(x == -99, -3, x);
  x <- ifelse(x == -9, -9, x);
  x <- ifelse(x == -8, -8, x);
  x <- ifelse(x == -7, -7, x);
  x[is.na(x) & x != -2] <- -2;
  return(x);
};

# Function to collapse NS-SEC categories
collapse_nssec <- function(x) {
  x <- as.numeric(x);
  mapping <- setNames(nssec_mapping$major, as.character(nssec_mapping$detailed));
  mapped <- mapping[as.character(x)];
  mapped[!as.character(x) %in% names(mapping)] <- NA;
  return(mapped);
};

# Create empty result dataframe
merged_data <- data.frame(NSID = character());

# Process waves
if("W4nsseccatYP" %in% names(wave_four)) {
  wave_four$nssec17 <- collapse_nssec(handle_missing(wave_four$W4nsseccatYP));
  merged_data <- merge(merged_data, wave_four[, c("NSID", "nssec17")], by = "NSID", all.x = TRUE);
}

if("W5nsseccatYP" %in% names(wave_five)) {
  wave_five$nssec18 <- collapse_nssec(handle_missing(wave_five$W5nsseccatYP));
  merged_data <- merge(merged_data, wave_five[, c("NSID", "nssec18")], by = "NSID", all.x = TRUE);
}

if("w6nsseccatYP" %in% names(wave_six)) {
  wave_six$nssec19 <- collapse_nssec(handle_missing(wave_six$w6nsseccatYP));
  merged_data <- merge(merged_data, wave_six[, c("NSID", "nssec19")], by = "NSID", all.x = TRUE);
}

if("W7NSSECCat" %in% names(wave_seven)) {
  wave_seven$nssec20 <- collapse_nssec(handle_missing(wave_seven$W7NSSECCat));
  merged_data <- merge(merged_data, wave_seven[, c("NSID", "nssec20")], by = "NSID", all.x = TRUE);
}

if("W8DNSSEC17" %in% names(ns8_derived)) {
  ns8_derived$nssec25 <- collapse_nssec(handle_missing(ns8_derived$W8DNSSEC17));
  merged_data <- merge(merged_data, ns8_derived[, c("NSID", "nssec25")], by = "NSID", all.x = TRUE);
}

if("W9NSSEC" %in% names(ns9_main)) {
  ns9_main$nssec32 <- collapse_nssec(handle_missing(ns9_main$W9NSSEC));
  merged_data <- merge(merged_data, ns9_main[, c("NSID", "nssec32")], by = "NSID", all.x = TRUE);
}

# Define labels
nssec_labels <- c("1" = "Employers and managers", 
                   "2" = "Higher professional occupations", 
                   "3" = "Lower professional occupations", 
                   "4" = "Intermediate occupations", 
                   "5" = "Routine and semi-routine occupations", 
                   "6" = "Never worked / Long-term unemployed / Full-time students / Not classifiable");

# Apply labels to variables
for (var in c("nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- factor(merged_data[[var]], 
                              levels = 1:6, 
                              labels = nssec_labels[1:6]);
  }
}

# Write the output to CSV
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)