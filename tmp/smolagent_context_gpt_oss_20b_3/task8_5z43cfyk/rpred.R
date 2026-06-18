# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(labelled)

# ---------- Load all files ----------
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
ns8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
ns9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# ---------- Merge all datasets by NSID ----------
merged <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(ns8_main, by = "NSID") %>%
  full_join(ns8_derived, by = "NSID") %>%
  full_join(ns9_main, by = "NSID") %>%
  full_join(ns9_derived, by = "NSID")

# ---------- Helper: map NVQ codes to 5-level scheme ----------
# academic and vocational NVQ variables use codes: -9 Refused, -8 Missing info, -1 Not applicable, 0 Entry level, 1-5 NVQ levels, 95 Other, 96 None
nvq_to_tier <- function(x){
  case_when(
    is.na(x) ~ NA_real_,
    x == -9 ~ -9,      # Refused
    x == -8 ~ -8,      # Missing info
    x == -1 ~ -1,      # Not applicable
    x == 0  ~ 2,       # Entry level -> tier 2
    x >= 1 & x <= 3 ~ 1,  # NVQ1-3 -> tier 1
    x >= 4 & x <= 5 ~ 0,  # NVQ4-5 -> tier 0
    x == 95 ~ 3,       # Other qualifications -> tier 3
    x == 96 ~ 4        # None of these qualifications -> tier 4
  )
}

# ---------- educ25 (age 25) ----------
# Academic tier from W8DHANVQH
acad_tier25 <- nvq_to_tier(merged$W8DHANVQH)

# Vocational tick boxes mapping to tier
voc_vars25 <- c(
  "W8VCQU0A","W8VCQU0B","W8VCQU0C","W8VCQU0D","W8VCQU0E","W8VCQU0F",
  "W8VCQU0G","W8VCQU0H","W8VCQU0I","W8VCQU0J","W8VCQU0K","W8VCQU0L",
  "W8VCQU0M","W8VCQU0N","W8VCQU0O"
)
# Tier mapping for each variable
voc_tiers_map <- c(
  W8VCQU0A = 1, # Youth training
  W8VCQU0B = 1, # Key Skills
  W8VCQU0C = 1, # Basic skills
  W8VCQU0D = 2, # Entry level
  W8VCQU0E = 1, # Modern apprenticeship
  W8VCQU0F = 1, # RSA/OCR
  W8VCQU0G = 1, # City & Guilds
  W8VCQU0H = 1, # GNVQ/GSVQ
  W8VCQU0I = 1, # NVQ 1-2
  W8VCQU0J = 0, # NVQ 3-5
  W8VCQU0K = 0, # HNC/HND
  W8VCQU0L = 0, # ONC/OND
  W8VCQU0M = 0, # BTEC etc
  W8VCQU0N = 0, # SCOTVEC
  W8VCQU0O = 0  # Other vocational
)

# Compute vocational tier per person
voc_tier25_vec <- apply(merged %>% select(all_of(voc_vars25)), 1, function(row){
  tiers <- sapply(seq_along(row), function(i){
    val <- row[i]
    if(is.na(val)) return(NA_real_)
    if(val == 1){
      voc_tiers_map[[voc_vars25[i]]]
    } else if(val == 0){
      NA_real_
    } else if(val == -9){
      -9
    } else if(val == -8){
      -8
    } else if(val == -1){
      -1
    } else {
      NA_real_
    }
  })
  valid <- tiers[tiers %in% 0:4]
  if(length(valid) > 0){
    min(valid)
  } else {
    if(any(tiers == -9, na.rm = TRUE)) return(-9)
    if(any(tiers == -8, na.rm = TRUE)) return(-8)
    if(any(tiers == -1, na.rm = TRUE)) return(-1)
    NA_real_
  }
})

# Combine academic and vocational tiers: choose best (min numeric) among 0-4
educ25_raw <- mapply(function(a,v){
  a_t <- if(is.na(a) || !(a %in% 0:4)) NA_real_ else a
  v_t <- if(is.na(v) || !(v %in% 0:4)) NA_real_ else v
  valid <- c(a_t, v_t)[!is.na(c(a_t, v_t))]
  if(length(valid) > 0){
    min(valid)
  } else {
    codes <- c(a, v)
    if(any(codes == -9, na.rm = TRUE)) return(-9)
    if(any(codes == -8, na.rm = TRUE)) return(-8)
    if(any(codes == -1, na.rm = TRUE)) return(-1)
    NA_real_
  }
}, acad_tier25, voc_tier25_vec)
educ25 <- ifelse(is.na(educ25_raw), -3, educ25_raw)

# ---------- educ32 (age 32) ----------
adic_tier32 <- nvq_to_tier(merged$W9DANVQH)
voc_tier32 <- nvq_to_tier(merged$W9DVNVQH)
educ32_raw <- mapply(function(a,v){
  a_valid <- !is.na(a) && a %in% 0:4
  v_valid <- !is.na(v) && v %in% 0:4
  if(a_valid && v_valid){
    min(a, v)
  } else if(a_valid){
    a
  } else if(v_valid){
    v
  } else {
    if(!is.na(a) && a %in% c(-9,-8,-1,-3,-2)) return(a)
    if(!is.na(v) && v %in% c(-9,-8,-1,-3,-2)) return(v)
    NA_real_
  }
}, adic_tier32, voc_tier32)
educ32 <- ifelse(is.na(educ32_raw), -3, educ32_raw)

# ---------- educadtl32 (detailed academic qualifications) ----------
acad_vars <- paste0("W9ACQU0", LETTERS[1:19]) # A-S
educadtl32_raw <- apply(merged %>% select(all_of(acad_vars)), 1, function(row){
  if(any(row == -1, na.rm = TRUE)) return(-1)
  if(all(row == 2, na.rm = TRUE) && !any(is.na(row))){
    return(19) # None of these qualifications
  }
  if(any(row == -8, na.rm = TRUE)) return(-8)
  if(any(row == -9, na.rm = TRUE)) return(-9)
  if(any(is.na(row) | row == -3, na.rm = TRUE)) return(-3)
  first_yes <- which(row == 1)[1]
  if(is.na(first_yes)) return(-3) else return(first_yes)
})
# Labels
acad_labels_map <- c(
  W9ACQU0A = "Doctorate or equivalent",
  W9ACQU0B = "Masters or equivalent",
  W9ACQU0C = "Undergraduate or equivalent",
  W9ACQU0D = "Post-graduate Diplomas and Certificates",
  W9ACQU0E = "Diplomas in higher education and other higher education qualifications",
  W9ACQU0F = "Teaching qualifications for schools or further education (below degree level)",
  W9ACQU0G = "A/AS Levels or equivalent",
  W9ACQU0H = "Grade A-C, Level 4-9",
  W9ACQU0I = "Grade D-G, Level 1-3",
  W9ACQU0J = "SCE Higher",
  W9ACQU0K = "Scottish Certificate Sixth Year Studies",
  W9ACQU0L = "SCE Standard",
  W9ACQU0M = "National 4 and 5",
  W9ACQU0N = "National 2 and 3",
  W9ACQU0O = "Leaving Certificate",
  W9ACQU0P = "Junior Certificate grade A-C",
  W9ACQU0Q = "Junior Certificate grade D and below",
  W9ACQU0R = "Other academic qualifications (including overseas)",
  W9ACQU0S = "None of these qualifications"
)
adac_label_vec <- as.vector(acad_labels_map[acad_vars])
levels_adtl <- c(1:19, -8,-9,-2,-1,-3)
labels_adtl <- c(adac_label_vec, "Don’t know", "Refused", "No answer", "Not applicable", "Not asked at fieldwork stage")
educadtl32 <- factor(educadtl32_raw, levels = levels_adtl, labels = labels_adtl)

# ---------- educvdtl32 (detailed vocational qualifications) ----------
voc_vars32 <- names(merged)[grepl("^W9VCQU0", names(merged))]
educvdtl32_raw <- apply(merged %>% select(all_of(voc_vars32)), 1, function(row){
  if(any(row == -1, na.rm = TRUE)) return(-1)
  if(all(row == 2, na.rm = TRUE) && !any(is.na(row))){
    return(length(voc_vars32)) # None of these qualifications
  }
  if(any(row == -3, na.rm = TRUE)) return(-3)
  if(any(row == -9, na.rm = TRUE)) return(-9)
  if(any(is.na(row), na.rm = TRUE)) return(-3)
  first_yes <- which(row == 1)[1]
  if(is.na(first_yes)) return(-3) else return(first_yes)
})
# Labels
voc_labels_map <- c(
  W9VCQU0A = "Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor",
  W9VCQU0B = "Nursing or other medical qualifications (below degree level)",
  W9VCQU0C = "Level 4 or 5",
  W9VCQU0D = "Level 3",
  W9VCQU0E = "Level 2",
  W9VCQU0F = "Level 1",
  W9VCQU0G = "GNVQ Advanced",
  W9VCQU0H = "GNVQ Intermediate",
  W9VCQU0I = "Level 3",
  W9VCQU0J = "Level 2",
  W9VCQU0K = "Level Foundation",
  W9VCQU0L = "Advanced Craft, Part III",
  W9VCQU0M = "Craft, Part II",
  W9VCQU0N = "Craft, Part I",
  W9VCQU0O = "Level 3",
  W9VCQU0P = "Level 2",
  W9VCQU0Q = "Level 1",
  W9VCQU0R = "Advanced Diploma",
  W9VCQU0S = "Higher Diploma",
  W9VCQU0T = "RSA Diploma",
  W9VCQU0U = "RSA Stage I, II,III",
  W9VCQU0V = "Higher Level BTEC",
  W9VCQU0W = "BTEC National",
  W9VCQU0X = "BTEC First",
  W9VCQU0Y = "SCOTVEC National Certificate",
  W9VCQU0Z = "SCOTVEC first or general diploma",
  W9VCQU0AA = "SCOTVEC general diploma",
  W9VCQU0AB = "SCOTVEC modules",
  W9VCQU0AC = "HND or HNC",
  W9VCQU0AD = "OND or ONCM",
  W9VCQU0AE = "Junior certificate",
  W9VCQU0AF = "Other vocational qualifications (including some overseas)",
  W9VCQU0AG = "None of these qualifications",
  W9VCQU0AH = "Don’t know",
  W9VCQU0AI = "Refused"
)
voc_label_vec <- as.vector(voc_labels_map[voc_vars32])
levels_vdtl <- c(1:length(voc_vars32), -8,-9,-2,-1,-3)
labels_vdtl <- c(voc_label_vec, "Don’t know", "Refused", "No answer", "Not applicable", "Not asked at fieldwork stage")
educvdtl32 <- factor(educvdtl32_raw, levels = levels_vdtl, labels = labels_vdtl)

# ---------- Prepare final dataset ----------
final_df <- merged %>%
  select(NSID) %>%
  mutate(
    educ25 = educ25,
    educ32 = educ32,
    educadtl32 = educadtl32,
    educvdtl32 = educvdtl32
  )

# ---------- Write output ----------
write_csv(final_df, "data/output/cleaned_data.csv")
