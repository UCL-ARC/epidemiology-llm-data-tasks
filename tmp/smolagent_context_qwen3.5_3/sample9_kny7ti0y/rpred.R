library(haven); library(dplyr); library(purrr); library(labelled); library(readr)
dir.create("data/output", showWarnings=FALSE, recursive=TRUE)
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim="\t")
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim="\t")
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim="\t")
data <- w1 %>% full_join(w2, by="NSID") %>% full_join(w4, by="NSID")
data <- data %>% select(NSID, W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad)
harm <- function(x) { case_when(x==-999~-2, x==-99~-3, x==-98~-3, x==-94~-8, x==-92~-9, x==-91~-1, x==-1~-8, TRUE~as.numeric(x)) }
data <- data %>% mutate(W1hiqualmum=harm(W1hiqualmum), W1hiqualdad=harm(W1hiqualdad), W2hiqualmum=harm(W2hiqualmum), W2hiqualdad=harm(W2hiqualdad), w4hiqualmum=harm(w4hiqualmum), w4hiqualdad=harm(w4hiqualdad))
consol <- function(a,b,c) { sapply(1:length(a), function(i) { if(!is.na(a[i])&&a[i]>0) a[i] else if(!is.na(b[i])&&b[i]>0) b[i] else if(!is.na(c[i])&&c[i]>0) c[i] else if(!is.na(a[i])&&a[i]<0) a[i] else if(!is.na(b[i])&&b[i]<0) b[i] else if(!is.na(c[i])&&c[i]<0) c[i] else -3 }) }
data <- data %>% mutate(educdtlma=consol(W1hiqualmum,W2hiqualmum,w4hiqualmum), educdtlpa=consol(W1hiqualdad,W2hiqualdad,w4hiqualdad))
coll <- function(x) { case_when(x%in%1:4~0, x%in%5:17~1, x==18~2, x==19~3, x==20~4, x<0~as.numeric(x), TRUE~as.numeric(x)) }
data <- data %>% mutate(educma=coll(educdtlma), educpa=coll(educdtlpa))
dlabs <- c("Higher Degree"=1, "First Degree"=2, "HE Diploma"=3, "HNC/HND/NVQ4"=4, "Teaching"=5, "Nursing"=6, "A Levels"=7, "OND/ONC"=8, "NVQ3"=9, "CSYS"=10, "Scot Higher"=11, "AS Level"=12, "Trade app"=13, "NVQ2"=14, "GCSE A-C"=15, "GCSE D-E"=16, "NVQ1"=17, "Youth train"=18, "Unspec"=19, "No qual"=20, "NA"=-1, "Sch NA"=-2, "Not asked"=-3, "DK"=-8, "Refusal"=-9)
clabs <- c("NVQ4-5"=0, "NVQ1-3"=1, "None"=2, "Other"=3, "No qual"=4, "NA"=-1, "Sch NA"=-2, "Not asked"=-3, "DK"=-8, "Refusal"=-9)
data <- data %>% mutate(educdtlma=labelled(educdtlma,labels=dlabs), educdtlpa=labelled(educdtlpa,labels=dlabs), educma=labelled(educma,labels=clabs), educpa=labelled(educpa,labels=clabs))
data <- data %>% select(NSID, educma, educpa, educdtlma, educdtlpa)
write_csv(data, "data/output/cleaned_data.csv")
cat("Done:", nrow(data), "rows\n")