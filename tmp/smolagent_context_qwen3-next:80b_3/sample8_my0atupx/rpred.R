library(haven);library(dplyr);library(purrr);library(readr)
w1<-read_delim("data/input/wave_one_lsype_young_person_2020.tab",delim="\t",show_col_types=FALSE)
w4<-read_delim("data/input/wave_four_lsype_young_person_2020.tab",delim="\t",show_col_types=FALSE)
w8m<-read_delim("data/input/ns8_2015_main_interview.tab",delim="\t",show_col_types=FALSE)
w8d<-read_delim("data/input/ns8_2015_derived.tab",delim="\t",show_col_types=FALSE)
w9m<-read_delim("data/input/ns9_2022_main_interview.tab",delim="\t",show_col_types=FALSE)
w9d<-read_delim("data/input/ns9_2022_derived_variables.tab",delim="\t",show_col_types=FALSE)
data<-w1%>%select(NSID)%>%full_join(w4%>%select(NSID),by="NSID")%>%full_join(w8m,by="NSID")%>%full_join(w8d,by="NSID")%>%full_join(w9m,by="NSID")%>%full_join(w9d,by="NSID")
mc<-c(-9,-8,-3,-2,-1)
map8<-function(x){if(is.na(x))return(NA_real_);if(x%in%mc)return(x);if(x>=4&&x<=5)return(0);if(x>=1&&x<=3)return(1);if(x==95)return(3);if(x==96)return(4);NA_real_}
map9<-function(x){if(is.na(x))return(NA_real_);if(x%in%mc)return(x);if(x>=4&&x<=5)return(0);if(x>=1&&x<=3)return(1);if(x==0)return(2);if(x==95)return(3);if(x==96)return(4);NA_real_}
comb<-function(a,v){if(!is.na(a)&&a%in%mc)return(a);if(!is.na(v)&&v%in%mc)return(v);if(!is.na(a)&&!is.na(v))return(min(a,v));if(!is.na(a))return(a);if(!is.na(v))return(v);NA_real_}
mapv<-function(n){if(is.na(n))return(NA_real_);if(n%in%mc)return(n);if(n>=4)return(0);if(n>=1&&n<=3)return(1);if(n==96)return(4);NA_real_}
gw8v<-function(...){v<-c(...);for(x in v)if(!is.na(x)&&x%in%mc)return(x);if(!is.na(v[10])&&v[10]==1)return(4);if(!is.na(v[11])&&v[11]==1)return(4);for(i in c(1:9,12:15))if(!is.na(v[i])&&v[i]==1)return(2);if(!is.na(v[16])&&v[16]==1)return(96);NA_real_}
data<-data%>%mutate(w8a=map_dbl(W8DHANVQH,map8),w9a=map_dbl(W9DANVQH,map9),w9v=map_dbl(W9DVNVQH,map9))
vcols<-paste0("W8VCQU0",LETTERS[1:18])
data$w8vr<-pmap_dbl(data[vcols],gw8v)
data$w8vc<-map_dbl(data$w8vr,mapv)
data$e25r<-mapply(comb,data$w8a,data$w8vc)
data$e32r<-mapply(comb,data$w9a,data$w9v)
data$educ25<-ifelse(is.na(data$e25r),-3,data$e25r)
data$educ32<-ifelse(is.na(data$e32r),-3,data$e32r)
albl<-c("Doctorate or equivalent","Masters or equivalent","Undergraduate or equivalent","Post-graduate Diplomas and Certificates","Diplomas in higher education and other higher education qualifications","Teaching qualifications for schools or further education (below degree level)","A/AS Levels or equivalent","Grade A-C, Level 4-9","Grade D-G, Level 1-3","SCE Higher","Scottish Certificate Sixth Year Studies","SCE Standard","National 4 and 5","National 2 and 3","Leaving Certificate","Junior Certificate grade A-C","Junior Certificate grade D and below","Other academic qualifications (including overseas)","None of these qualifications","Dont know","Refused","No answer")
av<-paste0("W9ACQU0",LETTERS[1:22])
gacad<-function(...){v<-c(...);for(i in 1:18)if(!is.na(v[i])){if(v[i]==1)return(albl[i]);if(v[i]%in%mc)return(as.character(v[i]))};for(i in 19:22)if(!is.na(v[i])){if(v[i]==1)return(albl[i]);if(v[i]%in%mc)return(as.character(v[i]))};an<-TRUE;for(i in 1:18)if(!is.na(v[i])&&v[i]!=2){an<-FALSE;break};if(an)return("None of these qualifications");NA_character_}
data$educadtl32<-pmap_chr(data[av],gacad)
vlbl<-c("Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor","Nursing or other medical qualifications (below degree level)","Level 4 or 5","Level 3","Level 2","Level 1","GNVQ Advanced","GNVQ Intermediate","Level 3","Level 2","Level Foundation","Advanced Craft, Part III","Craft, Part II","Craft, Part I","Level 3","Level 2","Level 1","Advanced Diploma","Higher Diploma","RSA Diploma","RSA Stage I, II,III","Higher Level BTEC","BTEC National","BTEC First","SCOTVEC National Certificate","SCOTVEC first or general diploma","SCOTVEC general diploma","SCOTVEC modules","HND or HNC","OND or ONCM","Junior certificate","Other vocational qualifications (including some overseas)","None of these qualifications","Dont know","Refused")
vv<-c(paste0("W9VCQU0",LETTERS[1:26]),"W9VCQUAA","W9VCQUAB","W9VCQUAC","W9VCQUAD","W9VCQUAE","W9VCQUAF","W9VCQUAG","W9VCQUAH","W9VCQUAI")
gvoc<-function(...){v<-c(...);for(i in 1:32)if(!is.na(v[i])){if(v[i]==1)return(vlbl[i]);if(v[i]%in%mc)return(as.character(v[i]))};for(i in 33:35)if(!is.na(v[i])){if(v[i]==1)return(vlbl[i]);if(v[i]%in%mc)return(as.character(v[i]))};an<-TRUE;for(i in 1:32)if(!is.na(v[i])&&v[i]!=2){an<-FALSE;break};if(an)return("None of these qualifications");NA_character_}
data$educvdtl32<-pmap_chr(data[vv],gvoc)
final_data<-data%>%select(NSID,educ25,educ32,educadtl32,educvdtl32)
dir.create("data/output",showWarnings=FALSE,recursive=TRUE)
write_csv(final_data,"data/output/cleaned_data.csv")
cat("Done. Rows:",nrow(final_data),"\n")