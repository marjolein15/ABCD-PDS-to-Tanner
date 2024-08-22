#Script to clean and score parent PDS data in ABCD baseline dataset. 
#Created by MEAB on 2021-11-02
#last edited 2021-11-02

#Libraries and set folders
library(dplyr)
library(psych)
library(tidyr)
origdatadir <- 'C:/Users/marjo/Box/PEP/ABCD_R01/OriginalData/4.0 - Package_1194258/'
scriptsdir <- 'C:/Users/marjo/Box/PEP/ABCD_R01/Data/scoring_pds/'

#Read in the ABCD PDS data
header_ppds_raw <- read.table(paste0(origdatadir,"abcd_ppdms01.txt", sep=''), header = F, nrows = 1, as.is = T) 
ppds_raw <- read.csv(paste0(origdatadir,"abcd_ppdms01.txt"), skip = 2, header = F, sep="\t") 
colnames(ppds_raw)<- header_ppds_raw

#Setting 999 to NA
ppds_raw[,grepl("pds",names(ppds_raw))][ppds_raw[,grepl("pds",names(ppds_raw))]==999] <- NA

#Recoding variables based on Birdie's syntax. Variables for female scoring end in f, male end in m.
#Final score in Tanner stage form is called 'stage' and includes both sexes.
#Adrenal score is called 'astage' and gonadal score 'gstage'.
ppds_scored <- ppds_raw %>%
 mutate(petaf=ifelse(sex=="F" & pds_1_p==1,1,
                    ifelse(sex=="F" & pds_1_p==2,2,
                           ifelse(sex=="F" & pds_1_p==3,3,
                                  ifelse(sex=="F" & pds_1_p==4,5,
                                         NA)))),
       petbf=ifelse(sex=="F" & pds_2_p==1,1,
                    ifelse(sex=="F" & pds_2_p==2,2,
                           ifelse(sex=="F" & pds_2_p==3,4,
                                  ifelse(sex=="F" & pds_2_p==4,5,
                                         NA)))),
       petcf=ifelse(sex=="F" & pds_3_p==1,1,
                    ifelse(sex=="F" & pds_3_p==2,2,
                           ifelse(sex=="F" & pds_3_p==3,4,
                                  ifelse(sex=="F" & pds_3_p==4,5,
                                         NA)))),
       petdf=ifelse(pds_f4_p==1,1,
                    ifelse(pds_f4_p==2,3,
                           ifelse(pds_f4_p==3,4,
                                  ifelse(pds_f4_p==4,5,
                                         NA)))),         
       petef=ifelse(pds_f5b_p==1,1,
                    ifelse(pds_f5b_p==4,5,
                           NA)),
       petam=ifelse(sex=="M" & pds_1_p==1,1,
                     ifelse(sex=="M" & pds_1_p==2,3,
                            ifelse(sex=="M" & pds_1_p==3,4,
                                   ifelse(sex=="M" & pds_1_p==4,5,
                                          NA)))),
       petbm=ifelse(sex=="M" & pds_2_p==1,1,
                    ifelse(sex=="M" & pds_2_p==2,2,
                           ifelse(sex=="M" & pds_2_p==3,4,
                                  ifelse(sex=="M" & pds_2_p==4,5,
                                         NA)))),
       petcm=ifelse(sex=="M" & pds_3_p==1,1,
                    ifelse(sex=="M" & pds_3_p==2,2,
                           ifelse(sex=="M" & pds_3_p==3,3,
                                  ifelse(sex=="M" & pds_3_p==4,4,
                                         NA)))),
       petdm=ifelse(sex=="M" & pds_m4_p==1,1,
                    ifelse(sex=="M" & pds_m4_p==2,2,
                           ifelse(sex=="M" & pds_m4_p==3,3,
                                  ifelse(sex=="M" & pds_m4_p==4,5,
                                         NA)))),
       petem=ifelse(sex=="M" & pds_m5_p!=999,pds_m5_p,NA)) %>%
  mutate(adrenf=rowMeans(cbind(petbf,petcf),na.rm=F)) %>%
  mutate(adrenf2=ifelse(adrenf==1,1,
                        ifelse(pds_2_p==1 & adrenf==1.5,1,
                           ifelse(pds_2_p==2 & adrenf==1.5,2,
                               ifelse(adrenf==2,2,
                                   ifelse(adrenf==2.5,3,
                                      ifelse(adrenf==3,3,
                                         ifelse(adrenf==3.5,4,
                                            ifelse(adrenf==4,4,
                                               ifelse(adrenf==4.5,5,
                                                  ifelse(adrenf==5,5,
                                                     NA))))))))))) %>%
  mutate(adrenm=rowMeans(cbind(petbm,petcm),na.rm=F)) %>%
  mutate(adrenm2=ifelse(adrenm==1,1,
                        ifelse(adrenm==1.5 & petcm==1,1,
                          ifelse(adrenm==1.5 & petcm==2,2,
                             ifelse(adrenm==2,2,
                               ifelse(adrenm==2.5 & petbm<4,2,
                                 ifelse(adrenm==2.5 & petbm==4,3, 
                                   ifelse(adrenm==3,3,
                                      ifelse(adrenm==3.5,4,
                                        ifelse(adrenm==4,4,
                                           ifelse(adrenm==4.5,5,
                                             ifelse(adrenm==5,5,
                                                NA)))))))))))) %>%
  mutate(gonadf=rowMeans(cbind(petaf,petdf),na.rm=F)) %>%
  mutate(gonadf2=ifelse(gonadf==1 & petef==1,1,
                        ifelse(gonadf==1.5 & petef==1,1,
                           ifelse(gonadf==2 & petef==1,2,
                              ifelse(gonadf==2.5 & petef==1,2,
                                 ifelse(gonadf==3 & petef==1,3,
                                    ifelse(gonadf==3.5 & petef==1,3,
                                      ifelse(gonadf==4 & petef==1,3,
                                          ifelse(gonadf==4.5 & petef==1,4,
                                               ifelse(gonadf==5 & petef==1,4,
                                                   ifelse(gonadf==1 & petef==5,2,
                                                       ifelse(gonadf==1.5 & petef==5,3,
                                                           ifelse(gonadf==2 & petef==5,4,
                                                               ifelse(gonadf==2.5 & petef==5,4,
                                                                  ifelse(gonadf==3 & petef==5,4,
                                                                      ifelse(gonadf==3.5 & petef==5,5,
                                                                           ifelse(gonadf==4 & petef==5,5,
                                                                              ifelse(gonadf==4.5 & petef==5,5,
                                                                                 ifelse(gonadf==5 & petef==5,5,
                                                                                    NA))))))))))))))))))) %>%
  mutate(gonadm=rowMeans(cbind(petam,petdm),na.rm=F)) %>%
  mutate(gonadm2=ifelse(gonadm==1 & petem==1,1,
                        ifelse(gonadm==1 & petem>1,2,
                          ifelse(gonadm==1.5 & petem==1,1,
                             ifelse(gonadm==1.5 & petem>1,2,
                                ifelse(gonadm==2 & petem==1 & pds_m4_p==1,1,
                                   ifelse(gonadm==2 & petem==1 & pds_m4_p>1,2,
                                       ifelse(gonadm==2 & petem>1,3,
                                          ifelse(gonadm==2.5 & petem==1,2,
                                               ifelse(gonadm==2.5 & petem>1,3,
                                                   ifelse(gonadm==3 ,3,
                                                       ifelse(gonadm==3.5 & petem<3,4,
                                                           ifelse(gonadm==3.5 & petem>2,5,
                                                              ifelse(gonadm==4 & petem<3,4,
                                                                  ifelse(gonadm==4 & petem>2,5,
                                                                     ifelse(gonadm>4,5,
                                                                        NA)))))))))))))))) %>%
  mutate(stage_missing_perc=100*((rowSums(is.na(cbind(pds_1_p,pds_2_p,pds_3_p,pds_f4_p,pds_f5b_p,pds_m4_p,pds_m5_p)))-2)/5),
         astage_missing_perc=100*(rowSums(is.na(cbind(pds_2_p,pds_3_p)))/2),
         gstage_missing_perc=100*((rowSums(is.na(cbind(pds_1_p,pds_f4_p,pds_f5b_p,pds_m4_p,pds_m5_p)))-2)/3) ) %>%
  mutate(stage=ifelse(stage_missing_perc>0, NA, rowMeans(cbind(adrenf2,gonadf2,adrenm2,gonadm2),na.rm=T)),
         astage=ifelse(astage_missing_perc>0, NA, rowMeans(cbind(adrenf2,adrenm2),na.rm=T)),
         gstage=ifelse(gstage_missing_perc>0, NA, rowMeans(cbind(gonadf2,gonadm2),na.rm=T)) )

#filter to right time point (can add other selection criteria) and select necessary variables
ppds_scored_filtered <- ppds_scored %>%
  filter(eventname=="baseline_year_1_arm_1") %>%
  select(abcd_ppdms01_id,subjectkey,src_subject_id,interview_date,interview_age,sex,eventname,stage,astage,gstage)
ppds_scored_2tps <- ppds_scored %>%
  filter(eventname=="baseline_year_1_arm_1"|eventname=="2_year_follow_up_y_arm_1") %>%
  select(abcd_ppdms01_id,subjectkey,src_subject_id,interview_date,interview_age,sex,eventname,stage,astage,gstage)

#Save
write.csv(ppds_scored, paste0(scriptsdir,'ppds_scored.csv'),row.names = F)
write.csv(ppds_scored_filtered, paste0(scriptsdir,'ppds_scored_filtered.csv'),row.names = F)
write.csv(ppds_scored_2tps, paste0(scriptsdir,'ppds_scored_baseline_2year.csv'),row.names = F)
