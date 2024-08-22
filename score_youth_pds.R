#Script to clean and score youth PDS data in ABCD dataset. 
#Created by MEAB on 2024-01-18
#last edited 2024-01-18

#Libraries and set folders
library(dplyr)
library(psych)
library(tidyr)
origdatadir <- 'C:/Users/marjo/Box/SPINS Project - ABCD R01/OriginalData/4.0 - Package_1221272/'
scriptsdir <- 'C:/Users/marjo/Box/SPINS Project - ABCD R01/Data/scoring_pds/'

#Read in the ABCD PDS data
header_ypds_raw <- read.table(paste0(origdatadir,"abcd_ypdms01.txt", sep=''), header = F, nrows = 1, as.is = T) 
ypds_raw <- read.csv(paste0(origdatadir,"abcd_ypdms01.txt"), skip = 2, header = F, sep="\t") 
colnames(ypds_raw)<- header_ypds_raw

#Setting 999 and 777 to NA
ypds_raw[,grepl("pds",names(ypds_raw))][ypds_raw[,grepl("pds",names(ypds_raw))]==999] <- NA
ypds_raw[,grepl("pds",names(ypds_raw))][ypds_raw[,grepl("pds",names(ypds_raw))]==777] <- NA

#Recoding variables based on Birdie's syntax. Variables for female scoring end in f, male end in m.
#Final score in Tanner stage form is called 'stage' and includes both sexes.
#Adrenal score is called 'astage' and gonadal score 'gstage'.
ypds_scored <- ypds_raw %>%
 mutate(petaf=ifelse(sex=="F" & pds_ht2_y==1,1,
                    ifelse(sex=="F" & pds_ht2_y==2,2,
                           ifelse(sex=="F" & pds_ht2_y==3,3,
                                  ifelse(sex=="F" & pds_ht2_y==4,5,
                                         NA)))),
       petbf=ifelse(sex=="F" & pds_bdyhair_y==1,1,
                    ifelse(sex=="F" & pds_bdyhair_y==2,2,
                           ifelse(sex=="F" & pds_bdyhair_y==3,4,
                                  ifelse(sex=="F" & pds_bdyhair_y==4,5,
                                         NA)))),
       petcf=ifelse(sex=="F" & pds_skin2_y==1,1,
                    ifelse(sex=="F" & pds_skin2_y==2,2,
                           ifelse(sex=="F" & pds_skin2_y==3,4,
                                  ifelse(sex=="F" & pds_skin2_y==4,5,
                                         NA)))),
       petdf=ifelse(pds_f4_2_y==1,1,
                    ifelse(pds_f4_2_y==2,3,
                           ifelse(pds_f4_2_y==3,4,
                                  ifelse(pds_f4_2_y==4,5,
                                         NA)))),         
       petef=ifelse(pds_f5_y==1,1,
                    ifelse(pds_f5_y==4,5,
                           NA)),
       petam=ifelse(sex=="M" & pds_ht2_y==1,1,
                     ifelse(sex=="M" & pds_ht2_y==2,3,
                            ifelse(sex=="M" & pds_ht2_y==3,4,
                                   ifelse(sex=="M" & pds_ht2_y==4,5,
                                          NA)))),
       petbm=ifelse(sex=="M" & pds_bdyhair_y==1,1,
                    ifelse(sex=="M" & pds_bdyhair_y==2,2,
                           ifelse(sex=="M" & pds_bdyhair_y==3,4,
                                  ifelse(sex=="M" & pds_bdyhair_y==4,5,
                                         NA)))),
       petcm=ifelse(sex=="M" & pds_skin2_y==1,1,
                    ifelse(sex=="M" & pds_skin2_y==2,2,
                           ifelse(sex=="M" & pds_skin2_y==3,3,
                                  ifelse(sex=="M" & pds_skin2_y==4,4,
                                         NA)))),
       petdm=ifelse(sex=="M" & pds_m4_y==1,1,
                    ifelse(sex=="M" & pds_m4_y==2,2,
                           ifelse(sex=="M" & pds_m4_y==3,3,
                                  ifelse(sex=="M" & pds_m4_y==4,5,
                                         NA)))),
       petem=ifelse(sex=="M" & pds_m5_y!=999,pds_m5_y,NA)) %>%
  mutate(adrenf=rowMeans(cbind(petbf,petcf),na.rm=F)) %>%
  mutate(adrenf2=ifelse(adrenf==1,1,
                        ifelse(pds_bdyhair_y==1 & adrenf==1.5,1,
                           ifelse(pds_bdyhair_y==2 & adrenf==1.5,2,
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
                                ifelse(gonadm==2 & petem==1 & pds_m4_y==1,1,
                                   ifelse(gonadm==2 & petem==1 & pds_m4_y>1,2,
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
  mutate(stage_missing_perc=100*((rowSums(is.na(cbind(pds_ht2_y,pds_bdyhair_y,pds_skin2_y,pds_f4_2_y,pds_f5_y,pds_m4_y,pds_m5_y)))-2)/5),
         astage_missing_perc=100*(rowSums(is.na(cbind(pds_bdyhair_y,pds_skin2_y)))/2),
         gstage_missing_perc=100*((rowSums(is.na(cbind(pds_ht2_y,pds_f4_2_y,pds_f5_y,pds_m4_y,pds_m5_y)))-2)/3) ) %>%
  mutate(stage=ifelse(stage_missing_perc>0, NA, rowMeans(cbind(adrenf2,gonadf2,adrenm2,gonadm2),na.rm=T)),
         astage=ifelse(astage_missing_perc>0, NA, rowMeans(cbind(adrenf2,adrenm2),na.rm=T)),
         gstage=ifelse(gstage_missing_perc>0, NA, rowMeans(cbind(gonadf2,gonadm2),na.rm=T)) )

#calculate pds score from simply averaging all items
ypds_scored <- ypds_scored %>% mutate(pdsscoref=rowMeans(ypds[,c("pds_ht2_y","pds_bdyhair_y","pds_skin2_y","pds_f4_2_y","pds_f5_y")]),
                        pdsscorem=rowMeans(ypds[,c("pds_ht2_y","pds_bdyhair_y","pds_skin2_y","pds_m4_y","pds_m5_y")]))
ypds_scored <- ypds_scored %>% mutate(pdsscore=ifelse(is.na(pdsscoref),pdsscorem,pdsscoref))

#filter to right time point (can add other selection criteria) and select necessary variables
ypds_scored_filtered <- ypds_scored %>%
  filter(eventname=="baseline_year_1_arm_1") %>%
  select(abcd_ypdms01_id,subjectkey,src_subject_id,interview_date,interview_age,sex,eventname,pdsscore,stage,astage,gstage)
ypds_scored_2tps <- ypds_scored %>%
  filter(eventname=="baseline_year_1_arm_1"|eventname=="2_year_follow_up_y_arm_1") %>%
  select(abcd_ypdms01_id,subjectkey,src_subject_id,interview_date,interview_age,sex,eventname,pdsscore,stage,astage,gstage)
ypds_scored_alltps <- ypds_scored %>%
  select(abcd_ypdms01_id,subjectkey,src_subject_id,interview_date,interview_age,sex,eventname,pdsscore,stage,astage,gstage)

#Save
write.csv(ypds_scored, paste0(scriptsdir,'ypds_scored.csv'),row.names = F)
write.csv(ypds_scored_filtered, paste0(scriptsdir,'ypds_scored_filtered.csv'),row.names = F)
write.csv(ypds_scored_2tps, paste0(scriptsdir,'ypds_scored_baseline_2year.csv'),row.names = F)
write.csv(ypds_scored_alltps, paste0(scriptsdir,'ypds_scored_anytimepoint.csv'),row.names = F)
