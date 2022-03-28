rm(list = ls())
##################################################
###cross-sectional analysis @13 sample may 2021###
##################################################

#load packages
library(dplyr)
library(readr)
library(foreign)
library(mice)
library(haven)

#F13
core <- readRDS("V:/medewerkers/038870 Durkut, M/genr_mri_core_data_20april2020_prelim.rds")
core13 <- subset(core, freesurfer_qc_F13=="usable" &  exclude_incidental_F13=="Include")
summary(core13)

#SRS13 data and covariates
cov <- read.spss("V:/medewerkers/038870 Durkut, M/Covariates_MRI_analyses.sav", to.data.frame = TRUE)

#removing subjects with missing data on SRS 
data_subset1 <- cov[ , c("srs_total_m_13")]
datasrs13cc <- cov[complete.cases(data_subset1), ]

#merging MRI and SRS data
#mri13$IDC <- as.character(mri13$IDC)
#trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function to remove empty spaces caused by the _F13 notation
#mri13$IDC <- trim(mri13$IDC)
#str(datasrs13cc)
#str(mri13)
#datasrs13cc$IDC <- as.character(datasrs13cc$IDC)
total <- merge(datasrs13cc, core13, by = 'IDC')

summary(total)
#2026 (old was 1969)


#####################################################################################################
#####add variables: HD13, IQ13, sum_att_problem_score, AGECHILD_GR1093, age_13MRI, dif_age13, ASD #####
#####################################################################################################


#handedness 13 
#new HD @13 variable
handedness <- read.spss("V:/medewerkers/038870 Durkut, M/CHILDMRI13_Gr1098_C1_EdingburghHandedness_26022021.sav", to.data.frame = TRUE)
handedness$HD13 <-  handedness$HD12_F13
handedness <- select(handedness, IDC, HD13)
totalhd <- merge(total, handedness, by="IDC", all.x = TRUE)

#IQ13 
IQ_13 <- read.spss("V:/medewerkers/038870 Durkut, M/19112020_WISC_PRELIMINARYDATA.sav", to.data.frame = TRUE)
IQ_13 <- select(IQ_13, IDC, WISC13_FSIQ)
totalIQ <- merge(totalhd, IQ_13, by="IDC", all.x = TRUE)

# ASD
v1 <- read.spss("V:/medewerkers/038870 Durkut, M/data_v1.sav", to.data.frame=TRUE)
asd <- select(v1, IDC, ASD)
totalasd <- merge(totalIQ, asd, by="IDC", all.x = TRUE)

#child attention problem score 13 
v1 <- read.spss("V:/medewerkers/038870 Durkut, M/GR1093-E1_CBCL_18062020.sav", to.data.frame=TRUE)
att13 <- select(v1, IDC, sum_att_14)
summary(att13)
dataa <- merge(totalasd, att13, by="IDC", all.x = TRUE)
summary(dataa)

#AGECHILD_GR1093
data_SRS13 <- read.spss("V:/medewerkers/038870 Durkut, M/GR1093-E2-SRS_24062020.sav", to.data.frame = TRUE)
AGECHILD_GR1093 <- select(data_SRS13, IDC, AGECHILD_GR1093)
totalagesrs <- merge(dataa, AGECHILD_GR1093, by="IDC", all.x = TRUE)

#age MRI13
age_13mri <- read.spss("V:/medewerkers/038870 Durkut, M/F13_MRI_IDC_AGE_11082020.sav", to.data.frame = TRUE)
totalagemri <- merge(totalagesrs, age_13mri, by="IDC", all.x = TRUE)

#dif_age13
totalagemri$dif_age13 <- totalagemri$AGECHILD_GR1093 - totalagemri$AGEMRI_F13
final <- totalagemri
summary(final)


#exclude one sibbling for each sibbling or twin pair 
final<- final[sample(nrow(final)),]
data_final <- final[!duplicated(final$MOTHER),]

#select needed variables
data_final <- select(data_final, IDC, GENDER, ETHNMv2, ETHNINFv2_3groups, EDUCM5, INCOME13, SMOKE_ALL, mdrink_updated, sum_att_14, AGECHILD_GR1093, dif_age13, srs_total_m_13, HD13, WISC13_FSIQ, ASD, AGEMRI_F13)
summary(data_final)

#recode categroical variables 
str(data_final)
library(car)
#EDUCM5
data_final$EDUCM5 <- recode(data_final$EDUCM5, "'no education finished'= 'primary'")
data_final$EDUCM5 <- recode(data_final$EDUCM5, "'secondary, phase 1'= 'secondary'")
data_final$EDUCM5 <- recode(data_final$EDUCM5, "'secondary, phase 2'= 'secondary'")
data_final$EDUCM5 <- recode(data_final$EDUCM5, "'higher, phase 1'= 'higher'")
data_final$EDUCM5 <- recode(data_final$EDUCM5, "'higher, phase 2'= 'higher'")
#ETHNMv2
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Indonesian' = 'nonDutch, western'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Cape Verdian' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Moroccan' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Dutch Antilles' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Surinamese' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Turkish' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'African' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'American,western' = 'nonDutch, western'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'American, non western' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Asian, western' = 'nonDutch, western'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Asian, non western' = 'nonDutch, nonwestern'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'European' = 'nonDutch, western'")
data_final$ETHNMv2 <- recode(data_final$ETHNMv2, "'Oceanie' = 'nonDutch, western'")
#INCOME13
summary(data_final$INCOME13)
data_final$INCOME13 <- recode(data_final$INCOME13, "'Less than ??? 800' = 'less than 1200'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 800-1199' = 'less than 1200'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 1200-1599' = '1200-2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 1600-1999' = '1200-2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 2000-2399' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 2400-2799' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 2800-3599' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 3600-4399' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 4400-5199' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'??? 5200-5999' = '>2000'")
data_final$INCOME13 <- recode(data_final$INCOME13, "'More than ??? 6000' = '>2000'")

summary(data_final)
setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
write_sav(data_final, "13cs.sav")
data_final <- read.spss("V:/medewerkers/038870 Durkut, M/13cs.sav", to.data.frame = TRUE)

#change IDC variable to match the RS IDC variable
as.factor(data_final$IDC)
data_final$IDC <- sub("^", "sub-", data_final$IDC)
data_final$IDC <- interaction(data_final$IDC,"_ses-F13", sep = "")

#Save this dataset to be able to check which subjects have no corthical thickness or surface area data
setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
write_sav(data_final, "df.sav") #Save this dataset to be able to check which subjects have no corthical thickness or surface area data.  data

###### Code for RS #######  See also "remove subjects without lgi.R" file 
#thickness
#lh.thickness.fwhm10.fsaverage.mgh
#library("foreign")
#df <- read.spss("/home/r074008/melisa_srs/df.sav", to.data.frame = TRUE)
#idcs <- df$IDC
#t <- file.exists(file.path("/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/", idcs, "surf", "lh.thickness.fwhm10.fsaverage.mgh"))
#summary(t)
#t <- as.data.frame(t)
#setwd("/home/r074008/melisa_srs")
#library(haven)
#write_sav(t, "thickness.sav")

#surface area
#library("foreign")
#df <- read.spss("/home/r074008/melisa_srs/df.sav", to.data.frame = TRUE)
#idcs <- df$IDC
#t <- file.exists(file.path("/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/", idcs, "surf", "lh.area.pial.fwhm10.fsaverage.mgh"))
#summary(t)
#t <- as.data.frame(t)
#setwd("/home/r074008/melisa_srs")
#library(haven)
#write_sav(t, "area.sav")

###########################################################
###### cortical thickness and surface area @13 sample #####
###########################################################
df <- read.spss("V:/medewerkers/038870 Durkut, M/df.sav", to.data.frame = TRUE)

#remove subjects without Cortical Thickness and surf area data (same for both)
data_final <- df[-c(759),]

#Save this dataset to be able to check which subjects have no LGI data
#There are no subjects that have LGI data but no thickness/area data. So the participants that have LGI data form a subsample of the participats that have surf area and cortical thickness data
setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
write_sav(data_final, "df_cs.sav") #Place this datafile in de RS folder

###### Code for RS #######  See also "remove subjects without lgi.R" file 
#library("foreign")
#df <- read.spss("/home/r074008/melisa_srs/df_cs.sav", to.data.frame = TRUE)
#idcs <- df$IDC
#t <- file.exists(file.path("/mnt/data/genr/mrdata/GenR_MRI/bids/derivatives/freesurfer/6.0.0/", idcs, "surf", "lh.pial_lgi.fwhm5.fsaverage.mgh"))
#summary(t)
#t <- as.data.frame(t)
#setwd("/home/r074008/melisa_srs")
#library(haven)
#write_sav(t, "lgisurf.sav")

##########################################
# LGI cross-sectional analysis @13 sample#
##########################################
#load packages
library(dplyr)
library(readr)
library(foreign)
library(mice)
df <- read.spss("V:/medewerkers/038870 Durkut, M/df_cs.sav", to.data.frame = TRUE)
lgi <- read.spss("V:/medewerkers/038870 Durkut, M/lgisurf.sav", to.data.frame = TRUE)
#get the row numbers that have no LGI data
lgi$IDC <- NA
lgi$IDC <- 1:length(lgi$t)
lgi <- subset(lgi, t < 1)
v <- as.vector(lgi$IDC)
v# 67  128  134  320  659  752  977 1113 1167 1564 1591 1763 1800

#make an extra variable to be able to remove these subjects after MICE
df$lgi <- NA
df$lgi <- 1
df$lgi[c(67, 128, 134, 320, 659, 752, 977, 1113, 1167, 1564, 1591, 1763, 1800)] <- 0
#The following subjects do not have lh.pial_lgi.fwhm5.fsaverage.mgh: sub-1195_ses-F13, sub-1262_ses-F13, sub-2645_ses-F13, sub-6413_ses-F13, sub-6577_ses-F13, sub-66_ses-F13, sub-7020_ses-F13, sub-7315_ses-F13, sub-7913_ses-F13, sub-8327_ses-F13
df$lgi[c(567, 488, 1917, 1219, 1320, 1940, 1151, 594, 916, 1098)] <- 0


df$ASD[is.na(df$ASD)] <- 0 #only for subettting (sens analysis)

#df$srs_total_m_13 <- (df$srs_total_m_13)/18
#hist(df$srs_total_m_13, col = "lightblue", xlim = range(0:3), xlab = "SRS", main = "Histogram SRS W3")
#df$srs_total_m_13 <- sqrt(df$srs_total_m_13)
#hist(df$srs_total_m_13, col = "lightblue", xlim = range(0:2), xlab = "SRS square root transformed", main = "Histogram SRS W3")


df$srs_total_m_13 <- sqrt(df$srs_total_m_13) #sqrt transform
summary(df)

#include extra variable for sensitivity analyses (ASD and srs cutoffs)
df$sens <- NA
for(i in 1:length(df$IDC)){if(df$GENDER[i] == "girl" &&  df$srs_total_m_13[i] > sqrt(18) ) {df$sens[i] = 0} else {df$sens[i] = 1}}
for(i in 1:length(df$IDC)){if(df$GENDER[i] == "boy" &&  df$srs_total_m_13[i] > sqrt(19.409)) {df$sens[i] = 0} else {df$sens[i] = df$sens[i]}}
for(i in 1:length(df$IDC)){if(df$ASD[i] > 0) {df$sens[i] = 0} else {df$sens[i] = df$sens[i]}}


##################################
####### @13 global measures ######
##################################
library(readr)
tbv <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_tbv_stats_pull23Nov2020_noDups.rds")
aseg13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aseg_stats_pull23Nov2020_noDups.rds")
aparc13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aparc_stats_pull23Nov2020_noDups.rds")

#select variables
tbv$MeanThickness_f13 <- (tbv$lh_MeanThickness_f13 + tbv$rh_MeanThickness_f13)/ 2

tbv$WhiteSurfArea_f13 <- (tbv$lh_WhiteSurfArea_f13 + tbv$rh_WhiteSurfArea_f13)/ 2
tbv13 <- select(tbv, IDC, CortexVol_f13, SubCortGrayVol_f13, CerebralWhiteMatterVol_f13, SupraTentorialVol_f13, MeanThickness_f13, WhiteSurfArea_f13)

aseg13$Cerebellum_Cortex_vol_f13 <- aseg13$Left_Cerebellum_Cortex_vol_f13 + aseg13$Right_Cerebellum_Cortex_vol_f13
aseg13$Cerebellum_White_Matter_vol_f13 <- aseg13$Left_Cerebellum_White_Matter_vol_f13 + aseg13$Right_Cerebellum_White_Matter_vol_f13
aseg13$Amygdala_vol_f13 <- (aseg13$Left_Amygdala_vol_f13 + aseg13$Right_Amygdala_vol_f13)/2

aseg13 <- select(aseg13, IDC, Cerebellum_Cortex_vol_f13, Cerebellum_White_Matter_vol_f13, Amygdala_vol_f13, CSF_vol_f13)



global13 <- merge(aseg13, tbv13, by = 'IDC')

global13$tbv13 <- global13$SupraTentorialVol_f13 + global13$Cerebellum_Cortex_vol_f13 + global13$Cerebellum_White_Matter_vol_f13
global13 <- subset(global13, select = -c(SupraTentorialVol_f13))

#mean gyrification 
meangyr <- read.spss("V:/medewerkers/038870 Durkut, M/mean_lGI.sav", to.data.frame = TRUE)
meangyr$meangyr13 <- (meangyr$mean_lGI_lh_f13 + meangyr$mean_lGI_rh_f13)/ 2
meangyr13 <- select(meangyr, IDC, meangyr13)
global13 <- merge(meangyr13, global13, by = 'IDC')
global13$IDC <- as.character(global13$IDC)

global13$new_IDC <- NA
for(i in 1:length(global13$IDC)){global13$new_IDC[i] <- strsplit(global13$IDC[i], '_')[[1]][1]} 
global13$IDC <- global13$new_IDC
global13$IDC <- sub("^", "sub-", global13$IDC)
global13$IDC <- interaction(global13$IDC,"_ses-F13", sep = "")
global13 <- subset(global13, select = -c(new_IDC))
summary(global13)

#merge
str(df$IDC)
global13$IDC <- as.factor(global13$IDC)
str(global13$IDC)
v <- merge(df, global13, by= 'IDC')
data_final<- v

#Baseline
#data_final$srs_total_m_13 <- (data_final$srs_total_m_13)^2
#data_final$srs_total_m_13 <- (data_final$srs_total_m_13)/18
summary(data_final$srs_total_m_13)

data_final <- data_final[-c(376),]
data_final <- data_final[-c(370),]

summary(data_final)

continuous <- function(x)
{
  standev <- sd(x, na.rm = T)
  meanvar <- mean(x, na.rm = T)
  paste(round(meanvar, 1), '(', round(standev, 1), ')')
}

continuousmedian <- function(x)
{
  iqr <- IQR(x, na.rm = T)
  median <- median(x, na.rm = T)
  paste(round(median, 1), '(', round(iqr, 2), ')')
}

categorical <- function(x)
{
  tab1 <- prop.table(table(x, useNA = 'always'))
  tab2 <- table(x, useNA = "always")
  print(paste(round(tab1 * 100, 1), '%', names(tab1), collapse = ','))
  print(paste(tab2, names(tab2)))
}

categorical(data_final$GENDER)
categorical(data_final$ETHNINFv2)
categorical(data_final$HD13)
summary(data_final$WISC13_FSIQ)
continuous(data_final$WISC13_FSIQ)
summary(data_final$sum_att_14)
continuousmedian(data_final$sum_att_14)
summary(data_final$srs_total_m_13)
continuous(data_final$AGECHILD_GR1093)
summary(data_final$AGECHILD_GR1093)
continuous(data_final$AGEMRI_F13)
summary(data_final$AGEMRI_F13)
categorical(data_final$ED2UCM5)
categorical(data_final$INCOME13)
categorical(data_final$SMOKE_ALL)
categorical(data_final$mdrink_updated)
categorical(data_final$sens)
categorical(data_final$lgi)

# Initial sample
library(foreign)
data_v1 <- read.spss("V:/medewerkers/038870 Durkut, M/data_v1.sav", to.data.frame=TRUE)
data_v1$IDC2 <- NA
data_v1$IDC2 <- as.factor(data_v1$IDC2)
data_v1$IDC2 <- sub("^", "sub-", data_v1$IDC)
data_v1$IDC2 <- interaction(data_v1$IDC2,"_ses-F13", sep = "")
data_v1$IDC <- data_v1$IDC2

library(car)
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'no education finished'= 'primary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'secondary, phase 1'= 'secondary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'secondary, phase 2'= 'secondary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'higher, phase 1'= 'higher'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'higher, phase 2'= 'higher'")

#income: less than 1200 = 1, 1200 - 2000 =2, >2000 = 3
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'Less than ??? 800' = 'less than 1200'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 800-1200' = 'less than 1200'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 1200-1600' = '1200-2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 1600-2000' = '1200-2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 2000-2400' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 2400-2800' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 2800-3200' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 3200-4000' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 4000-4800' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'??? 4800-5600' = '>2000'")
data_v1$INCOME5 <- recode(data_v1$INCOME5, "'More than ??? 5600' = '>2000'")


t <- merge(data_final, data_v1 , by = 'IDC')
cov <- read.spss("V:/medewerkers/038870 Durkut, M/Covariates_MRI_analyses.sav", to.data.frame = TRUE)
cov$ETHNINFv2
cov <- select(cov, IDC, ETHNINFv2)
data_v11 <- merge(cov, data_v1, by = 'IDC')


#non response
#chisquared test for categorical variables 
chisq.test(cbind(table(data_v1$EDUCM5), table(data_final$EDUCM5))) #X-squared = 25.07, df = 2, p-value = 3.599e-06
chisq.test(cbind(table(data_v1$INCOME5), table(t$INCOME5))) #X-squared = 27.869, df = 2, p-value = 8.878e-07
chisq.test(cbind(table(data_v1$SMOKE_ALL), table(data_final$SMOKE_ALL))) #X-squared = 8.7493, df = 2, p-value = 0.01259
chisq.test(cbind(table(data_v1$mdrink_updated), table(data_final$mdrink_updated))) # X-squared = 92.97, df = 3, p-value < 2.2e-16
chisq.test(cbind(table(cov$ETHNINFv2_3groups), table(data_final$ETHNINFv2))) #X-squared = 76.143, df = 2, p-value < 2.2e-16
chisq.test(cbind(table(data_v1$GENDER), table(data_final$GENDER))) #X-squared = 12.934, df = 1, p-value = 0.0003227

str(data_v1$mdrink_updated)
str(data_final$mdrink_updated)


setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
write_sav(data_final, "data_final_13cs.sav")
data_final$ETHNINFv2_3groups



#MICE
#impute covariates using MICE 
library("mice")
#imputation setup
dim(data_final)
missvalues <- cbind("# NA" = sort(colSums(is.na(data_final))),
                    "% NA" = round(sort(colMeans(is.na(data_final))) * 100, 2))
missvalues
# Rule of thumb for number of datasets to impute
mean(missvalues[, 2])

# Running setup imputation run
imp0 <- mice(data_final, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))

imp0$loggedEvents
methPAS <- imp0$method
methPAS
sort(methPAS)
# Change imputation method for non-normally distributed variables
methPAS["sum_att_14"] <- "pmm"
# Check predictor matrix
predPAS <- imp0$predictorMatrix
predPAS
# Changing predictor matrix
predPAS[, c("IDC", "dif_age13", "ASD", "lgi", "sens", "Cerebellum_Cortex_vol_f13", "Cerebellum_White_Matter_vol_f13", "Amygdala_vol_f13", "CSF_vol_f13", "CortexVol_f13", "SubCortGrayVol_f13", "CerebralWhiteMatterVol_f13","MeanThickness_f13", "WhiteSurfArea_f13", "tbv13")] <- 0

# Check visit sequence
visSeqPAS <- imp0$visitSequence
visSeqPAS
# Perform the imputation
impPAS <- mice(data_final, method = methPAS, predictorMatrix = predPAS, visitSequence = visSeqPAS,
               
               maxit = 30, m = 30, printFlag = FALSE, seed = 2020)
# Check whether there are logged events
impPAS$loggedEvents

# Summary of the imputed values
sapply(Filter(function(x) nrow(x) > 0, impPAS$imp),
       function(x) summary(unlist(x))
)

# Traceplot for mean and SD per incomplete variable (Figure 5)
plot(impPAS, layout = c(4, 7))
densityplot(impPAS)

setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(impPAS, file = "imp_13cs_total")

#Run file QDECR_13area.R and QDECR_13thickness.R with this dataset in RS
#And run file QDECR_13area_interaction.R and QDECR_13thickness_interaction.R with this dataset in RS

#seperate MI dataset for LGI
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total")
summary(imp$data$lgi)
imp.lgi <- complete(imp, include = T, action = "long")
imptest <- subset(imp.lgi, lgi > 0) #exclude subjects without LGI data
imp_lgi <- as.mids(imptest)
summary(imp_lgi$data$lgi) # 1
setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(imp_lgi, file = "imp_13cs_lgi")

#Run file QDECR_13cs.R and QDECR_cs13interaction.R with this dataset in RS

#MI dataset for sens analyses

#ASD or above SRS cutoff excluded
#cortical thickness and surface area dataset sens analysis
library("mice")
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total")

imp.sens <- complete(imp, include = T, action = "long")
imp.sens <- subset(imp.sens, sens > 0)
imp.sens <- as.mids(imp.sens)
summary(imp.sens$data$sens)

setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(imp.sens, file = "imp_13cs_total_sens")


#lgi dataset sens analysis
library("mice")
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_lgi")
com <- complete(imp)

imp.sens_lgi <- complete(imp, include = T, action = "long")
imp.sens_lgi <- subset(imp.sens_lgi, sens > 0)
com <- complete(imp.sens_lgi)
imp.sens_lgi <- as.mids(imp.sens_lgi)

setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(imp.sens_lgi, file = "imp_13cs_lgi_sens")

#Run file QDECR_cs13sens.R with these datasets in RS


#Dutch ethnicity parent sens analysis - No longer needed

#surf area and cortical thickness
#library("mice")
#imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total")
#summary(imp$data$ETHNMv2)
#imp.sens <- complete(imp, include = T, action = "long")
#imp.sens <- subset(imp.sens, ETHNMv2 == "Dutch")
#imp.sens <- as.mids(imp.sens)

# LGI
#library("mice")
#imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_lgi")
#summary(imp$data$ETHNMv2)
#imp.sens <- complete(imp, include = T, action = "long")
#imp.sens <- subset(imp.sens, ETHNMv2 == "Dutch")
#imp.sens <- as.mids(imp.sens)

###########################################################
##########  Global measures cross sectional  ##############
###########################################################

#load packages
library(dplyr)
library(readr)
library(foreign)
library(mice)

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total")

imp$data$MeanThickness_f13

#mean gyrification 13
fit1_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_gyr13), conf.int = TRUE) #sign


fit2_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_gyr13), conf.int = TRUE) #sign

fit3_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) #not sign

#mean surfarea 13
fit1_WhiteSurfArea_f13 <- with(imp, lm(scale(WhiteSurfArea_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_WhiteSurfArea_f13), conf.int = TRUE) #sign


fit2_gyr13 <- with(imp, lm(scale(WhiteSurfArea_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_gyr13), conf.int = TRUE) #sign

fit3_gyr13 <- with(imp, lm(scale(WhiteSurfArea_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) #not sign

#mean cortical thickness 13
fit1_MeanThickness_f13 <- with(imp, lm(scale(MeanThickness_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_MeanThickness_f13), conf.int = TRUE) #not sign

fit2_MeanThickness_f13 <- with(imp, lm(scale(MeanThickness_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_MeanThickness_f13), conf.int = TRUE) #not sign

fit3_MeanThickness_f13 <- with(imp, lm(scale(MeanThickness_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_MeanThickness_f13), conf.int = TRUE) 

#total brain volume (interaction is not sign)
fit1_tbv13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_tbv13), conf.int = TRUE)

fit2_tbv13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_tbv13), conf.int = TRUE)

fit3_gyr13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) 

#Total gray matter volume(interaction is not sign)
fit1_CortexVol_f13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CortexVol_f13), conf.int = TRUE)

fit2_CortexVol_f13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CortexVol_f13), conf.int = TRUE)

fit3_gyr13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) 

#subcortical gray matter volume (interaction is not sign)
fit1_SubCortGrayVol <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_SubCortGrayVol), conf.int = TRUE)

fit2_SubCortGrayVol <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_SubCortGrayVol), conf.int = TRUE)

fit3_gyr13 <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) 

#cerebral white matter volume(interaction is not sign)
fit1_CerebralWhiteMatterVol <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CerebralWhiteMatterVol), conf.int = TRUE)

fit2_CerebralWhiteMatterVol <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CerebralWhiteMatterVol), conf.int = TRUE)

fit3_gyr13 <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) 

#cerebrospinal fluid CSF(interaction is not sign)
fit1_CSF <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CSF), conf.int = TRUE)

fit2_CSF <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CSF), conf.int = TRUE)

fit3_gyr13 <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_gyr13), conf.int = TRUE) 

#cerebellum cortex volume (interaction is not sign)
fit1_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Cerebellum_Cortex_vol_f13), conf.int = TRUE) #sign

fit2_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_Cortex_vol_f13), conf.int = TRUE) #sign

fit3_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_Cerebellum_Cortex_vol_f13), conf.int = TRUE) 

#cerebellum white matter volume (interaction is not sign)
fit1_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

fit2_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

fit3_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

#amygdala volume(interaction is not sign)
fit1_Amygdala_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Amygdala_vol_f13), conf.int = TRUE)

fit2_Amygdala_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Amygdala_vol_f13), conf.int = TRUE)

fit3_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL + sum_att_14 + WISC13_FSIQ))
summary(pool(fit3_Cerebellum_White_Matter_vol_f13), conf.int = TRUE) 

#########################
######### sens ##########
#########################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total_sens")

fit2_tbv13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_tbv13), conf.int = TRUE)

fit2_CortexVol_f13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CortexVol_f13), conf.int = TRUE)

fit2_SubCortGrayVol <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_SubCortGrayVol), conf.int = TRUE)

fit2_CerebralWhiteMatterVol <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + HD13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CerebralWhiteMatterVol), conf.int = TRUE)

fit2_CSF <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CSF), conf.int = TRUE)

fit2_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_Cortex_vol_f13), conf.int = TRUE) 

fit2_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + HD13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

fit2_Amygdala_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Amygdala_vol_f13), conf.int = TRUE)

fit2_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_gyr13), conf.int = TRUE)

fit2_WhiteSurfArea_f13 <- with(imp, lm(scale(WhiteSurfArea_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_WhiteSurfArea_f13), conf.int = TRUE)

fit2_MeanThickness_f13 <- with(imp, lm(scale(MeanThickness_f13) ~ scale(srs_total_m_13) + GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_MeanThickness_f13), conf.int = TRUE)

#interactoin models
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13cs_total")

#mean gyrification 13
fit1_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_gyr13), conf.int = TRUE) 

fit2_gyr13 <- with(imp, lm(scale(meangyr13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_gyr13), conf.int = TRUE) 

#total brain volume (interaction is not sign)
fit1_tbv13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_tbv13), conf.int = TRUE)

fit2_tbv13 <- with(imp, lm(scale(tbv13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_tbv13), conf.int = TRUE)

#cortical gray matter volume(interaction is not sign)
fit1_CortexVol_f13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CortexVol_f13), conf.int = TRUE)

fit2_CortexVol_f13 <- with(imp, lm(scale(CortexVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CortexVol_f13), conf.int = TRUE)

#subcortical gray matter volume (interaction is not sign)
fit1_SubCortGrayVol <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_SubCortGrayVol), conf.int = TRUE)

fit2_SubCortGrayVol <- with(imp, lm(scale(SubCortGrayVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_SubCortGrayVol), conf.int = TRUE)

#cerebral white matter volume(interaction is not sign)
fit1_CerebralWhiteMatterVol <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CerebralWhiteMatterVol), conf.int = TRUE)

fit2_CerebralWhiteMatterVol <- with(imp, lm(scale(CerebralWhiteMatterVol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CerebralWhiteMatterVol), conf.int = TRUE)

#cerebrospinal fluid CSF(interaction is not sign)
fit1_CSF <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_CSF), conf.int = TRUE)

fit2_CSF <- with(imp, lm(scale(CSF_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_CSF), conf.int = TRUE)

#cerebellum cortex volume (interaction is not sign)
fit1_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Cerebellum_Cortex_vol_f13), conf.int = TRUE) #sign

fit2_Cerebellum_Cortex_vol_f13 <- with(imp, lm(scale(Cerebellum_Cortex_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_Cortex_vol_f13), conf.int = TRUE) #sign


#cerebellum white matter volume (interaction is not sign)
fit1_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

fit2_Cerebellum_White_Matter_vol_f13 <- with(imp, lm(scale(Cerebellum_White_Matter_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Cerebellum_White_Matter_vol_f13), conf.int = TRUE)

#amygdala volume(interaction is not sign)
fit1_Amygdala_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13))
summary(pool(fit1_Amygdala_vol_f13), conf.int = TRUE)

fit2_Amygdala_vol_f13 <- with(imp, lm(scale(Amygdala_vol_f13) ~ scale(srs_total_m_13) * GENDER + AGECHILD_GR1093 + dif_age13 + EDUCM5 + ETHNINFv2_3groups + mdrink_updated + SMOKE_ALL))
summary(pool(fit2_Amygdala_vol_f13), conf.int = TRUE)




