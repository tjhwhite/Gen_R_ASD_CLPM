############################################
############# CLPM May 2021 ################
############################################

#Packages
library(foreign)
library(dplyr)
library("mice")
library("lavaan")
library("semTools")
library(car)
library(haven)

rm(list = ls())

data_final2 <- read.spss("V:/medewerkers/038870 Durkut, M/df6.sav", to.data.frame = TRUE)
summary(data_final2)

###############################
############# F13 ############
###############################

core <- readRDS("V:/medewerkers/038870 Durkut, M/genr_mri_core_data_20april2020_prelim.rds") #9901
summary(core)
core$t1_braces_has_nii_F13
corex <- subset(core, t1_has_nii_F13=="1") #3625
corez <- subset(corex, has_braces_mri_F13=="No") #2582
coreb <- subset(corex, t1_braces_has_nii_F13=="1") #960

coret <- subset(core, freesurfer_qc_F13=="usable") #2317 so IF is 13
corey <- subset(coret, has_braces_mri_F13=="No")

core13 <- subset(core, freesurfer_qc_F13=="usable" &  exclude_incidental_F13=="Include") #2304
summary(core13)


#SRS13 data
data_SRS13 <- read.spss("V:/medewerkers/038870 Durkut, M/GR1093-E2-SRS_24062020.sav", to.data.frame = TRUE)

SRS13_total <- merge(core13, data_SRS13, by="IDC")

#removing subjects with missing data on SRS 
data_subset1 <- SRS13_total[ , c("srs_total_m_13")]
datasrs13cc <- SRS13_total[complete.cases(data_subset1), ]
summary(datasrs13cc)

data_final2_2 <- merge(datasrs13cc, data_final2, by="IDC")
data_final2 <- select(data_final2_2, IDC, agechildGR1076, dif_age6, GENDER, ETHNMv2, EDUCM5, INCOME5, SMOKE_ALL, mdrink_updated, sum_att_5, srs_6times18, AGECHILD_GR1093, srs_total_m_13, HD, WISC13_FSIQ, lgi)
summary(data_final2)

#child ethnicity variable
cov <- read.spss("V:/medewerkers/038870 Durkut, M/Covariates_MRI_analyses.sav", to.data.frame = TRUE)
ethn <- select(cov, IDC, ETHNINFv2_3groups)
data <- merge(ethn, data_final2, by="IDC")

#handedness 13 
handedness <- read.spss("V:/medewerkers/038870 Durkut, M/MRI5HANDEDNESS_21082013.sav", to.data.frame = TRUE)
handedness$HD13 <-  handedness$HD
handedness <- select(handedness, IDC, HD13)
totalhd <- merge(data, handedness, by="IDC", all.x = TRUE)

#child attention problem score 13 
v1 <- read.spss("V:/medewerkers/038870 Durkut, M/GR1093-E1_CBCL_18062020.sav", to.data.frame=TRUE)
att13 <- select(v1, IDC, sum_att_14)
summary(att13)
dataa <- merge(totalhd, att13, by="IDC", all.x = TRUE)
summary(dataa)

data_final <- select(dataa, IDC, agechildGR1076, dif_age6, GENDER, ETHNMv2, ETHNINFv2_3groups, EDUCM5, INCOME5, SMOKE_ALL, mdrink_updated, sum_att_5, sum_att_14, srs_6times18, AGECHILD_GR1093, srs_total_m_13, HD, HD13, WISC13_FSIQ, lgi)
summary(data_final)

#change IDC variable to match the RS 13 IDC variable (to see which subjects do not have LGI data)
data_final$IDC2 <- data_final$IDC
as.factor(data_final$IDC2)
data_final$IDC2 <- sub("^", "sub-", data_final$IDC2)
data_final$IDC2 <- interaction(data_final$IDC2,"_ses-F13", sep = "")
data_final$IDC <- data_final$IDC2
data_final <- select(data_final, IDC, agechildGR1076, dif_age6, GENDER, ETHNMv2, ETHNINFv2_3groups, EDUCM5, INCOME5, SMOKE_ALL, mdrink_updated, sum_att_5, sum_att_14, srs_6times18, AGECHILD_GR1093, srs_total_m_13, HD, HD13, WISC13_FSIQ, lgi)

#save dataset and copy to RS folder 
setwd("V:/medewerkers/038870 Durkut, M")
write_sav(data_final, "df13long.sav")


# Use R file "remove subjects without lgi.R". This creates dataframe lgisurf.sav. Copy to files and load in R
lgi <- read.spss("V:/medewerkers/038870 Durkut, M/lgisurf.sav", to.data.frame=TRUE)

#indicate subjects without LGI data 
data_final$lgi[c(7, 88, 249)] <- 0

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
methPAS["sum_att_5"] <- "pmm"

# Check predictor matrix
predPAS <- imp0$predictorMatrix
predPAS
# Changing predictor matrix
predPAS[, c("IDC", "dif_age6", "lgi", "HD13", "sum_att_14")] <- 0

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
saveRDS(impPAS, file = "imp_13datlong")

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_13datlong")
imp.long.13 <- complete(imp, include = T, action = "long")
imptest <- subset(imp.long.13, lgi > 0)
imp_13datlong_lgi <- as.mids(imptest)

setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(imp_13datlong_lgi, file = "imp_13datlong_lgi")


#save imp_13datlong and imp_13datlong_lgi on RS
#########################################################################
################# Run QDECR_6long13.R on Research Suite #################
#########################################################################
#explanation: 
# 1. run a random QDECR model (QDECR_6long13.R) on the right sample (longitudinal) for F6 and F13 for LGI, cortical thickness and surface area
# 2. extract clusters based on the independent sample from F9 (no F6)
# 3. files for extracting clusters are 'lgi_extract_clusters_@9', 'thickness_extract_clusters_@9' and 'area_extract_clusters_@9' 


#prepare dataset for CLPM
#########################################################################
################## Surface area and cortical thickness ##################
#########################################################################

#load packages
library(dplyr)
library(readr)
library(foreign)
library(mice)
library(lavaan)

# @6 data 
# line 138 for df6.sav
df6 <- read.spss("V:/medewerkers/038870 Durkut, M/df6.sav", to.data.frame = TRUE)
#df6$srs_6times18 <- (df6$srs_6times18)/18
#hist(df6$srs_6times18, col = "lightgreen", xlim = range(0:3), xlab = "SRS", main = "Histogram SRS W1")
#df6$sqrtsrs_6times18 <- sqrt(df6$srs_6times18)
#hist(df6$sqrtsrs_6times18, col = "lightgreen", xlim = range(0:2), xlab = "SRS square root transformed", main = "Histogram SRS W1")


# add extracted clusters RS ~ from independent sample F9 data 
library(readr)
clusters6area <- read_csv("V:/medewerkers/038870 Durkut, M/area_extracted_clusters_6y.csv")
clusters6thickness <- read_csv("V:/medewerkers/038870 Durkut, M/thickness_extracted_clusters_6y.csv")


df6_2 <- cbind(df6, clusters6area, clusters6thickness)

df6_3 <- select(df6_2, -IDC2)

# @13 data
# line 229 for df13long.sav

library(dplyr)
library(foreign)
df13 <- read.spss("V:/medewerkers/038870 Durkut, M/df13long.sav", to.data.frame = TRUE)

# add extracted clusters RS ~ from independent sample F9 data 
library(readr)
clusters13area <- read_csv("V:/medewerkers/038870 Durkut, M/area_extracted_clusters_13y.csv")
clusters13thickness <- read_csv("V:/medewerkers/038870 Durkut, M/thickness_extracted_clusters_13y.csv")

df13_2 <- cbind(df13, clusters13area, clusters13thickness)
summary(df13_2)
df13_3 <- select(df13_2, c(IDC, AGECHILD_GR1093, srs_total_m_13, thirteen_mean_vertex_area_lh, thirteen_mean_vertex_area_rh, thirteen_mean_thickness_rh, HD13, sum_att_14, ETHNINFv2_3groups))


#merge @6 and @13 data 
df <- merge(df13_3, df6_3, by="IDC")
summary(df)

#########################################
################## LGI ##################
#########################################

# @6 data 
# line 138 for df6.sav
library(foreign)
df6lgi <- read.spss("V:/medewerkers/038870 Durkut, M/df6.sav", to.data.frame = TRUE)
t <- select(df6lgi, c(IDC, lgi))
#subsample with LGI data available
df6lgi <- subset(t, lgi == 1)

library(readr)
# add extracted clusters RS ~ from independent sample F9 data 
#see file lgi_extract_clusters_@9
clusters6lgi <- read_csv("V:/medewerkers/038870 Durkut, M/lgi_extracted_clusters_6y.csv")

df6lgi_2 <- cbind(df6lgi, clusters6lgi)

# @13 data
# line 229 for df13long.sav
library(dplyr)
library(foreign)
df13lgi <- read.spss("V:/medewerkers/038870 Durkut, M/df13long.sav", to.data.frame = TRUE)

x <- select(df13lgi, c(IDC, lgi))
#subsample with LGI data available
df13lgi <- subset(x, lgi == 1)
# add extracted clusters RS ~ from independent sample F9 data 
#see file lgi_extract_clusters_@9
library(readr)
clusters13lgi <- read_csv("V:/medewerkers/038870 Durkut, M/lgi_extracted_clusters_13y.csv")

df13lgi_2 <- cbind(df13lgi, clusters13lgi)

df13lgi_3 <- select(df13lgi_2, c(IDC, thirteen_mean_vertex_lgi_lh, thirteen_mean_vertex_lgi_rh))


#merge @6 and @13 data 
dflgi <- merge(df13lgi_3, df6lgi_2, by="IDC")

#merge thickness and area data with lgi data 
df <- merge(df, dflgi, by="IDC", all.x = TRUE)

#make sure that NA's in the extra LGI variable are labelled as zero to get a 0-1variable for subsetting after MICE
df$lgi.y[is.na(df$lgi.y)] <- 0 
df$lgi <- df$lgi.y
#delete unneeded variables
df <- subset(df, select = -c(lgi.x, lgi.y))

#add ID mother and exclude one subblingfor each sibbling or twin pair 
data_motherid <- read.spss("V:/medewerkers/038870 Durkut, M/IDC-IDM-MOTHER.sav", to.data.frame = TRUE)
data_motherid <- select(data_motherid, IDC, MOTHER)
data_complete2 <- merge(df, data_motherid, by="IDC")
data_complete2<- data_complete2[sample(nrow(data_complete2)),]
df1 <- data_complete2[!duplicated(data_complete2$MOTHER),]

df <- subset(df1, select = -c(MOTHER))

setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
#write_sav(df, "finallong.sav")

#load packages
library(dplyr)
library(readr)
library(foreign)
library(mice)

df <- read.spss("V:/medewerkers/038870 Durkut, M/finallong.sav", to.data.frame = TRUE)
df$agedif <- df$AGECHILD_GR1093 - df$agechildGR1076
#Spearman correlation
#non-parametric: rank based
cor(df$srs_6times18, df$srs_total_m_13, method = "spearman")

library(ppcor)
pcor.test(df$srs_6times18, df$srs_total_m_13, df[,c("agedif")], method = "spearman")

cor.test(df$thirteen_mean_vertex_lgi_lh, df$six_mean_vertex_lgi_lh, method = "spearman")

#transform srs 
df$srs_6times18 <- sqrt(df$srs_6times18)
df$srs_total_m_13 <- sqrt(df$srs_total_m_13)

#partial correlation 
pcor.test(df$srs_6times18, df$srs_total_m_13, df[,c("agedif")], method = "pearson")

#age MRI13
library(foreign)

age_13mri <- read.spss("V:/medewerkers/038870 Durkut, M/F13_MRI_IDC_AGE_11082020.sav", to.data.frame = TRUE)
dat1 <- merge(df, age_13mri, by="IDC", all.x = TRUE)
dat1$dif_age13 <- dat1$AGECHILD_GR1093 - dat1$AGEMRI_F13
final <- dat1

#order factors 
final$EDUCM5 <- ordered(final$EDUCM5, levels = c("primary", "secondary", "higher"))
final$INCOME5 <- ordered(final$INCOME5, levels = c("less than 1200", "1200-2000", ">2000"))
final$mdrink_updated <- ordered(final$mdrink_updated, levels = c("mother never drank in pregnancy", "mother drank until pregnancy was known", "mother continued drinking occasionally", "mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)"))
final$SMOKE_ALL <- ordered(final$SMOKE_ALL, levels = c("never smoked during pregnancy", "smoked until pregnancy was known", "continued smoking in pregnancy"))

final$HD <-as.ordered(final$HD)
final$HD13 <-as.ordered(final$HD13)
final$GENDER <-as.ordered(final$GENDER)

final$ETHNINFv2 <-as.ordered(final$ETHNINFv2_3groups)

final <- subset(final, select = -c(ETHNINFv2_3groups))


#global parameters
# @6
library("foreign")
aseg <- read.spss("V:/medewerkers/038870 Durkut, M/freesurfer_Sept04_2013_n1070_aseg_stats.sav", to.data.frame = TRUE)

aseg6 <- dplyr::select(aseg, c(IDC, Left_Cerebellum_Cortex_vol, Right_Cerebellum_Cortex_vol, Left_Cerebellum_White_Matter_vol, Right_Cerebellum_White_Matter_vol, Left_Amygdala_vol, Right_Amygdala_vol, CSF_vol))
aseg6$cerebellum_cortex_volume <- aseg6$Left_Cerebellum_Cortex_vol + aseg6$Right_Cerebellum_Cortex_vol
aseg6$cerebellum_white_matter_volume <- aseg6$Left_Cerebellum_White_Matter_vol + aseg6$Right_Cerebellum_White_Matter_vol
aseg6$amygdala_vol <- (aseg6$Left_Amygdala_vol + aseg6$Right_Amygdala_vol)/2
aseg6 <- dplyr::select(aseg6, IDC, cerebellum_cortex_volume, cerebellum_white_matter_volume, amygdala_vol, CSF_vol)

tbv <- read.spss("V:/medewerkers/038870 Durkut, M/freesurfer_Sept04_2013_n1070_tbv_stats.sav", to.data.frame = TRUE)

tbv2 <- read.spss("V:/medewerkers/038870 Durkut, M/F06_freesurfer_Sept04_2013_n1070_tbvknicr.sav", to.data.frame = TRUE)
tbv6 <- dplyr::select(tbv, IDC, SubCortGrayVol, CortexVol, CorticalWhiteMatterVol)

tbv62 <- dplyr::select(tbv2, IDC, tbv_knicr)
global62 <- merge(tbv6, tbv62, by = 'IDC')
global6 <- merge(aseg6, global62, by = 'IDC')
summary(global6)
#mean gyrification
meangyr <- read.spss("V:/medewerkers/038870 Durkut, M/mean_lGI.sav", to.data.frame = TRUE)
meangyr$meangyr6 <- (meangyr$mean_lGI_lh_f5 + meangyr$mean_lGI_rh_f5)/ 2
meangyr6 <- dplyr::select(meangyr, IDC, meangyr6)
global6 <- merge(meangyr6, global6, by = 'IDC')

final6g <- merge(final, global6, by = 'IDC')


#mean surf area
area <- read_csv("V:/medewerkers/038870 Durkut, M/f5_freesurfer_06_09_2015_tbv_stats.csv")
area$lh_MeanThickness_f5
area$WhiteSurfArea_f5 <- NA
area$WhiteSurfArea_f5 <- (area$lh_WhiteSurfArea_f5 + area$rh_WhiteSurfArea_f5)/ 2
area$MeanThickness <- (area$lh_MeanThickness_f5 + area$rh_MeanThickness_f5)/ 2
area$IDC <- area$idc
areat <- dplyr::select(area, IDC, WhiteSurfArea_f5, MeanThickness)

final6g <- merge(final6g, areat, by = 'IDC')

# @13 global measures
library(readr)
tbv <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_tbv_stats_pull23Nov2020_noDups.rds")
aseg13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aseg_stats_pull23Nov2020_noDups.rds")
aparc13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aparc_stats_pull23Nov2020_noDups.rds")



#dataset to remove duplicates
#dup <- read.spss("V:/medewerkers/038870 Durkut, M/dup.sav", to.data.frame = TRUE)
# Returns string without leading or trailing white space
#dup$IDC <- as.character(dup$IDC)
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#dup$IDC <- trim(dup$IDC)

#tbv$IDC <- NA
#tbv$IDC <- tbv$idc

#x <- merge(tbv, dup, by ='IDC')
#remove duplicates
#tbv <- subset(x, duplicate_exclude < 1)

#create new IDC 
#tbv$IDC <- NA
#for(i in 1:length(tbv$idc)){tbv$IDC[i] <- strsplit(tbv$idc[i], '_')[[1]][1]}

#select variables
tbv$MeanThickness_f13 <- (tbv$lh_MeanThickness_f13 + tbv$rh_MeanThickness_f13)/ 2

tbv$WhiteSurfArea_f13 <- (tbv$lh_WhiteSurfArea_f13 + tbv$rh_WhiteSurfArea_f13)/ 2

tbv13 <- dplyr::select(tbv, IDC, CortexVol_f13, SubCortGrayVol_f13, CerebralWhiteMatterVol_f13, SupraTentorialVol_f13, MeanThickness_f13, WhiteSurfArea_f13)


aseg13$Cerebellum_Cortex_vol_f13 <- aseg13$Left_Cerebellum_Cortex_vol_f13 + aseg13$Right_Cerebellum_Cortex_vol_f13
aseg13$Cerebellum_White_Matter_vol_f13 <- aseg13$Left_Cerebellum_White_Matter_vol_f13 + aseg13$Right_Cerebellum_White_Matter_vol_f13
aseg13$Amygdala_vol_f13 <- (aseg13$Left_Amygdala_vol_f13 + aseg13$Right_Amygdala_vol_f13)/2


aseg13 <- dplyr::select(aseg13, IDC, Cerebellum_Cortex_vol_f13, Cerebellum_White_Matter_vol_f13, Amygdala_vol_f13, CSF_vol_f13)

global13 <- merge(aseg13, tbv13, by = 'IDC')
summary(global13)
global13$tbv13 <- global13$SupraTentorialVol_f13 + global13$Cerebellum_Cortex_vol_f13 + global13$Cerebellum_White_Matter_vol_f13
global13 <- subset(global13, select = -c(SupraTentorialVol_f13))
final <- merge(final6g, global13, by = 'IDC')
#mean gyrification 
meangyr$meangyr13 <- (meangyr$mean_lGI_lh_f13 + meangyr$mean_lGI_rh_f13)/ 2
meangyr13 <- dplyr::select(meangyr, IDC, meangyr13)
final <- merge(meangyr13, final, by = 'IDC')

summary(final)

# ASD
v1 <- read.spss("V:/medewerkers/038870 Durkut, M/data_v1.sav", to.data.frame=TRUE)
asd <- dplyr::select(v1, IDC, ASD)
final <- merge(final, asd, by="IDC", all.x = TRUE)
final$ASD[is.na(final$ASD)] <- 0 #only for subettting (sens analysis)
summary(final$ASD)

#add age MRi@6 variable
Age_mri6 <- read.spss("V:/medewerkers/038870 Durkut, M/F6_MRI_age_and_sex_idc.sav", to.data.frame = TRUE)
Age_mri6 <- dplyr::select(Age_mri6, IDC, age_yrs)
final <- merge(final, Age_mri6, by="IDC") #no missings on age


#include extra variable for sensitivity analyses (ASD and srs cutoffs)
final$sens <- NA
for(i in 1:length(final$IDC)){if(final$GENDER[i] == "girl" && final$srs_6times18[i] > sqrt(18) || final$srs_total_m_13[i] > sqrt(18) ) {final$sens[i] = 0} else {final$sens[i] = 1}}
for(i in 1:length(final$IDC)){if(final$GENDER[i] == "boy" && final$srs_6times18[i] > sqrt(19.409) || final$srs_total_m_13[i] > sqrt(19.409)) {final$sens[i] = 0} else {final$sens[i] = final$sens[i]}}
for(i in 1:length(final$IDC)){if(final$ASD[i] > 0) {final$sens[i] = 0} else {final$sens[i] = final$sens[i]}}

str(final)
hist(final$tbv13)
class(final$tbv_knicr)
cor(final$tbv13, final$tbv_knicr, method = "spearman")

#mean and SD for brain measires 
sd(final$tbv_knicr)
sd(final$tbv13)
mean(final$CortexVol)
sd(final$CortexVol_f13)
sd(final$SubCortGrayVol)
sd(final$SubCortGrayVol_f13)
mean(final$CerebralWhiteMatterVol_f13)
sd(final$CorticalWhiteMatterVol)
sd(final$CSF_vol)
mean(final$CSF_vol_f13)
sd(final$cerebellum_cortex_volume)
sd(final$Cerebellum_Cortex_vol_f13)
sd(final$cerebellum_white_matter_volume)
sd(final$Cerebellum_White_Matter_vol_f13)
sd(final$amygdala_vol)
sd(final$Amygdala_vol_f13)
sd(final$meangyr13, na.rm = T)
sd(final$meangyr6, na.rm = T)
sd(final$WhiteSurfArea_f5)
sd(final$WhiteSurfArea_f13)
sd(final$MeanThickness)
sd(final$MeanThickness_f13)

#non response dataset 
data_v1 <- read.spss("V:/medewerkers/038870 Durkut, M/data_v1.sav", to.data.frame=TRUE)
summary(data_v1$ETHNMv2)
cov <- read.spss("V:/medewerkers/038870 Durkut, M/Covariates_MRI_analyses.sav", to.data.frame = TRUE)
cov <- dplyr::select(cov, IDC, ETHNINFv2_3groups)
data_v1 <- merge(data_v1, cov, by = 'IDC')
data_v1$srs_6times18 <- sqrt(data_v1$srs_weighted*18)
summary(data_v1)

library(car)
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'no education finished'= 'primary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'secondary, phase 1'= 'secondary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'secondary, phase 2'= 'secondary'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'higher, phase 1'= 'higher'")
data_v1$EDUCM5 <- recode(data_v1$EDUCM5, "'higher, phase 2'= 'higher'")


data_v1$EDUCM5 <- ordered(data_v1$EDUCM5, levels = c("primary", "secondary", "higher"))

summary(data_v1$EDUCM5)
summary(final$EDUCM5)

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

data_v1$INCOME5 <- ordered(data_v1$INCOME5, levels = c("less than 1200", "1200-2000", ">2000"))

#non response
#chisquared test for categorical variables 
chisq.test(cbind(table(data_v1$EDUCM5), table(final$EDUCM5))) #X-squared = 9.5749, df = 2, p-value = 0.008334
chisq.test(cbind(table(data_v1$INCOME5), table(final$INCOME5))) #X-squared = 9.7985, df = 2, p-value = 0.007452
chisq.test(cbind(table(data_v1$SMOKE_ALL), table(final$SMOKE_ALL))) #X-squared = 2.8213, df = 2, p-value = 0.244
chisq.test(cbind(table(data_v1$mdrink_updated), table(final$mdrink_updated))) #X-squared = 54.083, df = 3, p-value = 1.077e-11
chisq.test(cbind(table(data_v1$ETHNINFv2_3groups), table(final$ETHNINFv2))) #X-squared = 63.336, df = 2, p-value = 1.765e-14
chisq.test(cbind(table(data_v1$GENDER), table(final$GENDER))) #X-squared = 1.4144, df = 1, p-value = 0.2343


final$srs_total_m_13 <- (final$srs_total_m_13)^2
final$srs_total_m_13 <- (final$srs_total_m_13)/18
summary(final$srs_total_m_13)

final$srs_6times18 <- (final$srs_6times18)^2
final$srs_6times18 <- (final$srs_6times18)/18
summary(final$srs_6times18)

#Baseline
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

categorical(final$GENDER)
categorical(final$ETHNINFv2)
categorical(final$HD)
continuous(final$AGECHILD_GR1093)
continuous(final$agechildGR1076)
continuous(final$AGEMRI_F13)
continuous(final$age_yrs)
continuous(final$WISC13_FSIQ)
summary(final$sum_att_14)

categorical(final$INCOME5)
categorical(final$SMOKE_ALL)
categorical(final$mdrink_updated)
summary(final$srs_6times18)
continuous(final$agechildGR1076)
summary(final$AGECHILD_GR1093)
summary(final$age_yrs)
summary(final$AGEMRI_F13)
categorical(final$sens) #17 children excluded for sens analysis

# Running setup imputation run
library("mice")
imp0 <- mice(final, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))
imp0$loggedEvents                                               
#0 logged events
methPAS <- imp0$method
methPAS
sort(methPAS)
# Change imputation method for non-normally distributed variables
methPAS["sum_att_5"] <- "pmm"
#variables that should not be imputed 
methPAS[c("six_mean_vertex_lgi_lh", "six_mean_vertex_lgi_rh", "thirteen_mean_vertex_lgi_lh", "thirteen_mean_vertex_lgi_rh")] <- ""
# Check predictor matrix
predPAS <- imp0$predictorMatrix
predPAS
# Changing predictor matrix
predPAS[, c("IDC","dif_age6", "dif_age13", "agedif", "thirteen_mean_vertex_lgi_lh", "thirteen_mean_vertex_lgi_rh", "Cerebellum_Cortex_vol_f13", "Cerebellum_White_Matter_vol_f13", "Amygdala_vol_f13", "CSF_vol_f13", "CortexVol_f13", "SubCortGrayVol_f13", "CerebralWhiteMatterVol_f13", "tbv13", "CortexVol", "SubCortGrayVol", "tbv_knicr", "CorticalWhiteMatterVol", "cerebellum_cortex_volume", "cerebellum_white_matter_volume", "amygdala_vol", "CSF_vol", "AGEMRI_F13", "age_yrs", "lgi", "sens", "ETHNMv2", "meangyr13", "meangyr6", "MeanThickness_f13", "MeanThickness", "WhiteSurfArea_f13", "WhiteSurfArea_f5")] <- 0


# Check visit sequence
visSeqPAS <- imp0$visitSequence
visSeqPAS
# Perform the imputation
impPAS <- mice(final, method = methPAS, predictorMatrix = predPAS, visitSequence = visSeqPAS,
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
saveRDS(impPAS, file = "imp_CLPMlongatlas")

#final dataset not imputed
#scale variables 
final$tbv_knicr <- scale(final$tbv_knicr)
final$thirteen_mean_vertex_lgi_lh <- scale(final$thirteen_mean_vertex_lgi_lh)
final$six_mean_vertex_lgi_lh <- scale(final$six_mean_vertex_lgi_lh)
final$thirteen_mean_vertex_lgi_rh <- scale(final$thirteen_mean_vertex_lgi_rh)
final$six_mean_vertex_lgi_rh <- scale(final$six_mean_vertex_lgi_rh)
final$tbv13 <- scale(final$tbv13)
final$srs_6times18 <- scale(final$srs_6times18)
final$srs_total_m_13 <- scale(final$srs_total_m_13)

final$CortexVol <- scale(final$CortexVol)
final$CortexVol_f13 <- scale(final$CortexVol_f13)
final$SubCortGrayVol <- scale(final$SubCortGrayVol)
final$SubCortGrayVol_f13 <- scale(final$SubCortGrayVol_f13)
final$CorticalWhiteMatterVol <- scale(final$CorticalWhiteMatterVol)
final$CerebralWhiteMatterVol_f13 <- scale(final$CerebralWhiteMatterVol_f13)
final$CSF_vol <- scale(final$CSF_vol)
final$CSF_vol_f13 <- scale(final$CSF_vol_f13)
final$cerebellum_cortex_volume <- scale(final$cerebellum_cortex_volume)
final$Cerebellum_Cortex_vol_f13 <- scale(final$Cerebellum_Cortex_vol_f13)
final$cerebellum_white_matter_volume <- scale(final$cerebellum_white_matter_volume)
final$Cerebellum_White_Matter_vol_f13 <- scale(final$Cerebellum_White_Matter_vol_f13)
final$amygdala_vol <- scale(final$amygdala_vol)
final$Amygdala_vol_f13 <- scale(final$Amygdala_vol_f13)
final$meangyr13 <- scale(final$meangyr13)
final$meangyr6 <- scale(final$meangyr6)

summary(final$eth_wes)
summary(final$ETHNINFv2)
final$eth_wes <- ifelse(final$ETHNINFv2 == "Dutch", 0,
                           ifelse(final$ETHNINFv2 == "Western", 1, 0))

final$eth_nonwes <- ifelse(final$ETHNINFv2 == "Dutch", 0,
                              ifelse(final$ETHNINFv2 == "Non-Western", 1, 0))
summary(final)



final$GENDER <- ifelse(final$GENDER == "boy", 0, 1)
final$HD13 <- ifelse(final$HD13 == "Left", 0, 1)
final$HD <- ifelse(final$HD == "Left", 0, 1)

str(final)

library(car)
final$EDUCM5 <- recode(final$EDUCM5, "'primary' = '1'")
final$EDUCM5 <- recode(final$EDUCM5, "'secondary' = '2'")
final$EDUCM5 <- recode(final$EDUCM5, "'higher' = '3'")
summary(final$EDUCM5)
final$EDUCM5 <- as.numeric(final$EDUCM5)

final$SMOKE_ALL <- recode(final$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
final$SMOKE_ALL <- recode(final$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
final$SMOKE_ALL <- recode(final$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
final$SMOKE_ALL <- as.numeric(final$SMOKE_ALL)

final$mdrink_updated <- recode(final$mdrink_updated, "'mother never drank in pregnancy' = '1'")
final$mdrink_updated <- recode(final$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
final$mdrink_updated <- recode(final$mdrink_updated, "'mother continued drinking occasionally' = '3'")
final$mdrink_updated <- recode(final$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
final$mdrink_updated <- as.numeric(final$mdrink_updated)
str(final)

setwd("V:/medewerkers/038870 Durkut, M")
library(haven)
write_sav(final, "final.sav")

final$ETHNINFv2 <- recode(final$ETHNINFv2, "'Dutch' = '0'")
final$ETHNINFv2 <- recode(final$ETHNINFv2, "'Non-Western' = '1'")
final$ETHNINFv2 <- recode(final$ETHNINFv2, "'Western' = '1'")
str(final$ETHNINFv2)
final$ETHNINFv2 <- as.numeric(final$ETHNINFv2)

##################################################
##################  LGI   ########################
##################################################
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#imp <- impPAS
imp.lgi <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
imptest <- subset(imp.lgi, lgi > 0)
imp_lgi <- as.mids(imptest)
summary(imp_lgi$data$lgi) # subsetted to lgi is 1
imp <- imp_lgi 
#setwd("V:/medewerkers/038870 Durkut, M")
#saveRDS(imp_6datlong_lgi, file = "imp_6datlong_lgi")

imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))


#scale variables 
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)



#CLPM
#lgi mean
require(lavaan)
library("semTools")

CLPMgyr1 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + GENDER
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13   
meangyr6  ~ agechildGR1076 + dif_age6 + GENDER
meangyr13 ~ AGECHILD_GR1093 + dif_age13   
'

CLPM.fit1mi <- lavaan.mi(model = CLPMgyr1, data = list)
meanlgi1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
meanlgi1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the covariance between the observed variables at the first wave. 

srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 



#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~  AGECHILD_GR1093 + dif_age13 
meangyr6 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
meangyr13 ~  AGECHILD_GR1093 + dif_age13  
'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sumlgi2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumlgi2$pvalue

#CLPM.fit1_2mi <- sem(model = CLPM1_2, data = final, missing = 'fiml')
#summary(CLPM.fit1_2mi, fit.measures = T)


#CLPM
#lgi LH
require(lavaan) 

CLPMlgi <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 


#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_lh ~ GENDER + HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13 
'

CLPM.fit1m <- lavaan.mi(model = CLPMlgi, data = list)

sum1 <- summary(CLPM.fit1m, se = TRUE, ci = TRUE, standardized = T, header = TRUE, fit.measures = TRUE, rsquare= TRUE)
sum1$pvalue


#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_lh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <-summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#lgi RH
#model1
CLPM2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_rh ~ GENDER + HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13
'

CLPM.fit2mi <- lavaan.mi(model = CLPM2, data = list)
sum1 <- summary(CLPM.fit2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression

srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


##################################################
###### LGI ROI's corrected for mean LGI ##########
##################################################

require(lavaan)
library("semTools")

#CLPM
#lgi LH

CLPMlgi <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 


#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + meangyr6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + meangyr13
six_mean_vertex_lgi_lh ~ GENDER + HD + agechildGR1076 + dif_age6 + meangyr6
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13 + meangyr13
'

CLPM.fit1m <- lavaan.mi(model = CLPMlgi, data = list)

sum1 <- summary(CLPM.fit1m, se = TRUE, ci = TRUE, standardized = T, header = TRUE, fit.measures = TRUE, rsquare= TRUE)
sum1$pvalue


#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + meangyr6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + meangyr13

six_mean_vertex_lgi_lh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD + meangyr6
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13 + meangyr13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <-summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#lgi RH
#model1
CLPM2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + meangyr6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + meangyr13
six_mean_vertex_lgi_rh ~ GENDER + HD + agechildGR1076 + dif_age6 + meangyr6
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13 + meangyr13
'

CLPM.fit2mi <- lavaan.mi(model = CLPM2, data = list)
sum1 <- summary(CLPM.fit2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression

srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + meangyr6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + meangyr13

six_mean_vertex_lgi_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + meangyr6 + HD
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13 + meangyr13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

##################################################
###### cortical thickness and surface area #######
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#scale variables 

imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)
imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$MeanThickness <- scale(imp$data$MeanThickness)
imp$data$MeanThickness_f13 <- scale(imp$data$MeanThickness_f13)
imp$data$WhiteSurfArea_f13 <- scale(imp$data$WhiteSurfArea_f13)
imp$data$WhiteSurfArea_f5 <- scale(imp$data$WhiteSurfArea_f5)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

#imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com1$eth_wes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                           ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com1$eth_nonwes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                              ifelse(com1$ETHNINFv2 == "Non-Western", 1, 0))
com2 <- complete(imp, 2)
com2$eth_wes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                       ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com2$eth_nonwes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                          ifelse(com2$ETHNINFv2 == "Non-Western", 1, 0))
com3 <- complete(imp, 3)
com3$eth_wes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                       ifelse(com3$ETHNINFv2 == "Western", 1, 0))

com3$eth_nonwes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                          ifelse(com3$ETHNINFv2 == "Non-Western", 1, 0))
com4 <- complete(imp, 4)
com4$eth_wes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                       ifelse(com4$ETHNINFv2 == "Western", 1, 0))

com4$eth_nonwes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                          ifelse(com4$ETHNINFv2 == "Non-Western", 1, 0))
com5 <- complete(imp, 5)
com5$eth_wes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                       ifelse(com5$ETHNINFv2 == "Western", 1, 0))

com5$eth_nonwes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                          ifelse(com5$ETHNINFv2 == "Non-Western", 1, 0))
com6 <- complete(imp, 6)
com6$eth_wes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                       ifelse(com6$ETHNINFv2 == "Western", 1, 0))

com6$eth_nonwes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                          ifelse(com6$ETHNINFv2 == "Non-Western", 1, 0))
com7 <- complete(imp, 7)
com7$eth_wes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                       ifelse(com7$ETHNINFv2 == "Western", 1, 0))

com7$eth_nonwes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                          ifelse(com7$ETHNINFv2 == "Non-Western", 1, 0))
com8 <- complete(imp, 8)
com8$eth_wes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                       ifelse(com8$ETHNINFv2 == "Western", 1, 0))

com8$eth_nonwes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                          ifelse(com8$ETHNINFv2 == "Non-Western", 1, 0))
com9 <- complete(imp, 9)
com9$eth_wes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                       ifelse(com9$ETHNINFv2 == "Western", 1, 0))

com9$eth_nonwes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                          ifelse(com9$ETHNINFv2 == "Non-Western", 1, 0))
com10 <- complete(imp, 10)
com10$eth_wes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                       ifelse(com10$ETHNINFv2 == "Western", 1, 0))

com10$eth_nonwes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                          ifelse(com10$ETHNINFv2 == "Non-Western", 1, 0))
com11 <- complete(imp, 11)
com11$eth_wes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                       ifelse(com11$ETHNINFv2 == "Western", 1, 0))

com11$eth_nonwes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                          ifelse(com11$ETHNINFv2 == "Non-Western", 1, 0))
com12 <- complete(imp, 12)
com12$eth_wes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                       ifelse(com12$ETHNINFv2 == "Western", 1, 0))

com12$eth_nonwes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                          ifelse(com12$ETHNINFv2 == "Non-Western", 1, 0))
com13 <- complete(imp, 13)
com13$eth_wes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                       ifelse(com13$ETHNINFv2 == "Western", 1, 0))

com13$eth_nonwes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                          ifelse(com13$ETHNINFv2 == "Non-Western", 1, 0))
com14 <- complete(imp, 14)
com14$eth_wes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                       ifelse(com14$ETHNINFv2 == "Western", 1, 0))

com14$eth_nonwes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                          ifelse(com14$ETHNINFv2 == "Non-Western", 1, 0))
com15 <- complete(imp, 15)
com15$eth_wes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                       ifelse(com15$ETHNINFv2 == "Western", 1, 0))

com15$eth_nonwes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                          ifelse(com15$ETHNINFv2 == "Non-Western", 1, 0))
com16 <- complete(imp, 16)
com16$eth_wes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                       ifelse(com16$ETHNINFv2 == "Western", 1, 0))

com16$eth_nonwes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                          ifelse(com16$ETHNINFv2 == "Non-Western", 1, 0))
com17 <- complete(imp, 17)
com17$eth_wes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                       ifelse(com17$ETHNINFv2 == "Western", 1, 0))

com17$eth_nonwes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                          ifelse(com17$ETHNINFv2 == "Non-Western", 1, 0))
com18 <- complete(imp, 18)
com18$eth_wes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                        ifelse(com18$ETHNINFv2 == "Western", 1, 0))

com18$eth_nonwes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                           ifelse(com18$ETHNINFv2 == "Non-Western", 1, 0))
com19 <- complete(imp, 19)
com19$eth_wes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                        ifelse(com19$ETHNINFv2 == "Western", 1, 0))

com19$eth_nonwes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                           ifelse(com19$ETHNINFv2 == "Non-Western", 1, 0))
com20 <- complete(imp, 20)
com20$eth_wes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                        ifelse(com20$ETHNINFv2 == "Western", 1, 0))

com20$eth_nonwes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                           ifelse(com20$ETHNINFv2 == "Non-Western", 1, 0))
com21 <- complete(imp, 21)
com21$eth_wes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                        ifelse(com21$ETHNINFv2 == "Western", 1, 0))

com21$eth_nonwes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                           ifelse(com21$ETHNINFv2 == "Non-Western", 1, 0))
com22 <- complete(imp, 22)
com22$eth_wes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                        ifelse(com22$ETHNINFv2 == "Western", 1, 0))

com22$eth_nonwes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                           ifelse(com22$ETHNINFv2 == "Non-Western", 1, 0))
com23 <- complete(imp, 23)
com23$eth_wes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                        ifelse(com23$ETHNINFv2 == "Western", 1, 0))

com23$eth_nonwes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                           ifelse(com23$ETHNINFv2 == "Non-Western", 1, 0))
com24 <- complete(imp, 24)
com24$eth_wes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                        ifelse(com24$ETHNINFv2 == "Western", 1, 0))

com24$eth_nonwes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                           ifelse(com24$ETHNINFv2 == "Non-Western", 1, 0))
com25 <- complete(imp, 25)
com25$eth_wes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                        ifelse(com25$ETHNINFv2 == "Western", 1, 0))

com25$eth_nonwes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                           ifelse(com25$ETHNINFv2 == "Non-Western", 1, 0))
com26 <- complete(imp, 26)
com26$eth_wes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                        ifelse(com26$ETHNINFv2 == "Western", 1, 0))

com26$eth_nonwes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                           ifelse(com26$ETHNINFv2 == "Non-Western", 1, 0))
com27 <- complete(imp, 27)
com27$eth_wes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                        ifelse(com27$ETHNINFv2 == "Western", 1, 0))

com27$eth_nonwes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                           ifelse(com27$ETHNINFv2 == "Non-Western", 1, 0))
com28 <- complete(imp, 28)
com28$eth_wes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                        ifelse(com28$ETHNINFv2 == "Western", 1, 0))

com28$eth_nonwes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                           ifelse(com28$ETHNINFv2 == "Non-Western", 1, 0))
com29 <- complete(imp, 29)
com29$eth_wes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                        ifelse(com29$ETHNINFv2 == "Western", 1, 0))

com29$eth_nonwes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                           ifelse(com29$ETHNINFv2 == "Non-Western", 1, 0))
com30 <- complete(imp, 30)
com30$eth_wes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                        ifelse(com30$ETHNINFv2 == "Western", 1, 0))

com30$eth_nonwes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                           ifelse(com30$ETHNINFv2 == "Non-Western", 1, 0))

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)

#cortical thickness mean
com30$MeanThickness
com30$MeanThickness_f13

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13 

#Regression

srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

MeanThickness  ~ GENDER + agechildGR1076 + dif_age6
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13  

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

MeanThickness  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


#cortical thickness rh (1 cluster, no lh)

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh 

#Regression

srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_thickness_rh  ~ GENDER + agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh  

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_thickness_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

# mean surface area model 1
require(lavaan) 
imp$data$WhiteSurfArea_f5

CLPM1 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

WhiteSurfArea_f5  ~ GENDER + agechildGR1076 + dif_age6 + HD 
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

# mean surface area model 2
CLPM1_2 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

WhiteSurfArea_f5  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


# surface area lh model 1
require(lavaan) 

CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_lh  ~ GENDER + agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

# surface area lh model 2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_lh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#surface area rh model1
CLPM2_1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_rh  ~ GENDER + agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13 

'
CLPM.fit2_1mi <- lavaan.mi(model = CLPM2_1, data = list)
sum1 <- summary(CLPM.fit2_1mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

##################################################################################
#####SA and Cortical THickness + mean SA and cortical thickness correction########
##################################################################################

#cortical thickness rh (1 cluster, no lh)

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh 

#Regression

srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + MeanThickness
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + MeanThickness_f13

six_mean_vertex_thickness_rh  ~ GENDER + agechildGR1076 + dif_age6 + HD + MeanThickness
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13 + MeanThickness_f13

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh  

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + MeanThickness
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + MeanThickness_f13

six_mean_vertex_thickness_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD + MeanThickness
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13 + MeanThickness_f13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


# surface area lh model 1
require(lavaan) 

CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + WhiteSurfArea_f5
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

six_mean_vertex_area_lh  ~ GENDER + agechildGR1076 + dif_age6 + HD + WhiteSurfArea_f5
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

# surface area lh model 2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + WhiteSurfArea_f5
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

six_mean_vertex_area_lh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD + WhiteSurfArea_f5
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#surface area rh model1
CLPM2_1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18  ~ GENDER + agechildGR1076 + dif_age6 + WhiteSurfArea_f5
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

six_mean_vertex_area_rh  ~ GENDER + agechildGR1076 + dif_age6 + HD + WhiteSurfArea_f5
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

'
CLPM.fit2_1mi <- lavaan.mi(model = CLPM2_1, data = list)
sum1 <- summary(CLPM.fit2_1mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + WhiteSurfArea_f5
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

six_mean_vertex_area_rh  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD + WhiteSurfArea_f5
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13 + WhiteSurfArea_f13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

##################################################
#####global measures + TBV correction(NO HD)######
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#scale variables
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)

imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)

imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$meangyr13 <- scale(imp$data$meangyr13)


imp$data$six_mean_vertex_thickness_rh <- scale(imp$data$six_mean_vertex_thickness_rh)
imp$data$thirteen_mean_vertex_thickness_rh <- scale(imp$data$thirteen_mean_vertex_thickness_rh)
imp$data$six_mean_vertex_area_lh <- scale(imp$data$six_mean_vertex_area_lh)
imp$data$thirteen_mean_vertex_area_lh <- scale(imp$data$thirteen_mean_vertex_area_lh)
imp$data$six_mean_vertex_area_rh <- scale(imp$data$six_mean_vertex_area_rh)
imp$data$thirteen_mean_vertex_area_rh <- scale(imp$data$thirteen_mean_vertex_area_rh)


#make a list of the MI datasets for lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)


#total brain volume (tbv)
imp$data$tbv_knicr #@6
imp$data$tbv13
#model 1
TBV1 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr ~ GENDER + agechildGR1076 + dif_age6 
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'

TBV.fit1 <- lavaan.mi(model = TBV1, data = list)
sumtbv1 <-summary(TBV.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumtbv1$pvalue

#model 2
TBV2 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'
TBV.fit2 <- lavaan.mi(model = TBV2, data = list)
sum <- summary(TBV.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum$pvalue


#CORTICAL gray matter volume
imp$data$CortexVol
imp$data$CortexVol_f13
#model 1
CortGrayVol1 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CortexVol ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

library("semTools")
TotalGrayVol.fit1 <- lavaan.mi(model = CortGrayVol1, data = list)
sumgray1 <- summary(TotalGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray1$pvalue
#model 2
CorticalGrayVol2 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CortexVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
TotalGrayVol.fit2 <- lavaan.mi(model = CorticalGrayVol2, data = list)
sumgray2<- summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray2$pvalue

#subcortical grey matter volume
imp$data$SubCortGrayVol_f13
imp$data$SubCortGrayVol #@6

#model 1
SubCortGrayVol1 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

SubCortGrayVol ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

SubCortGrayVol.fit1 <- lavaan.mi(model = SubCortGrayVol1, data = list)
sumsubcortgrayvol1 <- summary(SubCortGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)

sumsubcortgrayvol1$pvalue

#model 2
SubCortGrayVol2 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

SubCortGrayVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
SubCortGrayVol.fit2 <- lavaan.mi(model = SubCortGrayVol2, data = list)
sumsubcortgrayvol2 <- summary(SubCortGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumsubcortgrayvol2$pvalue


#cerebral white matter volume

imp$data$CerebralWhiteMatterVol_f13
imp$data$CorticalWhiteMatterVol #@6

CerebralWhiteMatterVol1 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CorticalWhiteMatterVol ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

CerebralWhiteMatterVol.fit1 <- lavaan.mi(model = CerebralWhiteMatterVol1, data = list)
sum1<- summary(CerebralWhiteMatterVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

CerebralWhiteMatterVol2 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13

#Regression

srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CorticalWhiteMatterVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
CerebralWhiteMatterVol.fit2 <- lavaan.mi(model = CerebralWhiteMatterVol2, data = list)
sum2 <- summary(CerebralWhiteMatterVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#cerebrospinal fluid (CSF)

imp$data$CSF_vol #@6
imp$data$CSF_vol_f13

CSF_vol1 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CSF_vol ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

library("semTools")
CSF_vol.fit1 <- lavaan.mi(model = CSF_vol1, data = list)
sumcsf1 <- summary(CSF_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf1$pvalue

CSF_vol2 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

CSF_vol ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
CSF_vol.fit2 <- lavaan.mi(model = CSF_vol2, data = list)
sumcsf2 <- summary(CSF_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf2$pvalue

#cerebellum cortex volume

imp$data$cerebellum_cortex_volume #@6
imp$data$Cerebellum_Cortex_vol_f13

Cerebellum_Cortex_vol1 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

cerebellum_cortex_volume  ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

Cerebellum_Cortex_vol.fit1 <- lavaan.mi(model = Cerebellum_Cortex_vol1, data = list)
sumcerebellum1 <- summary(Cerebellum_Cortex_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum1$pvalue

Cerebellum_Cortex_vol2 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

cerebellum_cortex_volume ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
Cerebellum_Cortex_vol.fit2 <- lavaan.mi(model = Cerebellum_Cortex_vol2, data = list)
sumcerebellum2 <- summary(Cerebellum_Cortex_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum2$pvalue


#cerebellum white matter volume

imp$data$cerebellum_white_matter_volume #@6
imp$data$Cerebellum_White_Matter_vol_f13

Cerebellum_White_Matter_vol1 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

cerebellum_white_matter_volume  ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

Cerebellum_White_Matter_vol.fit1 <- lavaan.mi(model = Cerebellum_White_Matter_vol1, data = list)
sum1 <- summary(Cerebellum_White_Matter_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

Cerebellum_White_Matter_vol2 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

cerebellum_white_matter_volume ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'
Cerebellum_White_Matter_vol.fit2 <- lavaan.mi(model = Cerebellum_White_Matter_vol2, data = list)
sum2 <- summary(Cerebellum_White_Matter_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#amygdala volume (mean left and right)
imp$data$amygdala_vol #@6
imp$data$Amygdala_vol_f13

amygdala_vol1 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

amygdala_vol  ~ GENDER + agechildGR1076 + dif_age6 + tbv_knicr
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

'

amygdala_vol.fit1 <- lavaan.mi(model = amygdala_vol1, data = list)
sum1 <- summary(amygdala_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

amygdala_vol2 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 + tbv13

amygdala_vol ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + tbv_knicr
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13 + tbv13
'
amygdala_vol.fit2 <- lavaan.mi(model = amygdala_vol2, data = list)
sum2 <- summary(amygdala_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


##################################################
#############global measures  (NO HD)#############
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#scale variables
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)

imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)

imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$meangyr13 <- scale(imp$data$meangyr13)


imp$data$six_mean_vertex_thickness_rh <- scale(imp$data$six_mean_vertex_thickness_rh)
imp$data$thirteen_mean_vertex_thickness_rh <- scale(imp$data$thirteen_mean_vertex_thickness_rh)
imp$data$six_mean_vertex_area_lh <- scale(imp$data$six_mean_vertex_area_lh)
imp$data$thirteen_mean_vertex_area_lh <- scale(imp$data$thirteen_mean_vertex_area_lh)
imp$data$six_mean_vertex_area_rh <- scale(imp$data$six_mean_vertex_area_rh)
imp$data$thirteen_mean_vertex_area_rh <- scale(imp$data$thirteen_mean_vertex_area_rh)


#make a list of the MI datasets for lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)


#total brain volume (tbv)
imp$data$tbv_knicr #@6
imp$data$tbv13
#model 1
TBV1 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr ~ GENDER + agechildGR1076 + dif_age6 
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'

TBV.fit1 <- lavaan.mi(model = TBV1, data = list)
sumtbv1 <-summary(TBV.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumtbv1$pvalue

#model 2
TBV2 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'
TBV.fit2 <- lavaan.mi(model = TBV2, data = list)
sum <- summary(TBV.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum$pvalue


#CORTICAL gray matter volume
imp$data$CortexVol
imp$data$CortexVol_f13
#model 1
CortGrayVol1 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol ~ GENDER + agechildGR1076 + dif_age6 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

library("semTools")
TotalGrayVol.fit1 <- lavaan.mi(model = CortGrayVol1, data = list)
sumgray1 <- summary(TotalGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray1$pvalue
#model 2
CorticalGrayVol2 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
TotalGrayVol.fit2 <- lavaan.mi(model = CorticalGrayVol2, data = list)
sumgray2<- summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray2$pvalue

#subcortical grey matter volume
imp$data$SubCortGrayVol_f13
imp$data$SubCortGrayVol #@6

#model 1
SubCortGrayVol1 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol ~ GENDER + agechildGR1076 + dif_age6 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

SubCortGrayVol.fit1 <- lavaan.mi(model = SubCortGrayVol1, data = list)
sumsubcortgrayvol1 <- summary(SubCortGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)

sumsubcortgrayvol1$pvalue

#model 2
SubCortGrayVol2 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
SubCortGrayVol.fit2 <- lavaan.mi(model = SubCortGrayVol2, data = list)
sumsubcortgrayvol2 <- summary(SubCortGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumsubcortgrayvol2$pvalue


#cerebral white matter volume

imp$data$CerebralWhiteMatterVol_f13
imp$data$CorticalWhiteMatterVol #@6

CerebralWhiteMatterVol1 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol ~ GENDER + agechildGR1076 + dif_age6 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13

'

CerebralWhiteMatterVol.fit1 <- lavaan.mi(model = CerebralWhiteMatterVol1, data = list)
sum1<- summary(CerebralWhiteMatterVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

CerebralWhiteMatterVol2 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13

#Regression

srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol  ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CerebralWhiteMatterVol.fit2 <- lavaan.mi(model = CerebralWhiteMatterVol2, data = list)
sum2 <- summary(CerebralWhiteMatterVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#cerebrospinal fluid (CSF)

imp$data$CSF_vol #@6
imp$data$CSF_vol_f13

CSF_vol1 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~ GENDER + agechildGR1076 + dif_age6 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'

library("semTools")
CSF_vol.fit1 <- lavaan.mi(model = CSF_vol1, data = list)
sumcsf1 <- summary(CSF_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf1$pvalue

CSF_vol2 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CSF_vol.fit2 <- lavaan.mi(model = CSF_vol2, data = list)
sumcsf2 <- summary(CSF_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf2$pvalue

#cerebellum cortex volume

imp$data$cerebellum_cortex_volume #@6
imp$data$Cerebellum_Cortex_vol_f13

Cerebellum_Cortex_vol1 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume  ~ GENDER + agechildGR1076 + dif_age6 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_Cortex_vol.fit1 <- lavaan.mi(model = Cerebellum_Cortex_vol1, data = list)
sumcerebellum1 <- summary(Cerebellum_Cortex_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum1$pvalue

Cerebellum_Cortex_vol2 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
Cerebellum_Cortex_vol.fit2 <- lavaan.mi(model = Cerebellum_Cortex_vol2, data = list)
sumcerebellum2 <- summary(Cerebellum_Cortex_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum2$pvalue


#cerebellum white matter volume

imp$data$cerebellum_white_matter_volume #@6
imp$data$Cerebellum_White_Matter_vol_f13

Cerebellum_White_Matter_vol1 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume  ~ GENDER + agechildGR1076 + dif_age6 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_White_Matter_vol.fit1 <- lavaan.mi(model = Cerebellum_White_Matter_vol1, data = list)
sum1 <- summary(Cerebellum_White_Matter_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

Cerebellum_White_Matter_vol2 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'
Cerebellum_White_Matter_vol.fit2 <- lavaan.mi(model = Cerebellum_White_Matter_vol2, data = list)
sum2 <- summary(Cerebellum_White_Matter_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#amygdala volume (mean left and right)
imp$data$amygdala_vol #@6
imp$data$Amygdala_vol_f13

amygdala_vol1 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13 

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol  ~ GENDER + agechildGR1076 + dif_age6 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

amygdala_vol.fit1 <- lavaan.mi(model = amygdala_vol1, data = list)
sum1 <- summary(amygdala_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

amygdala_vol2 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13

#Regression
srs_6times18 ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol ~ GENDER + agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13
'
amygdala_vol.fit2 <- lavaan.mi(model = amygdala_vol2, data = list)
sum2 <- summary(amygdala_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


##################################################
##############  sens analyses   ##################
##################################################
#no longer needed 

#ASD or above SRS cutoff excluded
library("mice")
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

imp.sens <- complete(imp, include = T, action = "long")
imp.sens <- subset(imp.sens, sens > 0)
imp.sens <- as.mids(imp.sens)
imp <- imp.sens 

#scale variables
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)

imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)

imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$meangyr13 <- scale(imp$data$meangyr13)


imp$data$six_mean_vertex_thickness_rh <- scale(imp$data$six_mean_vertex_thickness_rh)
imp$data$thirteen_mean_vertex_thickness_rh <- scale(imp$data$thirteen_mean_vertex_thickness_rh)
imp$data$six_mean_vertex_area_lh <- scale(imp$data$six_mean_vertex_area_lh)
imp$data$thirteen_mean_vertex_area_lh <- scale(imp$data$thirteen_mean_vertex_area_lh)
imp$data$six_mean_vertex_area_rh <- scale(imp$data$six_mean_vertex_area_rh)
imp$data$thirteen_mean_vertex_area_rh <- scale(imp$data$thirteen_mean_vertex_area_rh)

class(imp$data$HD)
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)

require(lavaan)

#Cortical gray matter volume
TotalGrayVol2 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*ETHNINFv2 + s8*mdrink_updated + s9*SMOKE_ALL

CortexVol + CortexVol_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*ETHNINFv2 + s8*mdrink_updated + s9*SMOKE_ALL

'
TotalGrayVol.fit2 <- lavaan.mi(model = TotalGrayVol2, data = list, ordered = c("GENDER", "EDUCM5", "ETHNINFv2", "SMOKE_ALL", "mdrink_updated"))
summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)

############################################################
#################### End of script #########################
############################################################

#Dutch ethinicity parent NO LONGER NEEDED

library("mice")
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")
summary(imp$data$ETHNMv2)
imp.sens <- complete(imp, include = T, action = "long")
imp.sens <- subset(imp.sens, ETHNMv2 == "Dutch")
imp.sens <- as.mids(imp.sens)
imp <- imp.sens 

imp$data$eTIV <- scale(imp$data$eTIV)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$eTIV_f13 <- scale(imp$data$eTIV_f13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)
imp$data$TotalGrayVol <- scale(imp$data$TotalGrayVol)
imp$data$TotalGrayVol_f13 <- scale(imp$data$TotalGrayVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)


imp$data$six_mean_vertex_thickness_rh <- scale(imp$data$six_mean_vertex_thickness_rh)
imp$data$thirteen_mean_vertex_thickness_rh <- scale(imp$data$thirteen_mean_vertex_thickness_rh)
imp$data$six_mean_vertex_area_lh <- scale(imp$data$six_mean_vertex_area_lh)
imp$data$thirteen_mean_vertex_area_lh <- scale(imp$data$thirteen_mean_vertex_area_lh)
imp$data$six_mean_vertex_area_rh <- scale(imp$data$six_mean_vertex_area_rh)
imp$data$thirteen_mean_vertex_area_rh <- scale(imp$data$thirteen_mean_vertex_area_rh)

class(imp$data$HD)
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)

require(lavaan)
#total brain volume

TIV1 <- '
srs_total_m_13 + eTIV_f13 ~ srs_6times18 + eTIV 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ eTIV  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ eTIV_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
eTIV ~~ eTIV 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
eTIV_f13 ~~ eTIV_f13 

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

eTIV + eTIV_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

'


library("semTools")
TIV.fit1 <- lavaan.mi(model = TIV1, data = list, ordered = c("HD", "GENDER"))
summary(TIV.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)

TIV2 <- '
srs_total_m_13 + eTIV_f13 ~ srs_6times18 + eTIV 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ eTIV  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ eTIV_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
eTIV ~~ eTIV 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
eTIV_f13 ~~ eTIV_f13

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

eTIV + eTIV_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

'
TIV.fit2 <- lavaan.mi(model = TIV2, data = list, ordered = c("GENDER", "HD", "EDUCM5", "INCOME5", "SMOKE_ALL", "mdrink_updated"))
summary(TIV.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)



#total gray matter volume
imp$data$TotalGrayVol_f13
imp$data$TotalGrayVol #@6

TotalGrayVol1 <- '
srs_total_m_13 + TotalGrayVol_f13 ~ srs_6times18 + TotalGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ TotalGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ TotalGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
TotalGrayVol ~~ TotalGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
TotalGrayVol_f13 ~~ TotalGrayVol_f13 

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

TotalGrayVol + TotalGrayVol_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

'

library("semTools")
TotalGrayVol.fit1 <- lavaan.mi(model = TotalGrayVol1, data = list, ordered = c("HD", "GENDER"))
summary(TotalGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)

TotalGrayVol2 <- '
srs_total_m_13 + TotalGrayVol_f13 ~ srs_6times18 + TotalGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ TotalGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ TotalGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
TotalGrayVol ~~ TotalGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
TotalGrayVol_f13 ~~ TotalGrayVol_f13

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

TotalGrayVol + TotalGrayVol_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

'
TotalGrayVol.fit2 <- lavaan.mi(model = TotalGrayVol2, data = list, ordered = c("GENDER", "HD", "EDUCM5", "INCOME5", "SMOKE_ALL", "mdrink_updated"))
summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)

#subcortical grey matter volume
imp$data$SubCortGrayVol_f13
imp$data$SubCortGrayVol #@6

SubCortGrayVol1 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13 

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

SubCortGrayVol + SubCortGrayVol_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13

'

SubCortGrayVol.fit1 <- lavaan.mi(model = SubCortGrayVol1, data = list, ordered = c("HD", "GENDER"))
summary(SubCortGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)

SubCortGrayVol2 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13

#Regression
srs_6times18 + srs_total_m_13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

SubCortGrayVol + SubCortGrayVol_f13 ~ s1*GENDER + s2*agechildGR1076 + s3*dif_age6 + s4*AGECHILD_GR1093 + s5*dif_age13 + s6*EDUCM5 + s7*INCOME5 + s8*mdrink_updated + s9*SMOKE_ALL

'
SubCortGrayVol.fit2 <- lavaan.mi(model = SubCortGrayVol2, data = list, ordered = c("GENDER", "HD", "EDUCM5", "INCOME5", "SMOKE_ALL", "mdrink_updated"))
summary(SubCortGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE)


##################################################
##################  Longitudinal subset girls  ###
##################################################

#Packages
library(foreign)
library(dplyr)
library("mice")
library("lavaan")
library("semTools")
library(car)
library(haven)
imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#imp <- impPAS
imp.lgi <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
summary(imp.lgi$GENDER)
imptest <- subset(imp.lgi, GENDER == "girl")
imptest <- subset(imptest, lgi > 0)
summary(imptest$lgi)
imp_girlslgi <- as.mids(imptest)
summary(imp_girlslgi$data$lgi) # subsetted to lgi is 1
imp <- imp_girlslgi 

imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))


#scale variables 
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)



#CLPM
#lgi mean
require(lavaan)
library("semTools")


CLPMgyr1 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13   
meangyr6  ~ agechildGR1076 + dif_age6
meangyr13 ~ AGECHILD_GR1093 + dif_age13   
'

CLPM.fit1mi <- lavaan.mi(model = CLPMgyr1, data = list)
meanlgi1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
meanlgi1
meanlgi1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the covariance between the observed variables at the first wave. 

srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 



#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~  AGECHILD_GR1093 + dif_age13 
meangyr6 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
meangyr13 ~  AGECHILD_GR1093 + dif_age13  
'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sumlgi2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumlgi2
sumlgi2$pvalue

#CLPM.fit1_2mi <- sem(model = CLPM1_2, data = final, missing = 'fiml')
#summary(CLPM.fit1_2mi, fit.measures = T)


#CLPM
#lgi LH
require(lavaan) 

CLPMlgi <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_lh ~ HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13 
'

CLPM.fit1m <- lavaan.mi(model = CLPMlgi, data = list)

sum1 <- summary(CLPM.fit1m, se = TRUE, ci = TRUE, standardized = T, header = TRUE, fit.measures = TRUE, rsquare= TRUE)
sum1
sum1$pvalue


#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_lh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <-summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

#lgi RH
#model1
CLPM2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_rh ~ HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13
'

CLPM.fit2mi <- lavaan.mi(model = CLPM2, data = list)
sum1 <- summary(CLPM.fit2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

#model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression

srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_rh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

##################################################
###### cortical thickness and surface area #######
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#imp <- impPAS
imp.t <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
summary(imp.t$GENDER)
imptest <- subset(imp.t, GENDER == "girl")
summary(imptest$GENDER)
imp_girls <- as.mids(imptest)
imp <- imp_girlslgi

#scale variables 

imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)
imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$MeanThickness <- scale(imp$data$MeanThickness)
imp$data$MeanThickness_f13 <- scale(imp$data$MeanThickness_f13)
imp$data$WhiteSurfArea_f13 <- scale(imp$data$WhiteSurfArea_f13)
imp$data$WhiteSurfArea_f5 <- scale(imp$data$WhiteSurfArea_f5)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

#imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com1$eth_wes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                       ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com1$eth_nonwes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                          ifelse(com1$ETHNINFv2 == "Non-Western", 1, 0))
com2 <- complete(imp, 2)
com2$eth_wes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                       ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com2$eth_nonwes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                          ifelse(com2$ETHNINFv2 == "Non-Western", 1, 0))
com3 <- complete(imp, 3)
com3$eth_wes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                       ifelse(com3$ETHNINFv2 == "Western", 1, 0))

com3$eth_nonwes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                          ifelse(com3$ETHNINFv2 == "Non-Western", 1, 0))
com4 <- complete(imp, 4)
com4$eth_wes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                       ifelse(com4$ETHNINFv2 == "Western", 1, 0))

com4$eth_nonwes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                          ifelse(com4$ETHNINFv2 == "Non-Western", 1, 0))
com5 <- complete(imp, 5)
com5$eth_wes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                       ifelse(com5$ETHNINFv2 == "Western", 1, 0))

com5$eth_nonwes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                          ifelse(com5$ETHNINFv2 == "Non-Western", 1, 0))
com6 <- complete(imp, 6)
com6$eth_wes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                       ifelse(com6$ETHNINFv2 == "Western", 1, 0))

com6$eth_nonwes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                          ifelse(com6$ETHNINFv2 == "Non-Western", 1, 0))
com7 <- complete(imp, 7)
com7$eth_wes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                       ifelse(com7$ETHNINFv2 == "Western", 1, 0))

com7$eth_nonwes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                          ifelse(com7$ETHNINFv2 == "Non-Western", 1, 0))
com8 <- complete(imp, 8)
com8$eth_wes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                       ifelse(com8$ETHNINFv2 == "Western", 1, 0))

com8$eth_nonwes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                          ifelse(com8$ETHNINFv2 == "Non-Western", 1, 0))
com9 <- complete(imp, 9)
com9$eth_wes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                       ifelse(com9$ETHNINFv2 == "Western", 1, 0))

com9$eth_nonwes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                          ifelse(com9$ETHNINFv2 == "Non-Western", 1, 0))
com10 <- complete(imp, 10)
com10$eth_wes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                        ifelse(com10$ETHNINFv2 == "Western", 1, 0))

com10$eth_nonwes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                           ifelse(com10$ETHNINFv2 == "Non-Western", 1, 0))
com11 <- complete(imp, 11)
com11$eth_wes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                        ifelse(com11$ETHNINFv2 == "Western", 1, 0))

com11$eth_nonwes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                           ifelse(com11$ETHNINFv2 == "Non-Western", 1, 0))
com12 <- complete(imp, 12)
com12$eth_wes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                        ifelse(com12$ETHNINFv2 == "Western", 1, 0))

com12$eth_nonwes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                           ifelse(com12$ETHNINFv2 == "Non-Western", 1, 0))
com13 <- complete(imp, 13)
com13$eth_wes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                        ifelse(com13$ETHNINFv2 == "Western", 1, 0))

com13$eth_nonwes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                           ifelse(com13$ETHNINFv2 == "Non-Western", 1, 0))
com14 <- complete(imp, 14)
com14$eth_wes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                        ifelse(com14$ETHNINFv2 == "Western", 1, 0))

com14$eth_nonwes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                           ifelse(com14$ETHNINFv2 == "Non-Western", 1, 0))
com15 <- complete(imp, 15)
com15$eth_wes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                        ifelse(com15$ETHNINFv2 == "Western", 1, 0))

com15$eth_nonwes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                           ifelse(com15$ETHNINFv2 == "Non-Western", 1, 0))
com16 <- complete(imp, 16)
com16$eth_wes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                        ifelse(com16$ETHNINFv2 == "Western", 1, 0))

com16$eth_nonwes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                           ifelse(com16$ETHNINFv2 == "Non-Western", 1, 0))
com17 <- complete(imp, 17)
com17$eth_wes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                        ifelse(com17$ETHNINFv2 == "Western", 1, 0))

com17$eth_nonwes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                           ifelse(com17$ETHNINFv2 == "Non-Western", 1, 0))
com18 <- complete(imp, 18)
com18$eth_wes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                        ifelse(com18$ETHNINFv2 == "Western", 1, 0))

com18$eth_nonwes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                           ifelse(com18$ETHNINFv2 == "Non-Western", 1, 0))
com19 <- complete(imp, 19)
com19$eth_wes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                        ifelse(com19$ETHNINFv2 == "Western", 1, 0))

com19$eth_nonwes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                           ifelse(com19$ETHNINFv2 == "Non-Western", 1, 0))
com20 <- complete(imp, 20)
com20$eth_wes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                        ifelse(com20$ETHNINFv2 == "Western", 1, 0))

com20$eth_nonwes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                           ifelse(com20$ETHNINFv2 == "Non-Western", 1, 0))
com21 <- complete(imp, 21)
com21$eth_wes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                        ifelse(com21$ETHNINFv2 == "Western", 1, 0))

com21$eth_nonwes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                           ifelse(com21$ETHNINFv2 == "Non-Western", 1, 0))
com22 <- complete(imp, 22)
com22$eth_wes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                        ifelse(com22$ETHNINFv2 == "Western", 1, 0))

com22$eth_nonwes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                           ifelse(com22$ETHNINFv2 == "Non-Western", 1, 0))
com23 <- complete(imp, 23)
com23$eth_wes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                        ifelse(com23$ETHNINFv2 == "Western", 1, 0))

com23$eth_nonwes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                           ifelse(com23$ETHNINFv2 == "Non-Western", 1, 0))
com24 <- complete(imp, 24)
com24$eth_wes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                        ifelse(com24$ETHNINFv2 == "Western", 1, 0))

com24$eth_nonwes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                           ifelse(com24$ETHNINFv2 == "Non-Western", 1, 0))
com25 <- complete(imp, 25)
com25$eth_wes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                        ifelse(com25$ETHNINFv2 == "Western", 1, 0))

com25$eth_nonwes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                           ifelse(com25$ETHNINFv2 == "Non-Western", 1, 0))
com26 <- complete(imp, 26)
com26$eth_wes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                        ifelse(com26$ETHNINFv2 == "Western", 1, 0))

com26$eth_nonwes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                           ifelse(com26$ETHNINFv2 == "Non-Western", 1, 0))
com27 <- complete(imp, 27)
com27$eth_wes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                        ifelse(com27$ETHNINFv2 == "Western", 1, 0))

com27$eth_nonwes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                           ifelse(com27$ETHNINFv2 == "Non-Western", 1, 0))
com28 <- complete(imp, 28)
com28$eth_wes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                        ifelse(com28$ETHNINFv2 == "Western", 1, 0))

com28$eth_nonwes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                           ifelse(com28$ETHNINFv2 == "Non-Western", 1, 0))
com29 <- complete(imp, 29)
com29$eth_wes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                        ifelse(com29$ETHNINFv2 == "Western", 1, 0))

com29$eth_nonwes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                           ifelse(com29$ETHNINFv2 == "Non-Western", 1, 0))
com30 <- complete(imp, 30)
com30$eth_wes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                        ifelse(com30$ETHNINFv2 == "Western", 1, 0))

com30$eth_nonwes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                           ifelse(com30$ETHNINFv2 == "Non-Western", 1, 0))

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)

#cortical thickness mean
com30$MeanThickness
com30$MeanThickness_f13

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13 

#Regression

srs_6times18  ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

MeanThickness  ~ agechildGR1076 + dif_age6
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13  

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

MeanThickness  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue


#cortical thickness rh (1 cluster, no lh)

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh 

#Regression

srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_thickness_rh  ~  agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh  

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_thickness_rh  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

# mean surface area model 1
require(lavaan) 
imp$data$WhiteSurfArea_f5

CLPM1 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

WhiteSurfArea_f5  ~ agechildGR1076 + dif_age6 + HD 
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

# mean surface area model 2
CLPM1_2 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

WhiteSurfArea_f5  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue


# surface area lh model 1
require(lavaan) 

CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_lh  ~ agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

# surface area lh model 2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_lh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

#surface area rh model1
CLPM2_1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_rh  ~ agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13 

'
CLPM.fit2_1mi <- lavaan.mi(model = CLPM2_1, data = list)
sum1 <- summary(CLPM.fit2_1mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

#surface area rh model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_rh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

##################################################
#############global measures  (NO HD)#############
##################################################

#total brain volume (tbv)
imp$data$tbv_knicr #@6
imp$data$tbv13
#model 1
TBV1 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13 

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr ~  agechildGR1076 + dif_age6 
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'

TBV.fit1 <- lavaan.mi(model = TBV1, data = list)
sumtbv1 <-summary(TBV.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumtbv1
sumtbv1$pvalue

#model 2
TBV2 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'
TBV.fit2 <- lavaan.mi(model = TBV2, data = list)
sum <- summary(TBV.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum
sum$pvalue


#CORTICAL gray matter volume
imp$data$CortexVol
imp$data$CortexVol_f13
#model 1
CortGrayVol1 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13 

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol ~ agechildGR1076 + dif_age6 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

library("semTools")
TotalGrayVol.fit1 <- lavaan.mi(model = CortGrayVol1, data = list)
sumgray1 <- summary(TotalGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray1
sumgray1$pvalue

#model 2
CorticalGrayVol2 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
TotalGrayVol.fit2 <- lavaan.mi(model = CorticalGrayVol2, data = list)
sumgray2<- summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray2
sumgray2$pvalue

#subcortical grey matter volume
imp$data$SubCortGrayVol_f13
imp$data$SubCortGrayVol #@6

#model 1
SubCortGrayVol1 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol ~  agechildGR1076 + dif_age6 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

SubCortGrayVol.fit1 <- lavaan.mi(model = SubCortGrayVol1, data = list)
sumsubcortgrayvol1 <- summary(SubCortGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumsubcortgrayvol1
sumsubcortgrayvol1$pvalue

#model 2
SubCortGrayVol2 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
SubCortGrayVol.fit2 <- lavaan.mi(model = SubCortGrayVol2, data = list)
sumsubcortgrayvol2 <- summary(SubCortGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumsubcortgrayvol2
sumsubcortgrayvol2$pvalue


#cerebral white matter volume

imp$data$CerebralWhiteMatterVol_f13
imp$data$CorticalWhiteMatterVol #@6

CerebralWhiteMatterVol1 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol ~  agechildGR1076 + dif_age6 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13

'

CerebralWhiteMatterVol.fit1 <- lavaan.mi(model = CerebralWhiteMatterVol1, data = list)
sum1<- summary(CerebralWhiteMatterVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

CerebralWhiteMatterVol2 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13

#Regression

srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CerebralWhiteMatterVol.fit2 <- lavaan.mi(model = CerebralWhiteMatterVol2, data = list)
sum2 <- summary(CerebralWhiteMatterVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

#cerebrospinal fluid (CSF)

imp$data$CSF_vol #@6
imp$data$CSF_vol_f13

CSF_vol1 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~  agechildGR1076 + dif_age6 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'

library("semTools")
CSF_vol.fit1 <- lavaan.mi(model = CSF_vol1, data = list)
sumcsf1 <- summary(CSF_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf1
sumcsf1$pvalue

CSF_vol2 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CSF_vol.fit2 <- lavaan.mi(model = CSF_vol2, data = list)
sumcsf2 <- summary(CSF_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf2
sumcsf2$pvalue

#cerebellum cortex volume

imp$data$cerebellum_cortex_volume #@6
imp$data$Cerebellum_Cortex_vol_f13

Cerebellum_Cortex_vol1 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume  ~  agechildGR1076 + dif_age6 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_Cortex_vol.fit1 <- lavaan.mi(model = Cerebellum_Cortex_vol1, data = list)
sumcerebellum1 <- summary(Cerebellum_Cortex_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum1
sumcerebellum1$pvalue

Cerebellum_Cortex_vol2 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
Cerebellum_Cortex_vol.fit2 <- lavaan.mi(model = Cerebellum_Cortex_vol2, data = list)
sumcerebellum2 <- summary(Cerebellum_Cortex_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum2
sumcerebellum2$pvalue


#cerebellum white matter volume

imp$data$cerebellum_white_matter_volume #@6
imp$data$Cerebellum_White_Matter_vol_f13

Cerebellum_White_Matter_vol1 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume  ~  agechildGR1076 + dif_age6 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_White_Matter_vol.fit1 <- lavaan.mi(model = Cerebellum_White_Matter_vol1, data = list)
sum1 <- summary(Cerebellum_White_Matter_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

Cerebellum_White_Matter_vol2 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'
Cerebellum_White_Matter_vol.fit2 <- lavaan.mi(model = Cerebellum_White_Matter_vol2, data = list)
sum2 <- summary(Cerebellum_White_Matter_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

#amygdala volume (mean left and right)
imp$data$amygdala_vol #@6
imp$data$Amygdala_vol_f13

amygdala_vol1 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol  ~  agechildGR1076 + dif_age6 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

amygdala_vol.fit1 <- lavaan.mi(model = amygdala_vol1, data = list)
sum1 <- summary(amygdala_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1
sum1$pvalue

amygdala_vol2 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13
'
amygdala_vol.fit2 <- lavaan.mi(model = amygdala_vol2, data = list)
sum2 <- summary(amygdala_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2
sum2$pvalue

##################################################
##################  Longitudinal subset boys  ####
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#imp <- impPAS
imp.lgi <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
summary(imp.lgi$GENDER)
imptest <- subset(imp.lgi, GENDER == "boy")
imptest <- subset(imptest, lgi > 0)
summary(imptest$lgi)
imp_boyslgi <- as.mids(imptest)
summary(imp_boyslgi$data$lgi) # subsetted to lgi is 1
imp <- imp_boyslgi 

imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))


#scale variables 
imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)

imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com2 <- complete(imp, 2)
com3 <- complete(imp, 3)
com4 <- complete(imp, 4)
com5 <- complete(imp, 5)
com6 <- complete(imp, 6)
com7 <- complete(imp, 7)
com8 <- complete(imp, 8)
com9 <- complete(imp, 9)
com10 <- complete(imp, 10)
com11 <- complete(imp, 11)
com12 <- complete(imp, 12)
com13 <- complete(imp, 13)
com14 <- complete(imp, 14)
com15 <- complete(imp, 15)
com16 <- complete(imp, 16)
com17 <- complete(imp, 17)
com18 <- complete(imp, 18)
com19 <- complete(imp, 19)
com20 <- complete(imp, 20)
com21 <- complete(imp, 21)
com22 <- complete(imp, 22)
com23 <- complete(imp, 23)
com24 <- complete(imp, 24)
com25 <- complete(imp, 25)
com26 <- complete(imp, 26)
com27 <- complete(imp, 27)
com28 <- complete(imp, 28)
com29 <- complete(imp, 29)
com30 <- complete(imp, 30)

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)



#CLPM
#lgi mean
require(lavaan)
library("semTools")


CLPMgyr1 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13   
meangyr6  ~ agechildGR1076 + dif_age6
meangyr13 ~ AGECHILD_GR1093 + dif_age13   
'

CLPM.fit1mi <- lavaan.mi(model = CLPMgyr1, data = list)
meanlgi1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
meanlgi1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + meangyr13 ~ srs_6times18 + meangyr6 

# Estimate the covariance between the observed variables at the first wave. 

srs_6times18 ~~ meangyr6  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ meangyr13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
meangyr6 ~~ meangyr6 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
meangyr13 ~~ meangyr13 



#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~  AGECHILD_GR1093 + dif_age13 
meangyr6 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
meangyr13 ~  AGECHILD_GR1093 + dif_age13  
'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sumlgi2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumlgi2$pvalue

#CLPM.fit1_2mi <- sem(model = CLPM1_2, data = final, missing = 'fiml')
#summary(CLPM.fit1_2mi, fit.measures = T)


#CLPM
#lgi LH
require(lavaan) 

CLPMlgi <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_lh ~ HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13 
'

CLPM.fit1m <- lavaan.mi(model = CLPMlgi, data = list)

sum1 <- summary(CLPM.fit1m, se = TRUE, ci = TRUE, standardized = T, header = TRUE, fit.measures = TRUE, rsquare= TRUE)
sum1$pvalue


#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_lh ~ srs_6times18 + six_mean_vertex_lgi_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_lh ~~ six_mean_vertex_lgi_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_lh ~~ thirteen_mean_vertex_lgi_lh 

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_lh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <-summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#lgi RH
#model1
CLPM2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13
six_mean_vertex_lgi_rh ~ HD + agechildGR1076 + dif_age6 
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13
'

CLPM.fit2mi <- lavaan.mi(model = CLPM2, data = list)
sum1 <- summary(CLPM.fit2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_lgi_rh ~ srs_6times18 + six_mean_vertex_lgi_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_lgi_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_lgi_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_lgi_rh ~~ six_mean_vertex_lgi_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_lgi_rh ~~ thirteen_mean_vertex_lgi_rh 

#Regression

srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_lgi_rh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_lgi_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

##################################################
###### cortical thickness and surface area #######
##################################################

imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlong")

#imp <- impPAS
imp.t <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
summary(imp.t$GENDER)
imptest <- subset(imp.t, GENDER == "boy")
summary(imptest$GENDER)
imp_boys <- as.mids(imptest)
imp <- imp_boyslgi

#scale variables 

imp$data$tbv_knicr <- scale(imp$data$tbv_knicr)
imp$data$thirteen_mean_vertex_lgi_lh <- scale(imp$data$thirteen_mean_vertex_lgi_lh)
imp$data$six_mean_vertex_lgi_lh <- scale(imp$data$six_mean_vertex_lgi_lh)
imp$data$thirteen_mean_vertex_lgi_rh <- scale(imp$data$thirteen_mean_vertex_lgi_rh)
imp$data$six_mean_vertex_lgi_rh <- scale(imp$data$six_mean_vertex_lgi_rh)
imp$data$tbv13 <- scale(imp$data$tbv13)
imp$data$srs_6times18 <- scale(imp$data$srs_6times18)
imp$data$srs_total_m_13 <- scale(imp$data$srs_total_m_13)
imp$data$CortexVol <- scale(imp$data$CortexVol)
imp$data$CortexVol_f13 <- scale(imp$data$CortexVol_f13)
imp$data$SubCortGrayVol <- scale(imp$data$SubCortGrayVol)
imp$data$SubCortGrayVol_f13 <- scale(imp$data$SubCortGrayVol_f13)
imp$data$CorticalWhiteMatterVol <- scale(imp$data$CorticalWhiteMatterVol)
imp$data$CerebralWhiteMatterVol_f13 <- scale(imp$data$CerebralWhiteMatterVol_f13)
imp$data$CSF_vol <- scale(imp$data$CSF_vol)
imp$data$CSF_vol_f13 <- scale(imp$data$CSF_vol_f13)
imp$data$cerebellum_cortex_volume <- scale(imp$data$cerebellum_cortex_volume)
imp$data$Cerebellum_Cortex_vol_f13 <- scale(imp$data$Cerebellum_Cortex_vol_f13)
imp$data$cerebellum_white_matter_volume <- scale(imp$data$cerebellum_white_matter_volume)
imp$data$Cerebellum_White_Matter_vol_f13 <- scale(imp$data$Cerebellum_White_Matter_vol_f13)
imp$data$amygdala_vol <- scale(imp$data$amygdala_vol)
imp$data$Amygdala_vol_f13 <- scale(imp$data$Amygdala_vol_f13)
imp$data$meangyr13 <- scale(imp$data$meangyr13)
imp$data$meangyr6 <- scale(imp$data$meangyr6)
imp$data$MeanThickness <- scale(imp$data$MeanThickness)
imp$data$MeanThickness_f13 <- scale(imp$data$MeanThickness_f13)
imp$data$WhiteSurfArea_f13 <- scale(imp$data$WhiteSurfArea_f13)
imp$data$WhiteSurfArea_f5 <- scale(imp$data$WhiteSurfArea_f5)

library(car)
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'primary' = '1'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'secondary' = '2'")
imp$data$EDUCM5 <- recode(imp$data$EDUCM5, "'higher' = '3'")
summary(imp$data$EDUCM5)
imp$data$EDUCM5 <- as.numeric(imp$data$EDUCM5)

imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'never smoked during pregnancy' = '1'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'smoked until pregnancy was known' = '2'")
imp$data$SMOKE_ALL <- recode(imp$data$SMOKE_ALL, "'continued smoking in pregnancy' = '3'")
imp$data$SMOKE_ALL <- as.numeric(imp$data$SMOKE_ALL)

imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother never drank in pregnancy' = '1'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother drank until pregnancy was known' = '2'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking occasionally' = '3'")
imp$data$mdrink_updated <- recode(imp$data$mdrink_updated, "'mother continued drinking frequently (1 or more glass/week for at least 2 trimesters)' = '4'")
imp$data$mdrink_updated <- as.numeric(imp$data$mdrink_updated)

imp$data$GENDER <- ifelse(imp$data$GENDER == "boy", 0, 1)
imp$data$HD13 <- ifelse(imp$data$HD13 == "Left", 0, 1)
imp$data$HD <- ifelse(imp$data$HD == "Left", 0, 1)
str(imp$data$age_yrs)
summary(imp$data$AGEMRI_F13)

#imp$data$eth_wes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                           ifelse(imp$data$ETHNINFv2 == "Western", 1, 0))

#imp$data$eth_nonwes <- ifelse(imp$data$ETHNINFv2 == "Dutch", 0,
#                              ifelse(imp$data$ETHNINFv2 == "Non-Western", 1, 0))

#make a list of the imputed datasets for the lavaan.mi function
library("mice")
com <- complete(imp)
com1 <- complete(imp, 1)
com1$eth_wes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                       ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com1$eth_nonwes <- ifelse(com1$ETHNINFv2 == "Dutch", 0,
                          ifelse(com1$ETHNINFv2 == "Non-Western", 1, 0))
com2 <- complete(imp, 2)
com2$eth_wes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                       ifelse(com1$ETHNINFv2 == "Western", 1, 0))

com2$eth_nonwes <- ifelse(com2$ETHNINFv2 == "Dutch", 0,
                          ifelse(com2$ETHNINFv2 == "Non-Western", 1, 0))
com3 <- complete(imp, 3)
com3$eth_wes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                       ifelse(com3$ETHNINFv2 == "Western", 1, 0))

com3$eth_nonwes <- ifelse(com3$ETHNINFv2 == "Dutch", 0,
                          ifelse(com3$ETHNINFv2 == "Non-Western", 1, 0))
com4 <- complete(imp, 4)
com4$eth_wes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                       ifelse(com4$ETHNINFv2 == "Western", 1, 0))

com4$eth_nonwes <- ifelse(com4$ETHNINFv2 == "Dutch", 0,
                          ifelse(com4$ETHNINFv2 == "Non-Western", 1, 0))
com5 <- complete(imp, 5)
com5$eth_wes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                       ifelse(com5$ETHNINFv2 == "Western", 1, 0))

com5$eth_nonwes <- ifelse(com5$ETHNINFv2 == "Dutch", 0,
                          ifelse(com5$ETHNINFv2 == "Non-Western", 1, 0))
com6 <- complete(imp, 6)
com6$eth_wes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                       ifelse(com6$ETHNINFv2 == "Western", 1, 0))

com6$eth_nonwes <- ifelse(com6$ETHNINFv2 == "Dutch", 0,
                          ifelse(com6$ETHNINFv2 == "Non-Western", 1, 0))
com7 <- complete(imp, 7)
com7$eth_wes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                       ifelse(com7$ETHNINFv2 == "Western", 1, 0))

com7$eth_nonwes <- ifelse(com7$ETHNINFv2 == "Dutch", 0,
                          ifelse(com7$ETHNINFv2 == "Non-Western", 1, 0))
com8 <- complete(imp, 8)
com8$eth_wes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                       ifelse(com8$ETHNINFv2 == "Western", 1, 0))

com8$eth_nonwes <- ifelse(com8$ETHNINFv2 == "Dutch", 0,
                          ifelse(com8$ETHNINFv2 == "Non-Western", 1, 0))
com9 <- complete(imp, 9)
com9$eth_wes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                       ifelse(com9$ETHNINFv2 == "Western", 1, 0))

com9$eth_nonwes <- ifelse(com9$ETHNINFv2 == "Dutch", 0,
                          ifelse(com9$ETHNINFv2 == "Non-Western", 1, 0))
com10 <- complete(imp, 10)
com10$eth_wes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                        ifelse(com10$ETHNINFv2 == "Western", 1, 0))

com10$eth_nonwes <- ifelse(com10$ETHNINFv2 == "Dutch", 0,
                           ifelse(com10$ETHNINFv2 == "Non-Western", 1, 0))
com11 <- complete(imp, 11)
com11$eth_wes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                        ifelse(com11$ETHNINFv2 == "Western", 1, 0))

com11$eth_nonwes <- ifelse(com11$ETHNINFv2 == "Dutch", 0,
                           ifelse(com11$ETHNINFv2 == "Non-Western", 1, 0))
com12 <- complete(imp, 12)
com12$eth_wes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                        ifelse(com12$ETHNINFv2 == "Western", 1, 0))

com12$eth_nonwes <- ifelse(com12$ETHNINFv2 == "Dutch", 0,
                           ifelse(com12$ETHNINFv2 == "Non-Western", 1, 0))
com13 <- complete(imp, 13)
com13$eth_wes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                        ifelse(com13$ETHNINFv2 == "Western", 1, 0))

com13$eth_nonwes <- ifelse(com13$ETHNINFv2 == "Dutch", 0,
                           ifelse(com13$ETHNINFv2 == "Non-Western", 1, 0))
com14 <- complete(imp, 14)
com14$eth_wes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                        ifelse(com14$ETHNINFv2 == "Western", 1, 0))

com14$eth_nonwes <- ifelse(com14$ETHNINFv2 == "Dutch", 0,
                           ifelse(com14$ETHNINFv2 == "Non-Western", 1, 0))
com15 <- complete(imp, 15)
com15$eth_wes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                        ifelse(com15$ETHNINFv2 == "Western", 1, 0))

com15$eth_nonwes <- ifelse(com15$ETHNINFv2 == "Dutch", 0,
                           ifelse(com15$ETHNINFv2 == "Non-Western", 1, 0))
com16 <- complete(imp, 16)
com16$eth_wes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                        ifelse(com16$ETHNINFv2 == "Western", 1, 0))

com16$eth_nonwes <- ifelse(com16$ETHNINFv2 == "Dutch", 0,
                           ifelse(com16$ETHNINFv2 == "Non-Western", 1, 0))
com17 <- complete(imp, 17)
com17$eth_wes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                        ifelse(com17$ETHNINFv2 == "Western", 1, 0))

com17$eth_nonwes <- ifelse(com17$ETHNINFv2 == "Dutch", 0,
                           ifelse(com17$ETHNINFv2 == "Non-Western", 1, 0))
com18 <- complete(imp, 18)
com18$eth_wes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                        ifelse(com18$ETHNINFv2 == "Western", 1, 0))

com18$eth_nonwes <- ifelse(com18$ETHNINFv2 == "Dutch", 0,
                           ifelse(com18$ETHNINFv2 == "Non-Western", 1, 0))
com19 <- complete(imp, 19)
com19$eth_wes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                        ifelse(com19$ETHNINFv2 == "Western", 1, 0))

com19$eth_nonwes <- ifelse(com19$ETHNINFv2 == "Dutch", 0,
                           ifelse(com19$ETHNINFv2 == "Non-Western", 1, 0))
com20 <- complete(imp, 20)
com20$eth_wes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                        ifelse(com20$ETHNINFv2 == "Western", 1, 0))

com20$eth_nonwes <- ifelse(com20$ETHNINFv2 == "Dutch", 0,
                           ifelse(com20$ETHNINFv2 == "Non-Western", 1, 0))
com21 <- complete(imp, 21)
com21$eth_wes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                        ifelse(com21$ETHNINFv2 == "Western", 1, 0))

com21$eth_nonwes <- ifelse(com21$ETHNINFv2 == "Dutch", 0,
                           ifelse(com21$ETHNINFv2 == "Non-Western", 1, 0))
com22 <- complete(imp, 22)
com22$eth_wes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                        ifelse(com22$ETHNINFv2 == "Western", 1, 0))

com22$eth_nonwes <- ifelse(com22$ETHNINFv2 == "Dutch", 0,
                           ifelse(com22$ETHNINFv2 == "Non-Western", 1, 0))
com23 <- complete(imp, 23)
com23$eth_wes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                        ifelse(com23$ETHNINFv2 == "Western", 1, 0))

com23$eth_nonwes <- ifelse(com23$ETHNINFv2 == "Dutch", 0,
                           ifelse(com23$ETHNINFv2 == "Non-Western", 1, 0))
com24 <- complete(imp, 24)
com24$eth_wes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                        ifelse(com24$ETHNINFv2 == "Western", 1, 0))

com24$eth_nonwes <- ifelse(com24$ETHNINFv2 == "Dutch", 0,
                           ifelse(com24$ETHNINFv2 == "Non-Western", 1, 0))
com25 <- complete(imp, 25)
com25$eth_wes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                        ifelse(com25$ETHNINFv2 == "Western", 1, 0))

com25$eth_nonwes <- ifelse(com25$ETHNINFv2 == "Dutch", 0,
                           ifelse(com25$ETHNINFv2 == "Non-Western", 1, 0))
com26 <- complete(imp, 26)
com26$eth_wes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                        ifelse(com26$ETHNINFv2 == "Western", 1, 0))

com26$eth_nonwes <- ifelse(com26$ETHNINFv2 == "Dutch", 0,
                           ifelse(com26$ETHNINFv2 == "Non-Western", 1, 0))
com27 <- complete(imp, 27)
com27$eth_wes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                        ifelse(com27$ETHNINFv2 == "Western", 1, 0))

com27$eth_nonwes <- ifelse(com27$ETHNINFv2 == "Dutch", 0,
                           ifelse(com27$ETHNINFv2 == "Non-Western", 1, 0))
com28 <- complete(imp, 28)
com28$eth_wes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                        ifelse(com28$ETHNINFv2 == "Western", 1, 0))

com28$eth_nonwes <- ifelse(com28$ETHNINFv2 == "Dutch", 0,
                           ifelse(com28$ETHNINFv2 == "Non-Western", 1, 0))
com29 <- complete(imp, 29)
com29$eth_wes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                        ifelse(com29$ETHNINFv2 == "Western", 1, 0))

com29$eth_nonwes <- ifelse(com29$ETHNINFv2 == "Dutch", 0,
                           ifelse(com29$ETHNINFv2 == "Non-Western", 1, 0))
com30 <- complete(imp, 30)
com30$eth_wes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                        ifelse(com30$ETHNINFv2 == "Western", 1, 0))

com30$eth_nonwes <- ifelse(com30$ETHNINFv2 == "Dutch", 0,
                           ifelse(com30$ETHNINFv2 == "Non-Western", 1, 0))

list <- list(com1, com2, com3, com4, com5, com6, com7, com8, com9, com10, com11, com12, com13, com14, com15, com16, com17, com18, com19, com20, com21, com22, com23, com24, com25, com26, com27, com28, com29, com30)

#cortical thickness mean
com30$MeanThickness
com30$MeanThickness_f13

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13 

#Regression

srs_6times18  ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

MeanThickness  ~ agechildGR1076 + dif_age6
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + MeanThickness_f13 ~ srs_6times18 + MeanThickness 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ MeanThickness  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ MeanThickness_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
MeanThickness ~~ MeanThickness 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
MeanThickness_f13 ~~ MeanThickness_f13  

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

MeanThickness  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
MeanThickness_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


#cortical thickness rh (1 cluster, no lh)

require(lavaan) 
#model1
CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh 

#Regression

srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_thickness_rh  ~  agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <- summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#model2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_thickness_rh ~ srs_6times18 + six_mean_vertex_thickness_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_thickness_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_thickness_rh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_thickness_rh ~~ six_mean_vertex_thickness_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_thickness_rh ~~ thirteen_mean_vertex_thickness_rh  

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_thickness_rh  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_thickness_rh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

# mean surface area model 1
require(lavaan) 
imp$data$WhiteSurfArea_f5

CLPM1 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

WhiteSurfArea_f5  ~ agechildGR1076 + dif_age6 + HD 
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

# mean surface area model 2
CLPM1_2 <- '
srs_total_m_13 + WhiteSurfArea_f13 ~ srs_6times18 + WhiteSurfArea_f5 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ WhiteSurfArea_f5  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ WhiteSurfArea_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
WhiteSurfArea_f5 ~~ WhiteSurfArea_f5 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
WhiteSurfArea_f13 ~~ WhiteSurfArea_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

WhiteSurfArea_f5  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
WhiteSurfArea_f13 ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue


# surface area lh model 1
require(lavaan) 

CLPM1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_lh  ~ agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13 

'

CLPM.fit1mi <- lavaan.mi(model = CLPM1, data = list)
sum1 <-summary(CLPM.fit1mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

# surface area lh model 2
CLPM1_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_lh ~ srs_6times18 + six_mean_vertex_area_lh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_lh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_lh 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_lh ~~ six_mean_vertex_area_lh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_lh ~~ thirteen_mean_vertex_area_lh

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_lh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_lh ~ AGECHILD_GR1093 + dif_age13

'

CLPM.fit1_2mi <- lavaan.mi(model = CLPM1_2, data = list)
sum2 <- summary(CLPM.fit1_2mi, se = TRUE, ci = TRUE, standardized = FALSE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#surface area rh model1
CLPM2_1 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18  ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

six_mean_vertex_area_rh  ~ agechildGR1076 + dif_age6 + HD 
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13 

'
CLPM.fit2_1mi <- lavaan.mi(model = CLPM2_1, data = list)
sum1 <- summary(CLPM.fit2_1mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

#surface area rh model2
CLPM2_2 <- '
srs_total_m_13 + thirteen_mean_vertex_area_rh ~ srs_6times18 + six_mean_vertex_area_rh 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ six_mean_vertex_area_rh  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ thirteen_mean_vertex_area_rh

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
six_mean_vertex_area_rh ~~ six_mean_vertex_area_rh 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
thirteen_mean_vertex_area_rh ~~ thirteen_mean_vertex_area_rh


#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13

six_mean_vertex_area_rh  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes + HD
thirteen_mean_vertex_area_rh ~ AGECHILD_GR1093 + dif_age13

'
CLPM.fit2_2mi <- lavaan.mi(model = CLPM2_2, data = list)
sum2 <- summary(CLPM.fit2_2mi, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

##################################################
#############global measures  (NO HD)#############
##################################################

#total brain volume (tbv)
imp$data$tbv_knicr #@6
imp$data$tbv13
#model 1
TBV1 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13 

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr ~  agechildGR1076 + dif_age6 
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'

TBV.fit1 <- lavaan.mi(model = TBV1, data = list)
sumtbv1 <-summary(TBV.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumtbv1$pvalue

#model 2
TBV2 <- '
srs_total_m_13 + tbv13 ~ srs_6times18 + tbv_knicr 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ tbv_knicr  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ tbv13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
tbv_knicr ~~ tbv_knicr 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
tbv13 ~~ tbv13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

tbv_knicr  ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
tbv13 ~ AGECHILD_GR1093 + dif_age13 

'
TBV.fit2 <- lavaan.mi(model = TBV2, data = list)
sum <- summary(TBV.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum$pvalue


#CORTICAL gray matter volume
imp$data$CortexVol
imp$data$CortexVol_f13
#model 1
CortGrayVol1 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13 

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol ~ agechildGR1076 + dif_age6 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

library("semTools")
TotalGrayVol.fit1 <- lavaan.mi(model = CortGrayVol1, data = list)
sumgray1 <- summary(TotalGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray1$pvalue
#model 2
CorticalGrayVol2 <- '
srs_total_m_13 + CortexVol_f13 ~ srs_6times18 + CortexVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CortexVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CortexVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CortexVol ~~ CortexVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CortexVol_f13 ~~ CortexVol_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CortexVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CortexVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
TotalGrayVol.fit2 <- lavaan.mi(model = CorticalGrayVol2, data = list)
sumgray2<- summary(TotalGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumgray2$pvalue

#subcortical grey matter volume
imp$data$SubCortGrayVol_f13
imp$data$SubCortGrayVol #@6

#model 1
SubCortGrayVol1 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol ~  agechildGR1076 + dif_age6 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

SubCortGrayVol.fit1 <- lavaan.mi(model = SubCortGrayVol1, data = list)
sumsubcortgrayvol1 <- summary(SubCortGrayVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)

sumsubcortgrayvol1$pvalue

#model 2
SubCortGrayVol2 <- '
srs_total_m_13 + SubCortGrayVol_f13 ~ srs_6times18 + SubCortGrayVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ SubCortGrayVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ SubCortGrayVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
SubCortGrayVol ~~ SubCortGrayVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
SubCortGrayVol_f13 ~~ SubCortGrayVol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

SubCortGrayVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
SubCortGrayVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
SubCortGrayVol.fit2 <- lavaan.mi(model = SubCortGrayVol2, data = list)
sumsubcortgrayvol2 <- summary(SubCortGrayVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumsubcortgrayvol2$pvalue


#cerebral white matter volume

imp$data$CerebralWhiteMatterVol_f13
imp$data$CorticalWhiteMatterVol #@6

CerebralWhiteMatterVol1 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol ~  agechildGR1076 + dif_age6 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13

'

CerebralWhiteMatterVol.fit1 <- lavaan.mi(model = CerebralWhiteMatterVol1, data = list)
sum1<- summary(CerebralWhiteMatterVol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

CerebralWhiteMatterVol2 <- '
srs_total_m_13 + CerebralWhiteMatterVol_f13 ~ srs_6times18 + CorticalWhiteMatterVol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CorticalWhiteMatterVol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CerebralWhiteMatterVol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CorticalWhiteMatterVol ~~ CorticalWhiteMatterVol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CerebralWhiteMatterVol_f13 ~~ CerebralWhiteMatterVol_f13

#Regression

srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CorticalWhiteMatterVol  ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CerebralWhiteMatterVol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CerebralWhiteMatterVol.fit2 <- lavaan.mi(model = CerebralWhiteMatterVol2, data = list)
sum2 <- summary(CerebralWhiteMatterVol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#cerebrospinal fluid (CSF)

imp$data$CSF_vol #@6
imp$data$CSF_vol_f13

CSF_vol1 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~  agechildGR1076 + dif_age6 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'

library("semTools")
CSF_vol.fit1 <- lavaan.mi(model = CSF_vol1, data = list)
sumcsf1 <- summary(CSF_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf1$pvalue

CSF_vol2 <- '
srs_total_m_13 + CSF_vol_f13 ~ srs_6times18 + CSF_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ CSF_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ CSF_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
CSF_vol ~~ CSF_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
CSF_vol_f13 ~~ CSF_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

CSF_vol ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
CSF_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
CSF_vol.fit2 <- lavaan.mi(model = CSF_vol2, data = list)
sumcsf2 <- summary(CSF_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcsf2$pvalue

#cerebellum cortex volume

imp$data$cerebellum_cortex_volume #@6
imp$data$Cerebellum_Cortex_vol_f13

Cerebellum_Cortex_vol1 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume  ~  agechildGR1076 + dif_age6 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_Cortex_vol.fit1 <- lavaan.mi(model = Cerebellum_Cortex_vol1, data = list)
sumcerebellum1 <- summary(Cerebellum_Cortex_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum1$pvalue

Cerebellum_Cortex_vol2 <- '
srs_total_m_13 + Cerebellum_Cortex_vol_f13 ~ srs_6times18 + cerebellum_cortex_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_cortex_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_Cortex_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_cortex_volume ~~ cerebellum_cortex_volume
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_Cortex_vol_f13 ~~ Cerebellum_Cortex_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_cortex_volume ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_Cortex_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'
Cerebellum_Cortex_vol.fit2 <- lavaan.mi(model = Cerebellum_Cortex_vol2, data = list)
sumcerebellum2 <- summary(Cerebellum_Cortex_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sumcerebellum2$pvalue


#cerebellum white matter volume

imp$data$cerebellum_white_matter_volume #@6
imp$data$Cerebellum_White_Matter_vol_f13

Cerebellum_White_Matter_vol1 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume  ~  agechildGR1076 + dif_age6 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

Cerebellum_White_Matter_vol.fit1 <- lavaan.mi(model = Cerebellum_White_Matter_vol1, data = list)
sum1 <- summary(Cerebellum_White_Matter_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

Cerebellum_White_Matter_vol2 <- '
srs_total_m_13 + Cerebellum_White_Matter_vol_f13 ~ srs_6times18 + cerebellum_white_matter_volume 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ cerebellum_white_matter_volume  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Cerebellum_White_Matter_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
cerebellum_white_matter_volume ~~ cerebellum_white_matter_volume 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Cerebellum_White_Matter_vol_f13 ~~ Cerebellum_White_Matter_vol_f13

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

cerebellum_white_matter_volume ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Cerebellum_White_Matter_vol_f13 ~ AGECHILD_GR1093 + dif_age13

'
Cerebellum_White_Matter_vol.fit2 <- lavaan.mi(model = Cerebellum_White_Matter_vol2, data = list)
sum2 <- summary(Cerebellum_White_Matter_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue

#amygdala volume (mean left and right)
imp$data$amygdala_vol #@6
imp$data$Amygdala_vol_f13

amygdala_vol1 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13 

#Regression
srs_6times18 ~  agechildGR1076 + dif_age6 
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol  ~  agechildGR1076 + dif_age6 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13 

'

amygdala_vol.fit1 <- lavaan.mi(model = amygdala_vol1, data = list)
sum1 <- summary(amygdala_vol.fit1, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum1$pvalue

amygdala_vol2 <- '
srs_total_m_13 + Amygdala_vol_f13 ~ srs_6times18 + amygdala_vol 

# Estimate the covariance between the observed variables at the first wave. 
srs_6times18 ~~ amygdala_vol  # Covariance

# Estimate the covariances between the residuals of the observed variables.
srs_total_m_13 ~~ Amygdala_vol_f13 

# Estimate the (residual) variance of the observed variables.
srs_6times18 ~~ srs_6times18  # Variances
amygdala_vol ~~ amygdala_vol 
srs_total_m_13 ~~ srs_total_m_13 # Residual variances
Amygdala_vol_f13 ~~ Amygdala_vol_f13

#Regression
srs_6times18 ~ agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes
srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 

amygdala_vol ~  agechildGR1076 + dif_age6 + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes 
Amygdala_vol_f13 ~ AGECHILD_GR1093 + dif_age13
'
amygdala_vol.fit2 <- lavaan.mi(model = amygdala_vol2, data = list)
sum2 <- summary(amygdala_vol.fit2, se = TRUE, ci = TRUE, standardized = TRUE, header = TRUE, fit.measures = TRUE)
sum2$pvalue





