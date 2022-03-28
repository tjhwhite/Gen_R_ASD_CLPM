#Packages
library(foreign)
library(dplyr)
library("mice")
library("lavaan")
library("semTools")
library(car)
library(haven)
library(readr)

install.packages("xlsx")
library("xlsx")

rm(list = ls())

#
#########################################################################
################## prepare dataset for CLPM ##################
#########################################################################

df <- read.spss("V:/medewerkers/038870 Durkut, M/finallong.sav", to.data.frame = TRUE)
df$agedif <- df$AGECHILD_GR1093 - df$agechildGR1076

#transform srs 
df$srs_6times18 <- sqrt(df$srs_6times18)
df$srs_total_m_13 <- sqrt(df$srs_total_m_13)

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
aparc6 <- read.spss("V:/medewerkers/038870 Durkut, M/freesurfer_Sept04_2013_n1070_aparc_stats.sav", to.data.frame = TRUE)

aparc13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aparc_stats_pull23Nov2020_noDups.rds")

dfvol6 <- dplyr::select(aparc6, IDC, ends_with("_vol"))
dfvol13 <- dplyr::select(aparc13, IDC, ends_with("_vol_F13"))
summary(dfvol6)

dfvol6$bankssts_vol <- (dfvol6$lh_bankssts_vol + dfvol6$rh_bankssts_vol)/2
dfvol6$caudalanteriorcingulate_vol <- (dfvol6$lh_caudalanteriorcingulate_vol + dfvol6$rh_caudalanteriorcingulate_vol)/2
dfvol6$caudalmiddlefrontal_vol <- (dfvol6$lh_caudalmiddlefrontal_vol + dfvol6$rh_caudalmiddlefrontal_vol)/2
dfvol6$cuneus_vol  <- (dfvol6$lh_cuneus_vol  + dfvol6$rh_cuneus_vol)/2
dfvol6$entorhinal_vol  <- (dfvol6$lh_entorhinal_vol  + dfvol6$rh_entorhinal_vol)/2
dfvol6$fusiform_vol <- (dfvol6$lh_fusiform_vol + dfvol6$rh_fusiform_vol)/2
dfvol6$inferiorparietal_vol <- (dfvol6$lh_inferiorparietal_vol + dfvol6$rh_inferiorparietal_vol)/2
dfvol6$inferiortemporal_vol <- (dfvol6$lh_inferiortemporal_vol + dfvol6$rh_inferiortemporal_vol)/2
dfvol6$isthmuscingulate_vol <- (dfvol6$lh_isthmuscingulate_vol + dfvol6$rh_isthmuscingulate_vol)/2
dfvol6$lateraloccipital_vol <- (dfvol6$lh_lateraloccipital_vol + dfvol6$rh_lateraloccipital_vol)/2

dfvol6$lateralorbitofrontal_vol <- (dfvol6$lh_lateralorbitofrontal_vol + dfvol6$rh_lateralorbitofrontal_vol)/2
dfvol6$lingual_vol <- (dfvol6$lh_lingual_vol + dfvol6$rh_lingual_vol)/2
dfvol6$medialorbitofrontal_vol <- (dfvol6$lh_medialorbitofrontal_vol + dfvol6$rh_medialorbitofrontal_vol)/2
dfvol6$middletemporal_vol <- (dfvol6$lh_middletemporal_vol + dfvol6$rh_middletemporal_vol)/2
dfvol6$parahippocampal_vol <- (dfvol6$lh_parahippocampal_vol + dfvol6$rh_parahippocampal_vol)/2
dfvol6$paracentral_vol <- (dfvol6$lh_paracentral_vol + dfvol6$rh_paracentral_vol)/2

dfvol6$parsopercularis_vol <- (dfvol6$lh_parsopercularis_vol + dfvol6$rh_parsopercularis_vol)/2
dfvol6$parsorbitalis_vol <- (dfvol6$lh_parsorbitalis_vol + dfvol6$rh_parsorbitalis_vol)/2
dfvol6$parstriangularis_vol <- (dfvol6$lh_parstriangularis_vol + dfvol6$lh_parstriangularis_vol)/2
dfvol6$pericalcarine_vol <- (dfvol6$lh_pericalcarine_vol + dfvol6$rh_pericalcarine_vol)/2
dfvol6$postcentral_vol <- (dfvol6$lh_postcentral_vol + dfvol6$rh_postcentral_vol)/2
dfvol6$posteriorcingulate_vol <- (dfvol6$lh_posteriorcingulate_vol + dfvol6$rh_posteriorcingulate_vol)/2
dfvol6$precentral_vol <- (dfvol6$lh_precentral_vol + dfvol6$rh_precentral_vol)/2

dfvol6$precuneus_vol <- (dfvol6$lh_precuneus_vol + dfvol6$rh_precuneus_vol)/2
dfvol6$rostralanteriorcingulate_vol <- (dfvol6$lh_rostralanteriorcingulate_vol + dfvol6$rh_rostralanteriorcingulate_vol)/2
dfvol6$rostralmiddlefrontal_vol <- (dfvol6$lh_rostralmiddlefrontal_vol + dfvol6$lh_rostralmiddlefrontal_vol)/2
dfvol6$superiorfrontal_vol <- (dfvol6$lh_superiorfrontal_vol + dfvol6$rh_superiorfrontal_vol)/2
dfvol6$superiorparietal_vol <- (dfvol6$lh_superiorparietal_vol + dfvol6$rh_superiorparietal_vol)/2
dfvol6$superiortemporal_vol <- (dfvol6$lh_superiortemporal_vol + dfvol6$rh_superiortemporal_vol)/2

dfvol6$supramarginal_vol <- (dfvol6$lh_supramarginal_vol + dfvol6$rh_supramarginal_vol)/2
dfvol6$frontalpole_vol <- (dfvol6$lh_frontalpole_vol + dfvol6$rh_frontalpole_vol)/2
dfvol6$temporalpole_vol <- (dfvol6$lh_temporalpole_vol + dfvol6$rh_temporalpole_vol)/2
dfvol6$transversetemporal_vol <- (dfvol6$lh_transversetemporal_vol + dfvol6$rh_transversetemporal_vol)/2
dfvol6$insula_vol <- (dfvol6$lh_insula_vol + dfvol6$rh_insula_vol)/2

dfvol6t <- dplyr::select(dfvol6, IDC, insula_vol, transversetemporal_vol, temporalpole_vol, 
                        frontalpole_vol, supramarginal_vol, superiortemporal_vol,
                        superiorparietal_vol, superiorfrontal_vol, rostralmiddlefrontal_vol,
                        rostralanteriorcingulate_vol, precuneus_vol, precentral_vol,
                        posteriorcingulate_vol, postcentral_vol, pericalcarine_vol,
                        parstriangularis_vol, parsorbitalis_vol, parsopercularis_vol, 
                        paracentral_vol, parahippocampal_vol, middletemporal_vol,
                        medialorbitofrontal_vol, lingual_vol, lateralorbitofrontal_vol,
                        lateraloccipital_vol, lateraloccipital_vol, isthmuscingulate_vol,
                        inferiortemporal_vol, inferiorparietal_vol, fusiform_vol,
                        entorhinal_vol, cuneus_vol, caudalmiddlefrontal_vol, 
                        caudalanteriorcingulate_vol, bankssts_vol)
##########
###13#####
##########
summary(dfvol13)

dfvol13$bankssts_vol_f13 <- (dfvol13$lh_bankssts_vol_f13 + dfvol13$rh_bankssts_vol_f13)/2
dfvol13$caudalanteriorcingulate_vol_f13 <- (dfvol13$lh_caudalanteriorcingulate_vol_f13 + dfvol13$rh_caudalanteriorcingulate_vol_f13)/2
dfvol13$caudalmiddlefrontal_vol_f13 <- (dfvol13$lh_caudalmiddlefrontal_vol_f13 + dfvol13$rh_caudalmiddlefrontal_vol_f13)/2
dfvol13$cuneus_vol_f13  <- (dfvol13$lh_cuneus_vol_f13  + dfvol13$rh_cuneus_vol_f13)/2
dfvol13$entorhinal_vol_f13  <- (dfvol13$lh_entorhinal_vol_f13  + dfvol13$rh_entorhinal_vol_f13)/2
dfvol13$fusiform_vol_f13 <- (dfvol13$lh_fusiform_vol_f13 + dfvol13$rh_fusiform_vol_f13)/2
dfvol13$inferiorparietal_vol_f13 <- (dfvol13$lh_inferiorparietal_vol_f13 + dfvol13$rh_inferiorparietal_vol_f13)/2
dfvol13$inferiortemporal_vol_f13 <- (dfvol13$lh_inferiortemporal_vol_f13 + dfvol13$rh_inferiortemporal_vol_f13)/2
dfvol13$isthmuscingulate_vol_f13 <- (dfvol13$lh_isthmuscingulate_vol_f13 + dfvol13$rh_isthmuscingulate_vol_f13)/2
dfvol13$lateraloccipital_vol_f13 <- (dfvol13$lh_lateraloccipital_vol_f13 + dfvol13$rh_lateraloccipital_vol_f13)/2

dfvol13$lateralorbitofrontal_vol_f13 <- (dfvol13$lh_lateralorbitofrontal_vol_f13 + dfvol13$rh_lateralorbitofrontal_vol_f13)/2
dfvol13$lingual_vol_f13 <- (dfvol13$lh_lingual_vol_f13 + dfvol13$rh_lingual_vol_f13)/2
dfvol13$medialorbitofrontal_vol_f13 <- (dfvol13$lh_medialorbitofrontal_vol_f13 + dfvol13$rh_medialorbitofrontal_vol_f13)/2
dfvol13$middletemporal_vol_f13 <- (dfvol13$lh_middletemporal_vol_f13 + dfvol13$rh_middletemporal_vol_f13)/2
dfvol13$parahippocampal_vol_f13 <- (dfvol13$lh_parahippocampal_vol_f13 + dfvol13$rh_parahippocampal_vol_f13)/2
dfvol13$paracentral_vol_f13 <- (dfvol13$lh_paracentral_vol_f13 + dfvol13$rh_paracentral_vol_f13)/2

dfvol13$parsopercularis_vol_f13 <- (dfvol13$lh_parsopercularis_vol_f13 + dfvol13$rh_parsopercularis_vol_f13)/2
dfvol13$parsorbitalis_vol_f13 <- (dfvol13$lh_parsorbitalis_vol_f13 + dfvol13$rh_parsorbitalis_vol_f13)/2
dfvol13$parstriangularis_vol_f13 <- (dfvol13$lh_parstriangularis_vol_f13 + dfvol13$lh_parstriangularis_vol_f13)/2
dfvol13$pericalcarine_vol_f13 <- (dfvol13$lh_pericalcarine_vol_f13 + dfvol13$rh_pericalcarine_vol_f13)/2
dfvol13$postcentral_vol_f13 <- (dfvol13$lh_postcentral_vol_f13 + dfvol13$rh_postcentral_vol_f13)/2
dfvol13$posteriorcingulate_vol_f13 <- (dfvol13$lh_posteriorcingulate_vol_f13 + dfvol13$rh_posteriorcingulate_vol_f13)/2
dfvol13$precentral_vol_f13 <- (dfvol13$lh_precentral_vol_f13 + dfvol13$rh_precentral_vol_f13)/2

dfvol13$precuneus_vol_f13 <- (dfvol13$lh_precuneus_vol_f13 + dfvol13$rh_precuneus_vol_f13)/2
dfvol13$rostralanteriorcingulate_vol_f13 <- (dfvol13$lh_rostralanteriorcingulate_vol_f13 + dfvol13$rh_rostralanteriorcingulate_vol_f13)/2
dfvol13$rostralmiddlefrontal_vol_f13 <- (dfvol13$lh_rostralmiddlefrontal_vol_f13 + dfvol13$lh_rostralmiddlefrontal_vol_f13)/2
dfvol13$superiorfrontal_vol_f13 <- (dfvol13$lh_superiorfrontal_vol_f13 + dfvol13$rh_superiorfrontal_vol_f13)/2
dfvol13$superiorparietal_vol_f13 <- (dfvol13$lh_superiorparietal_vol_f13 + dfvol13$rh_superiorparietal_vol_f13)/2
dfvol13$superiortemporal_vol_f13 <- (dfvol13$lh_superiortemporal_vol_f13 + dfvol13$rh_superiortemporal_vol_f13)/2

dfvol13$supramarginal_vol_f13 <- (dfvol13$lh_supramarginal_vol_f13 + dfvol13$rh_supramarginal_vol_f13)/2
dfvol13$frontalpole_vol_f13 <- (dfvol13$lh_frontalpole_vol_f13 + dfvol13$rh_frontalpole_vol_f13)/2
dfvol13$temporalpole_vol_f13 <- (dfvol13$lh_temporalpole_vol_f13 + dfvol13$rh_temporalpole_vol_f13)/2
dfvol13$transversetemporal_vol_f13 <- (dfvol13$lh_transversetemporal_vol_f13 + dfvol13$rh_transversetemporal_vol_f13)/2
dfvol13$insula_vol_f13 <- (dfvol13$lh_insula_vol_f13 + dfvol13$rh_insula_vol_f13)/2

dfvol13t <- dplyr::select(dfvol13, IDC, insula_vol_f13, transversetemporal_vol_f13, temporalpole_vol_f13, 
                         frontalpole_vol_f13, supramarginal_vol_f13, superiortemporal_vol_f13,
                         superiorparietal_vol_f13, superiorfrontal_vol_f13, rostralmiddlefrontal_vol_f13,
                         rostralanteriorcingulate_vol_f13, precuneus_vol_f13, precentral_vol_f13,
                         posteriorcingulate_vol_f13, postcentral_vol_f13, pericalcarine_vol_f13,
                         parstriangularis_vol_f13, parsorbitalis_vol_f13, parsopercularis_vol_f13, 
                         paracentral_vol_f13, parahippocampal_vol_f13, middletemporal_vol_f13,
                         medialorbitofrontal_vol_f13, lingual_vol_f13, lateralorbitofrontal_vol_f13,
                         lateraloccipital_vol_f13, lateraloccipital_vol_f13, isthmuscingulate_vol_f13,
                         inferiortemporal_vol_f13, inferiorparietal_vol_f13, fusiform_vol_f13,
                         entorhinal_vol_f13, cuneus_vol_f13, caudalmiddlefrontal_vol_f13, 
                         caudalanteriorcingulate_vol_f13, bankssts_vol_f13)

#TBV W1
tbv2 <- read.spss("V:/medewerkers/038870 Durkut, M/F06_freesurfer_Sept04_2013_n1070_tbvknicr.sav", to.data.frame = TRUE)
tbv62 <- dplyr::select(tbv2, IDC, tbv_knicr)

#TBV W3
tbv <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_tbv_stats_pull23Nov2020_noDups.rds")
tbv13 <- dplyr::select(tbv, IDC, CortexVol_f13, SubCortGrayVol_f13, CerebralWhiteMatterVol_f13, SupraTentorialVol_f13)

aseg13 <- readRDS("V:/medewerkers/038870 Durkut, M/f13_freesurfer_14oct2020_aseg_stats_pull23Nov2020_noDups.rds")
aseg13$Cerebellum_Cortex_vol_f13 <- aseg13$Left_Cerebellum_Cortex_vol_f13 + aseg13$Right_Cerebellum_Cortex_vol_f13
aseg13$Cerebellum_White_Matter_vol_f13 <- aseg13$Left_Cerebellum_White_Matter_vol_f13 + aseg13$Right_Cerebellum_White_Matter_vol_f13
aseg13$Amygdala_vol_f13 <- (aseg13$Left_Amygdala_vol_f13 + aseg13$Right_Amygdala_vol_f13)/2
aseg13 <- dplyr::select(aseg13, IDC, Cerebellum_Cortex_vol_f13, Cerebellum_White_Matter_vol_f13, Amygdala_vol_f13, CSF_vol_f13)


global13 <- merge(aseg13, tbv13, by = 'IDC')
global13$tbv13 <- global13$SupraTentorialVol_f13 + global13$Cerebellum_Cortex_vol_f13 + global13$Cerebellum_White_Matter_vol_f13
global13 <- dplyr::select(global13, IDC, tbv13)

#MERGING DATA
final1 <- merge(final, dfvol6t, by = 'IDC')
final2 <- merge(final1, dfvol13t, by = 'IDC')
final3 <- merge(final2, global13, by = 'IDC')
final <- merge(final3, tbv62, by = 'IDC')


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

summary(final)


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
predPAS[, c("IDC","dif_age6", "dif_age13", "agedif", "thirteen_mean_vertex_lgi_lh", "thirteen_mean_vertex_lgi_rh", "tbv13", "tbv_knicr", "AGEMRI_F13", "age_yrs", "lgi", "sens", "ETHNMv2", "HD")] <- 0


# Check visit sequence
visSeqPAS <- imp0$visitSequence
visSeqPAS
# Perform the imputation
impPAS <- mice(final, method = methPAS, predictorMatrix = predPAS, visitSequence = visSeqPAS,
               maxit = 30, m = 30, printFlag = FALSE, seed = 2020)

# Check whether there are logged events
impPAS$loggedEvents #NULL

# Summary of the imputed values
sapply(Filter(function(x) nrow(x) > 0, impPAS$imp),
       function(x) summary(unlist(x))
)


# Traceplot for mean and SD per incomplete variable (Figure 5)
plot(impPAS, layout = c(4, 7))
densityplot(impPAS)
setwd("V:/medewerkers/038870 Durkut, M")
saveRDS(impPAS, file = "imp_CLPMlongatlas")


imp <- readRDS("V:/medewerkers/038870 Durkut, M/imp_CLPMlongatlas")

#imp <- impPAS
#imp.lgi <- complete(imp, include = T, action = "long")
#exclude subjects without LGI data 
#imptest <- subset(imp.lgi, lgi > 0)
#imp_lgi <- as.mids(imptest)
#summary(imp_lgi$data$lgi) # subsetted to lgi is 1
#imp <- imp_lgi 
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

#scale regions F13
imp$data$bankssts_vol_f13 <- scale(imp$data$bankssts_vol_f13)
imp$data$caudalanteriorcingulate_vol_f13 <- scale(imp$data$caudalanteriorcingulate_vol_f13)
imp$data$caudalmiddlefrontal_vol_f13 <- scale(imp$data$caudalmiddlefrontal_vol_f13)
imp$data$cuneus_vol_f13 <- scale(imp$data$cuneus_vol_f13)
imp$data$entorhinal_vol_f13 <- scale(imp$data$entorhinal_vol_f13)
imp$data$fusiform_vol_f13 <- scale(imp$data$fusiform_vol_f13)
imp$data$inferiorparietal_vol_f13 <- scale(imp$data$inferiorparietal_vol_f13)
imp$data$inferiortemporal_vol_f13 <- scale(imp$data$inferiortemporal_vol_f13)
imp$data$isthmuscingulate_vol_f13 <- scale(imp$data$isthmuscingulate_vol_f13)
imp$data$lateraloccipital_vol_f13 <- scale(imp$data$lateraloccipital_vol_f13)
imp$data$lateralorbitofrontal_vol_f13 <- scale(imp$data$lateralorbitofrontal_vol_f13)
imp$data$lingual_vol_f13 <- scale(imp$data$lingual_vol_f13)
imp$data$medialorbitofrontal_vol_f13 <- scale(imp$data$medialorbitofrontal_vol_f13)
imp$data$middletemporal_vol_f13 <- scale(imp$data$middletemporal_vol_f13)
imp$data$parahippocampal_vol_f13 <- scale(imp$data$parahippocampal_vol_f13)
imp$data$paracentral_vol_f13 <- scale(imp$data$paracentral_vol_f13)
imp$data$parsopercularis_vol_f13 <- scale(imp$data$parsopercularis_vol_f13)
imp$data$parsorbitalis_vol_f13 <- scale(imp$data$parsorbitalis_vol_f13)
imp$data$parstriangularis_vol_f13 <- scale(imp$data$parstriangularis_vol_f13)
imp$data$pericalcarine_vol_f13 <- scale(imp$data$pericalcarine_vol_f13)
imp$data$postcentral_vol_f13 <- scale(imp$data$postcentral_vol_f13)
imp$data$posteriorcingulate_vol_f13 <- scale(imp$data$posteriorcingulate_vol_f13)
imp$data$precentral_vol_f13 <- scale(imp$data$precentral_vol_f13)
imp$data$precuneus_vol_f13 <- scale(imp$data$precuneus_vol_f13)
imp$data$rostralanteriorcingulate_vol_f13 <- scale(imp$data$rostralanteriorcingulate_vol_f13)
imp$data$rostralmiddlefrontal_vol_f13 <- scale(imp$data$rostralmiddlefrontal_vol_f13)
imp$data$superiorfrontal_vol_f13 <- scale(imp$data$superiorfrontal_vol_f13)
imp$data$superiorparietal_vol_f13 <- scale(imp$data$superiorparietal_vol_f13)
imp$data$superiortemporal_vol_f13 <- scale(imp$data$superiortemporal_vol_f13)
imp$data$supramarginal_vol_f13 <- scale(imp$data$supramarginal_vol_f13)
imp$data$frontalpole_vol_f13 <- scale(imp$data$frontalpole_vol_f13)
imp$data$temporalpole_vol_f13 <- scale(imp$data$temporalpole_vol_f13)
imp$data$transversetemporal_vol_f13 <- scale(imp$data$transversetemporal_vol_f13)
imp$data$insula_vol_f13 <- scale(imp$data$insula_vol_f13)

#scale regions F13
imp$data$bankssts_vol <- scale(imp$data$bankssts_vol)
imp$data$caudalanteriorcingulate_vol <- scale(imp$data$caudalanteriorcingulate_vol)
imp$data$caudalmiddlefrontal_vol <- scale(imp$data$caudalmiddlefrontal_vol)
imp$data$cuneus_vol <- scale(imp$data$cuneus_vol)
imp$data$entorhinal_vol <- scale(imp$data$entorhinal_vol)
imp$data$fusiform_vol <- scale(imp$data$fusiform_vol)
imp$data$inferiorparietal_vol <- scale(imp$data$inferiorparietal_vol)
imp$data$inferiortemporal_vol <- scale(imp$data$inferiortemporal_vol)
imp$data$isthmuscingulate_vol <- scale(imp$data$isthmuscingulate_vol)
imp$data$lateraloccipital_vol <- scale(imp$data$lateraloccipital_vol)
imp$data$lateralorbitofrontal_vol <- scale(imp$data$lateralorbitofrontal_vol)
imp$data$lingual_vol <- scale(imp$data$lingual_vol)
imp$data$medialorbitofrontal_vol <- scale(imp$data$medialorbitofrontal_vol)
imp$data$middletemporal_vol <- scale(imp$data$middletemporal_vol)
imp$data$parahippocampal_vol <- scale(imp$data$parahippocampal_vol)
imp$data$paracentral_vol <- scale(imp$data$paracentral_vol)
imp$data$parsopercularis_vol <- scale(imp$data$parsopercularis_vol)
imp$data$parsorbitalis_vol <- scale(imp$data$parsorbitalis_vol)
imp$data$parstriangularis_vol <- scale(imp$data$parstriangularis_vol)
imp$data$pericalcarine_vol <- scale(imp$data$pericalcarine_vol)
imp$data$postcentral_vol <- scale(imp$data$postcentral_vol)
imp$data$posteriorcingulate_vol <- scale(imp$data$posteriorcingulate_vol)
imp$data$precentral_vol <- scale(imp$data$precentral_vol)
imp$data$precuneus_vol <- scale(imp$data$precuneus_vol)
imp$data$rostralanteriorcingulate_vol <- scale(imp$data$rostralanteriorcingulate_vol)
imp$data$rostralmiddlefrontal_vol <- scale(imp$data$rostralmiddlefrontal_vol)
imp$data$superiorfrontal_vol <- scale(imp$data$superiorfrontal_vol)
imp$data$superiorparietal_vol <- scale(imp$data$superiorparietal_vol)
imp$data$superiortemporal_vol <- scale(imp$data$superiortemporal_vol)
imp$data$supramarginal_vol <- scale(imp$data$supramarginal_vol)
imp$data$frontalpole_vol3 <- scale(imp$data$frontalpole_vol)
imp$data$temporalpole_vol <- scale(imp$data$temporalpole_vol)
imp$data$transversetemporal_vol <- scale(imp$data$transversetemporal_vol)
imp$data$insula_vol <- scale(imp$data$insula_vol)


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

summary(com)

#Specify brain structures to loop over
brainstructures <- c("insula_vol", "transversetemporal_vol", "temporalpole_vol", 
                     "frontalpole_vol", "supramarginal_vol", "superiortemporal_vol",
                     "superiorparietal_vol", "superiorfrontal_vol", "rostralmiddlefrontal_vol",
                     "rostralanteriorcingulate_vol", "precuneus_vol", "precentral_vol",
                     "posteriorcingulate_vol", "postcentral_vol", "pericalcarine_vol",
                     "parstriangularis_vol", "parsorbitalis_vol", "parsopercularis_vol", 
                     "paracentral_vol", "parahippocampal_vol", "middletemporal_vol",
                     "medialorbitofrontal_vol", "lingual_vol", "lateralorbitofrontal_vol",
                     "lateraloccipital_vol", "lateraloccipital_vol", "isthmuscingulate_vol",
                     "inferiortemporal_vol", "inferiorparietal_vol", "fusiform_vol",
                     "entorhinal_vol", "cuneus_vol", "caudalmiddlefrontal_vol", 
                     "caudalanteriorcingulate_vol", "bankssts_vol") 

#Create empty dataframes
results_CLPM_2tp_genr_m1_t1w <- data.frame()
fitmeaures_CLPM_2tp_genr_m1_t1w <- data.frame()

#Specify rowcount to keep track of where we are in the loop
rowcount_clpm_2tp <- 1
rowcount_fit <- 1

#Model 1
#T1w
#Specify the model for all brain morphology measures
for(x in brainstructures){
  CLPM_2tp_genr <- paste0(
    '
    # Estimate the lagged effects between the variables
    srs_total_m_13 +', x,"_f13", ' ~ #Dit is de suffix die mijn breinvariabelen hebben, dit kun je veranderen naar je eigen suffix (verder hieronder gehighlight in geel)
    srs_6times18 +', x, 
    
    #Estimate time independent predictors
    ' \n \n srs_6times18 ~ GENDER \n ', 
    x, ' ~ GENDER \n 
    
    #Estimate time dependent predictors
    srs_6times18 ~ agechildGR1076 + dif_age6 \n 
    srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 \n ',
    x, ' ~ agechildGR1076 + dif_age6 \n ',
    x,"_f13", ' ~ AGECHILD_GR1093 + dif_age13 \n 
    
    # Estimate the covariance between variables at the first wave. 
    srs_6times18 ~~ ', x, # Covariance
    
    # Estimate the covariances between the residuals of variables
    ' \n srs_total_m_13 ~~ ', x,"_f13",
    
    # Estimate the (residual) variance of variables of interest
    ' \n srs_6times18 ~~ srs_6times18 \n ', # Variances
    x, ' ~~ ', x, 
    ' \n srs_total_m_13 ~~ srs_total_m_13 \n ', # Residual variances
    x,"_f13", ' ~~ ', x,"_f13"
  )
  
  #Fit the model specified above
  CLPM_2tp_genr_fit <- lavaan.mi(CLPM_2tp_genr, data = list, estimator = 'ML') 
  
  
  
  #Store coefficients of interest
  #Which brain region
  results_CLPM_2tp_genr_m1_t1w[(rowcount_clpm_2tp+2),1] <- x
  #Cross-sectional associations T1
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),2] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[15]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),3] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[15]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),4] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[15]
  #Autoregressive parameters
  #CBCL
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),5] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[1]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),6] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[1]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),7] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[1]
  #MRI
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),8] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[4]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),9] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[4]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),10] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[4]
  #Cross-lagged parameters
  #CBCL -> MRI
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),11] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[3]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),12] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[3]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),13] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[3]
  #MRI -> CBCL
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),14] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[2]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),15] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[2]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),16] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[2]
  #Which measure (actually only needs to be specified once)
  results_CLPM_2tp_genr_m1_t1w[1,c(3,6,9,12,15)] <- c("Cross-sectional", "CBCL", "MRI", "CBCL -> MRI", "MRI -> CBCL")
  results_CLPM_2tp_genr_m1_t1w[2,] <- c("Brain region", "B", "S.E.", "p-value", 
                                        "B", "S.E.", "p-value", "B", "S.E.", "p-value", 
                                        "B", "S.E.", "p-value","B", "S.E.", "p-value")
  
  #Store fit measures
  fitmeaures_CLPM_2tp_genr_m1_t1w[rowcount_fit, 1:5] <- 
    c(x, summary(CLPM_2tp_genr_fit, fit.measures = T, 
                 standardized = T)[[1]][c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")])
  #Adapt rowcounts to make sure results are stored properly
  rowcount_clpm_2tp <- rowcount_clpm_2tp + 1
  rowcount_fit <- rowcount_fit + 1
}

summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$lhs
summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$
summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)[[1]]
str(summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T))

#Store output
setwd("V:/medewerkers/038870 Durkut, M")
write.xlsx(results_CLPM_2tp_genr_m1_t1w, "results_CLPM_2tp_genr_m1_t1w.xlsx", row.names = F)


View(results_CLPM_2tp_genr_m1_t1w)
fitmeaures_CLPM_2tp_genr_m1_t1w



#Specify brain structures to loop over
brainstructures <- c("insula_vol", "transversetemporal_vol", "temporalpole_vol", 
                     "frontalpole_vol", "supramarginal_vol", "superiortemporal_vol",
                     "superiorparietal_vol", "superiorfrontal_vol", "rostralmiddlefrontal_vol",
                     "rostralanteriorcingulate_vol", "precuneus_vol", "precentral_vol",
                     "posteriorcingulate_vol", "postcentral_vol", "pericalcarine_vol",
                     "parstriangularis_vol", "parsorbitalis_vol", "parsopercularis_vol", 
                     "paracentral_vol", "parahippocampal_vol", "middletemporal_vol",
                     "medialorbitofrontal_vol", "lingual_vol", "lateralorbitofrontal_vol",
                     "lateraloccipital_vol", "lateraloccipital_vol", "isthmuscingulate_vol",
                     "inferiortemporal_vol", "inferiorparietal_vol", "fusiform_vol",
                     "entorhinal_vol", "cuneus_vol", "caudalmiddlefrontal_vol", 
                     "caudalanteriorcingulate_vol", "bankssts_vol") 

#Create empty dataframes
results_CLPM_2tp_genr_m1_t1w <- data.frame()
fitmeaures_CLPM_2tp_genr_m1_t1w <- data.frame()

#Specify rowcount to keep track of where we are in the loop
rowcount_clpm_2tp <- 1
rowcount_fit <- 1

#Model 2
#T1w
#Specify the model for all brain morphology measures
for(x in brainstructures){
  CLPM_2tp_genr <- paste0(
    '
    # Estimate the lagged effects between the variables
    srs_total_m_13 +', x,"_f13", ' ~ #Dit is de suffix die mijn breinvariabelen hebben, dit kun je veranderen naar je eigen suffix (verder hieronder gehighlight in geel)
    srs_6times18 +', x, 
    
    #Estimate time independent predictors
    ' \n \n srs_6times18 ~ GENDER + EDUCM5 + mdrink_updated + SMOKE_ALL + eth_wes + eth_nonwes \n ', 
    x, ' ~ GENDER \n 
    
    #Estimate time dependent predictors
    srs_6times18 ~ agechildGR1076 + dif_age6 \n 
    srs_total_m_13 ~ AGECHILD_GR1093 + dif_age13 \n ',
    x, ' ~ agechildGR1076 + dif_age6 \n ',
    x,"_f13", ' ~ AGECHILD_GR1093 + dif_age13 \n 
    
    # Estimate the covariance between variables at the first wave. 
    srs_6times18 ~~ ', x, # Covariance
    
    # Estimate the covariances between the residuals of variables
    ' \n srs_total_m_13 ~~ ', x,"_f13",
    
    # Estimate the (residual) variance of variables of interest
    ' \n srs_6times18 ~~ srs_6times18 \n ', # Variances
    x, ' ~~ ', x, 
    ' \n srs_total_m_13 ~~ srs_total_m_13 \n ', # Residual variances
    x,"_f13", ' ~~ ', x,"_f13"
  )
  
  #Fit the model specified above
  CLPM_2tp_genr_fit <- lavaan.mi(CLPM_2tp_genr, data = list, estimator = 'ML') 
  
  
  
  #Store coefficients of interest
  #Which brain region
  results_CLPM_2tp_genr_m1_t1w[(rowcount_clpm_2tp+2),1] <- x
  #Cross-sectional associations T1
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),2] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[19]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),3] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[19]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),4] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[19]
  #Autoregressive parameters
  #CBCL
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),5] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[1]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),6] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[1]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),7] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[1]
  #MRI
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),8] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[4]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),9] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[4]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),10] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[4]
  #Cross-lagged parameters
  #CBCL -> MRI
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),11] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[3]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),12] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[3]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),13] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[3]
  #MRI -> CBCL
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),14] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$est[2]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),15] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$se[2]
  results_CLPM_2tp_genr_m1_t1w[c((rowcount_clpm_2tp+2)),16] <- 
    summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$pvalue[2]
  #Which measure (actually only needs to be specified once)
  results_CLPM_2tp_genr_m1_t1w[1,c(3,6,9,12,15)] <- c("Cross-sectional", "CBCL", "MRI", "CBCL -> MRI", "MRI -> CBCL")
  results_CLPM_2tp_genr_m1_t1w[2,] <- c("Brain region", "B", "S.E.", "p-value", 
                                        "B", "S.E.", "p-value", "B", "S.E.", "p-value", 
                                        "B", "S.E.", "p-value","B", "S.E.", "p-value")
  
  #Store fit measures
  fitmeaures_CLPM_2tp_genr_m1_t1w[rowcount_fit, 1:5] <- 
    c(x, summary(CLPM_2tp_genr_fit, fit.measures = T, 
                 standardized = T)[[1]][c("cfi.robust", "tli.robust", "rmsea.robust", "srmr")])
  #Adapt rowcounts to make sure results are stored properly
  rowcount_clpm_2tp <- rowcount_clpm_2tp + 1
  rowcount_fit <- rowcount_fit + 1
}

summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$lhs
summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)$
  summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T)[[1]]
str(summary(CLPM_2tp_genr_fit, fit.measures = T, standardized = T))

#Store output
setwd("V:/medewerkers/038870 Durkut, M")
write.xlsx(results_CLPM_2tp_genr_m1_t1w, "results_CLPM_2tp_genr_m1_t1wMODEL2.xlsx", row.names = F)

