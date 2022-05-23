This README file provides a breakdown for the R Code to run the analyses for the cross-sectional analyses and longitudinal cross-lagged panel models 

Cross-sectional analyses:

The file named 'cs13final.R' contains the code for the cross-sectional analyses. 
* Lines 1-369 contain the code to create dataframes with the necessary variables for the main analysis, sensitivity analysis and non-response analysis. 
* Lines 372-379: non-response analysis. Missing data on covariates are imputed with MICE (lines 392-451). 
* Lines 505-735 contain the code of the regression analyses for the global brain measures, including interaction models and sensitivity analyses. 
* The files 'QDECR_13area.R', 'QDECR_13thickness.R' and 'QDECR_13cs.R' contain the code for the vertex-wise analyses for surface area, cortical thickness and gyrification respectively. 

Longitudinal analyses: 

The file named 'CLPM062021.R' contains the code for the cross-lagged panel models (CLPMs). 
* Lines 1-89 create the longitudinal dataframe with the necessary variables. 
* Lines 150-157 are an explanation on how the clusters (from W2) are extracted for W1 and W3. 
* Lines 160-498 contain the code to prepare the dataset for the CLPMs, including the sensitivity and non-response analyses. 
* Lines 500-507: non-response analysis. Lines 518-558: baseline characteristics. Missing data on covariates are imputed with MICE (lines 561-600). 
* Lines 676-970 contain the code of the CLPM for gyrification (mean and ROI's). 
* Lines 973-1096 contain the code of the CLPM for the gyrification ROI's corrected for mean gyrfication. 
* Lines 1099-1655 contain the code of the CLPM for surface area and cortical thickness (mean and ROI's) and lines 1657-1839 contain the code of the CLPM for the surface area and cortical thickness ROI's corrected for mean surface area and mean cortical thickness respectively. 
* Lines 1941-2972: the CLPM for the global brain measures. 
* The file 'CLPMregionsAtlas.R' contains the code for the  sensitivity analysis in which all ROIs from the Desikan-Killiany atlas were included in the CLPMs. 