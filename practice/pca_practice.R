## Practice Script for Some PCA
##
## Grace Tiegs 6-3-2025
##
## https://www.datacamp.com/tutorial/pca-analysis-r
# ############## #
# ############## #


## Install functions first
install.packages("corrr", "ggcorrplot", "FactoMineR")

## Load
library('corrr', 'ggcorrplot', 'FactoMineR')


## Overview the data -----------------------------------------------------------

## Check for null values -------------------------------------------------------

## Normalize the data ----------------------------------------------------------
# only numerical values
# data_normalized <- scale(numerical_data)

## Applying PCA ----------------------------------------------------------------
# data.pca <- princomp(data_normalized) ## computes the PCA
# summary(data.pca) ## show the result

