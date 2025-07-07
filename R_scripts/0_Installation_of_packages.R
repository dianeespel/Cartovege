# Title: Useful packages to install
# Author : Diane ESPEL
# Objectives : install only necessary packages to run the CARTOVEGE pipeline

#----------------------------------------------------------------------------


# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # this empties your R environment, restart from 0
graphics.off() # this empties your R graphs, restart from 0


#  Packages installation  ----------------------------------------------------------------------------

# Install required packages, one per line
install.packages("ade4")
install.packages("caret")
install.packages("corrplot")
install.packages("data.table")
install.packages("dplyr")
install.packages("e1071")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("fs")
install.packages("ggplot2")
install.packages("GLCMTextures", repos="https://cloud.r-project.org") # need specific environment #not in conda
install.packages("igraph")
install.packages("landscapemetrics")
install.packages("nngeo")
install.packages("polylabelr")
install.packages("pROC")
install.packages("purrr")
install.packages("randomForest")
install.packages("RColorBrewer")
install.packages("readr")
install.packages("resample")
install.packages("reshape2")
install.packages("Rcpp")
install.packages("rlang")
install.packages("rmapshaper")
install.packages("RStoolbox")  # need specific environment (terra >)install.packages("sf")
install.packages("sp")
install.packages("spatialEco")
install.packages("stringr")
install.packages("terra")
install.packages("tidyr")
install.packages("viridis")

