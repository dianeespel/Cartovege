
# Title : Hierarchical Clustering (HC)
# Author : Diane ESPEL
# Objectives : Perform Hierarchical Clustering on habitat classification levels based on spectral/topographic data


# ----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required packages -------------------------------------------------------

library(FactoMineR)  # For multivariate data analysis (not directly used here but often coupled with factoextra)
library(factoextra)  # For visualizing clustering and multivariate data
library(dplyr)       # For data manipulation (e.g., filtering, summarizing)
library(ggplot2)     # For plotting
library(stats)      # For statistical functions such as clustering, distance calculations, and other classic statistical methods (e.g., hclust, dist)

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
YearRef="2022" # acquisition year of multispectral imagery
MonthRef="02" # acquisition month of multispectral imagery
ResRef = "50cm"  # spatial resolution of multispectral imagery
maxTypoLevel=4  # Define maximum typology level

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open input learning data
open_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")

# Path to save HC results
save_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")



# Load learning data  -------------------------------------------------------------

FILE1 <- paste0(open_learning_primary_path, "/Learning_plots_", District, "_", Island,"_",Satellite1,"_",YearRef,"_",MonthRef,"_",ResRef,"_ALL_SOURCES_EPSG32739.csv")
learning_data=read.csv(FILE1, sep=";",dec=".",stringsAsFactors=FALSE) # `stringsAsFactors=F` ensures character strings don't import as factors


# Perform Hierarchical Clustering of habitat classes for each typology level ---------------------------------------------

# Loop through typology levels (from 1 to maxTypoLevel)
for (l in seq(1:maxTypoLevel)){
  
  
  print(paste0("Processing typology level: ", l))
  
  # Create output folder for this classification level
  LevelFolder=paste0(save_learning_primary_path,"/","Hab_L",l)
  dir.create(LevelFolder,showWarnings=F) # Avoid warning if folder already exists
  
  
  # Define columns to remove before analysis (ID and spatial coordinates)
  variables_to_remove <- c("ID", "xcoord_m", "ycoord_m","Longitude","Latitude","Date","Source","Surface","geometry")
  print(variables_to_remove)

  # Remove unwanted columns from the dataset
  spectral_df <- learning_data[, !(names(learning_data) %in% variables_to_remove)]
  
  # Rename current classification level column to a common name
  colnames(spectral_df)[colnames(spectral_df) == paste0("Hab_L", l)] <- "hab_col"
  
  # Compute mean and median for each habitat class
  means_df<- aggregate(. ~ hab_col, data = spectral_df, FUN = mean)
  medians_df<- aggregate(. ~ hab_col, data = spectral_df, FUN = median)
  
  # Assign row names using the habitat labels (for clustering)
  rownames(means_df)   <- means_df$hab_col
  rownames(medians_df) <- medians_df$hab_col
  
  # Keep only quantitative variables for clustering (remove label column)
  quantitative_vars_means   <- means_df[, -1]
  quantitative_vars_medians <- medians_df[, -1]
  
  # Perform hierarchical clustering (using Euclidean distance)
  hc_means   <- hclust(dist(quantitative_vars_means,method = "euclidean"))
  hc_medians <- hclust(dist(quantitative_vars_medians,method = "euclidean"))
  
  # Define maximum number of clusters for dendrogram visualization
  n_classes <- nrow(quantitative_vars_means)
  n_classes_max=50
  k_clusters <- min(n_classes_max, n_classes)  # Ensure the number of clusters does not exceed the number of classes
  
  
  # Plot dendrogram (means)
  png(filename = paste0(LevelFolder, "/Means_HC_", District, "_", Island, "_", Satellite1,"_",YearRef,"_",MonthRef,"_",ResRef, "_L", l, "_ALL_SOURCES.png"))
  dendro_plot_means <- fviz_dend(hc_means, cex = 0.5, k = k_clusters, color_labels_by_k = TRUE, rect = TRUE) +
    labs(title = paste("Dendrogram of Means - Level", l))
  print(dendro_plot_means)
  dev.off()
  
  NOMsvg=paste0(LevelFolder,"/","Means_HC_", District, "_", Island, "_", Satellite1,"_",YearRef,"_",MonthRef,"_",ResRef, "_L", l, "_ALL_SOURCES.svg")
  svg(file = NOMsvg)
  print(dendro_plot_means)
  dev.off()
  
  
  # Plot dendrogram (medians)
  png(filename = paste0(LevelFolder , "/Medians_HC_", District, "_", Island, "_", Satellite1,"_",YearRef,"_",MonthRef,"_",ResRef, "_L", l, "_ALL_SOURCES.png"))
  dendro_plot_medians <- fviz_dend(hc_medians, cex = 0.5, k =k_clusters, color_labels_by_k = TRUE, rect = TRUE) +
    labs(title = paste("Dendrogram of Medians - Level", l))
  print(dendro_plot_medians)
  dev.off()
  
  NOMsvg=paste0(LevelFolder,"/","Medians_HC_", District, "_", Island, "_", Satellite1,"_",YearRef,"_",MonthRef,"_",ResRef, "_L", l, "_ALL_SOURCES.svg")
  svg(file = NOMsvg)
  print(dendro_plot_medians)
  dev.off()
  
  
} # End of loop for typology levels
