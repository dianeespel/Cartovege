
# Title   : Spectral Separability of Habitat Classes
# Author  : Diane ESPEL
# Objectives : Analyze and regroup habitat classes based on spectral separability

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 


# Load required packages -------------------------------------------------------

library(spatialEco)    # For spectral.separability() function
library(dplyr)         # For Data manipulation
library(ggplot2)       # For Plotting
library(reshape2)      # For Matrix reshaping for ggplot
library(RColorBrewer)  # For Color palettes
library(igraph)        # For Graph-based grouping of classes
library(rlang)         # For tidy evaluation (sym() function)

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"    # acquisition year of multispectral imagery
maxTypoLevel=4  # Define maximum typology level

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open input learning data
open_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")

# Path to save new typology results
save_learning_new_path=paste0(localscratch,"data/Learning_data/NewTypo")


# Load learning data  -------------------------------------------------------------

FILE1 <- paste0(open_learning_primary_path, "/Selected_learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv")
learning_data=read.csv(FILE1, sep=";",dec=".",stringsAsFactors=FALSE) # `stringsAsFactors=F` ensures character strings don't import as factors

# Compute spectral separability -------------------------------------------------------------


#Define threshold for minimum number of samples required per class
min_samples_class=5

# Loop over each typology level
for (l in seq(1:maxTypoLevel)) {
  
  cat("Processing class level:", l, "\n")
  
  # Create output folder for current level
  LevelFolder <- paste0(open_learning_primary_path, "/", "Hab_L",l)
  dir.create(LevelFolder, showWarnings = FALSE)
  
  
  # Remove low frequency class -------------------------------------------------------------
  
  # Filter out classes with fewer samples than threshold
  class_col=paste0("Hab_L",l)
  class_counts <- table(learning_data[, class_col])
  keep_classes <- names(class_counts[class_counts >= min_samples_class,])
  Filtered_learning_data <- dplyr::filter(learning_data, !!sym(class_col) %in% keep_classes)
  
  
  # Select spectral features: from "R" column to the end, keeping only numeric columns
  spectral_vars <- Filtered_learning_data %>%
    select(which(colnames(.) == "R"):ncol(.)) %>%
    select_if(is.numeric)  # Keep only numeric columns
  
  
  # Extract class labels as factor
  classes <- as.factor(Filtered_learning_data[[class_col]])
  
  # Multivariate spectral separability -------------------------------------------------------------
  
  # Compute multivariate separability using Jeffries-Matusita distance
  sep_multi <- spectral.separability(spectral_vars, classes, jeffries.matusita = TRUE)
  print(sep_multi)
  
  # Convert separability matrix to long format  keeping upper triangle only (to avoid duplicates)
  sep_multi_mep <- melt(sep_multi)
  colnames(sep_multi_mep) <- c("Class1", "Class2", "Separability")
  sep_multi_mep <- sep_multi_mep[as.character(sep_multi_mep$Class1) < as.character(sep_multi_mep$Class2), ]
  print(sep_multi_mep)
  
  # Save separability matrix as CSV 
  FILE2 <- paste0(LevelFolder, "/Multibands_seperability_", District, "_", Island, "_", Satellite1, "_", Year1, "_level_", l, "_ALL_SOURCES_EPSG32739.csv")
  write.table(sep_multi_mep, FILE2, sep = ";", dec = ".", row.names = FALSE)
  
  # Plot multiband spectral separability heatmap 
  png_name <- paste0(LevelFolder, "/Multibands_seperability_", District, "_", Island, "_", Satellite1, "_", Year1, "_level_", l, "_ALL_SOURCES_EPSG32739.png")
  png(file = png_name, width = 1000, height = 600)
  p_multi <- ggplot(sep_multi_mep, aes(x = Class1, y = Class2, fill = Separability)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = brewer.pal(9, "YlGnBu"), name = "Separability") +
    theme_minimal() +
    labs(title = paste("Spectral Separability (Multiband) -", class_col),
         x = "Class", y = "Class") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_multi)
  dev.off()
  
  svg_name <- paste0(LevelFolder, "/Multibands_seperability_", District, "_", Island, "_", Satellite1, "_", Year1, "_level_", l, "_ALL_SOURCES_EPSG32739.svg")
  svg(file = svg_name)
  print(p_multi)
  dev.off()
  

  # Group classes with low separability (JM < threshold)-------------------------------------------------------------------------
 
  # Define a threshold below which classes are not considered separable
  threshold_sep <- 1.2  # You can change this value
  
  # Find class pairs below threshold
  low_sep <- sep_multi_mep %>% filter(Separability < threshold_sep)
  
  # Initialize fusion_lookup with identity (each class maps to itself)
  unique_classes <- unique(Filtered_learning_data[[class_col]])
  fusion_lookup <- data.frame(original = unique_classes, new_class = unique_classes, stringsAsFactors = FALSE)
  
  # If low separability pairs exist, create groups by connectivity
  if (nrow(low_sep) > 0) {
    g <- graph_from_data_frame(low_sep[, c("Class1", "Class2")], directed = FALSE)
    comps <- components(g)
    fusion_map <- data.frame(
      original = names(comps$membership),
      group_id = comps$membership,
      stringsAsFactors = FALSE
    )
    fusion_map <- fusion_map %>%
      group_by(group_id) %>%
      mutate(new_class = paste0(sort(unique(original)), collapse = "")) %>%
      ungroup()
    
    # Update fusion_lookup for classes involved in regrouping
    for (i in 1:nrow(fusion_map)) {
      fusion_lookup$new_class[fusion_lookup$original == fusion_map$original[i]] <- fusion_map$new_class[i]
    }
  }
  
  # Create new corrected class column with regrouped classes
  col_corr_name <- paste0(class_col, "_corr")
  Filtered_learning_data[[col_corr_name]] <- fusion_lookup$new_class[match(Filtered_learning_data[[class_col]], fusion_lookup$original)]
  
  # Univariate spectral separability (per band)-------------------------------------------------------------------------

  sep_by_band <- lapply(1:ncol(spectral_vars), function(i) {
    spectral.separability(spectral_vars[, i, drop = FALSE], classes, jeffries.matusita = TRUE)
  })
  names(sep_by_band) <- colnames(spectral_vars)
  
  # Plot heatmaps for each band separately
  for (i in 1:length(sep_by_band)) {
    sep_band_melt <- melt(sep_by_band[[i]])
    colnames(sep_band_melt) <- c("Class1", "Class2", "Separability")
    
    png_name <- paste0(LevelFolder, "/", names(sep_by_band)[i], "_band_separability_", District, "_", Island, "_", Satellite1, "_", Year1, "_level_", l, "_ALL_SOURCES_EPSG32739.png")
    png(file = png_name, width = 1000, height = 600)
    p_band <- ggplot(sep_band_melt, aes(x = Class1, y = Class2, fill = Separability)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), name = "Separability") +
      theme_minimal() +
      labs(title = paste("Band:", names(sep_by_band)[i], "-", class_col),
           x = "Class", y = "Class") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p_band)
    dev.off()
    
    svg_name <- paste0(LevelFolder, "/", names(sep_by_band)[i], "_band_separability_", District, "_", Island, "_", Satellite1, "_", Year1, "_level_", l, "_ALL_SOURCES_EPSG32739.svg")
    svg(file = svg_name)
    print(p_band)
    dev.off()
  }
}


# Export corrected dataset with regrouped classes-------------------------------------------------------------------------

learning_data_corrected <- Filtered_learning_data %>%
  select(everything(), matches("_corr$"))

FILE3= paste0(save_learning_new_path, "/","Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv")
write.table(learning_data_corrected,file =FILE3,sep=";",dec=".",row.names = FALSE)