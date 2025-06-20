# Title : Compute surface for predicted final map (loop on years)
# Author : Diane ESPEL
# Objective : Compute diversity and landscape metrics from predicted habitat maps for all available years

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics-----------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required packages -----------------------------------------------------------

library(sf)        # For handling spatial vector data with simple features framework
library(dplyr)     # For data manipulation and transformation (filter, select, mutate, etc.)
library(nngeo)     # For nearest neighbor analysis in spatial data
library(stringr)   # For consistent and convenient string manipulation functions


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
maxTypoLevel=4  # maximum typology level


# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open spatial predictions
open_predictions_path=paste0(localscratch, "results/Predictions")

# Path to save metrics
save_metrics_path=paste0(localscratch,"results/Landscape_metrics")


# Compute habitat metrics for each classification level---------------------------------------


#Define type of models to test
type_model_list=c("FLAT","HIERARCHICAL")


# Loop through model types
for (type_model in type_model_list){
  
  #type="FLAT" #debug
  print(paste0("Modelling strategy: ",type_model)) 

  # Detect available years from shapefile names 
  shp_files = list.files(path = open_predictions_path, pattern = ".shp$", recursive = TRUE, full.names = TRUE)
  
  # Extract years (4-digit numbers) from filenames
  all_years = unique(str_extract(shp_files, "\\d{4}"))
  all_years = all_years[!is.na(all_years)]
  
  # Loop through each available year
  for (Year in all_years) {
    
    print(paste0("Processing year ", Year))
    
    # Loop through each classification level
    for (l in seq(1:maxTypoLevel)) {
      
      print(paste0("Processing typology level ", l))
      
      # Path to folder for this classification level
      newFolder = paste0(open_predictions_path, "/", "Hab_L", l)
      
      # Build expected shapefile name for this year and level
      FILE1 = paste0(newFolder, "/", "Smoothed_simplified_final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year, "_level_", l, ".shp")
      
      # Check if the shapefile exists before proceeding
      if (!file.exists(FILE1)) {
        warning(paste0("Shapefile not found: ", FILE1))
        next
      }
      
      print("Opening final predicted habitat map")
      final_map = st_read(FILE1)
      
      # Rename the habitat column to "Hab"
      ihab = which(colnames(final_map) == paste0("Hab_L", l))
      colnames(final_map)[ihab] = "Hab"
      
      # Save folders
      SaveFolder=paste0(save_metrics_path,"/","Hab_L",l)
      dir.create(SaveFolder, showWarnings = FALSE, recursive = TRUE)
      
      
      # Diversity and heterogeneity metrics ------------------------------------------
      
      print("Calculating total island surface")
      Island_surface = sum(final_map$Surface)
      
      print("Calculating total surface per habitat class")
      surface_table = final_map %>%
        group_by(Hab) %>%
        summarise(Surface_totale = sum(as.numeric(Surface))) %>%
        mutate(Proportion = Surface_totale / sum(Surface_totale))
      
      # Shannon diversity index
      shannon = -sum(surface_table$Proportion * log(surface_table$Proportion))
      
      # Simpson diversity index
      simpson = 1 - sum(surface_table$Proportion^2)
      
      # Pielou's evenness index
      richness = nrow(surface_table)
      pielou = if (richness > 1) shannon / log(richness) else NA
  
      # Stack all heterogeneity indices
      indices_df = data.frame(
        Year_map = Year,
        Typo_level = l,
        Shannon_index = shannon,
        Simpson_index = simpson,
        Pielou_index = pielou
      )
      
      FILE2 = paste0(SaveFolder, "/", "Diversity_indices_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year, "_level_", l, ".csv")
      write.table(indices_df, FILE2, sep = ";", dec = ".", row.names = FALSE)
      
      # Landscape metrics by habitat classes ------------------------------------------
      
      print("Computing landscape metrics by habitat class")
      
      summary_table = final_map %>%
        group_by(Hab) %>%
        mutate(
          nearest_neighbor_distance = if (n() >= 2) {
            nearest_neighbors = st_nn(geometry, geometry, k = 2, returnDist = TRUE)
            distances = sapply(nearest_neighbors$dist, function(x) x[2])
          } else {
            NA
          },
          Nb_total_polygons = n()
        ) %>%
        summarise(
          Nb_patchs = n(),
          Relative_abundance = n() / unique(Nb_total_polygons),
          Mean_patch_size_m2 = mean(Surface),
          Min_patch_size_m2 = min(Surface),
          Max_patch_size_m2 = max(Surface),
          Total_patch_size_m2 = sum(Surface),
          Largest_patch_index = max(Surface) / Island_surface,
          Mean_euclidian_distance = mean(nearest_neighbor_distance, na.rm = TRUE)
        )
      
      FILE3 = paste0(SaveFolder, "/", "Landscape_metrics_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year, "_level_", l, ".csv")
      write.table(summary_table, file = FILE3, sep = ";", dec = ".", row.names = FALSE)
      
    } # End of typology level loop
    
  } # End of year loop
  
} # End of model type loop