
# Title : Create Difference maps
# Author : Diane ESPEL
# Objectives : Compare predicted and observed maps

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required packages -------------------------------------------------------

library(terra) # For raster and vector spatial data handling

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"   # acquisition year of multispectral imagery
Res1="50cm" # spatial resolution of multispectral imagery
maxTypoLevel=4  # Define maximum typology level

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open rasterized observed map
open_NewObsMap_raster_path=paste0(localscratch,"data/raster/Observed_map/NewTypo")

# Path to open predict habitat map 
open_predictions_path=paste0(localscratch,"results/Predictions")

# Path to save map of differences
save_differences_path=paste0(localscratch,"results/Difference_maps")



# Compute binary difference maps -------------------------------------------------------------

# Define list of modelling strategy
type_model_list=c("FLAT","HIERARCHICAL")


# Loop through model types
for (type_model in type_model_list){
  
  #type="FLAT" #debug
  print(paste0("Modeling strategy: ",type_model)) 
  
  # Initialize a summary table to store the ratio of differences
  results_summary <- data.frame(model = character(),
                                Year = character(),
                                level = character(),
                                ratio_diff = numeric())
  

  # Compute difference map for each level of classification
  for (l in seq (1:maxTypoLevel)){
    
    rint(paste0("Processing habitat typology level ", l))
    
    # Load predicted raster map
    print("Opening smoothed predicted habitat raster")
    LevelFolder=paste0(open_predictions_path,"/","Hab_L",l)
    FILE1=paste0(LevelFolder,"/","Smoothed_final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year1, "_", Res1,"_level_", l, ".TIF")
    raster_pred=rast(FILE1)
    print(raster_pred)
    
    # Load rasterized observed map
    print("Opening rasterized observed habitat map")
    FILE2=paste0(open_NewObsMap_raster_path,"/","Corrected_observed_map_NewTypo_",District,"_",Island,"_",Satellite1,"_",Year1,"_", Res1,"_level_",l,"_EPSG32739.TIF")
    raster_obs=rast(FILE2)
    print(raster_obs)
    
    # Align spatial extent if needed
    if (!identical(ext(raster_obs), ext(raster_pred))) {
      print("Rasters do not have the same extent. Resampling observed raster to match prediction.")
      raster_obs <- resample(raster_obs, raster_pred)
    }else{
      print("Rasters have identical spatial extent.")
    }
    
    # Compute difference map (TRUE where values differ, FALSE where they match)
    print("Computing raster of differences")
    diff_raster <- raster_obs != raster_pred
    
    # Release memory
    rm(raster_obs) 
    rm(raster_pred)
    
    # Plot and save the difference map (.png)
    cols <- c("yellow", "red") # Yellow = no difference, red = difference
    legend_labels <- c("Pas de différence", "Différence") #legend
    
    png(filename = paste0(save_differences_path,"/","Difference_map_RF_",type_model,"_model_",District,"_",Island,"_",Year1,"_", Res1,"_level_",l,".png"))
    p=plot(diff_raster, col = cols, legend = TRUE, legend.labels = legend_labels,
           main = "Raster de Différence entre les habitats observés et les habitats prédits")
    legend("topright", legend = legend_labels, fill = cols)
    print(p)
    dev.off()
    
    # Save difference map (.tif)
    NOMtiff=paste0(save_differences_path,"/","Difference_map_RF_",type_model,"_model_",District,"_",Island,"_",Year1,"_", Res1,"_level_",l,".TIF")
    writeRaster(diff_raster,NOMtiff,overwrite=T)
    
    # Calculate the ratio of differing cells
    num_true_cells <- sum(diff_raster[], na.rm = TRUE)
    num_total_cells <- ncell(diff_raster)
    ratio <- num_true_cells / num_total_cells
    cat("Ratio of different cells:", ratio, "\n") # Print the ratio
   
    # Append results to summary table
    results_summary <- rbind(results_summary, data.frame(
      model = paste0(type, "_model"),
      Year = Year1,
      level = paste0("Hab_L", l),
      ratio_diff = ratio))
  
    # Release memory
    rm(diff_raster)
    
    
  } # End of typology level loop
  
  # Save the summary results to CSV ---------------------------------------------------------
  
  write.table(results_summary, 
              file = paste0(save_differences_path, "/Difference_ratios_RF_",type_model,"_model_",District,"_",Island,"_",Year1,".csv"),
              sep = ";", dec=".", row.names = FALSE)
  
} # End of model type loop


