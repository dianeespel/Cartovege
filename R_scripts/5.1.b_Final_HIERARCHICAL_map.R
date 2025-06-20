
# Title : Building Final map (Loop on years)
# Author: Diane ESPEL
# Objectives : Apply Final model on raster final stacks for all years

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics -----------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required libraries ------------------------------------------------------------

library(terra)        # For handling spatial raster data and geospatial analysis
library(ggplot2)      # For creating advanced and customizable data visualizations
library(randomForest) # For building Random Forest models for classification/regression
library(viridis)      # Provides color palettes optimized for perceptual uniformity, useful in plots/maps
library(sf)           # For handling spatial vector data (simple features) in R
library(dplyr)        # For efficient data manipulation (filter, select, mutate, etc.)
library(purrr)        # For functional programming tools, simplifies working with lists and iteration
library(rmapshaper)   # For simplifying and processing spatial vector data (e.g. ms_simplify for reducing complexity)
library(stats)        # Base R stats package; provides functions like `focal` for neighborhood operations on rasters

# Define global variables -------------------------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Res1="50cm" # spatial resolution of multispectral imagery
maxTypoLevel=4  # Define maximum typology level
type_model="HIERARCHICAL" # modelling strategy (FLAT or HIERARCHICAL)

# Set working directories ------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open Final model 
open_final_model_path=paste0(localscratch, "results/Model/Final_model")

# Path to open raster stack
open_cut_raster_path=paste0(localscratch, "data/raster/Cut_image/")

# Path to save results
save_predictions_path=paste0(localscratch, "results/Predictions")


# Predict habitat classes -----------------------------------------------------

# Define a list of all raster files
raster_files <- list.files(open_cut_raster_path, pattern = paste0(District, "_", Island, "_Final_raster_stack_.*_cut\\.TIF$"), full.names = TRUE)


for (l in seq_len(maxTypoLevel)) {
    
    print(paste0("Working with habitat classification level: ", l))
    
    # Define specific folder for model path
    ModelFolder=paste0(open_final_model_path, "/","Hab_L", l)
    set.seed(72143 * l)
    
    # Load final model
    FILE2=paste0(ModelFolder,"/","Final_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_level_", l, ".rds")
    FinalModel <- readRDS(FILE2)

    # Loop on each raster/year ------------------------------------------------
    
    for (raster_file in raster_files) {
      
      message("File treatment: ", basename(raster_file))
      
      # Extract Year from file name
      file_base <- basename(raster_file)
      parts <- strsplit(file_base, "_")[[1]]
      Year <- parts[5]
     
      
      # Load raster stack
      message("Loading raster stack : ", file_base)
      
      raster_sat <- terra::rast(raster_file)
      names(raster_sat) <- c("G", "NIR","GRVI","Greenness","BSI","NDWI","DTM")
    
      if (l == 1) {
        raster_final=raster_sat
        names(raster_final) <- names(raster_sat)
      }else{
        # Load also Hab_L-i predicted raster(s)
        if(l==2){
          raster_L1 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-1,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l-1, ".TIF"))
          raster_final=c(raster_L1,raster_sat)

        }
        if(l==3){
          raster_L1 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-2,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l-2, ".TIF"))
          raster_L2 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-1,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year, "_", Res1,"_level_", l-1, ".TIF"))
          raster_final=c(raster_L1,raster_L2, raster_sat)
        }
        if(l==4){
          raster_L1 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-3,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l-3, ".TIF"))
          raster_L2 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-2,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l-2, ".TIF"))
          raster_L3 <- terra::rast(paste0(save_predictions_path,"/","Hab_L",l-1,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l-1, ".TIF"))
          raster_final=c(raster_L1,raster_L2,raster_L3, raster_sat)
        } 
        
        hab_names <- paste0("Hab_L", 1:(l-1))
        sat_names <- names(raster_sat)
        names(raster_final) <- c(hab_names, sat_names)
      }
     
      
      # Prediction on raster stack
      raster_pred <- terra::predict(raster_final, FinalModel)
      
      # Define specific folder for spatial predictions
      MapFolder=paste0(save_predictions_path,"/","Hab_L",l)
      dir.create(MapFolder, showWarnings = FALSE, recursive = TRUE)
    
      # Save spatial predictions (.tif)
      NOMtiff=paste0(MapFolder,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year, "_", Res1,"_level_", l, ".TIF")
      writeRaster(raster_pred, NOMtiff, overwrite = TRUE)
      
      # Save spatial predictions (.png)
      NOMpng=paste0(MapFolder,"/","Final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res1, "_level_", l, ".png")
      png(file = NOMpng, width = 1000, height = 1000)
      plot(raster_pred, col = viridis(length(unique(values(raster_pred)))), 
           main = paste0("Carte finale des habitats niveau ", l), 
           axes = FALSE, cex.lab = 1.5)
      dev.off()
      
      # Apply modal filter with a square window of size (2*window_size + 1)
      resolution <- res(raster_pred) # Compute resolution of the raster
      radius <- 10 # Define the radius of the smoothing window (in map units)
      window_size <- radius / resolution # Calculate window size in pixels
      window_size <- ceiling(window_size) # Round up to ensure window fully covers the radius
      window_size <- ifelse(window_size %% 2 == 0, window_size + 1, window_size) # Ensure window size is odd (required for a centered focal filter)
      filtered_raster_pred <- focal(raster_pred, 
                                    w = matrix(1, nrow = window_size[1]*2+1, 
                                               ncol = window_size[2]*2+1), fun = modal) # apply smooth filter
      rm(raster_pred) # free memory
      
      # Save smooth map (.tif)
      NOMtiff2=paste0(MapFolder,"/","Smoothed_final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res, "_level_", l, ".TIF")
      writeRaster(filtered_raster_pred, NOMtiff2, overwrite = TRUE)
      
      # Convert raster to vector
      vector_map <- terra::as.polygons(filtered_raster_pred, fun = function(x) { x > 0 })
      vector_layer <- terra::project(vector_map, "EPSG:32739")
      rm(filtered_raster_pred)
      
      # Simplify geometry
      vector_layer_sf <- st_as_sf(vector_layer)
      vector_layer_sf_simplified <- ms_simplify(vector_layer_sf, keep = 1, method = "dp", explode = TRUE, drop_null_geometries = TRUE)
      
      # Add surface and perimeter
      vector_layer_sf_simplified$Surface <- st_area(vector_layer_sf_simplified)
      vector_layer_sf_simplified$Perimeter <- st_length(st_boundary(vector_layer_sf_simplified))
      
      # Save shapefile
      FILE3=paste0(MapFolder,"/","Smoothed_simplified_final_map_RF_",type_model,"_model_", District, "_", Island, "_", Satellite1, "_", Year,"_", Res, "_level_", l, ".shp")
      st_write(vector_layer_sf_simplified, FILE3, driver = 'ESRI Shapefile', append = FALSE)
      
      # Cleanup
      rm(vector_map, vector_layer, vector_layer_sf, vector_layer_sf_simplified)
      
    } # End of raster files loop
    
} # End of typology level loop
