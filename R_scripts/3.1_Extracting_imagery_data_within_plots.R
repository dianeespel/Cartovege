
# Title : Extraction of pixels data 
# Author : Diane ESPEL
# Objectives : Extract raster data (mean pixel values) within polygons using weighted averaging method

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(terra)    # For raster data manipulation
library(dplyr)    # For data wrangling and manipulation
library(sf)       # For handling vector spatial data (e.g., shapefiles)

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022" # acquisition year of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery


# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")
localscratch=paste0("/scratch/despel/CARTOVEGE/")

# Path to open  input raster stack
open_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")

# Path to open  vector plots
open_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

#  Path to save learning data 
save_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")
  


# Open and prepare data ----------------------------------------------

# Load the total raster stack
print("Loading raster stack")
raster_total=rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1,"_Total_raster_stack_",Year1,"_",Res1,"_cut.TIF")) #SpatRaster
names(raster_total) = c("R", "G", "B", "NIR","NDVI","GRVI","VARI","GCCI","Brightness","BSI","NDWI","Dtm","Slope")

cat("Raster CRS:", crs(raster_total), "\n") # Print raster Coordinate Reference System

# Load the quadrats (learning plots)
print("Loading learning plots")
FILE1=paste0(open_plots_path,"/Quadrats_", District, "_", Island, "_ALL_SOURCES_Polygons_EPSG32739.shp")

quadrats_sf <- st_read(shapefile_path) # Load shapefile as sf object
cat("Quadrats CRS:", st_crs(quadrats_sf)$epsg, "\n") # Print CRS of shapefile

# Convert sf object to terra-compatible SpatVector
quadrats_vect <- vect(quadrats_sf)



# Extract raster data within quadrats ------------------------------------------------

print("Extracting pixel values within each quadrat")

# Perform weighted extraction of raster values using polygon area coverage
# Returns mean values per polygon, accounting for partial pixel contributions
extracted_data <- terra::extract(
  x=raster_total,            # SpatRaster (raster stack) to extract from
  y=quadrats_vect,           # 	SpatVector polygons (quadrats)
  fun = mean,                # Function to apply to pixels (mean)
  weights = TRUE,            # Use area-based weights (partial pixel contributions)
  normalizeWeights = TRUE,   # Normalize weights to sum to 1 per polygon
  df = TRUE,                 # Return as data.frame
  na.rm = TRUE               # Exclude NA values
)


# Combine the extracted raster values with original quadrat attributes
learning_data <- cbind(quadrats_sf, extracted_data[,-1]) # Remove first column (ID) before merging
learning_data <- st_as_sf(learning_data, coords = c("xcoord_m", "ycoord_m"), crs = 32739) # Reconvert to sf object with geometry (coordinates must exist!)


# Saving learning data ----------------------------------------------

# Save complete dataset
all_learning_plots=learning_data
print(paste0("Number of all learning plots: ", nrow(all_learning_plots)))
save(all_learning_plots, file = paste0(save_learning_primary_path,"/Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.Rdata"))
write.table(all_learning_plots,file =paste0(save_learning_primary_path,"/Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv"), sep = ";", dec = ".", row.names = FALSE)

# Save a filtered dataset excluding photo-interpreted plots
true_learning_plots=subset(learning_data$Source!="PHOTO-INTERPRETATION")
print(paste0("Number of field learning plots: ", nrow(true_learning_plots)))
save(true_learning_plots, file = paste0(save_learning_primary_path,"/Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_TRUE_SOURCES_EPSG32739.Rdata"))
write.table(true_learning_plots,file =paste0(save_learning_primary_path,"/Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_TRUE_SOURCES_EPSG32739.csv"), sep = ";", dec = ".", row.names = FALSE)

# Save the entire R session image for reproducibility
save.image(paste0("Extraction_raster_data_",District,"_",Island,"_",Satellite1,"_",Year1,".Rdata"))
