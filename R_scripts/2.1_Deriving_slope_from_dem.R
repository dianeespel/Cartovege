
# Title   : Derive slope from dem
# Author  : Diane ESPEL
# Objectives:  Compute slope from a pre-cut Digital Elevation Model (DEM) raster

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(terra) # For raster manipulation and terrain analysis

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite2="SRTM" # Satellite name of DEM
Year2="2012" # Acquisition year of DEM
Res2="30m"  # Spatial resolution of DEM

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Path to open input dem raster
open_precut_raster_path=paste0(localHOME,"data/raster/Precut_image")

# Path to save slope raster
save_precut_raster_path=paste0(localHOME,"data/raster/Precut_image")


# Load DEM raster ----------------------------------------------------------

print("Loading pre-cut DEM raster")
dtm <- rast(paste0(open_precut_raster_path,"/",District,"_",Island,"_",Satellite2, "_",Year2,"_",Res2, "_dtm_precut.tif"))

# Derive Slope from DEM values  ----------------------------------------------------------

# Compute slope in degrees (default output is % slope if not specified)
print("Slope computing")
slope <- terrain(dtm, v = "slope",unit="degrees", neighbors=8)  # 'v' specifies the terrain variable

# save slope raster
slope_file <- paste0(save_precut_raster_path,"/",District,"_",Island,"_",Satellite2, "_",Year2,"_",Res2, "_slope_precut.tif")
writeRaster(slope, filename = slope_file, format = "GTiff")

