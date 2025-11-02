
# Title : Apply Island masking function
# Author : Diane ESPEL
# Objectives #

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required packages -------------------------------------------------------

library(sp)      # For spatial data compatibility
library(sf)      # For reading and manipulating shapefiles
library(terra)   # For raster and spatial operations

# Create functions------------------------------------------------------


# Function to check if two rasters have the same resolution 
is_Same_Resolution <- function(band1, band2){
  # Extract the resolution (pixel size) of each raster
  res_band1 <- res(band1)
  res_band2 <- res(band2)
  # Compare the resolutions and return TRUE if they are equal, otherwise FALSE
  return(all.equal(res_band1, res_band2) == TRUE)
}



# Function to mask raster based on polygon and handle MS-specific issues
Raster_Masking <- function(archipelago, island, vector_mask, raster, nameSat, year, month = NULL, resolution, raster_type, save_path){
  
  # Reproject vector if CRS differ
  if (!crs(raster) == crs(vector_mask)) {
    vector_mask <- project(vector_mask, crs(raster))
  }
  
  num_bands <- nlyr(raster)
  
  if (raster_type == "PMS") {
    
    # Harmonize resolution if needed
    res_ok <- is_Same_Resolution(raster[[1]], raster[[num_bands]])
    if (!res_ok) {
      factor <- res(raster[[1]])[1] / res(raster[[num_bands]])[1]
      raster[[num_bands]] <- aggregate(raster[[num_bands]], fact = factor, fun = mean)
    }
    
    # Mask each band individually and save
    for (i in 1:num_bands) {
      path_band <- file.path(save_path, paste0(archipelago, "_", island, "_", nameSat, "_", year, "_", month, "_", resolution, "_band", i, "_cut.TIF"))
      mask(raster[[i]], vector_mask, filename = path_band, overwrite = TRUE)
    }
    
  } else {
    # For slope or DTM
    path_raster <- file.path(save_path, paste0(archipelago, "_", island, "_", nameSat, "_", year, "_", resolution, "_", raster_type, "_cut.TIF"))
    mask(raster, vector_mask, filename = path_raster, overwrite = TRUE)
  }
}


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery
Satellite2='SRTM'  # satellite name of DEM
Year2="2012"  # acquisition year of DEM
Res2="30m" # spatial resolution of DEM



# Set working directory -------------------------------------------------------------

# Define local root directory
localHOME = paste0("D:/")
#localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE_2/")

# path where to open vector data
open_mask_path=paste0(localHOME,"data/vector/mask")

# path where to open raster data
open_precut_raster_path=paste0(localHOME,"data/raster/Precut_image")

# path where to save your results
save_cut_raster_path=paste0(localHOME,"data/raster/Cut_image")


# Load rasters and mask  ----------------------------------------------------------

all_years <- c("2025","2024","2023","2022","2021","2020","2015","2011")
all_months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

# Load ROI polygon shapefile
print("Loading island ROI shapefile")
ROI_mask <- st_read(dsn = paste0(open_mask_path, "/", District, "_", Island, "_POLY_", Year1, "_EPSG32743.shp"))
ROI_mask<- vect(ROI_mask) # convert to SpatVector

print("Loading slope raster")
raster_slope <- rast(paste0(open_precut_raster_path, "/", District, "_", Island, "_", Satellite2, "_", Year2, "_", Res2, "_slope_precut.tif"))

print("Loading DTM raster")
raster_DTM <- rast(paste0(open_precut_raster_path, "/", District, "_", Island, "_", Satellite2, "_", Year2, "_", Res2, "_dtm_precut.tif"))

# Apply RasterMasking functions on MS rasters ----------------------------------------------------------

for (Year1 in all_years) {
  
  print(paste0("Year: ", Year1))
  
  for (Month1 in all_months) {
    print(paste0("Month: ", Month1))
    
    raster_path <- file.path(open_precut_raster_path, paste0(District, "_", Island, "_", Satellite1, "_", Year1, "_", Month1, "_", Res1, "_PMS_precut.tif"))
    
    if (file.exists(raster_path)) {
      
      print("Loading MS raster")
      raster_MS <- rast(raster_path)
      
      print("Masking MS raster using ROI polygon")
      Raster_Masking(archipelago = District, island = Island, vector_mask = ROI_mask, raster = raster_MS, nameSat = Satellite1, year = Year1, month = Month1, resolution = Res1, raster_type = "PMS",save_path = save_cut_raster_path)
    } else {
      message("Raster not found for Year ", Year1, " Month ", Month1, " -> skipping.")
    }
  }
}



# Apply RasterMasking functions on DEM rasters ----------------------------------------------------------

# Mask slope raster
print("Masking slope raster using ROI polygon")
Raster_Masking(archipelago = District, island = Island, vector_mask = ROI_mask, raster = raster_slope, nameSat = Satellite2, year = Year2, resolution = Res2, raster_type = "slope",save_path = save_cut_raster_path)

# Mask DTM raster
print("Masking DTM raster using ROI polygon")
Raster_Masking(archipelago = District, island = Island, vector_mask = ROI_mask, raster = raster_DTM, nameSat = Satellite2, year = Year2, resolution = Res2, raster_type = "dtm",save_path = save_cut_raster_path)
