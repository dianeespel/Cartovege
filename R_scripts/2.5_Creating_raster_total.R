
# Title : Create a raster total
# Author : Diane ESPEL
# Objectives ## stack all bands into a raster_total

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required packages -------------------------------------------------------

library(terra)      # For handling raster data and spatial operations

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"    # acquisition year of multispectral imagery
Month1="02" # acquisition month of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery
Satellite2="SRTM" # satellite name of DEM
Year2="2012"  # acquisition year of DEM
Res2="30m" # spatial resolution of DEM

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open potential cloud mask
open_mask_path <- paste0(localHOME, "/data/vector/mask")

# Path to open input raster 
open_cut_raster_path=paste0(localHOME,"data/raster/Cut_image")

# Path to save resampled topographic rasters
save_cut_topo_raster_path=paste0(localHOME,"data/raster/Cut_image")

# Path to save raster stack
save_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")


# Load and prepare rasters -------------------------------------------------------------


# Open MS features
print("Loading multispectral bands")
R <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1, "_",Res1,"_band3_cut.TIF"))
G <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band2_cut.TIF"))
B <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band1_cut.TIF"))
NIR <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band4_cut.TIF"))

# Open Spectral indices
print("Loading spectral indices")
NDVI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1, "_",Res1,"_ndvi_cut.TIF"))
GRVI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_grvi_cut.TIF"))
VARI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_vari_cut.TIF"))
GCCI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_gcci_cut.TIF"))
Brightness <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_brightness_cut.TIF"))
BSI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_bsi_cut.TIF"))
NDWI <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_ndwi_cut.TIF"))

# Open Topographic rasters : DTM and slope
print("Loading dtm and slope")
Dtm <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite2,"_",Year2,"_",Res2,"_dtm_cut.TIF")) # Digital Surface model
Slope <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite2,"_",Year2,"_",Res2,"_slope_cut.TIF")) # slope


# Resample topographic rasters to match multispectral resolution
print("Resampling DTM and slope to match the finer multispectral resolution")
if (res(Dtm)[1]>res(R)[1]){
  dtm_res=terra::resample(Dtm,R,method="bilinear")  #Resample topographic features to match R raster resolution
  slope_res=terra::resample(Slope,R,method="bilinear") # Resample topographic features to match R raster  resolution
}else{
  dtm_res=Dtm
  slope_res=Slope
}

rm(Dtm,Slope)# Remove original unresampled rasters

# Correct slope values
print("Correcting invalid slope values")
slope_res_corr <- classify(slope_res, matrix(c(-Inf, 0, 0, 90, Inf, 90), ncol=3, byrow=TRUE))# Reclassify values of slope_res 
slope_res_corr <- clamp(slope_res_corr, lower=0, upper=90,values=FALSE) # limit values to the range [0, 90], removing any outside values
rm(slope_res) #remove useless layers

# save new layers
print("Saving cleaned and resampled DTM and slope rasters")
writeRaster(dtm_res,paste0(save_cut_topo_raster_path,"/",District,"_",Island,"_",Satellite2,"_",Year2,"_",Res1,"_dtm_cut.TIF"),overwrite=T)
writeRaster(slope_res_corr,paste0(save_cut_topo_raster_path,"/",District,"_",Island,"_",Satellite2,"_",Year2,"_",Res1,"_slope_cut.TIF"),overwrite=T)


#  Stack rasters -------------------------------------------------------------

#  Stack MS bands into raster_MSbands 
print("Stacking multispectral bands")
raster_MS=c(R,G,B,NIR) # stack bands
names(raster_MS) = c("R", "G", "B", "NIR") # rename layers in the raster stack
rm(R,G,B,NIR) # remove useless bands
writeRaster(raster_MS,paste0(save_cut_raster_path,"/",District,"_",Island,"_",Satellite1,"_",Year1,"_",Month1,"_",Res1,"_MSbands_cut.TIF"),overwrite=T)
print(raster_MS)

#  Stack all indices into raster_indices 
print("Stacking spectral indices")
raster_indices=c(NDVI,GRVI,VARI,GCCI,Brightness,BSI,NDWI) # stack indices
names(raster_indices) = c("NDVI","GRVI","VARI","GCCI","Brightness","BSI","NDWI") # rename layers in the raster stack
rm(NDVI,GRVI,VARI,GCCI,Brightness,BSI,NDWI) # remove useless layers
writeRaster(raster_indices,paste0(save_cut_raster_path,"/",District,"_",Island,"_",Satellite1,"_",Year1,"_",Month1,"_",Res1,"_Indices_cut.TIF"),overwrite=T)
print(raster_indices)



#  Stack all rasters into raster_total 
print("Creating raster stack: all MS bands + indices + topographic layers")
raster_total<- c(raster_MS,raster_indices,dtm_res,slope_res_corr) #stack all layers
names(raster_total)= c("R", "G", "B", "NIR","NDVI","GRVI","VARI","GCCI","Brightness","BSI","NDWI","Dtm","Slope") # rename layers in the raster stack
rm(raster_MS,raster_indices,dtm_res,slope_res_corr) # remove useless layers

#  Mask clouds and buildings if present
cloud_mask_file <- paste0(open_mask_path, "/",District,"_",Island,"_Clouds_Buildings_",Year1,"_EPSG32739.shp")

if (file.exists(cloud_mask_file)) {
  
  cat("Mask shapefile found, masking out pixels inside the polygon...\n")
  
  # Load the mask shapefile as a vector object
  cloud_mask <- vect(cloud_mask_file)
  
  # Check CRS of raster and vector, reproject vector if different
  raster_crs <- crs(raster_total)
  vector_crs <- crs(cloud_mask)
  
  if (!identical(raster_crs, vector_crs)) {
    cloud_mask <- project(cloud_mask, raster_crs)
  }
  
  # Rasterize mask polygon: pixels inside polygon = 1, outside = 0
  mask_raster <- rasterize(cloud_mask, raster_total, field=1, background=0)
  
  # Invert mask: TRUE outside polygon (0), FALSE inside polygon (1)
  inverted_mask <- mask_raster == 0
  
  # Mask raster_total to remove pixels inside polygon
  raster_total2 <- mask(raster_total, inverted_mask, maskvalues=FALSE)
  
} else {
  cat("Mask shapefile NOT found, skipping masking.\n")
  raster_total2=raster_total
}

writeRaster(raster_total2,paste0(save_cut_raster_path,"/",District,"_",Island,"_",Satellite1,"_Total_raster_stack_",Year1,"_",Res1,"_cut.TIF"))
print(raster_total2)

