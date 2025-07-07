
# Title : Computing spectral indices
# Author : Diane ESPEL

# Objectives 
#   - Compute spectral indices from MS imagery
#   - Ensure that RGB and NIR bands have the same spatial resolution

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(sp)      # For spatial vector data compatibility
library(terra)   # For raster data processing and spatial analysis
library(stats)   # For basic statistical functions (usually loaded by default)

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"    # acquisition year of multispectral imagery
Month1="02" # acquisition month of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery


# Create functions -------------------------------------------------------

# Function to compute and export several spectral indices from MS bands
SpectralIndices<- function(archipelago,island,red,green,blue,nir,nameSat,year,month,resImage){
  
  # Formulae for spectral indices
  ndvi_raster=(nir-red)/(nir+red) # Normalized difference vegetation index (Pettorelli, 2013)
  grvi_raster=(green-red)/(green+red)  # Green red vegetation index  (Tucker 1979)
  vari_raster=(green-red)/(green+red-blue)  # Visible atmospherically resistant index (Gitelson et al., 2002)
  gcci_raster=green/(green+red+blue)  # Green Chromatic Coordinates index (Richardson et al, 2013) 
  brightness_raster=sqrt(red^2+nir^2) # Brightness index (khan et al 2005)
  bsi_raster=(green+nir)/(green-nir) #  Bare soil index (Lillo-Saavedra et al., 2018)
  ndwi_raster=(green-nir)/(green+nir) # Normalized Difference Water Index  (Gao, 1996)
  
  
  # Recording path for spectral indices
  path_ndvi <- paste(archipelago,"_",island,"_",nameSat, "_",year,"_",month,"_",resImage, "_ndvi_cut.TIF", sep = "") 
  path_grvi <- paste(archipelago,"_",island,"_",nameSat, "_",year, "_",month,"_",resImage, "_grvi_cut.TIF", sep = "") 
  path_vari <- paste(archipelago,"_",island,"_",nameSat, "_",year,"_", month,"_",resImage, "_vari_cut.TIF", sep = "")
  path_gcci <- paste(archipelago,"_",island,"_",nameSat, "_",year,"_", month,"_",resImage, "_gcci_cut.TIF", sep = "")
  path_brightness <- paste(archipelago,"_",island,"_",nameSat, "_",year,"_", month, "_",resImage,"_brightness_cut.TIF", sep = "") 
  path_bsi <- paste(archipelago,"_",island,"_",nameSat, "_",year,"_", month,"_",resImage, "_bsi_cut.TIF", sep = "") 
  path_ndwi <- paste(archipelago,"_",island,"_",nameSat, "_",year, "_",month,"_",resImage,"_ndwi_cut.TIF", sep = "") 
   
  
  # Recording indices
  
  writeRaster(ndvi_raster, filename = path_ndvi, overwrite = TRUE)
  return(ndvi_raster)
  
  writeRaster(grvi_raster, filename = path_grvi, overwrite = TRUE)
  return(grvi_raster)
  
  writeRaster(vari_raster, filename = path_vari, overwrite = TRUE)
  return(vari_raster)
  
  writeRaster(gcci_raster, filename = path_gcci, overwrite = TRUE)
  return(gcci_raster)
  
  writeRaster(brightness_raster, filename = path_brightness, overwrite = TRUE)
  return(brightness_raster)
  
  writeRaster(bsi_raster, filename = path_bsi, overwrite = TRUE)
  return(bsi_raster)
  
  writeRaster(ndwi_raster, filename = path_ndwi, overwrite = TRUE)
  return(ndwi_raster)
  
  
}



# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Path to open  input MS raster data
open_cut_raster_path=paste0(localHOME ,"data/raster/Cut_image")

# Path to save indices rasters
save_cut_raster_path=paste0(localHOME,"data/raster/Cut_image")


# Load input MS raster per band----------------------------------------------------------

print("Loading raster bands")
R <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band3_cut.TIF"))
G <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band2_cut.TIF"))
B <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band1_cut.TIF"))
NIR <- rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_band4_cut.TIF"))

setwd(save_cut_raster_path)
getwd()

# Run SpectralIndices() function ----------------------------------------------------------


print("Spectral indices are computed")
SpectralIndices(archipelago=District,island=Island,red=R,green=G,blue=B,nir=NIR,nameSat=Satellite1,year=Year1,month=Month1,resImage=Res1)


    
