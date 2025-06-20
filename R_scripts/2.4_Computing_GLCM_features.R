
# Title : Apply GLCM function
# Author : Diane ESPEL
# Objectives ## Extract textural features (Haralick indices)


#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(GLCMTextures)  # For computing Gray Level Co-occurrence Matrix (GLCM) texture features (Haralick indices)
library(terra)         # For raster data manipulation and spatial analysis

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"    # acquisition year of multispectral imagery
Month1="02" # acquisition month of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localscratch = paste0("your_local_path/")
localscratch=paste0("/scratch/despel/CARTOVEGE/")

# Path to open input index raster 
open_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")

# Path to save texture rasters
save_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")


# Load spectral index ------------------------------------------------------------

print("loading spectral index")
selected_raster="ndvi"
spectral_raster=rast(paste0(open_cut_raster_path,"/",District,"_",Island,"_",Satellite1, "_",Year1,"_",Month1,"_",Res1, "_",selected_raster,"_cut.TIF"))


# Compute textural features----------------------------------------------------------

# Define window size for GLCM
print("Defining calculation window; must be odd and larger than training plot size")

radius=2 # radius of learning plots = 2 m
res_pleiades=res(selected_raster)[1] # # Spatial resolution of raster (assumed 0.5m)
nb_pixels_radius=radius/res_pleiades # Convert radius from meters to number of pixels
npix <- round(nb_pixels_radius + 1) # Number of pixels in window dimension (rounded and incremented)
window_size=c(npix,npix) # Window size as number of rows and columns (must be square)

# computing GLCM matrix
print("6 Haralick indices")

# apply glmc_textures() function
GLCM_textures <- GLCMTextures::glcm_textures(spectral_raster[[1]], # SpatRaster (in case nlyr(spectral_raster)>1,spectral_raster[[1]] ; if not, write only spectral_raster
                                             w = window_size,  # The size of the window used to compute pairs of pixels for the GLCM (Gray Level Co-occurrence Matrix)
                                             n_levels = 16, # The number of grey levels used to discretize the image. A higher number means more detail in texture representation.
                                             quantization = "equal prob", # Method of quantizing the pixel values. "equal prob" assigns equal probability to each grey level during quantization. 
                                             # This simplifies the texture analysis, but other methods like "uniform" or "linear" could also be used for different quantization schemes.
                                             shift = list(c(1, 0), # (1,0) corresponds to a vertical direction (90° shift)
                                                          c(1, 1), # (1,1) corresponds to a diagonal direction (45° shift)
                                                          c(0, 1), # (0,1) corresponds to a horizontal direction (0° shift)
                                                          c(-1, 1) # (-1,1) corresponds to a direction of 135° shift
                                                          ),  #The default textures are calculated using a 45 degree shift (c(1,1)); the default is to return directionally/rotationally invariant textures that are averaged across all 4 directions 
                                             metrics = c("glcm_homogeneity", # Measure of the similarity of pixel pairs in the GLCM; higher values indicate more uniformity in the texture.
                                                         "glcm_contrast",    # Measure of the contrast or difference between the values of neighboring pixels; higher values indicate more variation in texture.
                                                         "glcm_dissimilarity", # Measure of how different two pixel values are; higher values indicate more dissimilarity.
                                                         "glcm_entropy",     # Measure of the randomness or complexity in the texture; higher values indicate more complex or unpredictable patterns.
                                                         "glcm_ASM",         # Angular Second Moment; a measure of texture uniformity.
                                                         "glcm_correlation"  # Measure of the linear dependency between pixel pairs; higher values indicate more correlation in texture.
                                                         )
                                            )
# Rename output layers for clarity
names(GLCM_textures)=c("Homogeneity","Contrast","Dissimilarity","Entropy","Second_moment","Correlation")



#  Save computed Haralick texture layers----------------------------------------------------------


# Extract each texture layer by looping through the names in the GLCM_textures list
for (name_texture in names(GLCM_textures)) {
  
  print(paste0("Saving texture index: ", name_texture))  # Print the texture name that is currently being processed
  texture_layer <- GLCM_textures[[name_texture]]  # Extract the texture layer using the name (from GLCM_textures list)
  
  # Save the texture layer to a file
  FILE2 <- paste0(save_cut_raster_path, "/", District, "_", Island, "_", Satellite1, "_", Year1, "_", Month1, "_", Res1, "_", name_texture, "_cut.TIF")
  writeRaster(texture_layer, FILE2, overwrite = TRUE)  # Write the texture layer to disk as a GeoTIFF file
  
}


print("All texture indices have been successfully saved.")

