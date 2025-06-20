# Title : Pansharpening multi-year
# Author : Diane ESPEL
# Objectives : Perform pansharpening on PAN + XS images per year and per month

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics -------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required packages -------------------------------------------------------

library(sp)                   # Classes and methods for spatial data
library(terra)                # For raster data handling
library(stringr)              # For string manipulation (regex, etc.)
library(RStoolbox)            # Includes panSharpen() function for image fusion

# Create functions ---------------------------------------------------------

# Function to normalize raster values between 0 and 1
normalize <- function(x) {
  (x - global(x, "min", na.rm = TRUE)[1]) / 
    (global(x, "max", na.rm = TRUE)[1] - global(x, "min", na.rm = TRUE)[1])
}


# Define global variables ---------------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # Name of satellite used for multispectral imagery
Res1 = "50cm"  # Spatial resolution of multispectral imagery



# Set working directory ----------------------------------------------------

# Base local path (customize to your local environment)
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")
#localHOME = paste0("your_local_path/")

# Path to open input MS and panchromatic imagery
open_precut_raster_path = paste0(localHOME, "data/raster/Precut_image")

# Path to save pansharpened imagery
save_precut_raster_path = paste0(localHOME, "data/raster/Precut_image")


#  Load information about available PAN and XS image files -------------------------

# Get list of all PAN and XS image files
pan_files <- list.files(path = open_precut_raster_path , pattern = "_PAN_precut.tif$", recursive = TRUE, full.names = TRUE)
xs_files  <- list.files(path = open_precut_raster_path , pattern = "_XS_precut.tif$", recursive = TRUE, full.names = TRUE)

# Extract year and month from PAN filenames
pan_info <- data.frame(
  file = pan_files,
  year = str_extract(pan_files, "\\d{4}"),
  month = str_extract(pan_files, "_\\d{4}_(\\d{2})_PAN") %>% str_remove_all("_\\d{4}_|_PAN")
)

# Loop to pansharpen each PAN-XS image pair ------------------------------------------------

for (i in 1:nrow(pan_info)) {

  Year <- pan_info$year[i]
  Month <- pan_info$month[i]
  
  print(paste0("Processing Year ", Year, ", Month ", Month))
  
  # Get file paths for current PAN and corresponding XS image
  pan_file <- pan_info$file[i]
  xs_file  <- xs_files[grepl(paste0(Year, "_", Month, "_XS_precut.tif"), xs_files)]
  
  if (length(xs_file) == 0) {
    warning(paste("No XS file found for Year", Year, "Month", Month))
    next # Skip if no matching XS image
  }
  
  # Load raster images
  PAN <- rast(pan_file)
  XS  <- rast(xs_file)
  
  # Extract min/max values for both rasters
  pan_range <- global(PAN, c("min", "max"), na.rm = TRUE)
  xs_range  <- global(XS,  c("min", "max"), na.rm = TRUE)
  
  # Check if PAN and XS have similar value ranges (within 1e-3 tolerance)
  same_range <- all(abs(pan_range - xs_range) < 1e-3)
  
  # Normalize if ranges differ
  if (!same_range) {
    message("PAN and XS are not on the same scale — applying normalization to [0,1]")
    
    PAN <- normalize(PAN)
    XS  <- normalize(XS)
  }
  
  # Assign RGB channels (Pleiades = BGRN => XS[[1]] = B, [[2]] = G, [[3]] = R)
  B   <- XS[[1]] # blue band
  G   <- XS[[2]] # green band
  R   <- XS[[3]]  # red band
  
  # Perform Brovey pansharpening (RStoolbox)
  Pansharpened_raster <- panSharpen(img = XS, pan = PAN, r = R, g = G, b = B, 
                                    method = "brovey") # Ensure images are on the same scale (e.g. 0:1, or 0:255) 
   
  # Save output pansharpened image
  NOMraster <- paste0(save_precut_raster_path , "/", District, "_", Island, "_", Satellite1, "_", Year, "_", Month, "_", Res1, "_PMS_precut.tif") # Define output file name
  writeRaster(Pansharpened_raster, NOMraster, overwrite = TRUE) # Save the pansharpened raster
  
}
