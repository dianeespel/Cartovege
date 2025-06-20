# Title : Create a final raster stack (one or multiple years)
# Author : Diane ESPEL
# Objectives: To load and filter a raster stack, keeping only the relevant layers, 
# and save the results in both R data and TIF formats.


#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required packages -------------------------------------------------------

library(terra)            # For raster and spatial operations
library(dplyr)            # For data manipulation
library(stringr)          # For regular expressions and string operations


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022" # acquisition year of multispectral imagery
Month1="02"  # acquisition month of multispectral imagery
Res1 = "50cm"  # spatial resolution of multispectral imagery
Satellite2="SRTM" # satellite name of DEM
Year2="2012" # acquisition year of DEM
Res2 = "30m"  # spatial resolution of DEM


# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open input results from PCA
open_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")

# Path to open input rasters layers or stack
open_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")

# Path to save the final raster stack
save_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")



# Open selected variables data -------------------------------------------------------------

# Load selected variables used for model training
FILE1 <- paste0(open_learning_primary_path, "/Selected_learning_plots_", District, "_", Island, "_", Satellite1, "_", Year1, "_ALL_SOURCES_EPSG32739.csv")
learning_data <- read.csv(FILE1, sep=";", dec=".", stringsAsFactors=FALSE)

# Drop habitat classes and coordinate/id columns to keep only predictor variables
variable_df <- learning_data %>%
  select(-matches("^Hab_L[1-4]$"), -id, -xcoord_m, -ycoord_m)

# Extract variable names
list_variables <- names(variable_df)
cat("Selected variables:", paste(list_variables, collapse=", "), "\n")


# Stacking Loop over years -------------------------------------------------------------

# Identify available years for Satellite1 rasters based on file names
all_satellite1_files <- list.files(open_cut_raster_path, pattern = paste0(District, "_", Island, "_", Satellite1, "_\\d{4}_.+_", Res1, "_.*\\.TIF$"))
available_satellite1_years <- sort(unique(str_extract(all_satellite1_files, "\\d{4}")))
available_satellite1_years <- available_satellite1_years[!is.na(available_satellite1_years)]
cat("Detected years:", paste(available_satellite1_years, collapse = ", "), "\n")


# Loop through each available year to create raster stacks
for (Year in available_satellite1_years) {
  
  # Special treatment for Year1 (reference year)
  if (Year == Year1) {
    
    print("The raster stack will be used for model construction")
    
    cat("Processing Year:", Year, "\n")
    
    # Load full raster stack for this year and assign layer names
    raster_total <- rast(paste0(open_cut_raster_path, "/", District, "_", Island, "_", Satellite1, "_Total_raster_stack_", Year, "_", Res1, "_cut.TIF"))
    expected_names <- c("R", "G", "B", "NIR", "NDVI", "GRVI", "VARI", "GCCI", "Brightness", "BSI", "NDWI", "Dtm", "Slope")
    
    # Only rename layers if the number of layers matches
    if (nlayers(raster_total) == length(expected_names)) {
      names(raster_total) <- expected_names
    } else {
      warning("Unexpected number of layers in total raster stack")
    }
    
    # Keep only selected layers used in modeling
    raster_total_filtered <- raster_total[[list_variables]]
    
    # Save filtered raster as RData and GeoTIFF
    save(raster_total_filtered, file=paste0(save_cut_raster_path, "/", District, "_", Island, "_Final_raster_stack_", Year, "_", Res1, "_cut.Rdata"))
    writeRaster(raster_total_filtered, paste0(save_cut_raster_path, "/", District, "_", Island, "_Final_raster_stack_", Year, "_", Res1, "_cut.TIF"), overwrite=TRUE)
    
  } else { 
    
    print("The raster stack will be used for temporal monitoring")
    
    # For other years, reconstruct stack layer by layer
    cat("\nProcessing year:", Year, "\n")
    
    raster_list <- list()  # Initialize list of rasters
    
    for (var in list_variables) {
      
      # Search for the raster file in Satellite1 (main)
      pattern_year  <- paste0(District, "_", Island, "_", Satellite1, "_", Year, "_", Res1, "_", var, "_cut\\.TIF$")
      file_year     <- list.files(open_cut_raster_path, pattern = pattern_year, full.names = TRUE)
      
      # If not found, search in Satellite2 (fallback)
      pattern_year2 <- paste0(District, "_", Island, "_", Satellite2, "_", Year2, "_", Res2, "_", var, "_cut\\.TIF$")
      file_year2    <- list.files(open_cut_raster_path, pattern = pattern_year2, full.names = TRUE)
      
      # If found in Satellite1
      if (length(file_year) > 0) {
        raster_ref <- rast(file_year[1])
        raster_list[[var]] <- raster_ref
        
        # If only found in Satellite2 (SRTM for example)
      } else if (length(file_year2) > 0) {
        raster_year2 <- rast(file_year2[1])
        
        # If a reference raster is already loaded, use it for resampling
        if (exists("raster_ref")) {
          if (res(raster_year2)[1] > res(raster_ref)[1]) {
            cat("Resampling", var, "from", Satellite2, "to match higher resolution\n")
            raster_resampled <- resample(raster_year2, raster_ref)  # Match resolution
            raster_list[[var]] <- raster_resampled
          } else {
            raster_list[[var]] <- raster_year2
          }
        } else {
          warning(paste("Cannot resample", var, ": no reference raster defined."))
          raster_list[[var]] <- raster_year2
        }
        
      } else {
        warning(paste("No raster found for variable", var, "in", Year, "or fallback year", Year2))
      }
      
    } # end variable loop
    
    # If at least one variable was loaded, stack and save
    if (length(raster_list) > 0) {
      raster_stack <- rast(raster_list)
      
      # Save final stack for this year
      save(raster_stack, file=paste0(save_cut_raster_path, "/", District, "_", Island, "_Final_raster_stack_", Year, "_", Res1, "_cut.Rdata"))
      writeRaster(raster_stack, paste0(save_cut_raster_path, "/", District, "_", Island, "_Final_raster_stack_", Year, "_", Res1, "_cut.TIF"), overwrite=TRUE)
      
    } else {
      warning(paste("No variables found for year", Year, "- stack not generated."))
    }
    
  } # end if/else block
  
} # end year loop
