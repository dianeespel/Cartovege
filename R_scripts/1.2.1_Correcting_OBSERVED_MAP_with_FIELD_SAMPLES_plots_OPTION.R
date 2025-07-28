# Title: Correcting observed MAP with FIELD SAMPLE plots
# Author : Diane ESPEL
# Objectives : To Correct a polygon-based habitat map based on ground truth field samples

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics --------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required libraries ------------------------------------------------------------------------

library(sp)            # Classes and methods for handling spatial data (legacy package)
library(sf)            # Simple Features for spatial data - modern and efficient spatial data package
library(terra)         # Raster and vector spatial data manipulation and analysis
library(dplyr)         # Data manipulation functions like filter, join, select, mutate, etc.

# Create functions  ---------------------------------------------------

# Function to find the most frequent habitat class(es) and return the dominant one
get_class_dom <- function(vector_classes) {
  
  # Count frequency of each habitat class in the input vector
  classes_freqs <- table(vector_classes)
  
  # Find the highest frequency count
  max_freq <- max(classes_freqs)
  
  # Identify all classes with the highest frequency
  max_categories <- names(classes_freqs)[classes_freqs == max_freq]
  
  # Return dominant class based on number of max frequency classes:
  # - If only one class, return it
  # - If three or more classes tie, return the first one
  # - If exactly two classes tie, return the second one
  if (length(max_categories) == 1) {
    classdom <- max_categories
  } else if (length(max_categories) >= 3) {
    classdom <- max_categories[1]
  } else {
    classdom <- max_categories[-1]
  }
  
  # Return the dominant habitat class
  return(classdom)
}

# Function to insert dots between each character in a string (for formatting habitat codes)
transform_chain <- function(chain) {
  
  # Split string into individual characters
  divided_chain <- strsplit(chain, "")[[1]]
  
  # Join characters by inserting dots between them
  transformed_chain <- paste(divided_chain, collapse = ".")
  
  # Return the transformed string with dots
  return(transformed_chain)
}

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades"   # Name of satellite used for multispectral imagery
Year1="2022"            # Year of multispectral imagery acquisition


# Set working directory paths -------------------------------------------------------------

# Base local path (customize to your local environment)
#localHOME = paste0("your_local_path/")
localHOME = paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Path to open input plot vector data and observed map vector data
open_plots_path = paste0(localHOME, "data/vector/Plots/PrimaryTypo")
open_ObsMap_path = paste0(localHOME, "data/vector/Observed_map/PrimaryTypo")

# Path to save corrected observed maps
save_ObsMap_path = paste0(localHOME, "data/vector/Observed_map/PrimaryTypo")

# Correct observed maps based on field samples ---------------------------------------------------

# Load input spatial data 
print("We want to correct the photo-interpreted map with FIELD SAMPLES")

# Compose file paths for polygon map and field samples shapefiles
FILE1 = paste0(open_ObsMap_path, "/", "Observed_map_", District, "_", Island, "_", Satellite1, "_", Year1, "_EPSG32739.shp")
FILE2 = paste0(open_plots_path, "/", "Quadrats_", District, "_", Island, "_ALL_FIELD_SAMPLES_Polygons_corrected_EPSG32739.shp")

# Read shapefiles into sf objects
observed_map <- st_read(FILE1)
field_samples <- st_read(FILE2)

# Check for duplicate IDs in field samples and observed map
print("There should be no duplicate IDs")
colnames(field_samples)[colnames(field_samples) == "N_obs"] <- "id"  # Rename ID column for consistency
id_doublons_field <- which(duplicated(field_samples$id) | duplicated(field_samples$id, fromLast = TRUE))
print(id_doublons_field)  # Print any duplicate IDs found in field samples
id_doublons_map <- which(duplicated(observed_map$id) | duplicated(observed_map$id, fromLast = TRUE))
print(id_doublons_map)    # Print any duplicate IDs found in observed map

# Intersect FIELD_SAMPLES and observed_MAP to find polygons with possible errors  -------------------------------------------------------------

print("Identify FIELD samples falling within polygons of the map")

# Check and match coordinate reference systems (CRS)
st_crs(observed_map) == st_crs(field_samples)  

# Transform observed_map to the CRS of field_samples if different
observed_map <- st_transform(observed_map, st_crs(field_samples)) 

# Compute spatial intersection between polygons and field samples
intersect_polys <- st_intersection(observed_map, field_samples)

print(paste0('Number of FIELD samples located within map polygons: ', nrow(intersect_polys)))

# Correct intersected polygons dataframe using dominant FIELD samples values ----------------------------------------------------------------------------

intersect_polys_corr = intersect_polys

# Confirm class is sf object
class(intersect_polys_corr)  
intersect_polys_corr <- st_as_sf(intersect_polys_corr) # Ensure it's an sf object

# Find dominant habitat class of FIELD samples inside each polygon of observed_MAP
print("Finding the dominant habitat class of FIELD_SAMPLES within each polygon")

# Initialize Dominant_class column as NA
intersect_polys_corr$Dominant_class <- NA

# Identify column indices for id and habitat classification
idmap = which(colnames(intersect_polys_corr) == "id")           # ID column index
ih4sample = which(colnames(intersect_polys_corr) == "Hab_L4.1") # Habitat level 4 (sample) column index

# Extract unique polygon IDs from intersected data
ID_list <- unique(intersect_polys_corr[[idmap]]) 

# Loop over each polygon ID
for (i in ID_list) {
  
  # Select samples belonging to polygon i
  print(paste0("Retrieving all samples present in polygon ", i, " of the map"))
  polygon_of_interest <- intersect_polys_corr[intersect_polys_corr[[idmap]] == i, ]
  polygon_of_interest <- st_as_sf(polygon_of_interest)
  
  # Determine dominant habitat class among samples in polygon i
  print("Finding dominant class among selected samples")
  dominant_class <- get_class_dom(polygon_of_interest[[ih4sample]])
  
  # Assign the dominant class to all samples of polygon i
  intersect_polys_corr[intersect_polys_corr[[idmap]] == i, "Dominant_class"] <- dominant_class[1]
}

# Format Dominant_class habitat codes to match observed_MAP typology (insert dots)
intersect_polys_corr$Dominant_class <- sapply(intersect_polys_corr$Dominant_class, transform_chain) 

# Update habitat classification columns based on dominant class
ih1map = which(colnames(intersect_polys_corr) == "Hab_L1")
ih2map = which(colnames(intersect_polys_corr) == "Hab_L2")
ih3map = which(colnames(intersect_polys_corr) == "Hab_L3")
ih4map = which(colnames(intersect_polys_corr) == "Hab_L4")

intersect_polys_corr$New_hab = intersect_polys_corr$Dominant_class

# Assign progressively detailed habitat codes to columns Hab_L1 through Hab_L4
intersect_polys_corr[, ih1map] = substr(intersect_polys_corr$New_hab, 1, 1)    # Level 1 code (first character)
intersect_polys_corr[, ih2map] = substr(intersect_polys_corr$New_hab, 1, 3)    # Level 2 code (first 3 characters)
intersect_polys_corr[, ih3map] = substr(intersect_polys_corr$New_hab, 1, 5)    # Level 3 code (first 5 characters)
intersect_polys_corr[, ih4map] = intersect_polys_corr$New_hab                   # Level 4 code (full string)

# Keep only columns up to 'id' column to trim data frame
idmap = which(colnames(intersect_polys_corr) == "id")
intersect_polys_corr = intersect_polys_corr[, 1:idmap]

print("Corrected dataframe of intersected polygons is ready")

# Correct the original observed_MAP with updated classification from intersected polygons -------------------------------------------------------------

print("Correcting the observed_MAP using the corrected intersected polygons dataframe")

# Create a copy of observed_map for correction
observed_map_corr <- observed_map

# Loop over each polygon ID to update habitat classes
for (i in ID_list) {
  
  print(paste0("Retrieving all samples present in polygon ", i, " of the map"))
  
  # Extract corrected polygon data for polygon i
  corrected_polygon <- intersect_polys_corr[intersect_polys_corr[[idmap]] == i, ]
  
  if (nrow(corrected_polygon) > 0) {
    # Update habitat classification columns in observed_map_corr with corrected values
    observed_map_corr[observed_map_corr[[idmap]] == i, c("Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4")] <- 
      corrected_polygon[1, c("Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4")]
  }
}

# Save the corrected observed map to disk as a shapefile
st_write(observed_map_corr, paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Satellite1, "_", Year1, "_EPSG32739.shp"),
         driver = 'ESRI Shapefile', append = FALSE)

