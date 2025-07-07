
# Title: Create SAMPLES plots
# Author : Diane ESPEL
# Objectives : draw plots with a quadrat form : Create a squared buffer zone around each gps points

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)

# Load required packages -------------------------------------------------------

library(sp)         # For spatial data structures (SpatialPolygons, SpatialPoints, etc.)
library(sf)         # For handling 'simple features' spatial objects (modern alternative to sp)
library(dplyr)      # For data manipulation (filter, mutate, select, etc.)
library(data.table) # For fast data operations; here mainly used for rbindlist()

# Define global variables ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)

# Set working directory -------------------------------------------------------------

# Define local root directory
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Define path to load input sample data
open_samples_path=paste0(localHOME,"data/samples")

# Define path to save generated outputs (plots)
save_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

# Create field reference samples plots-----------------------------------------------------

# Define list of sources: FIELD = recent samples, HFI = historical samples
source_list=list("FIELD","HFI")

# Loop over source list
for (source in source_list){
  
  #source="FIELD" # For debugging
  
  print(paste0("Working with source: ", source))
  
  # Open and prepare data -----------------------------------------------------
  
  # Build file name pattern for input CSV files
  pattern <- paste0("Classified_", District, "_", Island, "_", source, "_SAMPLES_",".*.csv")
  
  # List all matching files in the samples folder
  all_files <- list.files(path = open_samples_path, pattern = pattern, full.names = TRUE)
  
  print(all_files) # Print the list of matched filenames
  
  file_list <- list()
  for (i in 1:length(all_files)) {
    file_list[[i]] <- read.csv(all_files[i], sep = ";", dec = ".", stringsAsFactors = FALSE) # Read each file and store in list
    assign(paste0(source,i), read.csv(all_files[i], sep = ";", dec = ".", stringsAsFactors = FALSE)) # Assign file to dynamic variable
  }
  
  Nb_database = length(all_files) # Count number of files imported
  print(paste0("Number of imported databases: ", Nb_database))
  
  # Merge all dataframes into a single one (even if only one input file)
  df = rbindlist(file_list) 
  
  # Remove potential duplicates
  New_df = unique(df) 
  New_df = as.data.frame(New_df)
  
  # Add source identifier
  New_df$Source = source
  
  print(paste0("Number of unique observations: ", length(unique(New_df$N_observation))))
  
  # Convert XY coordinates columns -------------------------------------------
  
  filt_points <- New_df
  
  # Convert coordinates from EPSG 4326 (decimal degrees) to EPSG 32739 (UTM Zone 39S)
  print("Columns Longitude_ddd and Latitude_ddd are in EPSG 4326 (decimal degrees) and must be converted to EPSG 32739 (meters)")
  filt_points <- st_as_sf(filt_points, coords = c("Longitude_ddd", "Latitude_ddd"), crs = 4326) %>%
    st_transform(32739)
  
  # Extract converted coordinates (X and Y in meters)
  filt_points_m <- st_coordinates(filt_points)
  
  # Re-add longitude and latitude columns
  filt_points$xcoord_m <- filt_points_m[, "X"]
  filt_points$ycoord_m <- filt_points_m[, "Y"]
  filt_points$Longitude_ddd <- New_df[, "Longitude_ddd"]
  filt_points$Latitude_ddd <- New_df[, "Latitude_ddd"]
  
  # Select columns of interest
  final_points <- filt_points[, c("N_observation", "Date_manip", "New_protocole","Source", "Longitude_ddd", "Latitude_ddd", "xcoord_m","ycoord_m", "Aire_m2", "Longueur_m", "Largeur_m", "Habitat_L1", "Habitat_L2", "Habitat_L3", "Habitat_L4")]
  
  # Rename columns for clarity
  colnames(final_points) <- c("N_obs", "Date", "Protocole","Source", "Longitude", "Latitude","xcoord_m","ycoord_m", "Surface", "Longueur_m", "Largeur_m", "Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4","geometry")
  
  # Create spatial polygons  -----------------------------------------
  
  # Compute corners of quadrat based on center (coordinates) and desired size
  print("Defining quadrat side length (m) and calculating corner coordinates")
  length_quadrat <- 4
  radius <- length_quadrat / 2 
  poly_points <- data.frame(final_points)
  poly_points$yPlus <- poly_points$ycoord_m + radius  # North
  poly_points$xPlus <- poly_points$xcoord_m + radius  # East
  poly_points$yMinus <- poly_points$ycoord_m - radius # South
  poly_points$xMinus <- poly_points$xcoord_m - radius # West
  
  print("Check for duplicate observation IDs (should be unique)")
  indices_doublons <- which(duplicated(poly_points$N_obs) | duplicated(poly_points$N_obs, fromLast = TRUE))
  print(indices_doublons)
  
  # Save centroid coordinates as CSV
  FILE3 = paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_", source, "_SAMPLES_Centroids_EPSG32739.csv")
  write.table(poly_points, file = FILE3, sep = ";", dec = ".", row.names = FALSE)
  
  # Construct quadrat polygons (5 points: NW to NW clockwise to close the shape)
  quadrats <- cbind(poly_points$xMinus, poly_points$yPlus,   # NW
                    poly_points$xPlus, poly_points$yPlus,    # NE
                    poly_points$xPlus, poly_points$yMinus,   # SE
                    poly_points$xMinus, poly_points$yMinus,  # SW
                    poly_points$xMinus, poly_points$yPlus)   # back to NW
  
  # Create spatial polygons from coordinates
  ID <- poly_points$N_obs # Extract observation IDs
  print("Creating spatial polygons (quadrat plots)")
  polysHabitat <- SpatialPolygons(
    mapply(function(poly, id) {
      xy <- matrix(poly, ncol=2, byrow=TRUE)      # Create coordinate matrix
      Polygons(list(Polygon(xy)), ID = id)        # Create Polygon object with ID
    }, 
    split(quadrats, row(quadrats)), ID),          # Split coordinates by row (one polygon per row)
    proj4string = CRS("+init=EPSG:32739")         # Define projection
  )
  
  plot(polysHabitat)  # Plot polygons for visual check
  
  # Create SpatialPolygonsDataFrame to store attributes
  # This object links each quadrat polygon with its corresponding metadata (attributes)
  polys.df <- SpatialPolygonsDataFrame(polysHabitat,  # The SpatialPolygons object containing geometry
                                       data.frame(ID = ID, row.names = ID) # Create a minimal data.frame with IDs as row names
                                       ) 
  polys.df$xcoord_m = poly_points$xcoord_m # Add additional attribute fields to the SpatialPolygonsDataFrame
  polys.df$ycoord_m = poly_points$ycoord_m
  polys.df$Longitude = poly_points$Longitude
  polys.df$Latitude = poly_points$Latitude
  polys.df$Date = poly_points$Date
  polys.df$Source = poly_points$Source
  polys.df$Surface = poly_points$Surface
  polys.df$Hab_L1 = poly_points$Hab_L1
  polys.df$Hab_L2 = poly_points$Hab_L2
  polys.df$Hab_L3 = poly_points$Hab_L3
  polys.df$Hab_L4 = poly_points$Hab_L4
  
  # Save the final shapefiles  -------------------
  
  # Export polygons as shapefile
  polys.sf <- st_as_sf(polys.df)
  st_write(polys.sf, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_", source, "_SAMPLES_Polygons_EPSG32739.shp"), driver = "ESRI Shapefile", append = FALSE)
  
  # Convert sampling centroids to spatial points shapefile
  pts.in.polys <- st_as_sf(poly_points, coords = c("xcoord_m", "ycoord_m"), crs = 32739)
  st_write(pts.in.polys, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_", source, "_SAMPLES_Centroids_EPSG32739.shp"), driver = "ESRI Shapefile", append = FALSE)
  
} # end of source loop

