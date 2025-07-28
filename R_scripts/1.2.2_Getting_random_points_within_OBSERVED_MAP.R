
# Title : Creating random points inside each polygon from POLYGONS_MAP
# Author : Diane ESPEL
# Objectives : Generate different types of internal points in land cover polygons:
#              poles of inaccessibility, surface points, and additional random points in large polygons.

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(sp)
library(sf)
library(dplyr)
library(polylabelr)


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades"   # Name of satellite used for multispectral imagery
Year1="2022"            # Year of multispectral imagery acquisition


# Set working directory -------------------------------------------------------------

# Local path 
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# path where to open observed map
open_ObsMap_path=paste0(localHOME,"data/vector/Observed_map/PrimaryTypo")

# path where to save your results
save_ObsMap_path=paste0(localHOME,"data/vector/Observed_map/PrimaryTypo")


# Load an prepare polygons map -------------------------------------------------------------

# List all shapefiles matching expected filename pattern
files_corrected <- list.files(open_ObsMap_path,pattern = paste0("^Corrected_observed_map_", District, "_", Island, "_", Satellite1, "_", Year1, "_EPSG32739\\.shp$"),full.names = TRUE)
files_observed <- list.files(open_ObsMap_path, pattern = paste0("^Observed_map_", District, "_", Island, "_", Satellite1, "_", Year1, "_EPSG32739\\.shp$"),full.names = TRUE)

# If a corrected file exists, use it; otherwise, use the uncorrected version (if it exists)
if (length(files_corrected) > 0) {
  FILE1 <- files_corrected[1]
} else if (length(files_observed) > 0) {
  FILE1 <- files_observed[1]
}
polygons_map <- st_read(FILE1)

# Convert SpatialPolygons into sf object
polygons_map_sf=st_as_sf(polygons_map)

# Get Poles of Inaccessibility -------------------------------------------------------------

print("Computing coordinates of poles of inaccessibility for all polygons")

# Compute pole of inaccessibility
poles <- poi(polygons_map_sf)
poles <- data.table::rbindlist(poles)

# Convert poles to spatial object and filter them according to distance from polygon boundaries
distance = 5 # Define threshold distance from polygons boundaries
poles_sf <- poles %>%
  filter(dist >= distance) %>%  # Filter to keep only POIs at least 5 meters from boundaries
  select(1:2) %>%
  rename(xcoord_m = 1, ycoord_m = 2) %>%
  mutate(id = row_number()) %>%
  st_as_sf(coords = c("xcoord_m", "ycoord_m"), crs = 32739)

# Intersect with original polygons to extract attributes
polesInaccess <- st_intersection(poles_sf, polygons_map_sf)

# Add coordinates in both EPSG:32739 and EPSG:4326
polesInaccess <- cbind(polesInaccess,
                       st_coordinates(polesInaccess),
                       st_coordinates(st_transform(polesInaccess, 4326)))
names(polesInaccess)[names(polesInaccess) == "X"] <- "xcoord_m"
names(polesInaccess)[names(polesInaccess) == "Y"] <- "ycoord_m"
names(polesInaccess)[names(polesInaccess) == "X.1"] <- "Longitude"
names(polesInaccess)[names(polesInaccess) == "Y.1"] <- "Latitude"

# Prepare final dataset
polesInaccess$Date = "2022"
polesInaccess$Source = "PHOTO-INTERPRETATION"
polesInaccess = polesInaccess[, c("id", "Date", "Source", "xcoord_m", "ycoord_m", "Longitude", "Latitude", "Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4", "geometry")]

# Save results
st_write(polesInaccess, paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Poles_of_inaccessibility_EPSG32739.shp"), driver = 'ESRI Shapefile', append = FALSE)
write.table(polesInaccess, file = paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Poles_of_inaccessibility_EPSG32739.csv"), sep = ";", dec = ".", row.names = FALSE)

# Get Points on Surface -------------------------------------------------------------

print("Computing coordinates of internal surface points for all polygons")

# Compute internal points (centroids)
ptOnSurface <- polygons_map_sf %>%
  st_point_on_surface() %>%
  mutate(id = max(polesInaccess$id) + row_number())

# Add coordinates in both EPSG:32739 and EPSG:4326
ptOnSurface <- cbind(ptOnSurface,
                     st_coordinates(ptOnSurface),
                     st_coordinates(st_transform(ptOnSurface, 4326)))
names(ptOnSurface)[names(ptOnSurface) == "X"] <- "xcoord_m"
names(ptOnSurface)[names(ptOnSurface) == "Y"] <- "ycoord_m"
names(ptOnSurface)[names(ptOnSurface) == "X.1"] <- "Longitude"
names(ptOnSurface)[names(ptOnSurface) == "Y.1"] <- "Latitude"

# Prepare final dataset
ptOnSurface$Date = "2022"
ptOnSurface$Source = "PHOTO-INTERPRETATION"
ptOnSurface = ptOnSurface[, c("id", "Date", "Source", "xcoord_m", "ycoord_m", "Longitude", "Latitude", "Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4", "geometry")]

# Save results
st_write(ptOnSurface, paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Pts_on_surface_EPSG32739.shp"), driver = 'ESRI Shapefile', append = FALSE)
write.table(ptOnSurface, file = paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Pts_on_surface_EPSG32739.csv"), sep = ";", dec = ".", row.names = FALSE)



#  Get random points  -------------------------------------------------------------

# Computing coordinates from random points 
print("Computing coordinates for additional random points inside large polygons")


# Identify large polygons
Area_min=2000
big_polygons_map_sf=subset(polygons_map_sf,polygons_map_sf$Surface>=Area_min)
print(paste0("Il y a ",nrow(big_polygons_map_sf)," polygones qui font plus de ", Area_min," m2"))

# Fix random seed for reproducibility
set.seed(3024)


# Set parameters
n_random <- 8  # Number of random points per polygon
distance <- 12  # Minimum distance (m) between random points

# List to store generated points
points_within_polygons <- list()

# Generate random points within each large polygon
for (p in 1:nrow(big_polygons_map_sf)) {
  
  # Select the current polygon
  each_polygon <- big_polygons_map_sf[p, ]
  
  # Plot the polygon before generating points
  plot(each_polygon$geometry, col = "lightblue", main = paste("Polygon ID:", each_polygon$id))
  
  # Generate random points within the polygon
  random_points <- st_sample(each_polygon, size = n_random, exact = TRUE)
  
  # Check if any points were generated
  if (length(random_points) > 0) {
    
    # Plot the generated random points inside the polygon
    plot(random_points, col = "red", add = TRUE, pch = 20) # Red points on top of the polygon
    
    # Create an sf object with random points and attributes
    random_points_sf <- st_sf(
      id = rep(each_polygon$id, each = length(random_points)), 
      Nom_site = rep(each_polygon$Nom_site, each = length(random_points)),
      Surface = rep(each_polygon$Surface, each = length(random_points)), 
      Hab_L1 = rep(each_polygon$Hab_L1, each = length(random_points)), 
      Hab_L2 = rep(each_polygon$Hab_L2, each = length(random_points)), 
      Hab_L3 = rep(each_polygon$Hab_L3, each = length(random_points)), 
      Hab_L4 = rep(each_polygon$Hab_L4, each = length(random_points)), 
      geometry = random_points # Set geometry column
    )
    
    # Compute coordinates in EPSG:32739 (meters)
    coords_32739 <- st_coordinates(random_points_sf)
    colnames(coords_32739) <- c("xcoord_m", "ycoord_m")
    
    # Transform to EPSG:4326 (Latitude & Longitude)
    random_points_sf <- st_transform(random_points_sf, crs = 4326)
    coords_4326 <- st_coordinates(random_points_sf)
    colnames(coords_4326) <- c("Longitude", "Latitude")
    
    # Append coordinate columns to the points dataset
    random_points_sf <- cbind(random_points_sf, coords_32739, coords_4326)
    
    # Store the results in the list
    points_within_polygons[[p]] <- random_points_sf
  }
}

# Merge all generated points into a single sf object
RandomPts <- do.call(rbind, points_within_polygons)

# Assign a new unique ID to each generated point
RandomPts$id <- (max(ptOnSurface$id, na.rm = TRUE) + 1):(max(ptOnSurface$id, na.rm = TRUE) + nrow(RandomPts))

#Transform to EPSG:32739 (xcoord_m nd ycoord_m)
RandomPts <- st_transform(RandomPts, crs = 32739)

# Prepare final file
RandomPts$Date = "2022"
RandomPts$Source = "PHOTO-INTERPRETATION"
RandomPts=RandomPts[,c("id","Date","Source","xcoord_m","ycoord_m","Longitude","Latitude","Hab_L1","Hab_L2","Hab_L3","Hab_L4","geometry")]


# Save as Shapefile
st_write(RandomPts, paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Random_points_EPSG32739.shp"),driver = 'ESRI Shapefile', append = FALSE)
write.table(RandomPts,file=paste0(save_ObsMap_path, "/", "Corrected_observed_map_", District, "_", Island, "_", Year1, "_Random_points_EPSG32739.csv"),sep = ";", dec = ".", row.names = FALSE)

