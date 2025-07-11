
# Title : Create and sort quadrats based on random points 
# Authors : Diane ESPEL
# Objectives : draw learning polygons with a fixed form (quadrats) : Create a squared buffer zone around each gps points

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(sp)       # For spatial data structures (legacy)
library(sf)       # For modern simple features spatial data handling
library(terra)    # For raster and vector data manipulation (newer alternative to raster)


# Create fucntions  ---------------------------------------------------

# Function to build a square polygon from bounding box coordinates
create_quadrat <- function(xmin, xmax, ymin, ymax) {
  st_polygon(list(matrix(c(
    xmin, ymax,  # Top-left (NW)
    xmax, ymax,  # Top-right (NE)
    xmax, ymin,  # Bottom-right (SE)
    xmin, ymin,  # Bottom-left (SW)
    xmin, ymax   # Close polygon (back to NW)
  ), ncol = 2, byrow = TRUE)))
}

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Year1="2022"            # Year of multispectral imagery acquisition


# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Path to open input  photo-interpreted points 
open_ObsMap_path=paste0(localHOME,"data/vector/Observed_map/PrimaryTypo")

# Path to save photo-interpreted plots
save_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

# Read shapefiles ----------------------------------------------------

# Define full paths to the shapefiles to be loaded
FILE1 <- paste0(open_ObsMap_path, paste0("Corrected_observed_map_", District, "_", Island, "_", Year1, "_Poles_of_inaccessibility_EPSG32739.shp"))
FILE2 <-  paste0(open_ObsMap_path, paste0("Corrected_observed_map_", District, "_", Island, "_", Year1, "_Pts_on_surface_EPSG32739.shp"))
FILE3 <-  paste0(open_ObsMap_path, paste0("Corrected_observed_map_", District, "_", Island, "_", Year1, "_Random_points_EPSG32739.shp"))

# Load the shapefiles
PolesInaccess <- st_read(FILE1)
PtOnSurface <- st_read(FILE2)
RandomPts <- st_read(FILE3)

# Merge points and harmonize columns across layers --------------------------------------------------

# Identify common columns shared between all three layers
common_columns <- Reduce(intersect, list(names(PolesInaccess ), names(PtOnSurface), names(RandomPts)))
cat("Common columns:\n")
print(common_columns)

# Subset each layer to retain only the common columns
PolesInaccess  <- PolesInaccess [, common_columns]
PtOnSurface <- PtOnSurface[, common_columns]
RandomPts <- RandomPts[, common_columns]

# Merge all point layers into one
all_points <- bind_rows(PolesInaccess , PtOnSurface, RandomPts)

# Check for duplicate IDs and correct if needed
if (any(duplicated(all_points$id))) {
  warning("Duplicate IDs detected — correcting them.")
  print(unique(all_points$id[duplicated(all_points$id)]))
  all_points$id <- as.character(seq_len(nrow(all_points))) # Replace with unique IDs
} else {
  print("No duplicate IDs.")
}



# Create quadrats -----------------------------------------------------

length_quadrat <- 4        # Length of square side in meters
radius <- length_quadrat / 2  # Half-length for buffer calculation

# Coordinates must be in meters (EPSG:32739 UTM Zone 39S)
all_points <- all_points %>%
  mutate(
    xcoord_m = st_coordinates(.)[,1],   # Extract X (easting)
    ycoord_m = st_coordinates(.)[,2],   # Extract Y (northing)
    xMinus = xcoord_m - radius,         # Lower X bound
    xPlus  = xcoord_m + radius,         # Upper X bound
    yMinus = ycoord_m - radius,         # Lower Y bound
    yPlus  = ycoord_m + radius,         # Upper Y bound
    Surface = (length_quadrat^2)        # Quadrat area
  )

# Add metadata columns
all_points$Source <- "PHOTO-INTERPRETATION"
all_points$Date <- "2022"


# Generate list of quadrats as polygons
quadrat_list <- mapply(
  create_quadrat,
  all_points$xMinus, all_points$xPlus,
  all_points$yMinus, all_points$yPlus,
  SIMPLIFY = FALSE)

# Create an sf object for the quadrats with selected attribute columns
Spdf_field_polys <- st_sf(
  all_points %>%
    select(id, Longitude, Latitude, xcoord_m, ycoord_m, Date, Source, Surface, Hab_L1, Hab_L2, Hab_L3, Hab_L4),
  geometry = st_sfc(quadrat_list, crs = 32739))



# Remove overlapping quadrats ----------------------------------------------


cat("Filtering overlapping quadrats...\n")
print("If overlap occurs, keep the first quadrat and remove the intersecting ones.")

keep_rows <- rep(TRUE, nrow(Spdf_field_polys)) # Start by keeping all rows

# Loop to detect overlaps and flag the later duplicates
for (i in seq_len(nrow(Spdf_field_polys))) {
  if (!keep_rows[i]) next  # Skip if already excluded
  for (j in seq((i+1), nrow(Spdf_field_polys))) {
    if (!keep_rows[j]) next
    if (st_intersects(Spdf_field_polys[i, ], Spdf_field_polys[j, ], sparse = FALSE)[1,1]) {
      cat(sprintf("Entity %d overlaps with %d — removing %d\n", j, i, j))
      keep_rows[j] <- FALSE
    }
  }
}



# Filter to retain only non-overlapping quadrats
Allpolys <- Spdf_field_polys[keep_rows, ]

# Reorder columns for final export
Allpolys <- Allpolys[, c("id", "xcoord_m", "ycoord_m", "Longitude", "Latitude", "Date", "Source", "Surface", "Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4", "geometry")]



# Save the final shapefiles  -------------------

# Rename columns to match export conventions
names(Allpolys) <- c("ID", "xcoord_m", "ycoord_m", "Longitude", "Latitude", "Date", "Source", "Surface", "Hab_L1", "Hab_L2", "Hab_L3", "Hab_L4", "geometry")

# Write final quadrat polygons to a shapefile
st_write(Allpolys, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_PHOTO-INTERPRETED_Polygons_EPSG32739.shp"), driver = "ESRI Shapefile", append = FALSE)

# Generate centroids from the quadrats
centroids <- st_centroid(Allpolys)

# Ensure correct geometry structure for output
centroids <- st_as_sf(centroids, coords = c("xcoord_m", "ycoord_m"), crs = 32739)

# Write centroid shapefile
st_write(centroids, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_PHOTO-INTERPRETED_Centroids_EPSG32739.shp"), driver = "ESRI Shapefile", append = FALSE)