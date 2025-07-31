
# Title : Create all learning plots 
# Author : Diane ESPEL
# Objectives : merge all plots (HFI plots, FIELD samples plots and field polygons plots)

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 


# Required packages -------------------------------------------------------

library(sf)     # For handling spatial vector data (modern replacement for 'rgdal')

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# Path to open input multisources plots 
open_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

# Path to save all learning plots
save_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")


# Open  all data sources  -------------------------------------------------------------

# Define the file paths for the three possible shapefile sources:
# 1. FIELD samples (mandatory)
# 2. HFI samples (optional)
# 3. Photointerpreted plots (optional)
FILE1 <- paste0(open_plots_path, "/Quadrats_", District, "_", Island, "_ALL_FIELD_SAMPLES_Polygons_corrected_EPSG32739.shp")
FILE2 <- paste0(open_plots_path, "/Quadrats_", District, "_", Island, "_ALL_HFI_SAMPLES_Polygons_corrected_EPSG32739.shp")
FILE3 <- paste0(open_plots_path, "/Quadrats_", District, "_", Island, "_PHOTO-INTERPRETED_Polygons_EPSG32739.shp")

# Load the FIELD shapefile (mandatory)
FIELD_true_plots <- st_read(FILE1)

# If HFI file exists, load it
if (file.exists(FILE2)) {
  assign("HFI_true_plots", st_read(FILE2))
} 

# If photointerpreted file exists, load it
if (file.exists(FILE3)) {
  assign("photointerpreted_plots", st_read(FILE3))
}

# Merge plots and filter overlapping plots -------------------------------------------------------------

# Start with FIELD plots (always present)
all_plots<- FIELD_true_plots

# If photointerpreted plots exist, append them to the dataset
if (exists("photointerpreted_plots")) {
  all_plots<- rbind(all_plots, photointerpreted_plots)
}

# If HFI plots exist append them to the dataset
if (exists("HFI_true_plots")) {
  all_plots<- rbind(all_plots, HFI_true_plots)
}

# At this stage, all_plots contains FIELD plots and other sources if they exist

# Define a priority order for conflict resolution: lower number = higher priority
priority_order <- c("FIELD" = 1, "PHOTO-INTERPRETATION" = 2, "HFI" = 3) # assuming HFI correspond to historical values
all_plots$priority <- priority_order[ all_plots$Source ] # Create a priority column based on the Source column


# Compute all intersecting pairs (list of neighbors for each polygon)
intersections_list <- st_intersects(all_plots, sparse = TRUE)

# Create a data frame with pairs (i,j) where i < j to avoid duplicates
pairs <- do.call(rbind, lapply(seq_along(intersections_list), function(i) {
  js <- intersections_list[[i]]
  js <- js[js > i]  # only keep pairs where j > i
  if (length(js) == 0) return(NULL)
  data.frame(i = i, j = js)
}))

# Initialize vector to keep track of polygons to retain
keep <- rep(TRUE, nrow(all_plots))

# Loop over intersecting pairs to resolve overlaps by priority
for (k in seq_len(nrow(pairs))) {
  i <- pairs$i[k]
  j <- pairs$j[k]
  
  # Skip if either polygon already excluded
  if (!keep[i] || !keep[j]) next
  
  # Compare priorities (lower number = higher priority)
  if (all_plots$priority[i] <= all_plots$priority[j]) {
    keep[j] <- FALSE
  } else {
    keep[i] <- FALSE
  }
}

# Filter the polygons to keep only the highest priority non-overlapping ones
filtered_plots <- all_plots[keep, ]

# Print final retained plots to the console
print(filtered_plots)


# Save filtered plots -----------------------------------------------------------

# Rename N_obs column into ID column
names(filtered_plots)[names(filtered_plots) == "N_obs"] <- "ID"

# Save the filtered plots to a shapefile
st_write(filtered_plots, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_SOURCES_Polygons_EPSG32739.shp"), driver = "ESRI Shapefile", append = FALSE)

# Extract centroids from filtered plots and save as a new shapefile and as a csv file for tabular use
filtered_plots_sf <- st_as_sf(filtered_plots) # Ensure it's an sf object
centroids <- st_centroid(filtered_plots_sf) # Compute centroids
st_write(centroids, paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_SOURCES_Centroids_EPSG32739.shp"),driver = "ESRI Shapefile", append = FALSE)
write.table(centroids, file = paste0(save_plots_path, "/Quadrats_", District, "_", Island, "_ALL_SOURCES_Centroids_EPSG32739.csv"), sep = ";", dec = ".", row.names = FALSE)

