
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

# Initialize a logical vector to keep track of plots to retain
keep <- rep(TRUE, nrow(all_plots)) # Initially assume all plots are first marked as TRUE (to keep)

# Loop to detect and resolve spatial overlaps by priority
for (i in seq_len(nrow(all_plots) - 1)) {
  print(paste0("Processing quadrat", i)) # Debug message
  
  if (!keep[i]) next  # Skip if quadrat i is already marked for removal, we skip it
  for (j in (i + 1):nrow(all_plots)) {
    if (!keep[j]) next  # Skip if quadrat j is already marked for removal, we skip it
    
    # Check if quadrat i and quadrat j intersect
    if (st_intersects(all_plots[i, ], all_plots[j, ], sparse = FALSE)[1, 1]) {
      
      # Compare priorities: lower number means higher priority
      if (all_plots$priority[i] <= all_plots$priority[j]) {
        # If quadrat i is of equal or higher priority than j, mark j for removal
        keep[j] <- FALSE
      } else {
        # Otherwise, mark i for removal and exit the inner loop
        keep[i] <- FALSE
        break
      }
    }
  }
}

# Filter dataset to retain only non-overlapping, highest-priority plots
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

