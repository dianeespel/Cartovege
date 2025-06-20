# Title: Create necessary folder structure for CARTOVEGE pipeline
# Author: Diane ESPEL
# Objective: Create all required directories for the CARTOVEGE processing workflow on both localHOME and localscratch

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects in the R environment to start fresh
graphics.off()  # Close all graphics devices to reset plots


# Define base paths ----------------------------------------------------------------------------

localHOME <- "/home/genouest/cnrs_umr6553/despel/CARTOVEGE/"
localscratch <- "/scratch/despel/CARTOVEGE/"

# List of directories to create in localHOME ------------------------------------------------------

dirs_localHOME <- c(
  file.path(localHOME, "data/samples"),
  file.path(localHOME, "data/raster/Precut_image"),
  file.path(localHOME, "data/vector/Plots/PrimaryTypo"),
  file.path(localHOME, "data/vector/Observed_map/PrimaryTypo"),
  file.path(localHOME, "data/vector/mask")
)

# List of directories to create in localscratch ---------------------------------------------------

dirs_localscratch <- c(
  file.path(localscratch, "data/raster/Cut_image"),
  file.path(localscratch, "data/raster/Observed_map/NewTypo"),
  file.path(localscratch, "data/Learning_data/PrimaryTypo"),
  file.path(localscratch, "data/Learning_data/NewTypo"),
  file.path(localscratch, "data/vector/Observed_map/NewTypo"),
  file.path(localscratch, "results/Model/Tuned_model"),
  file.path(localscratch, "results/Model/Final_model"),
  file.path(localscratch, "results/Predictions"),
  file.path(localscratch, "results/Landscape_metrics"),
  file.path(localscratch, "results/Difference_maps")
)

# Function to create directories if they do not exist ----------------------------------------------

create_dirs <- function(dirs) {
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)   # Create directory and any necessary parent folders
      message("Created directory: ", dir)
    } else {
      message("Directory already exists: ", dir)
    }
  }
}

# Execute directory creation -----------------------------------------------------------------------

create_dirs(dirs_localHOME)
create_dirs(dirs_localscratch)
