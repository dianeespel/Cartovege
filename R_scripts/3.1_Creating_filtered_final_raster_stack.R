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


# Create function ---------------------------------------------------------
# Define function that remove Collinear variable baded on selection critera
removeCollinearVariables <- function(raster.stack,multicollinearity.cutoff = 0.7,selection = c("variance", "completeness", "ecological", "random"),priority = NULL,expected_names = NULL,sample.points = FALSE,
                                     nb.points = 10000,plot = FALSE, correlation_method = c("pearson", "spearman", "kendall")) {
  
  selection <- match.arg(selection)
  correlation_method <- match.arg(correlation_method)  # ✅ corrected
  
  #----------------------------------
  # Class handling
  #----------------------------------
  if (inherits(raster.stack, "RasterStack")) {
    raster.stack <- terra::rast(raster.stack)
  } else if (!inherits(raster.stack, "SpatRaster")) {
    stop("raster.stack must be a SpatRaster or RasterStack")
  }
  
  #----------------------------------
  # Sampling
  #----------------------------------
  if (sample.points) {
    env.df <- terra::spatSample(
      raster.stack,
      size = nb.points,
      na.rm = TRUE
    )
  } else {
    env.df <- terra::values(raster.stack)
  }
  
  #----------------------------------
  # Cleaning (CRUCIAL)
  #----------------------------------
  
  # remove rows with NA
  env.df <- env.df[stats::complete.cases(env.df), ]
  
  # remove rows with Inf / -Inf / NaN
  env.df <- env.df[apply(env.df, 1, function(x) all(is.finite(x))), ]
  
  # remove (near) constant variables
  vars_var <- apply(env.df, 2, stats::var, na.rm = TRUE)
  env.df <- env.df[, vars_var > 1e-10, drop = FALSE]
  
  # safety check
  if (ncol(env.df) < 2) {
    stop("Not enough valid variables after cleaning")
  }
  
  #----------------------------------
  # Correlation & clustering
  #----------------------------------
  cor.matrix <- 1 - abs(stats::cor(env.df, method = correlation_method))
  dist.matrix <- stats::as.dist(cor.matrix)
  ahc <- stats::hclust(dist.matrix, method = "complete")
  groups <- stats::cutree(ahc, h = 1 - multicollinearity.cutoff)
  
  #----------------------------------
  # Plot
  #----------------------------------
  if (plot) {
    plot(
      ahc, hang = -1,
      ylab = "Distance (1 - |r|)",
      main = paste("Collinearity groups (correlation method:", correlation_method, ")")
    )
    stats::rect.hclust(ahc, h = 1 - multicollinearity.cutoff)
  }
  
  #----------------------------------
  # Selection per group
  #----------------------------------
  selected <- character(0)
  
  for (g in unique(groups)) {
    vars <- names(groups)[groups == g]
    
    if (length(vars) == 1) {
      selected <- c(selected, vars)
      next
    }
    
    sub <- env.df[, vars, drop = FALSE]
    
    best <- switch(
      selection,
      variance = vars[which.max(apply(sub, 2, stats::var))],
      completeness = vars[which.max(colSums(!is.na(sub)))],
      ecological = {
        if (is.null(priority)) stop("priority must be provided for selection = 'ecological'")
        priority[priority %in% vars][1]
      },
      random = sample(vars, 1)
    )
    
    selected <- c(selected, best)
  }
  
  #----------------------------------
  # Reorder according to expected_names
  #----------------------------------
  if (exists("expected_names")) {
    selected <- intersect(expected_names, selected)
  }
  
  return(selected)
}


# Define global variables  ---------------------------------

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
YearRef="2022" #  acquisition year of reference multispectral imagery
MonthRef="02" #  acquisition month of reference multispectral imagery
ResRef="50cm"
Satellite2="SRTM" # satellite name of DEM
Year2="2012" # acquisition year of DEM



# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE_2/")
localscratch=paste0("/scratch/despel/CARTOVEGE_2/")
#localscratch = paste0("your_local_path/")

# Path to open input rasters layers or stack
open_cut_raster_path_scratch=paste0(localscratch,"data/raster/Cut_image")
open_cut_raster_path_HOME=paste0(localHOME,"data/raster/Cut_image")

# Path to save the final raster stack
save_cut_raster_path=paste0(localscratch,"data/raster/Cut_image")


# Define available files -------------------------------------------------------------

# Identify available years for Satellite1 rasters based on file names
all_satellite1_files <- list.files(open_cut_raster_path_HOME, pattern = paste0(District, "_", Island, "_", Satellite1,"_\\d{4}_\\d{2}_.+_cut\\.TIF$"))
available_satellite1_years <- sort(unique(str_extract(all_satellite1_files, "\\d{4}")))
available_satellite1_years <- available_satellite1_years[!is.na(available_satellite1_years)]
cat("Detected years:", paste(available_satellite1_years, collapse = ", "), "\n")

# List of months
all_months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

# Mapping between logical variable names and file band names
band_mapping <- c(r   = "band3",g   = "band2",b   = "band1",nir = "band4")


# Filter reference data -------------------------------------------------------------

# Defined filtered reference raster_total
ref_file <- paste0(save_cut_raster_path, "/", District, "_", Island,"_Final_raster_stack_", YearRef, "_", MonthRef, "_", ResRef, "_cut.TIF")

if (!file.exists(ref_file)) {
  
  cat("Processing reference raster stack for collinearity filtering\n")
  
  # load reference raster_total
  raster_total_ref <- rast(paste0(open_cut_raster_path_scratch, "/", District, "_", Island,"_", Satellite1, "_Total_raster_stack_", YearRef, "_", MonthRef, "_", ResRef, "_cut.TIF"))
  
  expected_var_names <- c("R","G","B","NIR","NDVI","GRVI","VARI","GCCI","Brightness","BSI","NDWI","Dtm","Slope")
  if (nlyr(raster_total_ref) == length(expected_var_names)) {
    names(raster_total_ref) <- expected_var_names
  } else {
    warning("Unexpected number of layers in reference raster stack")
  }
  
  # Apply collinearity filtering : remove collinear variables
  list_variables <- removeCollinearVariables(raster_total_ref, multicollinearity.cutoff = 0.7,
                                             correlation_method = "pearson",
                                             selection = "variance",
                                             expected_names=expected_var_names,
                                             plot = F,
                                             sample.points = TRUE,
                                             nb.points = 5000)
  
  # Save plot
  NOMpng=paste0(save_cut_raster_path,"/","Selecting_variables_Reference", District, "_", Island, "_", Satellite1, "_", YearRef,"_",MonthRef,"_", ResRef, ".png")
  png(file = NOMpng, width = 1000, height = 1000)
  list_variables <- removeCollinearVariables(
    raster_total_ref,
    multicollinearity.cutoff = 0.7,
    correlation_method = "pearson",
    selection = "variance",
    plot = TRUE,
    sample.points = TRUE,
    nb.points = 5000
  )
  dev.off()
  
  
  # Keep only selected layers
  raster_total_filtered <- raster_total_ref[[list_variables]]
  
  # Save final raster stack of reference
  save(raster_total_filtered, file = ref_file_Rdata)
  writeRaster(raster_total_filtered, paste0(save_cut_raster_path,"/", District, "_", Island,"_Final_raster_stack_", YearRef, "_", MonthRef, "_", ResRef, "_cut.TIF"), overwrite = TRUE)
  
} else {
  cat("Reference raster already exists -> loading\n")
  
  # Load filtered raster stack
  raster_total_filtered <- rast(ref_file)
  
  
}

# Process other years / months / resolutions using reference variables -------------------------------------------------

# List of available resolutions
all_optical_founded_resolutions <- c("30cm","50cm","1.2m","2m")

# Step: map raster variable names to actual file band names on disk
list_variables <- tolower(names(raster_total_filtered))
file_vars <- ifelse(list_variables %in% names(band_mapping),
                    band_mapping[list_variables],  # map R/G/B/NIR → band3/band2/band1/band4
                    list_variables)                # keep other indices as is (NDVI, GRVI, ...)

# Loop through all years/months 
for (Year in available_satellite1_years) {
  for (Month in all_months) {
    
    if (Year == YearRef && Month == MonthRef) next  # skip reference
    
    cat("Processing temporal raster stack Year:", Year, "Month:", Month, "\n")
    
    for (ResToMap in all_optical_founded_resolutions) {
      
      # Build pattern to match optical raster files
      pattern_optical <- paste0(District, "_", Island, "_", Satellite1, "_", Year, "_", Month, "_", ResToMap,"_(", paste(file_vars, collapse="|"), ")_cut\\.TIF$")
      
      # List all optical files that exist for the variables
      optical_files <- list.files(open_cut_raster_path_HOME, pattern = pattern_optical, full.names = TRUE)
      
      if(length(optical_files)==0) {
        warning(paste("No optical raster found for", Year, Month, "Res:", ResToMap))
        next
      }
      
      print(paste("Optical raster found for resolution", ResToMap, "in", Year, Month))
      
      # Open optical raster stack
      cat("  → Optical data found:", Year, Month, ResToMap, "\n")
      optical_raster_stack <- rast(optical_files)
      
      # Prepare regex pattern for topographic variables (Satellite2)
      pattern_topo <- paste0(District, "_", Island, "_", Satellite2, "_", Year2, "_", ResRef, "_(", paste(list_variables, collapse="|"), ")_cut\\.TIF$")
      
      # List all topographic files that exist
      topo_files <- list.files(open_cut_raster_path_HOME, pattern = pattern_topo, full.names = TRUE)
      
      # Stack optical variables
      if (length(topo_files)>0) {
        topo_raster_stack <- rast(topo_files)
        # Optional: resample topographic layers to match the first optical raster
        if (!all(res(topo_raster_stack) == res(optical_raster_stack))) {
          topo_raster_stack <- resample(topo_raster_stack,optical_raster_stack[[1]],method = "bilinear")
        }
        all_raster_stack <- c(optical_raster_stack, topo_raster_stack)
      } else {
        all_raster_stack <- optical_raster_stack
      }
      
      # Check variable completeness
      vars_present <- names(all_raster_stack)
      missing_vars <- setdiff(list_variables, vars_present)
      if (length(missing_vars) > 0) {
        warning(paste("Missing variables for",Year, Month, ResToMap, ":",paste(missing_vars, collapse = ", ")))
        next
      }
      
      
      # Reorder layers to match reference structure
      all_raster_stack <- all_raster_stack[[list_variables]]
      
      # Save final raster stack
      save(all_raster_stack, file = paste0(save_cut_raster_path,"/", District, "_", Island,"_Final_raster_stack_", Year, "_", Month, "_", ResToMap, "_cut.Rdata"))
      writeRaster(all_raster_stack, paste0(save_cut_raster_path,"/", District, "_", Island,"_Final_raster_stack_", Year, "_", Month, "_", ResToMap, "_cut.TIF"), overwrite = TRUE)
      
    } # Res loop
  } # Month loop
} # Year loop

cat("Raster stack processing completed.\n")


