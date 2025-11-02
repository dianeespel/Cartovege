
# Title :  Cartovege Workflow
# Author : Diane ESPEL
# Objectives ## Run all project scripts sequentially (with simple checks)

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# ------------------------- USER OPTIONS -------------------------
# If TRUE each script is executed in a separate R process (Rscript).
# If FALSE scripts are sourced in the current R session (share objects).
run_in_subprocess <- FALSE

# If TRUE the loop continues even if one script fails (errors are logged).
# If FALSE the Main script stops at the first error.
continue_on_error <- FALSE

# Path to the scripts folder (relative to project root)
scripts_dir_name <- "scripts"

# ------------------------- SET PROJECT ROOT ---------------------
# Try to set working directory to the project root (works in RStudio if opened
# from the project file). Fallback to here::here() if available, else getwd().
project_root <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    getwd()
  }
}, error = function(e) getwd())

setwd(project_root)
message("Project root: ", project_root)

# ------------------------- SCRIPTS LIST -------------------------
script_list <- c(
  "00_Folder_initialization.R",
  "0_Installation_of_packages.R",
  "1.1.1_Creating_SAMPLES_plots.R",
  "1.1.2.a_Centroids_corrections_on_Qgis_OPTION_.txt",
  "1.1.2.b_Correcting_SAMPLES_plots_OPTION_.R",
  "1.2.1_Correcting_OBSERVED_MAP_with_FIELD_SAMPLES_plots_OPTION.R",
  "1.2.2_Getting_random_points_within_OBSERVED_MAP.R",
  "1.2.3_Creating_and_Sorting_PHOTO-INTERPRETED_plots.R",
  "1.3_Merging_all_learning_plots.R",
  "1.4_Nb_initial_plots_per_class.R",
  "2.0_Pansharpening_(OPTION).R",
  "2.1_Deriving_slope_from_dem.R",
  "2.2_Masking_rasters.R",
  "2.3_Computing_spectral_indices.R",
  "2.4_Computing_GLCM_features.R",
  "2.5_Creating_raster_total.R",
  "3.1_Extracting_imagery_data_within_plots.R",
  "3.2.1_Selecting_relevant_predictors_with_PCA.R",
  "3.2.2_Creating_filtered_final_raster_stack.R",
  "3.3.1.a_Analyzing_variable_distribution_violin_plots.R",
  "3.3.1.b_Find_spectral_confusions_with_HAC.R",
  "3.3.2_Assessing_spectral_separability_for_New_typology.R",
  "3.4_Nb_final_plots_per_class.R",
  "4.1.a_Training_RF_FLAT_model.R",
  "4.1.b_Training_RF_HIERARCHICAL_model.R",
  "4.2_Building_Final_model.R",
  "5.1.a_Final_FLAT_map.R",
  "5.1.b_Final_HIERARCHICAL_map.R",
  "5.2_Computing_habitat_metrics.R",
  "6.1.1_Identifying_misclassified_plots.R",
  "6.1.2_Analyzing link between misclassifications and variables.R",
  "6.2.1_Correcting_observed_map_with_new_typology.R",
  "6.2.2_Rasterizing_new_polygon-based_observed_map.R",
  "6.2.3_Computing_map_of_differences.R",
  "7_Analyzing_time_series_of_habitat_metrics.R"
)

# ------------------------- HELPERS ------------------------------
full_script_path <- function(fname) file.path(project_root, scripts_dir_name, fname)
file_ext <- function(fname) tools::file_ext(fname)

# Create a simple log container
results <- data.frame(
  script = script_list,
  status = NA_character_,
  message = NA_character_,
  stringsAsFactors = FALSE
)

# ------------------------- EXECUTION LOOP -----------------------
for (i in seq_along(script_list)) {
  script <- script_list[i]
  fpath <- full_script_path(script)
  
  message(sprintf('\n----- [%02d/%02d] %s -----', i, length(script_list), script))
  
  if (!file.exists(fpath)) {
    warning(sprintf('File not found: %s', fpath))
    results$status[i] <- 'missing'
    results$message[i] <- 'file not found'
    if (!continue_on_error) next
    else next
  }
  
  ext <- tolower(file_ext(script))
  
  if (ext != 'r') {
    message(sprintf('Skipping non-R file: %s (ext=%s)', script, ext))
    results$status[i] <- 'skipped'
    results$message[i] <- paste0('non-R file (', ext, ')')
    next
  }
  
  if (run_in_subprocess) {
    # Execute in a new R process using Rscript
    cmd <- sprintf('Rscript %s', shQuote(fpath))
    message('Running as subprocess: ', cmd)
    exit_status <- system(cmd)
    if (exit_status == 0) {
      results$status[i] <- 'ok'
      results$message[i] <- 'ran successfully (subprocess)'
    } else {
      results$status[i] <- 'error'
      results$message[i] <- paste0('subprocess exit code: ', exit_status)
      if (!continue_on_error) stop('Script failed: ', script)
    }
  } else {
    # Source in the current R session
    message('Sourcing: ', fpath)
    tt <- tryCatch({
      source(fpath, echo = TRUE, max.deparse.length = Inf)
      TRUE
    }, error = function(e) {
      message('Error: ', conditionMessage(e))
      assign('last_error', e, envir = .GlobalEnv)
      FALSE
    })
    
    if (isTRUE(tt)) {
      results$status[i] <- 'ok'
      results$message[i] <- 'sourced successfully'
    } else {
      results$status[i] <- 'error'
      results$message[i] <- 'error during source()'
      if (!continue_on_error) stop('Script failed while sourcing: ', script)
    }
  }
}

# ------------------------- SUMMARY ------------------------------
message('\n==================== SUMMARY ====================')
print(results)

# Optionally save summary to disk
write.csv(results, file = file.path(project_root, 'outputs', 'main_run_summary.csv'), row.names = FALSE)
message('\nSummary written to outputs/main_run_summary.csv (if outputs/ exists)')

message('\n✅ Main script finished.')
