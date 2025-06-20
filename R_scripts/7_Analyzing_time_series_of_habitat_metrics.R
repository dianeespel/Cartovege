
# Title : Visualize diversity and landscape metrics over years
# Author : Diane ESPEL
# Objectives : Plot diversity indices over years and visualize changes in landscape metrics by habitat


#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required packages ------------------------------------------------------------

library(ggplot2)   # For creating elegant and customizable graphics using the grammar of graphics
library(dplyr)     # For data manipulation: filtering, selecting, grouping, summarizing, etc.
library(tidyr)     # For reshaping and tidying data: e.g., pivoting longer/wider, handling missing values
library(stringr)   # For consistent and simple string operations (pattern matching, extraction, etc.)
library(viridis)   # For color palettes optimized for perceptual uniformity and colorblind-friendly visualization

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
maxTypoLevel = 4 # Define the maximum classification level

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open time series of landscape metrics
open_metrics_path=paste0(localscratch,"results/Landscape_metrics")

# Path to save temporal monitoring results
save_metrics_path=paste0(localscratch,"results/Landscape_metrics")



# Plot Metrics over Years --------------------------------------------------


# Define list of modeling strategy
type_model_list=c("FLAT","HIERARCHICAL")


# Loop through model types
for (type_model in type_model_list){
  
  #type="FLAT" #debug
  print(paste0("Modeling strategy: ",type_model)) 

  
  # Loop through each classification level
  for (l in seq(1:maxTypoLevel)) {
  
    # Define the folder corresponding to the typology level
    LevelFolder=paste0(open_metrics_path,"/","Hab_L",l)
    
    # Diversity indices over time -------------------------------------------------------
    
    # Load diversity indices 
    pattern_csv <- paste0("^Diversity_indices_RF_", type_model, "_model_.*\\.csv$")
    div_files <- list.files(path = LevelFolder, pattern = pattern_csv, full.names = TRUE)
    print(div_files)
    
    # Read and combine all diversity index files
    div_data <- div_files %>%
      lapply(read.csv, sep = ";", dec = ".") %>%
      bind_rows() %>%
      mutate(Year_map = as.integer(as.character(Year_map)))
    
    
    # Plot Diversity Indices Over Time (.png)
    NOMpng=paste0(LevelFolder,"/","Diversity_Indices_Over_Time_RF_",type_model,"_model_",District,"_",Island,"_",Satellite1,"_Level_",l,".png")
    png(file = NOMpng)
    div_plot <- ggplot(div_data, aes(x =as.factor(Year_map))) +
      geom_point(aes(y = Shannon_index, color = "Shannon"),size=2.5) +
      geom_point(aes(y = Simpson_index, color = "Simpson"),size=2.5) +
      geom_point(aes(y = Pielou_index, color = "Pielou"),size=2.5) +
      labs(title = "Diversity indices over time",
           x = "Year",
           y = "Index Value",
           color = "Index") +
      theme_minimal() +
      #scale_color_manual(values = c("Shannon" = "darkgreen", "Simpson" = "steelblue", "Pielou" = "darkorange"))
      scale_color_viridis_d(option = "D")  # discrete viridis
    
    print(div_plot)
    dev.off()
    
    
    # Plot Diversity Indices Over Time (.svg)
    NOMsvg=paste0(LevelFolder,"/","Diversity_Indices_Over_Time_RF_",type_model,"_model_",District,"_",Island,"_",Satellite1,"_Level_",l,".svg")
    svg(file = NOMsvg)
    print(div_plot)
    dev.off()
    
    
    
    # Landscape metrics over time  --------------------------------------------------
    
    # Load landscape files
    pattern_csv2 <- paste0("^Landscape_metrics_RF_", type_model, "_model_.*\\.csv$")
    landscape_files <- list.files(path = LevelFolder, 
                                  pattern =pattern_csv2, 
                                  full.names = TRUE)
    
    # Read and combine all landscape metrics files
    landscape_data <- landscape_files %>%
      lapply(function(f) {
        df <- read.csv(f, sep = ";", dec = ".")
        # Extract year and typology level from filename
        year <- str_extract(f, "\\d{4}")
        level <- str_extract(f, "level_(\\d)") %>% str_remove("level_")
        df$Year <- as.integer(year)
        df$Typo_level <- as.integer(level)
        return(df)
      }) %>%
      bind_rows()
    
    
    # Reshape before plotting
    metrics_long <- landscape_data %>%
      pivot_longer(cols = c(
        Relative_abundance,
        Mean_patch_size_m2,
        Min_patch_size_m2,
        Max_patch_size_m2,
        Total_patch_size_m2,
        Largest_patch_index,
        Mean_euclidian_distance
      ),
      names_to = "Metric",
      values_to = "Value")
    
    
    # Plot landscape metrics Over Time  (.png)
    NOMpng=paste0(LevelFolder,"/","Landscape_Metrics_Over_Time_RF_",type_model,"_model_",District,"_",Island,"_",Satellite1,"_Level_",l,".png")
    png(file = NOMpng)
    landscape_plot <- ggplot(metrics_long, aes(x = as.factor(Year), y = Value, color = Hab, group = Hab)) +
      geom_point(size=2.5) +
      facet_wrap(~ Metric, scales = "free_y") + # to create a sub graph by metric
      scale_color_viridis_d(option = "D") +  # Palette viridis discrète
      labs(title = "Temporal evolution of landscape metrics by habitat",
           x = "Year", y = "Metric value", color = "Habitat") +
      theme_minimal()+
      theme(legend.position = "bottom")
    
    print(landscape_plot)
    dev.off()
    
    
    # Plot landscape metrics Over Time  (.svg)
    NOMsvg=paste0(LevelFolder,"/","Landscape_Metrics_Over_Time_RF_",type_model,"_model_",District,"_",Island,"_",Satellite1,"_Level_",l,".svg")
    svg(file = NOMsvg)
    print(landscape_plot)
    dev.off()
  
  }

} # End of model type loop