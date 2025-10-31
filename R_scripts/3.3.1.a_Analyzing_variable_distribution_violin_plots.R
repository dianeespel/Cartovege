
# Title: Analyzing Distribution of Spectral/Topographic Data
# Author: Diane ESPEL
# Objective: Violin plots


#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open) 

# Load required packages -------------------------------------------------------

library(dplyr)         # For data manipulation
library(ggplot2)       # For plotting

# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022"    # acquisition year of multispectral imagery
maxTypoLevel=4  # Define maximum typology level

# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
localscratch=paste0("/scratch/despel/CARTOVEGE/")
#localscratch = paste0("your_local_path/")

# Path to open input learning data
open_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")

# Path to save violin plot results
save_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")


# Load learning data  -------------------------------------------------------------

FILE1 <- paste0(open_learning_primary_path, "/Selected_learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv")
learning_data=read.csv(FILE1, sep=";",dec=".",stringsAsFactors=FALSE) # `stringsAsFactors=F` ensures character strings don't import as factors

# Analyze distribution of each predictor value across habitat levels -------------------------------------------------------------

# Normalize spectral/topographic variables (between 0 and 1)
ihab=which(colnames(learning_data)==paste0("Hab_L",l))
ihab4=which(colnames(learning_data)=="Hab_L4")
learning_normalized <- learning_data%>%
  mutate(across((ihab4 + 1):(ncol(learning_data)), ~ (.-min(.))/(max(.)-min(.))))

# Loop through each habitat classification level
for (l in seq (1:maxTypoLevel)){
  
  print(paste0("Processing classification level ", l))
  
  # Create a directory to store results for the current level
  LevelFolder=paste0(save_learning_primary_path,"/","Hab_L",l)
  dir.create(LevelFolder,showWarnings=F) # ShowWarnings=F to remove warnings message if file already exists

  
  # Get list of variable names to analyze
  variable_list <- colnames(learning_normalized)[(ihab4 + 1):ncol(learning_normalized)]
  
  # Loop through each variable
  for (v in variable_list){

    # Compute statistics for each habitat class----------------------------------------------------------------------
    
    print(paste0("Computing statistics for variable: ", v))
    
    # Prepare data for current variable
    spectral_df<- learning_normalized %>% 
      rename(Type_habitat = paste0("Hab_L", l)) %>%
      mutate(Type_habitat = as.factor(Type_habitat))
    
    # Compute summary statistics per habitat class
    summary_stats <- spectral_df %>%
      group_by(Type_habitat) %>%
      summarise(
        Mean = mean(get(v), na.rm = TRUE),
        Median = median(get(v), na.rm = TRUE),
        Q1 = quantile(get(v), 0.25, na.rm = TRUE),
        Q3 = quantile(get(v), 0.75, na.rm = TRUE),
        SD = sd(get(v), na.rm = TRUE),
        SE = SD / sqrt(n()),
        Min = min(get(v), na.rm = TRUE),
        Max = max(get(v), na.rm = TRUE)
      ) %>%
      mutate(Variable = v)
    
    # Save summary statistics
    FILE2=paste0(LevelFolder,"/","Variable_distribution_",District,"_",Island,"_",Satellite1,"_",Year1,"_",v,"_L",l,"_ALL_SOURCES.csv")
    write.table(summary_stats,FILE2,sep = ";", dec = ".", row.names = FALSE)
    
    # Generate violin plot for current variable -------------------------------------------------------

    print(paste0("Generating violin plot for variable: ", v))
  
    ivar=which(colnames(learning_normalized)==v)
    ihab=which(colnames(learning_normalized)==paste0("Hab_L",l))
   
    # Define y-axis and x-axis limits based on variable type
    ylim_values <- c(v = 1)
    xlim_values <- c(v = 0)
    
    # Set default axis limits if variable is not in predefined list
    default_ylim <- 1
    default_xlim <- 0
    ylim <- ylim_values[v] %>% coalesce(default_ylim)
    xlim <- xlim_values[v] %>% coalesce(default_xlim)
    
    # Violin plots with png format
    NOMpng=paste0(LevelFolder,"/","Variable_distribution_",District,"_",Island,"_",Satellite1,"_",Year1,"_",v,"_L",l,"_ALL_SOURCES.png")
    png(file = NOMpng, width = 1000, height = 600)
    p <- ggplot(learning_normalized, aes_string(x = paste0("Hab_L", l), y = v)) +
      geom_violin(aes_string(fill = paste0("Hab_L", l), color = paste0("Hab_L", l)), position = position_dodge(), draw_quantiles = c(0), show.legend = TRUE) +
      geom_boxplot(width = 0.1, color = "black", fill = "white", position = position_dodge(width = 0.9), show.legend = FALSE) +
      stat_summary(fun = mean, geom = "crossbar", width = 0.1, aes(color = "Mean")) +
      stat_summary(fun = median, geom = "point", size = 2, aes(color = "Median")) +
      scale_color_manual(name = "Statistics", values = c("Mean" = "blue", "Median" = "red")) +
      theme_classic() +
      theme(legend.position = "right", 
            axis.text.x = element_text(size = 13, angle = 90, hjust = 1), 
            axis.text.y = element_text(size = 13), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16), 
            plot.title = element_text(size = 22, face = "bold"), 
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 16, face = "bold")) +
      labs(title = paste("Level ", l, "- Variable :", v), 
           fill = "Habitat classes", x = "Habitat classes", 
           y = paste0("Variable value ", v)) +
      scale_y_continuous(expand = c(0, 0), limits = c(xlim, ylim))
    
    print(p)
    dev.off()
    
    
    # Violin plots with svg format
    NOMsvg=paste0(LevelFolder,"/","Variable_distribution_",District,"_",Island,"_",Satellite1,"_",Year1,"_",v,"_L",l,"_ALL_SOURCES.svg")
    svg(file = NOMsvg)
    print(p)
    dev.off()
    
    
    
  } # End of variable loop


  
} # End of classification level loop
