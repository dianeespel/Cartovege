
# Title : Estimate nb of plots per habitat class
# Author : Diane ESPEL
# Objectives : Analyse habitat class distribution

#----------------------------------------------------------------------------

# R version: R-4.4.2

# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(ggplot2) # For data visualization


# Create functions ---------------------------------------------------

# Function to add points between character chain
transform_chain <- function(chain) {
  divided_chain <- strsplit(chain, "")[[1]]
  transformed_chain <- paste(divided_chain, collapse = ".")
  return(transformed_chain)
}


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Year1="2022"   # Acquisition year of multispectral imagery
maxTypoLevel=4 # Maximum level of habitat classification

# Set working directory -------------------------------------------------------------

# Local path 
#localHOME = paste0("your_local_path/")
localHOME=paste0("/home/genouest/cnrs_umr6553/despel/CARTOVEGE/")

# path where to open samples data
open_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

# path where to save your results
save_plots_path=paste0(localHOME,"data/vector/Plots/PrimaryTypo")

# Load learning data -------------------------------------------------------------

FILE1=paste0(open_plots_path,"/Quadrats_", District, "_", Island, "_ALL_SOURCES_Centroids_EPSG32739.csv")
learning_points <- read.csv(FILE1, sep=";",dec=".",stringsAsFactors=FALSE) # `stringsAsFactors=F` ensures character strings don't import as factors


# Analyse habitat class distribution  -------------------------------------------------------------

# Define the type of samples plot you have
source_list=c("FIELD","HFI","PHOTO-INTERPRETATION","ALL")

# Run the script for each sample type in plot_list and for each level of classification 
for (s in source_list){
  
  print(paste0("Plot source: ", s))
  if(s=="ALL"){
    learning_points_source=learning_points
  }else{
    learning_points_source=subset(learning_points,learning_points$Source==s)
  }
  
  # Level loop
  for (l in seq (1:maxTypoLevel)){
    
    print(paste0("Working at classication level ", l))
    
    # Create the folder for each level of classification
    LevelFolder=paste0(save_plots_path,"/","Hab_L",l)
    dir.create(LevelFolder,showWarnings=F) # ShowWarnings=F to remove warnings message if file already exists
    
    
    # Compute frequency of habitat types  for level of interest
    ihab=which(colnames(learning_points_source)==paste0("Hab_L",l))
    frequency_hab <- as.data.frame(table(learning_points_source[,ihab]))
    frequency_hab[,1]=as.character(frequency_hab[,1])
    colnames(frequency_hab)=c(paste0("Hab_L",l), "Plot_number")
    
    # Define column of habitat from the frequency_hab dataframe
    Type_habitat=frequency_hab[,1]
    
    # Bar plot (png format)
    NOMpng=paste0(LevelFolder,"/","Nb_quadrats_",District,"_",Island,"_",s,"_SOURCES_level_",l,".png")
    png(file = NOMpng, width = 1000, height = 600)
    p <- ggplot(frequency_hab, aes(x = Type_habitat, y = Plot_number, fill = Type_habitat)) +
      geom_bar(stat = "identity") + # Create a bar plot
      geom_text(aes(label =Plot_number), vjust = -0.5, color = "black", size = 8) + # Add data labels
      labs(title = paste0("Source :",s, " Plot number per habitat class - Level ", l), # Set plot titles and axis labels
           x = "Habitat class",
           y = "Number of plots") +
      theme_classic() + # Apply classic theme
      theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),  # Customize axis labels
            axis.text.y = element_text(size = 13), 
            axis.title.x = element_text(size = 16), 
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 22,face = "bold"),  # Customize plot title
            legend.text = element_text(size = 14),  # Customize legend text
            legend.title = element_text(size = 16,face = "bold")) +  # Customize legend title
      scale_y_continuous(expand = c(0, 0))  +  # Adjust the y-axis range
      labs(fill = "Habitat class")  # Rename the legend title
    print(p)
    dev.off()
    
    # Bar plot (svg format)
    NOMsvg <- paste0(LevelFolder,"/Nb_quadrats_",District,"_",Island,"_",s,"_SOURCES_level_",l,".svg")
    svg(file = NOMsvg)
    print(p)
    dev.off()
    
    # Save frequency of habitat types in a dataframe
    frequency_hab[,1]<- sapply(frequency_hab[,1], transform_chain) # Add points in habitat code
    frequency_hab[,1] <- paste(paste0(District,"-"), frequency_hab[,1], sep = "") # Add CRO- before characters
    write.table(frequency_hab,file=paste0(LevelFolder,"/","Nb_quadrats_",District,"_",Island,"_",s,"_SOURCES_level_",l,".csv"),sep = ";", dec = ".", row.names = FALSE)
    
    
  } #end of level loop
  
} # end of source loop