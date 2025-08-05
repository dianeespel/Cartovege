
# Title : Feature selection with PCA
# Authors : Diane ESPEL
# Objectives : Sort predictive variables with PCA

#----------------------------------------------------------------------------

# R version: R-4.4.2


# Clear environment and graphics ----------------------------------------------------------------------------

rm(list = ls()) # Clear all objects from the R environment to start fresh
graphics.off()   # Close all graphics devices (if any plots are open)


# Load required packages -------------------------------------------------------

library(readr)        # For reading delimited text files
library(FactoMineR)   # For running PCA and multivariate analyses
library(factoextra)   # For enhanced PCA visualization (ggplot2-based)
library(corrplot)     # For correlation matrix visualizations


# Define global variables  ---------------------------------------------------

District='CRO' # 3-letter code for archipelago (e.g. Crozet)
Island='POS'   # 3-letter code for island (e.g. Possession)
Satellite1="Pleiades" # satellite name of multispectral imagery
Year1="2022" # acquisition year of multispectral imagery


# Set working directory -------------------------------------------------------------

# Base local path (customize to your local environment)
#localscratch = paste0("your_local_path/")
localscratch=paste0("/scratch/despel/CARTOVEGE/")

# Path to open input learning dataset
open_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")

# Path to save results from PCA
save_learning_primary_path=paste0(localscratch,"data/Learning_data/PrimaryTypo")


# Load and prepare learning dataset -------------------------------------------------------------

# Open data
FILE1=paste0(open_learning_primary_path,"/","Learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv")
learning_data<- read.csv(FILE1, sep=";",dec=".",stringsAsFactors=FALSE) # `stringsAsFactors=F` ensures character strings don't import as factors
  
# Define Index of variables
imeR=which(colnames(learning_data)=="R") # First raster-derived variable
imeS=which(colnames(learning_data)=="Slope") # Last raster-derived variable

# Standardize variables (mean=0, sd=1)
tab_norm <- as.data.frame(scale(learning_data[,c(imeR:imeS)])) 


# Compute initial correlations  -------------------------------------------------------------

# Compute correlations between variables 
correlation_matrix <- cor(tab_norm) # Pearson correlations
FILE2=paste0(save_learning_primary_path,"/","Correlation_matrix_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES.csv")
write.table(correlation_matrix,FILE2,sep = ";", dec = ",", row.names = FALSE)

# Heatmap of correlations
NOMpng=paste0(save_learning_primary_path,"/","Correlation_matrix_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES.png")
png(file = NOMpng, width = 500, height = 400)
p=corrplot(correlation_matrix, method = "color", type = "upper",
           tl.cex = 0.8, tl.col = "black", addCoef.col = "black",
           number.cex = 0.7, diag = FALSE)
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","Correlation_matrix_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES.svg")
svg(file = NOMsvg)
print(p)
dev.off()

# Apply PCA  -------------------------------------------------------------

res_pca=PCA(tab_norm,  # dateframe with n rows and p columns
            ncp = ncol(tab_norm), # maximum number of dimensions
            graph = F)# don't display the graph


# Eigen values  ------------------------------------------------------------

# Get eigen values
eig.val=as.data.frame(res_pca$eig) # Percentage of variance explained
FILE3=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Eigenvalues.csv")
write.table(eig.val,FILE3,sep = ";", dec = ",", row.names = FALSE)

# Plot of Eigen values
NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Eigenvalues.png")
png(file = NOMpng, width = 500, height = 400)
p=fviz_eig(res_pca,title="Eigen values")+ 
  theme(text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 15))
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Eigenvalues.svg")
svg(file = NOMsvg)
print(p)
dev.off()

# Correlation circles  -------------------------------------------------------------

# Variable graphs PC1 and PC2
NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_PC1-2.png")
png(file = NOMpng, width = 700, height = 700)
p=fviz_pca_var(res_pca, repel = T, #repel to avoid label overlaps
               labelsize=3,pointsize=4)+ 
  ylim (-1,1)+
  xlim(-1,1)+
  theme(text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 17))

print(p)
dev.off()

NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_color_PC1-2.png")
png(file = NOMpng, width = 700, height = 700)
p=fviz_pca_var(res_pca, repel=T, col.var = "contrib",axes=c(1,2),gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               labelsize=3,pointsize=4)+ 
  ylim (-1,1)+
  xlim(-1,1)+
  theme(text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 17))
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_color_PC1-2.svg")
svg(file = NOMsvg)
print(p)
dev.off()


NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_PC1-2.png")
png(file = NOMpng, width = 700, height = 700)
p=plot(res_pca,cex=1,cex.axis=1.8,font.axis=1.5)
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_PC1-2.svg") #svg format to modify into Inkscape
svg(file = NOMsvg)
print(p)
dev.off()



# Variable graphs PC3 and PC4
NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_color_PC3-4.png")
png(file = NOMpng, width = 700, height = 700)
p=fviz_pca_var(res_pca, repel=T, col.var = "contrib",axes=c(3,4),gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               labelsize=3,pointsize=4)+ 
  ylim (-1,1)+
  xlim(-1,1)+
  theme(text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 17))
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_color_PC3-4.svg")
svg(file = NOMsvg)
print(p)
dev.off()

# Individuals graphs-------------------------------------------------------------

# Plot individuals with cos2 coloring
NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Individuals.png")
png(file = NOMpng, width = 700, height = 700)
p=fviz_pca_ind(res_pca, repel=TRUE, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize=5,pointsize=4,ggrepel = TRUE, geom = c("text","point"))+ 
  ylim (-7.5, 7.5)  + xlim(-10,10) +
  theme(text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 15))
print(p)

dev.off()


NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Individuals2.png")
png(file = NOMpng, width = 700, height = 700)
p=plot(res_pca, choix = "ind",ylim=c(-7.5,7.5),xlim=c(-10,10))
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Individuals2.svg") #svg format
svg(file = NOMsvg)
print(p)
dev.off()

# Variable metrics  -------------------------------------------------------------

# Get results for variables metrics (cos2, contribution, etc.)
var=get_pca_var(res_pca) # for both quantitative and qualitative variables


# Variables quality  -------------------------------------------------------------

#All variables quality to dimensions
Var_quality=as.data.frame(var$cos2)
FILE4=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_quality_of_representation.csv")
write.table(Var_quality,FILE4,sep = ";", dec = ",", row.names = FALSE)

NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Corrplot_Quantitative_Variables.png")
png(file = NOMpng, width = 700, height = 700)
corrplot(na.omit(var$cos2), is.corr=FALSE,tl.cex=1.4,tl.col = "black",cl.cex=1.4,cl.align.text="l") # show the link between variables , representation quality and correlations with variables and dimensions
dev.off()


# Compute cumulative Quality of the representation of each variable to the first 4 PCA dimensions 
total_quality=rowSums(var$cos2[, 1:4])
total_quality_df <- data.frame(Variable = names(total_quality),
                               Cumulative_Quality = total_quality)
FILE4bis=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_Total_quality.csv")
write.table(total_quality_df,FILE4bis,sep = ";", dec = ",", row.names = FALSE)


print(total_quality_df)


# Variables contribution  -------------------------------------------------------------

#All variables Contributions to dimensions
Var_contrib=as.data.frame(var$contrib)
Var_contrib$Variable <- rownames(Var_contrib)
FILE5=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_contributions_to_axis.csv")
write.table(Var_contrib,FILE5,sep = ";", dec = ",", row.names = FALSE)

for (dim in seq(1:4)){
  NOMpng=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Variables_contributions_DIM",dim,".png")
  png(file = NOMpng, width = 500, height = 400) # doted line show average value if contributions were uniform 
  p=fviz_contrib(res_pca, "var", axes = dim)+ # Contribution to 1st dimension (DIM1) 
    theme(text = element_text(size = 17),
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 10))
  print(p)
  dev.off()
}


# Compute cumulative contribution of each variable to the first 4 PCA dimensions
total_contrib <- rowSums(res_pca$var$contrib[, 1:4])
total_contrib_df <- data.frame(Variable = names(total_contrib),
                               Cumulative_Contribution = total_contrib)
FILE6=paste0(save_learning_primary_path,"/","PCA_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_AllVariables_Total_contributions.csv")
write.table(total_contrib_df,FILE6,sep = ";", dec = ",", row.names = FALSE)


print(total_contrib_df)



# Variables selection -------------------------------------------------------------

# Preselect variables with quality above threshold 
preselected_vars_with_high_quality <- total_quality_df$Variable[total_quality_df$Cumulative_Quality > 0.5]

# Preselect variables with contribution above threshold 
print("Selecting variables above contribution threshold")
threshold=100 / ncol(tab_norm) # Define contribution threshold (100/number of variables)
preselected_vars_with_high_contribution<- total_contrib_df$Variable[total_contrib_df$Cumulative_Contribution > threshold] # fibd the mostcontributive variables on the 4 PC

# Get variables that are both high quality and high contribution
selected_vars <- intersect(preselected_vars_with_high_quality, preselected_vars_with_high_contribution)


# Compute correlation matrix among selected variables
cor_matrix <- cor(tab_norm[, selected_vars])

NOMpng=paste0(save_learning_primary_path,"/","Correlation_matrix_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Variables_selected.png")
png(file = NOMpng, width = 500, height = 400)
p=corrplot(correlation_matrix, method = "color", type = "upper",
           tl.cex = 0.8, tl.col = "black", addCoef.col = "black",
           number.cex = 0.7, diag = FALSE)
print(p)
dev.off()

NOMsvg=paste0(save_learning_primary_path,"/","Correlation_matrix_",District,"_",Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_Variables_selected.svg")
svg(file = NOMsvg)
print(p)
dev.off()

# Identify highly correlated pairs (|r| > 0.9)
highly_corr_pairs <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)

# Remove redundancy by keeping only one variable per highly correlated pair
vars_to_remove <- c()

for (k in seq_len(nrow(highly_corr_pairs))) {
  i <- rownames(cor_matrix)[highly_corr_pairs[k, 1]]
  j <- colnames(cor_matrix)[highly_corr_pairs[k, 2]]
  
  # Get contribution values
  contrib_i <- total_contrib_df$Cumulative_Contribution[total_contrib_df$Variable == i]
  contrib_j <- total_contrib_df$Cumulative_Contribution[total_contrib_df$Variable == j]
  
  # If equal, compare quality
  if (contrib_i == contrib_j) {
    qual_i <- total_quality_df$Cumulative_Quality[total_quality_df$Variable == i]
    qual_j <- total_quality_df$Cumulative_Quality[total_quality_df$Variable == j]
    
    if (qual_i < qual_j) {
      vars_to_remove <- c(vars_to_remove, i)
    } else {
      vars_to_remove <- c(vars_to_remove, j)
    }
  } else if (contrib_i < contrib_j) {
    vars_to_remove <- c(vars_to_remove, i)
  } else {
    vars_to_remove <- c(vars_to_remove, j)
  }
}

# Final selection (remove duplicates in case of overlap)
final_selected_vars <- setdiff(selected_vars, unique(vars_to_remove))

# Print results
cat("Initial selected variables:", length(selected_vars), "\n")
cat("Removed due to correlation:", length(unique(vars_to_remove)), "\n")
cat("Final selected variables:", length(final_selected_vars), "\n")
print(final_selected_vars)


# Final learning dataset --------------------------------------------------

# Create final dataset with the selected variables
learning_data_selected <- learning_data[, !(colnames(learning_data) %in% vars_to_remove)]
FILE7=paste0(save_learning_primary_path,"/Selected_learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_ALL_SOURCES_EPSG32739.csv")
write.table(learning_data_selected ,file =FILE7, sep = ";", dec = ".", row.names = FALSE)

# Save filtered dataset
true_learning_plots_selected=subset(learning_data_selected,learning_data_selected$Source!="PHOTO-INTERPRETATION")
FILE8=paste0(save_learning_primary_path,"/Selected_learning_plots_", District, "_", Island,"_",Satellite1,"_",Year1,"_TRUE_SOURCES_EPSG32739.csv")
write.table(true_learning_plots_selected,file =FILE8, sep = ";", dec = ".", row.names = FALSE)