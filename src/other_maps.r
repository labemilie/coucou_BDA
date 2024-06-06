#####################################################################################################
library(emmeans)
library(corrplot)
#install.packages("dplyr")
library(dplyr)
library(fmsb)

# Basic Statistics 

# CORELATION MATRIX AND CORPLOT
# Load and clean the dataset
df <- matrix_full_elev_eco_clim_ndvi
df <- na.omit(df)  # Remove rows with missing values

# Separate continuous and discrete variables
df_continous <- df[, colnames(df) %in% c("vec_mean_temp", "elevation_points", "NDVI", "vec_mean_precip")]
df_discrete <- df[, !(colnames(df) %in% c("vec_meam_temp", "elevation_points", "NDVI", "vec_mean_precip"))]

# Compute the correlation matrix for continuous variables
mydata.cor <- cor(df_continous)

# Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3)
    # big circle = strong correlation between two variables
    # red = negatively correlated / blue = positively correlated
    # example: + elevation - temperature
    # example: + elevation + precipitations

# Perform Pearson correlation test
data_stat <- matrix_full_elev_eco_clim_ndvi
cor.test(data_stat$vec_mean_temp, data_stat$NDVI)
# positive correlation between temperature and NDVI (vegetation cover)
# the corplot can be used to quickly display correlations between variables 2 by 2.
# Statistical tests could be performed on all these variables: elevation/precip; elevation/temp; elevation/ndvi; precip/temp; temp/ndvi

# Fit a linear model
linear_model <- lm(temp ~ NDVI, data = data_stat)
summary(linear_model)
anova(linear_model)
# positive correlation between temperature and NDVI --> p-value = 2.2e-16 = significant

###########################################################################################################
##### Annotated Heatmap 

### Simple heat map using only numeric values 

# Prepare data for heatmap = organise tous les échantillons par similarités
data <- df_continous
row.names(data) <- c(1:nrow(data))

# Generate a basic heatmap
heatmap(scale(data))

### Advanced heat map with annotation 

## Factor for annotation
my_group <- df_discrete[c("Landcover")]  # Only use "Landcover" for annotation
row.names(my_group) <- c(1:nrow(my_group))

# Generate an advanced heatmap with annotations
pheatmap(scale(data),
         annotation_row = my_group)
# difficult to analyze this giantic plot

#### Customize the heatmap
#install.packages("randomcoloR")
library(randomcoloR)

# Define custom colors for the heatmap
data_col <- grDevices::colorRampPalette(c("black", "darkgreen", "white", "darkred"))

# Display the customized heatmap in a new window

ht <- pheatmap(scale(data),
         annotation_row = my_group,
         cutree_rows = 2,
         cutree_cols = 2,
         cellwidth = 100,
         cellheight = 0.2,
         color = data_col(10))
ht
# the graph indicates that different land cover types exhibit distinct patterns and clusters based on environmental variables
# such as elevation, precipitation, temperature, and NDVI.