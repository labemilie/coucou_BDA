# MAIN SCRIPT

# Which variables best explain Lepus timidus and Lepus europaeus differences in distribution?
# How the distribution of Lepus timidus and Lepus europaeus is changing overtime with climate change?

source("src/import_data.r")
# retrieve occurence data from GBIF and INAT
source("src/filterandmerged.r")             # matrix_full
# Merge all the data into one single matrix
source("src/elevation.r")                   # matrix_full_elev
# Get elevation data for each occurence point
source("src/3d_maps_renderpoints.r")
# Representation of occurence points in a 3D map and Kernel density plot
source("src/ecosystems.r")                  # matrix_full_elev_eco
# Retrieve ecosystem type data
source("src/climaticdata.r")                # matrix_full_elev_eco_clim
# Retrieve temperature and precipitations data
source("src/ndvi.r")                        # matrix_full_elev_eco_clim_ndvi
# Retrieve vegetation cover data
source("src/maps_stats.r") 
# plots and small statistical analysis
source("src/machine_learning.r") 
# machine learning
# source("src/other_maps.r") # other useful plots