##############################################################################################
# ecosystems

# packages
library(raster)
library(ggplot2)
library(rnaturalearth)
library(sf)
sf_use_s2(FALSE)

# Set the file path to your GeoTIFF
file_path <- "C:/Users/emili/Documents/UniNE/Master/SemestrePrintemps/BiodiversityDataAnalysis/Project/Data/WorldEcosystem.tif"

# Read the raster GeoTIFF
ecosystem_raster <- raster(file_path)
countries <- ne_countries(scale = "medium", returnclass = "sf", country = c("Switzerland", "Austria"))

## crop and mask
r2 <- crop(ecosystem_raster, extent(countries))
ecosystem <- mask(r2, countries)
#plot(ecosystem)

################################################################################################################
# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_elev[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
#plot(spatial_points,add=T,pch=16,cex= 1)

# Extract values
eco_values <- raster::extract(ecosystem, spatial_points)
matrix_full_elev$eco_values <- eco_values

##################################################################################################################################
##################################################################################################################################
# metadata
metadat_eco <- read.delim("C:/Users/emili/Documents/UniNE/Master/SemestrePrintemps/BiodiversityDataAnalysis/Project/Data/WorldEcosystem.metadata.tsv")

# Transform ecosystem "code" into ecosystem "name"
matrix_full_elev_eco <- merge(matrix_full_elev, metadat_eco, by.x="eco_values", by.y="Value", all.x =T)
matrix_full_elev_eco <- subset(matrix_full_elev_eco, select = -c(color, Blue, Red, Climate_Re, Landforms, Moisture))
matrix_full_elev_eco <- na.omit(matrix_full_elev_eco)
view(matrix_full_elev_eco)

#
ggplot_ecosystem <- ggplot(matrix_full_elev_eco, aes(x=as.factor(Landcover), fill=species)) +
    geom_bar(position="dodge") +
    labs(
        title="Count of observations of Each species by land properties",
        x="Climate",
        y="Count of observations") + 
    theme(plot.title = element_text(size=25, face="bold"),        # Title text size
          axis.title.x = element_text(size=25, face="bold"),      # X-axis title text size
          axis.title.y = element_text(size=25, face="bold"),      # Y-axis title text size
          axis.text.x = element_text(face="bold", size=25, angle=45),  # X-axis text size
          axis.text.y = element_text(size=25, face="bold")        # Y-axis text size
    )
#print(ggplot_ecosystem)