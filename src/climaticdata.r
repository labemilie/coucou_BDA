#################################################################################################################
# climatic data

# packages
library(raster)
library(geodata)
library(ggplot2)

#################################################################################################################
#################################################################################################################
# climatic data

# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_elev_eco[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

list_countries <- c("Switzerland", "Austria")

# Retrieve precipitation data for each country
clim_temp_1 <- worldclim_country(list_countries[1], var = "tavg", path = tempdir())
clim_temp_2 <- worldclim_country(list_countries[2], var = "tavg", path = tempdir())

clim_temp_tot <- merge(clim_temp_1, clim_temp_2)
clim_temp_brick <- brick(clim_temp_tot)
#plot(clim_temp_tot)

# create matrix that will be added to the matrix full
matrix_temp = NULL
vec_colnames1 = NULL

# BOUCLE
# Create a loop to have data in from January to September
for (i in 1:12)
{
raster_temp_avg <- as(clim_temp_brick[[i]], "Raster")
vec_colnames1 <- c(vec_colnames1, names(raster_temp_avg))

temp <- raster::extract(raster_temp_avg, spatial_points, method = 'bilinear')
matrix_temp <- cbind(matrix_temp, temp) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_temp) <- vec_colnames1

# Make the mean of these annual value and store it into the matrix
vec_mean_temp <- as.vector(rowMeans(matrix_temp))
matrix_temp <- data.frame(matrix_temp, vec_mean_temp)

# Control that there is the good data
#View(matrix_temp)

# Plot the distribution of temperature values across the sampled locations
# Adjust = 3
ggplot(matrix_temp, aes(x = vec_mean_temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()
#View(matrix_temp)
#####################################################################################################
#####################################################################################################
# precipitation data
# Retrieve precipitation data (same that for temperature before)
# Retrieve precipitation data for each country
clim_precip_1 <- worldclim_country(list_countries[1], var = "prec", path = tempdir())
clim_precip_2 <- worldclim_country(list_countries[2], var = "prec", path = tempdir())

clim_precip_tot <- merge(clim_precip_1, clim_precip_2)
clim_precip_brick <- brick(clim_precip_tot)

matrix_precip = NULL
vec_colnames2 = NULL

# Create a loop to have data in from January to September
for (i in 1:12)
{
raster_precip <- as(clim_precip_brick[[i]], "Raster")
vec_colnames2 <- c(vec_colnames2, names(raster_precip))

precip <- raster::extract(raster_precip, spatial_points, method = 'bilinear')
matrix_precip <- cbind(matrix_precip, precip) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_precip) <- vec_colnames2

# Make the mean of these annual value and store it into the matrix
vec_mean_precip <- as.vector(rowMeans(matrix_precip))
matrix_precip <- data.frame(matrix_precip, vec_mean_precip)

# Plot density of precipitation data for species occurrences
ggplot(matrix_precip, aes(x = vec_mean_precip)) +
  geom_density(color = "black", fill = "lightblue", adjust = 2) +
  theme_bw()

#############################################################################################
# Add data to the matrix full
matrix_full_elev_eco_clim <- data.frame(matrix_full_elev_eco, matrix_temp, matrix_precip)
view(matrix_full_elev_eco_clim)

#############################################################################################
# Create ggplots of the different possible combination

#
ggplot_temp <- ggplot(matrix_full_elev_eco_clim, aes(x=vec_mean_temp, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
#print(ggplot_temp)

#
ggplot_precip <- ggplot(matrix_full_elev_eco_clim, aes(x=vec_mean_precip, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
#print(ggplot_precip)

#
ggplot_climatic <- ggplot(matrix_full_elev_eco_clim, aes(x = vec_mean_precip,y=vec_mean_temp, color = species)) +
  geom_point() +
  theme_minimal()
#print(ggplot_climatic)