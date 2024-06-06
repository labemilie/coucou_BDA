##########################################################################################################

# packages
library(sf)               # For working with spatial data
sf_use_s2(FALSE)
library(elevatr)          # For accessing elevation data
library(raster)
library(rayshader)        # For creating 3D visualizations

# Define borders
countries <- ne_countries(scale = "medium", returnclass = "sf", country = c("Switzerland", "Austria"))

# Get elevation data for both Switzerland and Austria
elevation_data <- get_elev_raster(countries, z = 8)

# Crop the elevation data to the boundaries of Switzerland and Austria
cropped_elevation <- crop(elevation_data, extent(countries))
elevation_data <- mask(cropped_elevation, countries)

# Plot the cropped elevation data
#plot(elevation_data)

# Convert raster to matrix for rayshader
elmat <- raster_to_matrix(elevation_data)
attr(elmat, "extent") <- extent(elevation_data)

# 2D plot
#elmat %>% 
#  sphere_shade(texture = "bw") %>%
#  plot_map()

# 3D plot
#elmat %>% 
#  sphere_shade(texture = "bw") %>%
#  plot_3d(elmat, zscale = 80, fov = 0, theta = 135, zoom = 0.75, 
#          phi = 45, windowsize = c(1500, 800))

# 3D plot with shadow, and water
elmat %>% 
  sphere_shade(texture = "bw") %>%
  add_shadow(cloud_shade(elmat, zscale = 80, start_altitude = 500, end_altitude = 2000), 0) %>%
  add_water(detect_water(elmat), color = "lightblue") %>%
  plot_3d(elmat, zscale = 80, fov = 0, theta = 135, zoom = 0.75,
          phi = 45, windowsize = c(1500, 800))

# 3d plot with occurence data of Species1 and Species2
# Extract elevation values at both species occurrences
coord_full <- subset(matrix_full, select = c("longitude", "latitude"))

  ll_prj <- "EPSG:4326" 
  points <- sp::SpatialPoints(coord_full,
                             proj4string = sp::CRS(SRS_string = ll_prj))
  elevation_points <- raster::extract(elevation_data, points, method = 'bilinear')
  elevation_df <- data.frame(elevation = elevation_points)

# Add elevation data in the matrix full
matrix_full_elev <- data.frame(matrix_full, elevation_points)
view(matrix_full_elev)

# Plot occurrence points
#plot(points)