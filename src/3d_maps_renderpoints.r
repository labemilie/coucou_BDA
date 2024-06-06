#################################################################################################################
# packages

library(sf)
library(elevatr)
library(raster)
library(RColorBrewer)
library(rayshader)
library(eks)
sf_use_s2(FALSE)

# convert elmat from raster to matrix
elmat <- elmat <- raster_to_matrix(elevation_data)

  ###### 3d version 
  elmat %>%
    sphere_shade(texture = "bw") %>%
    add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
    add_water(detect_water(elmat), color = "lightblue") %>%
    plot_3d(elmat, zscale = 80, fov = 0, theta = 135, zoom = 0.75, 
          phi = 45, windowsize = c(1500, 800))

  # Keep this map open to add render points!
    # Render points on the 3D elevation map
    # organise colours by Species1/Species2
  colors <- c("Species1" = "darkgreen", "Species2" = "darkred") 
  render_points(
    extent = extent(countries), size = 8,
    lat = matrix_full$latitude, long = matrix_full$longitude,
    altitude = elevation_points + 100, zscale = 80, color = colors)

# ggplot: Distribution of the two species in relation to elevation
ggplot_elevation <- ggplot(matrix_full_elev, aes(x=elevation_points, fill= species)) +
  geom_density(adjust = 1,alpha=0.5)+
  scale_fill_manual(values=c("darkgreen","darkred","darkblue"))+
  geom_histogram(aes(y=..density..), alpha=0.3, position="identity")+
  theme_classic()
#print(ggplot_elevation)

# There are two distinct groups. Lepus timidus is mainly found between 1500 and 2500m altitude,
# while Lepus europaeus is mainly found below 1000m altitude. However, between 1000 and 2400m altitude, 
# the curves overlap, which means that the two species coexist at the same altitudes in certain places.


 sf_points <- data.frame(
    lat = matrix_full_elev$latitude,
    lon = matrix_full_elev$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
#plot(sf_points)

skde1 <- st_kde(sf_points, gridsize = c(100, 100))
# plot(skde1)
dataxx = st_get_contour(skde1, cont = c(seq(1, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette <- colorRampPalette(c("darkolivegreen4","darkolivegreen3","darkseagreen1","yellow","orange","red","darkred"))

# Define the number of colors in the palette
num_colors <- 20  # Adjust as needed

# Generate the color palette
palette <- color_palette(num_colors)

elmat <- raster_to_matrix(elevation_data)

elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(generate_polygon_overlay(dataxx, 
        palette = palette, linewidth=0,
        extent = extent(elevation_data), heightmap = elmat),
        alphalayer=0.7)  %>%
add_overlay(generate_point_overlay(sf_points, color="black", size=5,
attr(elevation_data,"extent"), heightmap = elmat)) %>%
add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
plot_3d(elmat, zscale = 80, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800)) %>%
plot_map()